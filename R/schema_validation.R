# ==============================================================================
# JSON-Schema validation for _DBconfig.json + METADATA.json (Item 3)
# ==============================================================================
#
# Schemas live under inst/schemas/. Validation runs at read time
# (load_DBconfig, gather_metadata read paths) and at write time
# (store_DBconfig, set_metadata_*). Default mode is soft-warn so existing
# corpora keep loading; opt in to hard errors with
# `options(reindeer.schema_strict = TRUE)`.

#' @keywords internal
.schema_path <- function(name) {
  base <- getOption("reindeer.schema_dir", default = NULL)
  if (is.null(base) || !nzchar(base)) {
    base <- system.file("schemas", package = "reindeer")
  }
  if (!nzchar(base) || !dir.exists(base)) {
    return(NA_character_)
  }
  file.path(base, name)
}

#' @keywords internal
.schema_validators <- new.env(parent = emptyenv())

#' @keywords internal
.get_schema_validator <- function(schema_name) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    return(NULL)
  }
  if (exists(schema_name, envir = .schema_validators, inherits = FALSE)) {
    return(get(schema_name, envir = .schema_validators))
  }
  schema_file <- .schema_path(schema_name)
  if (is.na(schema_file) || !file.exists(schema_file)) {
    return(NULL)
  }
  v <- jsonvalidate::json_validator(schema_file, engine = "imjv")
  assign(schema_name, v, envir = .schema_validators)
  v
}

#' @keywords internal
.format_schema_errors <- function(errs) {
  if (is.null(errs) || (is.data.frame(errs) && nrow(errs) == 0L)) {
    return("(no detail)")
  }
  if (is.data.frame(errs)) {
    msgs <- character(nrow(errs))
    for (i in seq_len(nrow(errs))) {
      field <- if ("dataPath" %in% names(errs)) errs$dataPath[i]
               else if ("instancePath" %in% names(errs)) errs$instancePath[i]
               else if ("field" %in% names(errs)) errs$field[i]
               else "<root>"
      msg <- if ("message" %in% names(errs)) errs$message[i] else "invalid"
      msgs[i] <- paste0(field, ": ", msg)
    }
    return(msgs)
  }
  as.character(errs)
}

#' @keywords internal
.handle_schema_failure <- function(file_path, schema_name, errs, write = FALSE) {
  bullets <- .format_schema_errors(errs)
  bullets <- stats::setNames(bullets, rep("x", length(bullets)))
  ctx <- if (is.null(file_path)) "<inline>" else file_path
  strict <- isTRUE(getOption("reindeer.schema_strict", FALSE)) || isTRUE(write)
  if (strict) {
    .schema_abort(c(
      "Schema validation failed for {.path {ctx}} ({schema_name})",
      bullets
    ))
  } else {
    cli::cli_warn(c(
      "Schema validation issues in {.path {ctx}} ({schema_name})",
      bullets,
      "i" = "Set {.code options(reindeer.schema_strict = TRUE)} to make this an error."
    ))
  }
}

#' @keywords internal
.validate_against_schema <- function(json_text_or_obj, schema_name,
                                     file_path = NULL, write = FALSE) {
  v <- .get_schema_validator(schema_name)
  if (is.null(v)) return(invisible(TRUE))   # no validator available

  json_text <- if (is.character(json_text_or_obj)) {
    paste(json_text_or_obj, collapse = "\n")
  } else {
    jsonlite::toJSON(json_text_or_obj, auto_unbox = TRUE, null = "null", na = "null")
  }
  ok <- tryCatch(
    v(json_text, verbose = TRUE, greedy = TRUE),
    error = function(e) {
      structure(FALSE, errors = conditionMessage(e))
    }
  )
  if (isTRUE(ok)) return(invisible(TRUE))
  errs <- attr(ok, "errors")
  .handle_schema_failure(file_path, schema_name, errs, write = write)
  invisible(FALSE)
}

#' Validate a corpus's JSON files against reindeer schemas
#'
#' Walks the corpus and reports schema-validation issues for the database
#' `_DBconfig.json` and every `METADATA.json` under it. Returns a tibble
#' summarising files validated and any issues found.
#'
#' Validation is soft-warn by default (existing non-conformant files emit a
#' `cli` warning rather than aborting). Set
#' `options(reindeer.schema_strict = TRUE)` to make any issue an error.
#'
#' @param corpus_obj A `corpus` object.
#' @return A tibble with columns `file`, `schema`, `level`, `ok`.
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' validate_corpus(corp)
#' @export
validate_corpus <- function(corpus_obj) {
  if (!S7::S7_inherits(corpus_obj, corpus)) {
    .schema_abort("Input must be a {.cls corpus} object")
  }
  base_path <- corpus_obj@basePath
  db_name <- corpus_obj@dbName
  results <- list()

  # _DBconfig.json
  config_file <- file.path(base_path, paste0(db_name, "_DBconfig.json"))
  if (file.exists(config_file)) {
    txt <- paste(readLines(config_file, warn = FALSE), collapse = "\n")
    ok <- isTRUE(.validate_against_schema(txt, "dbconfig.schema.json",
                                          file_path = config_file))
    results[[length(results) + 1L]] <- tibble::tibble(
      file = config_file, schema = "dbconfig.schema.json",
      level = "database", ok = ok
    )
  }

  # METADATA.json at all three levels
  db_meta <- file.path(base_path, "METADATA.json")
  if (file.exists(db_meta)) {
    txt <- paste(readLines(db_meta, warn = FALSE), collapse = "\n")
    ok <- isTRUE(.validate_against_schema(txt, "metadata.schema.json",
                                          file_path = db_meta))
    results[[length(results) + 1L]] <- tibble::tibble(
      file = db_meta, schema = "metadata.schema.json",
      level = "database", ok = ok
    )
  }

  con <- get_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  uuid <- get_db_uuid(corpus_obj)

  sessions <- list_sessions_from_cache(con, uuid)
  for (s in sessions$name) {
    f <- file.path(base_path, paste0(s, "_ses"), "METADATA.json")
    if (file.exists(f)) {
      txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
      ok <- isTRUE(.validate_against_schema(txt, "metadata.schema.json",
                                            file_path = f))
      results[[length(results) + 1L]] <- tibble::tibble(
        file = f, schema = "metadata.schema.json",
        level = "session", ok = ok
      )
    }
  }

  bundles <- list_bundles_from_cache(con, uuid)
  for (i in seq_len(nrow(bundles))) {
    f <- file.path(base_path,
                   paste0(bundles$session[i], "_ses"),
                   paste0(bundles$name[i], "_bndl"),
                   "METADATA.json")
    if (file.exists(f)) {
      txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
      ok <- isTRUE(.validate_against_schema(txt, "metadata.schema.json",
                                            file_path = f))
      results[[length(results) + 1L]] <- tibble::tibble(
        file = f, schema = "metadata.schema.json",
        level = "bundle", ok = ok
      )
    }
  }

  if (length(results) == 0L) {
    return(tibble::tibble(
      file = character(), schema = character(),
      level = character(), ok = logical()
    ))
  }
  do.call(rbind, results)
}
