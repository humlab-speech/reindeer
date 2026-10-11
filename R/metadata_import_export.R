#' Raw per-level metadata as a wide, typed table (one row per scope)
#' @noRd
.raw_level_wide <- function(con, sql, db_uuid, keys, scopes) {
  long <- DBI::dbGetQuery(con, sql, params = list(db_uuid))
  out <- as.data.frame(scopes, stringsAsFactors = FALSE)
  if (nrow(long) == 0L) return(out)
  types <- tapply(long$field_type, long$field_name, .resolve_field_type)
  wide <- as.data.frame(data.table::dcast(
    data.table::as.data.table(long), stats::as.formula(paste(paste(keys, collapse = " + "), "~ field_name")),
    value.var = "field_value", fun.aggregate = function(x) x[1]))
  for (f in setdiff(names(wide), keys)) {
    wide[[f]] <- switch(types[[f]],
      numeric = as.numeric(wide[[f]]), integer = as.integer(wide[[f]]),
      logical = as.logical(wide[[f]]), wide[[f]])
  }
  merge(out, wide, by = keys, all.x = TRUE, sort = FALSE)[, c(keys, setdiff(names(wide), keys)), drop = FALSE]
}

#' Export metadata to Excel file
#'
#' Creates a multi-sheet Excel workbook with bundle, session, and database
#' metadata. Requires the \pkg{openxlsx} package.
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Output path for the Excel file
#' @param overwrite Overwrite existing file (default: FALSE)
#' @param mandatory Character vector of metadata columns to always include,
#'   even if empty (default: \code{c("Age", "Gender")})
#' @return A data.frame of bundle metadata, invisibly
#'
#' @examplesIf requireNamespace("openxlsx", quietly = TRUE)
#' xlsx <- tempfile(fileext = ".xlsx")
#' export_metadata(demo_corpus(), xlsx)
#' openxlsx::getSheetNames(xlsx)
#' @export
export_metadata <- function(corpus_obj, Excelfile, overwrite = FALSE, 
                           mandatory = c("Age", "Gender")) {
  
  if (!overwrite && file.exists(Excelfile)) {
    cli::cli_abort("File {.path {Excelfile}} exists. Use overwrite=TRUE to replace.")
  }
  
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  
  # RAW bundle-level values only: inherited (session/database) values live on
  # their own sheets, so the workbook round-trips through import_metadata()
  # without copying inherited values down to every bundle.
  bundle_metadata <- .raw_level_wide(
    con, "SELECT session, bundle, field_name, field_value, field_type FROM metadata_bundle WHERE db_uuid = ?",
    db_uuid, c("session", "bundle"),
    DBI::dbGetQuery(con, "SELECT session, name AS bundle FROM bundle WHERE db_uuid = ? ORDER BY session, name",
                    params = list(db_uuid)))
  
  # Ensure mandatory columns exist
  for (col in mandatory) {
    if (!col %in% names(bundle_metadata)) {
      bundle_metadata[[col]] <- NA
    }
  }
  
  session_metadata <- .raw_level_wide(
    con, "SELECT session, field_name, field_value, field_type FROM metadata_session WHERE db_uuid = ?",
    db_uuid, "session", data.frame(session = list_sessions_from_cache(con, db_uuid)$name))

  # Ensure mandatory columns in session metadata
  for (col in mandatory) {
    if (!col %in% names(session_metadata)) {
      session_metadata[[col]] <- NA
    }
  }
  
  # Get database-level metadata
  db_metadata <- DBI::dbGetQuery(con,
    "SELECT field_name, field_value FROM metadata_database WHERE db_uuid = ?",
    params = list(db_uuid)
  )
  
  if (nrow(db_metadata) > 0) {
    db_df <- as.data.frame(t(db_metadata$field_value))
    names(db_df) <- db_metadata$field_name
  } else {
    db_df <- data.frame()
  }
  
  # Create Excel workbook
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook(paste(corpus_obj@dbName, "metadata"))
    
    # Bundles sheet
    openxlsx::addWorksheet(wb, "bundles")
    openxlsx::writeDataTable(wb, "bundles", x = bundle_metadata, keepNA = FALSE, withFilter = FALSE)
    openxlsx::freezePane(wb, "bundles", firstActiveCol = 3)
    openxlsx::setColWidths(wb, "bundles", cols = 3:30, widths = 18)
    
    # Sessions sheet
    openxlsx::addWorksheet(wb, "sessions")
    openxlsx::writeDataTable(wb, "sessions", x = session_metadata, keepNA = FALSE, withFilter = FALSE)
    openxlsx::freezePane(wb, "sessions", firstActiveCol = 2)
    openxlsx::setColWidths(wb, "sessions", cols = 2:30, widths = 18)
    
    # Database sheet
    openxlsx::addWorksheet(wb, "database")
    if (nrow(db_df) > 0) {
      openxlsx::writeDataTable(wb, "database", x = db_df, keepNA = FALSE, withFilter = FALSE)
    } else {
      openxlsx::writeComment(wb, "database", col = 1, row = 1,
        openxlsx::createComment(
          "Set database-wide metadata by adding column headers and values",
          author = "Reindeer"
        ))
    }
    
    openxlsx::saveWorkbook(wb, file = Excelfile, overwrite = overwrite)
    cli::cli_alert_success("Metadata exported to {.path {Excelfile}}")
  } else {
    cli::cli_abort("Package {.pkg openxlsx} required for Excel export")
  }
  
  invisible(bundle_metadata)
}

#' Import metadata from Excel file
#'
#' Reads bundle, session, and database metadata from an Excel workbook
#' (as created by \code{\link{export_metadata}}) and writes it back to
#' the corpus JSON files and SQLite cache.
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Path to Excel file to import
#' @return The corpus object, invisibly
#'
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' import_metadata(corp, "corpus_metadata.xlsx")
#'
#' @export
import_metadata <- function(corpus_obj, Excelfile) {
  .deprecated_alias("import_metadata", "set_metadata", parent.frame())
  
  if (!file.exists(Excelfile)) {
    cli::cli_abort("File {.path {Excelfile}} does not exist")
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg openxlsx} required for Excel import")
  }
  
  # Read all sheets
  bundle_meta <- openxlsx::read.xlsx(Excelfile, sheet = "bundles", detectDates = TRUE)
  session_meta <- suppressWarnings(
    openxlsx::read.xlsx(Excelfile, sheet = "sessions", detectDates = TRUE))
  
  tryCatch({
    db_meta <- suppressWarnings(  # "No data found" = empty sheet, which is fine
      openxlsx::read.xlsx(Excelfile, sheet = "database", detectDates = TRUE))
  }, error = function(e) {
    db_meta <<- data.frame()
  })
  
  if (is.null(db_meta)) db_meta <- data.frame()
  if (is.null(session_meta)) session_meta <- data.frame(session = character())
  if (is.null(bundle_meta)) bundle_meta <- data.frame(session = character(), bundle = character())

  # Plan every write first and validate the whole workbook: a bad row must
  # abort before the first file is touched (all-or-nothing).
  ops <- list()
  clean <- function(df, drop) {
    lst <- as.list(df[, !names(df) %in% drop, drop = FALSE])
    lst[!vapply(lst, function(v) length(v) == 0L || is.na(v[1]), logical(1))]
  }
  for (i in seq_len(nrow(bundle_meta))) {
    ops[[length(ops) + 1L]] <- list(
      level = "bundle", session = bundle_meta$session[i], bundle = bundle_meta$bundle[i],
      meta = clean(bundle_meta[i, , drop = FALSE], c("session", "bundle")))
  }
  for (i in seq_len(nrow(session_meta))) {
    ops[[length(ops) + 1L]] <- list(
      level = "session", session = session_meta$session[i], bundle = NULL,
      meta = clean(session_meta[i, , drop = FALSE], "session"))
  }
  if (nrow(db_meta) > 0) {
    ops[[length(ops) + 1L]] <- list(level = "database", session = NULL, bundle = NULL,
                                    meta = clean(db_meta[1, , drop = FALSE], character()))
  }
  ops <- Filter(function(o) length(o$meta) > 0, ops)
  .validate_import_plan(corpus_obj, ops, bundle_meta, session_meta, Excelfile)

  # Bulk import: suppress per-row FAIR regeneration, then regenerate once at
  # the end so hundreds of add_metadata() calls don't each rewrite artifacts.
  old_suppress <- getOption("reindeer._auto_cmdi_suppress", FALSE)
  options(reindeer._auto_cmdi_suppress = TRUE)
  on.exit(options(reindeer._auto_cmdi_suppress = old_suppress), add = TRUE)

  for (o in ops) {
    add_metadata(corpus_obj, o$meta, session = o$session, bundle = o$bundle)
  }

  # Rebuild cache
  cli::cli_alert_info("Rebuilding metadata cache...")
  gather_metadata(corpus_obj, verbose = FALSE)

  # Restore auto-regeneration and emit FAIR artifacts once for the whole batch.
  options(reindeer._auto_cmdi_suppress = old_suppress)
  .auto_describe(corpus_obj)

  cli::cli_alert_success("Metadata imported from {.path {Excelfile}}")

  invisible(corpus_obj)
}

#' Check a planned Excel import against the corpus before anything is written
#' @noRd
.validate_import_plan <- function(corpus_obj, ops, bundle_meta, session_meta, file) {
  fail <- function(msg) {
    cli::cli_abort(c("Cannot import {.path {file}}; nothing was written.", "x" = msg),
                   class = c("reindeer_metadata_error", "reindeer_error"))
  }
  if (!all(c("session", "bundle") %in% names(bundle_meta))) {
    fail("Sheet {.val bundles} needs {.field session} and {.field bundle} columns.")
  }
  if (!"session" %in% names(session_meta)) {
    fail("Sheet {.val sessions} needs a {.field session} column.")
  }
  con <- get_corpus_connection(corpus_obj)
  known_b <- list_bundles_from_cache(con, corpus_obj@.uuid)
  known_s <- unique(known_b$session)
  for (o in ops) {
    if (o$level != "database") {
      if (is.na(o$session) || !o$session %in% known_s) {
        fail("Unknown session {.val {o$session}}.")
      }
    }
    if (o$level == "bundle" &&
        !any(known_b$session == o$session & known_b$name == o$bundle)) {
      fail("Unknown bundle {.val {o$session}/{o$bundle}}.")
    }
    .validate_against_schema(.jsonable_metadata(o$meta), "metadata.schema.json",
                             file_path = file, write = TRUE)
  }
  invisible(TRUE)
}

#' @param reset.before.add Deprecated spelling of `reset`.
#' @export
#' @rdname set_metadata
add_metadata <- function(corpus_obj, metadataList, session = NULL, bundle = NULL,
                        reset.before.add = FALSE) {
  .deprecated_alias("add_metadata", "set_metadata", parent.frame())

  if (!is.null(bundle) && is.null(session)) {
    cli::cli_abort("Bundle requires session")
  }

  level <- if (is.null(session)) "database"
           else if (is.null(bundle)) "session"
           else "bundle"

  if (reset.before.add) {
    clear_metadata(corpus_obj, session, bundle, level)
  }

  # Route through the bracket-assignment dispatcher so there is exactly one
  # write path for metadata. Literal names are anchored as regex so they
  # do not accidentally match other sessions/bundles.
  to_pattern <- function(s) {
    if (is.null(s)) return(NULL)
    paste0("^", gsub("([\\.\\*\\+\\?\\^\\$\\{\\}\\(\\)\\|\\[\\]\\\\])",
                     "\\\\\\1", s, perl = TRUE), "$")
  }

  corpus_assign_metadata(
    corpus_obj,
    session_pattern = to_pattern(session),
    bundle_pattern = to_pattern(bundle),
    metadata_list = metadataList
  )

  # Keep standard-compliant artifacts in sync automatically as the user works
  # (drift-guarded; no-op when suppressed during bulk edits).
  .auto_describe(corpus_obj)

  invisible(corpus_obj)
}

#' Hash of the metadata state that feeds the FAIR artifacts
#'
#' Used by [describe_corpus()]'s drift guard (the only gate: regeneration is
#' skipped when this is unchanged since the last emission). Hashes the stored
#' per-level metadata rows and the bundle list directly, which is far cheaper
#' than pivoting the resolved metadata table.
#' @keywords internal
#' @noRd
.metadata_state_hash <- function(corpus_obj) {
  con <- get_corpus_connection(corpus_obj)
  uuid <- corpus_obj@.uuid
  q <- function(sql) {
    DBI::dbGetQuery(con, sql, params = list(uuid))
  }
  state <- tryCatch(list(
    name = corpus_obj@dbName,
    database = q("SELECT field_name, field_value, field_type FROM metadata_database WHERE db_uuid = ? ORDER BY field_name"),
    session = q("SELECT session, field_name, field_value, field_type FROM metadata_session WHERE db_uuid = ? ORDER BY session, field_name"),
    bundle = q("SELECT session, bundle, field_name, field_value, field_type FROM metadata_bundle WHERE db_uuid = ? ORDER BY session, bundle, field_name"),
    bundles = q("SELECT session, name FROM bundle WHERE db_uuid = ? ORDER BY session, name")),
    error = function(e) list(name = corpus_obj@dbName))
  digest::digest(state)
}

#' Automatically regenerate FAIR artifacts after a metadata change
#'
#' The mechanism that makes standard-compliant corpus metadata "created
#' automatically while the user works". Gated by
#' `getOption("reindeer.auto_cmdi", TRUE)` (default on; opt out with
#' `options(reindeer.auto_cmdi = FALSE)`) and suppressible
#' during bulk edits via `getOption("reindeer._auto_cmdi_suppress", FALSE)`.
#' A failure to emit FAIR artifacts must never break the metadata write, so
#' errors are swallowed. The drift guard in [describe_corpus()] keeps this
#' cheap and idempotent.
#' @keywords internal
#' @noRd
.auto_describe <- function(corpus_obj) {
  if (!isTRUE(getOption("reindeer.auto_cmdi", TRUE))) return(invisible(FALSE))
  if (isTRUE(getOption("reindeer._auto_cmdi_suppress", FALSE))) return(invisible(FALSE))
  tryCatch(describe_corpus(corpus_obj, verbose = FALSE), error = function(e) {
    cli::cli_warn("Auto-CMDI regeneration failed (the metadata write succeeded): {conditionMessage(e)}",
                  class = c("reindeer_cmdi_warning", "reindeer_warning"))
  })
  invisible(TRUE)
}

#' Clear metadata at a specific level
#' @keywords internal
#' @noRd
clear_metadata <- function(corpus_obj, session, bundle, level) {
  basePath <- corpus_obj@basePath

  # Keep the cache in step with the file we are about to delete.
  con <- get_corpus_connection(corpus_obj)
  uuid <- corpus_obj@.uuid
  switch(level,
    database = DBI::dbExecute(con, "DELETE FROM metadata_database WHERE db_uuid = ?",
                              params = list(uuid)),
    session = DBI::dbExecute(con,
      "DELETE FROM metadata_session WHERE db_uuid = ? AND session = ?",
      params = list(uuid, session)),
    bundle = DBI::dbExecute(con,
      "DELETE FROM metadata_bundle WHERE db_uuid = ? AND session = ? AND bundle = ?",
      params = list(uuid, session, bundle)))
  
  if (level == "database") {
    # Delete METADATA.json from database root
    db_name <- basename(basePath)
    db_name <- sub("_emuDB$", "", db_name)
    meta_file <- file.path(basePath, metadata.filename)
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }

  } else if (level == "session") {
    # Delete METADATA.json from session directory
    meta_file <- file.path(basePath, paste0(session, "_ses"), metadata.filename)
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }

  } else if (level == "bundle") {
    # Delete METADATA.json from bundle directory
    meta_file <- file.path(
      basePath,
      paste0(session, "_ses"),
      paste0(bundle, "_bndl"),
      metadata.filename
    )
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }
  }
}

#' Add speaker and recording metadata to segments
#'
#' Looks up the metadata of each segment's bundle (with session and corpus
#' defaults resolved, see [set_metadata()]) and appends it as columns.
#'
#' @param segs_tbl A `segment_list`, `lazy_segment_list` or data frame with
#'   `session` and `bundle` columns. A lazy list stays lazy: the join happens
#'   at [collect()].
#' @param corpus_obj A [corpus()]. Default: the corpus the segments came from.
#' @param compute_digests Also store a checksum of each media file as metadata
#'   (default `FALSE`).
#' @param algorithm Checksum algorithm, e.g. `"sha1"` (default).
#' @return The input with one extra column per metadata field (for example
#'   `Age` and `Gender`), in the same row order. A `segment_list` stays a
#'   `segment_list`; a field already present in the input gets the suffix
#'   `.y` (and the input's own column `.x`).
#' @family metadata
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' segs <- query(demo_corpus(), "Phonetic == t") |> collect()
#' biographize(segs)
#'
#' @export
biographize <- function(segs_tbl, corpus_obj = NULL, compute_digests = FALSE, algorithm = "sha1") {

  # The corpus a segment list came from is recoverable from it.
  if (is.null(corpus_obj)) {
    corpus_obj <- if (S7::S7_inherits(segs_tbl, lazy_segment_list) && !is.null(segs_tbl@corpus)) {
      segs_tbl@corpus
    } else if (S7::S7_inherits(segs_tbl, lazy_segment_list)) {
      get_corpus_cached(segment_list(data.frame(), db_path = dirname(segs_tbl@db_path)))
    } else {
      get_corpus_cached(segs_tbl)
    }
  }

  # Lazy input: defer the metadata join until collect(), keeping the chain
  # lazy (query |> quantify |> biographize |> collect). The collect() loop
  # replays this spec against the materialized segment_list.
  if (S7::S7_inherits(segs_tbl, lazy_segment_list)) {
    spec <- list(type = "biographize", corpus_obj = corpus_obj,
                 args = list(compute_digests = compute_digests, algorithm = algorithm))
    return(invisible(.fork_lazy(segs_tbl, "post", spec)))
  }

  if (!is.data.frame(segs_tbl) || !all(c("session", "bundle") %in% names(segs_tbl))) {
    cli::cli_abort("Input must be a data.frame with 'session' and 'bundle' columns")
  }
  
  if (compute_digests) {
    add_digests(corpus_obj, algorithm = algorithm)
  }
  
  # Get metadata
  metadata <- get_metadata(corpus_obj)
  
  # Left join that keeps the input row order (merge() sorts by the keys).
  idx <- match(paste(segs_tbl$session, segs_tbl$bundle, sep = "\x01"),
               paste(metadata$session, metadata$bundle, sep = "\x01"))
  meta <- as.data.frame(metadata)[idx, setdiff(names(metadata), c("session", "bundle")), drop = FALSE]
  x <- as.data.frame(segs_tbl)
  clash <- intersect(names(x), names(meta))
  names(x)[names(x) %in% clash] <- paste0(names(x)[names(x) %in% clash], ".x")
  names(meta)[names(meta) %in% clash] <- paste0(names(meta)[names(meta) %in% clash], ".y")
  rownames(meta) <- NULL
  result <- cbind(x, meta)

  # Preserve S7 class (and db_uuid/db_path) after join
  if (S7::S7_inherits(segs_tbl, segment_list)) {
    result <- extended_segment_list(
      data = as.data.frame(result),
      db_uuid = segs_tbl@db_uuid,
      db_path = segs_tbl@db_path,
      dsp_function = if (is_extended_segment_list(segs_tbl)) segs_tbl@dsp_function else "",
      dsp_columns = if (is_extended_segment_list(segs_tbl)) segs_tbl@dsp_columns else character(0)
    )
  }

  if (S7::S7_inherits(segs_tbl, segment_list)) {
    result <- .record_step(result, segs_tbl, "biographize", sys.call(-1L))
  }

  result
}

#' Add file digests to metadata
#'
#' Computes checksums for signal files and stores them as metadata.
#'
#' @param corpus_obj A corpus object
#' @param sessionPattern Regex pattern to filter sessions (default: ".*")
#' @param bundlePattern Regex pattern to filter bundles (default: ".*")
#' @param algorithm Hash algorithm (default: "sha1")
#' @return The corpus object, invisibly
#' @keywords internal
#' @noRd
add_digests <- function(corpus_obj, sessionPattern = ".*", bundlePattern = ".*",
                       algorithm = "sha1") {
  
  # This would require emuR functions to list files
  # Simplified version for now
  cli::cli_alert_info("Digest computation requires emuR integration")
  invisible(corpus_obj)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get database UUID from corpus object
#' @keywords internal
#' @noRd
get_db_uuid <- function(corpus_obj) corpus_obj@.uuid

#' Get connection from corpus object (alias for get_corpus_connection)
#' @keywords internal
#' @noRd
get_connection <- function(corpus_obj) {
  get_corpus_connection(corpus_obj)
}

#' List sessions from cache
#' @keywords internal
#' @noRd
list_sessions_from_cache <- function(con, db_uuid) {
  DBI::dbGetQuery(con,
    "SELECT name FROM session WHERE db_uuid = ?",
    params = list(db_uuid)
  )
}

#' List bundles from cache
#' @keywords internal
#' @noRd
list_bundles_from_cache <- function(con, db_uuid) {
  DBI::dbGetQuery(con,
    "SELECT session, name FROM bundle WHERE db_uuid = ?",
    params = list(db_uuid)
  )
}
