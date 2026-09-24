#' @include corpus_class.R segment_list_quantify.R reindeer_corpus_config.R
NULL

# ==============================================================================
# quantify.corpus: corpus-wide DSP materialization + ssffTrackDefinitions
# provenance. See specs/2026-09-24-quantify-unification-design.md.
# ==============================================================================

#' Resolve a DSP function's identity (name/package/version) for provenance
#'
#' `dsp_fun_name` is the caller's deparsed expression (e.g.
#' `"superassp::trk_formant_forest"` or `"trk_formant_forest"` if the
#' package was attached). A qualified expression is parsed directly;
#' a bare name falls back to the function's own defining namespace.
#'
#' @param dsp_fun The evaluated DSP function.
#' @param dsp_fun_name Character; the caller's deparsed expression.
#' @return List with `function`, `package` (`NA` if unresolvable),
#'   `version` (`NA` if `package` is `NA` or has no installed version).
#' @noRd
.resolve_dsp_identity <- function(dsp_fun, dsp_fun_name) {
  qualified <- regmatches(
    dsp_fun_name,
    regexec("^([[:alnum:].]+)::([[:alnum:]._]+)$", dsp_fun_name)
  )[[1]]

  if (length(qualified) == 3) {
    pkg <- qualified[2]
    fn_name <- qualified[3]
  } else {
    fn_name <- dsp_fun_name
    pkg <- tryCatch({
      env_name <- environmentName(topenv(environment(dsp_fun)))
      # Under full-suite `devtools::test()` loading (unlike a lone
      # `testthat::test_file()` run), a closure defined inline in a test
      # has this package's own namespace reachable in its environment
      # chain, so `topenv()` resolves to the package itself rather than
      # "R_GlobalEnv" — exclude it too, computed from this very function's
      # home so it stays correct if the package is ever renamed.
      own_pkg <- environmentName(topenv(environment(.resolve_dsp_identity)))
      if (nzchar(env_name) && !env_name %in% c("R_GlobalEnv", "base", own_pkg)) {
        env_name
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
  }

  version <- if (!is.na(pkg)) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  } else {
    NA_character_
  }

  list(`function` = fn_name, package = pkg, version = version)
}

#' Build the `generator` block recorded on an `ssffTrackDefinitions` entry
#'
#' `args` contains only what the caller explicitly passed to `quantify()` —
#' never metadata-derived or per-bundle-resolved values (those have no home
#' in a single global track definition; see the design spec's "Non-goals").
#'
#' @param dsp_fun The evaluated DSP function.
#' @param dsp_fun_name Character; the caller's deparsed expression.
#' @param user_params Named list of explicit caller args (`list(...)` at the
#'   `quantify.corpus` method boundary).
#' @return A `generator` list ready to embed in an `ssffTrackDefinitions[i]`.
#' @noRd
.build_generator_block <- function(dsp_fun, dsp_fun_name, user_params) {
  identity <- .resolve_dsp_identity(dsp_fun, dsp_fun_name)
  list(
    `function` = identity$`function`,
    package = identity$package,
    version = identity$version,
    args = if (length(user_params)) user_params else stats::setNames(list(), character(0)),
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' Upsert one entry into a corpus' ssffTrackDefinitions
#'
#' @param corpus_obj A corpus object.
#' @param track_def A list with at least `name`; typically also
#'   `columnName`, `fileExtension`, and optionally `generator`/`from`/`index`.
#' @param overwrite If `FALSE` (default) and an entry with the same `name`
#'   already exists, aborts. If `TRUE`, replaces it in place.
#' @return The corpus, invisibly.
#' @noRd
.upsert_ssff_track_definition <- function(corpus_obj, track_def, overwrite = FALSE) {
  dbConfig <- load_DBconfig(corpus_obj)
  existing <- dbConfig$ssffTrackDefinitions %||% list()
  existing_names <- vapply(existing, function(t) t$name %||% "", character(1))

  idx <- match(track_def$name, existing_names)
  if (!is.na(idx)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort(c(
        "SSFF track {.val {track_def$name}} is already registered.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ), class = c("reindeer_track_collision_error", "reindeer_error"))
    }
    existing[[idx]] <- track_def
  } else {
    existing[[length(existing) + 1L]] <- track_def
  }

  dbConfig$ssffTrackDefinitions <- existing
  store_DBconfig(corpus_obj, dbConfig)
  invisible(corpus_obj)
}

#' Discover the on-disk SSFF column name(s) for a just-written track
#'
#' `attr(dsp_fun, "tracks")` gives the in-memory display name (e.g.
#' `"RMS[dB]"`), which is not always the name superassp writes into the
#' SSFF binary header. Reading one just-written file back with
#' `superassp::read_track()` gets the ground-truth column name(s) instead
#' of guessing from the display label.
#'
#' @param written_file Path to one SSFF file this call just wrote.
#' @param display_tracks Character vector from `attr(dsp_fun, "tracks")`,
#'   used as a fallback (sanitized) if the file can't be read back.
#' @return Character vector, same length/order as `display_tracks`.
#' @noRd
.discover_written_column_names <- function(written_file, display_tracks) {
  sanitized <- gsub("\\[.*\\]$", "", display_tracks)
  if (is.null(written_file) || !file.exists(written_file)) {
    return(sanitized)
  }
  read_back <- tryCatch(superassp::read_track(written_file), error = function(e) NULL)
  if (is.null(read_back) || !is.list(read_back) || length(read_back) != length(display_tracks)) {
    return(sanitized)
  }
  on_disk_names <- names(read_back)
  if (is.null(on_disk_names) || any(!nzchar(on_disk_names))) {
    return(sanitized)
  }
  on_disk_names
}

# Internal implementation of corpus-level DSP compute-and-register.
# (Was `.enrich_corpus_impl()` in the removed R/reindeer_enrich.R; the
# bundle loop, metadata-derived params, caching, and parallelism are
# unchanged. New: track/column inference + ssffTrackDefinitions upsert.)
.quantify_corpus_compute <- function(corpus_obj, .using, dsp_fun_name, user_params,
                                     name, columnName, fileExtension, overwrite,
                                     sessionPattern, bundlePattern,
                                     .metadata_fields, .signal_extension,
                                     .force, .verbose, .parallel, .workers,
                                     .use_cache, .cache_dir, .cache_format) {
  .cache_format <- match.arg(.cache_format, choices = c("auto", "qs", "rds"))

  if (!S7::S7_inherits(corpus_obj, reindeer::corpus)) {
    cli::cli_abort("{.arg object} must be a corpus object")
  }
  if (!is.function(.using)) {
    cli::cli_abort("{.arg .using} must be a function")
  }

  display_tracks <- attr(.using, "tracks")
  if (is.null(display_tracks)) display_tracks <- dsp_fun_name
  dsp_ext <- attr(.using, "ext") %||% fileExtension
  if (is.null(dsp_ext)) {
    cli::cli_abort(c(
      "Cannot determine a file extension for {.fn {dsp_fun_name}}.",
      "i" = "Pass {.arg fileExtension} explicitly."
    ))
  }

  if (!is.null(name) && length(name) != length(display_tracks)) {
    cli::cli_abort(c(
      "{.arg name} has {length(name)} element{?s} but {.fn {dsp_fun_name}} produces {length(display_tracks)} track{?s}.",
      "i" = "Pass one name per output track, in order, or omit {.arg name} to auto-derive."
    ))
  }

  if (is.null(.signal_extension)) {
    .signal_extension <- corpus_obj@config$mediafileExtension
    if (is.null(.signal_extension)) {
      .signal_extension <- "wav"
      if (.verbose) cli::cli_alert_info("Using default extension: {.val wav}")
    }
  }

  if (.verbose) {
    cli::cli_h2("Quantifying corpus with {.fn {dsp_fun_name}}")
    cli::cli_alert_info("Processing {.val {.signal_extension}} files")
  }

  signal_files <- peek_signals(corpus_obj)
  signal_files <- signal_files[signal_files$extension == .signal_extension, ]
  signal_files <- signal_files[grepl(sessionPattern, signal_files$session) &
                                grepl(bundlePattern, signal_files$bundle), ]

  if (nrow(signal_files) == 0) {
    cli::cli_alert_warning("No signal files found with extension {.val {.signal_extension}}")
    return(invisible(corpus_obj))
  }

  if (.verbose) cli::cli_alert_success("Found {nrow(signal_files)} signal file{?s}")

  con <- get_corpus_connection(corpus_obj)
  db_uuid <- corpus_obj@.uuid
  needed_bundles <- unique(paste(signal_files$session, signal_files$bundle, sep = "||"))
  placeholders <- paste(rep("?", length(needed_bundles)), collapse = ", ")
  bundle_metadata_query <- sprintf(
    "SELECT session, bundle, field_name, field_value FROM metadata_bundle WHERE db_uuid = ? AND (session || '||' || bundle) IN (%s)",
    placeholders
  )
  bundle_metadata_long <- tryCatch({
    DBI::dbGetQuery(con, bundle_metadata_query, params = c(list(db_uuid), as.list(needed_bundles)))
  }, error = function(e) {
    all_meta <- DBI::dbReadTable(con, "metadata_bundle")
    all_meta <- all_meta[all_meta$db_uuid == db_uuid, ]
    sf_keys <- paste(signal_files$session, signal_files$bundle, sep = "||")
    meta_keys <- paste(all_meta$session, all_meta$bundle, sep = "||")
    all_meta[meta_keys %in% sf_keys, c("session", "bundle", "field_name", "field_value")]
  })

  if (nrow(bundle_metadata_long) > 0) {
    dt_meta <- data.table::as.data.table(bundle_metadata_long)
    bundle_metadata <- data.table::dcast(dt_meta, session + bundle ~ field_name,
                                         value.var = "field_value")
    bundle_metadata <- as.data.frame(bundle_metadata, stringsAsFactors = FALSE)
  } else {
    bundle_metadata <- data.frame(session = character(), bundle = character(),
                                  stringsAsFactors = FALSE)
  }

  signal_files_with_meta <- merge(signal_files, bundle_metadata, by = c("session", "bundle"), all.x = TRUE)

  if (.parallel && .use_parallel_workers(nrow(signal_files_with_meta), .workers)) {
    .workers <- .reindeer_workers(nrow(signal_files_with_meta), .workers)
    if (.verbose) cli::cli_alert_info("Using parallel processing with {.workers} worker{?s}")
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = .workers)
  }

  if (.verbose) cli::cli_progress_bar("Processing bundles", total = nrow(signal_files_with_meta))

  cache_conn <- NULL
  if (.use_cache) {
    resolved_cache_dir <- .cache_dir %||% corpus_obj@.cache_dir
    cache_conn <- .get_persistent_cache_connection(resolved_cache_dir, verbose = .verbose)
  }

  process_bundle <- function(i, signal_files_with_meta, dsp_fun,
                             metadata_fields, user_params, verbose = FALSE,
                             cache_conn = NULL, cache_format = "auto") {
    bundle_row <- signal_files_with_meta[i, ]
    dsp_params <- derive_dsp_parameters(
      dsp_fun = dsp_fun, metadata = bundle_row,
      metadata_fields = metadata_fields, user_params = user_params
    )
    cache_key <- NA_character_
    if (!is.null(cache_conn)) {
      cache_key <- digest::digest(list(
        bundle_row$full_path, file.info(bundle_row$full_path)$mtime,
        dsp_params, dsp_fun_name
      ))
      if (!.force) {
        cached <- .get_persistent_cache(cache_key, cache_conn)
        if (!is.null(cached)) {
          return(list(success = TRUE, bundle = bundle_row$bundle,
                      session = bundle_row$session, cached = TRUE,
                      full_path = bundle_row$full_path))
        }
      }
    }
    tryCatch({
      do.call(dsp_fun, c(
        list(listOfFiles = bundle_row$full_path), dsp_params,
        list(toFile = TRUE, verbose = FALSE)
      ))
      list(success = TRUE, bundle = bundle_row$bundle, session = bundle_row$session,
           cache_key = cache_key, full_path = bundle_row$full_path)
    }, error = function(e) {
      list(success = FALSE, bundle = bundle_row$bundle, session = bundle_row$session,
           error = e$message)
    })
  }

  if (.parallel) {
    results <- furrr::future_map(
      seq_len(nrow(signal_files_with_meta)), process_bundle,
      signal_files_with_meta = signal_files_with_meta, dsp_fun = .using,
      metadata_fields = .metadata_fields, user_params = user_params,
      verbose = FALSE, cache_conn = cache_conn, cache_format = .cache_format,
      .progress = .verbose, .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    results <- list()
    for (i in seq_len(nrow(signal_files_with_meta))) {
      results[[i]] <- process_bundle(
        i, signal_files_with_meta, .using, .metadata_fields, user_params, FALSE,
        cache_conn = cache_conn, cache_format = .cache_format
      )
      if (.verbose) cli::cli_progress_update()
    }
  }

  if (.verbose) cli::cli_progress_done()

  if (!is.null(cache_conn)) {
    miss_keys <- unique(vapply(
      Filter(function(x) isTRUE(x$success) && !isTRUE(x$cached), results),
      function(x) x$cache_key %||% NA_character_, character(1)))
    miss_keys <- miss_keys[!is.na(miss_keys)]
    if (length(miss_keys) > 0) {
      .set_persistent_cache_batch(
        lapply(miss_keys, function(k) list(cache_key = k, result = TRUE)),
        cache_conn, format = .cache_format)
    }
  }

  successes <- Filter(function(x) isTRUE(x$success), results)
  errors <- Filter(function(x) !isTRUE(x$success), results)
  if (length(errors) > 0) {
    cli::cli_alert_warning("{length(errors)} bundle{?s} failed processing")
    for (err in errors) cli::cli_alert_info("{err$session}/{err$bundle}: {err$error}")
  }

  if (length(successes) == 0) {
    cli::cli_abort("No bundle was processed successfully; nothing to register.")
  }

  written_file <- sub(paste0("\\.", .signal_extension, "$"), paste0(".", dsp_ext),
                      successes[[1]]$full_path)
  column_names <- .discover_written_column_names(written_file, display_tracks)

  for (i in seq_along(display_tracks)) {
    track_name <- if (!is.null(name)) name[i] else gsub("\\[.*\\]$", "", display_tracks[i])
    track_def <- list(
      name = track_name,
      columnName = if (!is.null(columnName)) columnName[i] else column_names[i],
      fileExtension = dsp_ext,
      generator = .build_generator_block(.using, dsp_fun_name, user_params)
    )
    .upsert_ssff_track_definition(corpus_obj, track_def, overwrite = overwrite)
  }

  if (.verbose) cli::cli_alert_success("Quantification complete")
  invisible(corpus_obj)
}

#' @rdname quantify
#' @param name,columnName,fileExtension Track identity for the corpus
#'   method. In compute-and-register mode (`.using` given), auto-derived
#'   from the DSP function when omitted — one entry per output track
#'   group (e.g. formants register `F` and `B` separately). In
#'   connect-existing mode (`.using` omitted), `name` is required.
#' @param from,index Connect-existing mode only: address one column
#'   (`index`, 1-based) of an already-registered multi-column track
#'   (`from`) instead of a whole file.
#' @param write_files Connect-existing-mode escape hatch; unused when
#'   `.using` is supplied (files are always written there).
#' @param overwrite Replace an existing same-`name` track definition
#'   instead of erroring. Default `FALSE`.
#' @param sessionPattern,bundlePattern Regex filters over which bundles
#'   to process. Default `".*"` (all).
#' @usage NULL
#' @name quantify.corpus
S7::method(quantify, corpus) <- function(object, .using = NULL, ...,
                                         name = NULL,
                                         columnName = NULL,
                                         fileExtension = NULL,
                                         from = NULL, index = NULL,
                                         write_files = NULL,
                                         overwrite = FALSE,
                                         sessionPattern = ".*",
                                         bundlePattern = ".*",
                                         .metadata_fields = c("Gender", "Age"),
                                         .signal_extension = NULL,
                                         .force = FALSE,
                                         .verbose = TRUE,
                                         .parallel = TRUE,
                                         .workers = NULL,
                                         .use_cache = TRUE,
                                         .cache_dir = NULL,
                                         .cache_format = c("auto", "qs", "rds")) {
  if (!is.null(.using) && (!is.null(from) || !is.null(index))) {
    cli::cli_abort(c(
      "Cannot supply both {.arg .using} and {.arg from}/{.arg index}.",
      "i" = "Compute mode ({.arg .using}) registers every column the DSP produces.",
      "i" = "{.arg from}/{.arg index} only make sense when connecting to something that already exists."
    ))
  }

  if (!is.null(.using)) {
    dsp_fun_name <- deparse(substitute(.using))
    .quantify_corpus_compute(
      corpus_obj = object, .using = .using, dsp_fun_name = dsp_fun_name,
      user_params = list(...), name = name, columnName = columnName,
      fileExtension = fileExtension, overwrite = overwrite,
      sessionPattern = sessionPattern, bundlePattern = bundlePattern,
      .metadata_fields = .metadata_fields, .signal_extension = .signal_extension,
      .force = .force, .verbose = .verbose, .parallel = .parallel,
      .workers = .workers, .use_cache = .use_cache, .cache_dir = .cache_dir,
      .cache_format = .cache_format
    )
  } else {
    .quantify_corpus_connect(
      corpus_obj = object, name = name, columnName = columnName,
      fileExtension = fileExtension, from = from, index = index,
      overwrite = overwrite, dots = list(...)
    )
  }
}

#' Connect-existing mode for quantify.corpus (no DSP call, no file write)
#' @noRd
.quantify_corpus_connect <- function(corpus_obj, name, columnName, fileExtension,
                                     from, index, overwrite, dots) {
  if (is.null(name)) {
    cli::cli_abort(c(
      "{.arg name} is required when connecting an existing track (no {.arg .using}).",
      "i" = "Pass {.arg name} plus either {.arg fileExtension} or {.arg from}/{.arg index}."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }
  if (length(name) != 1L) {
    cli::cli_abort("{.arg name} must be a single string in connect-existing mode.",
                   class = c("reindeer_track_validation_error", "reindeer_error"))
  }

  has_ext <- !is.null(fileExtension)
  has_sub <- !is.null(from) || !is.null(index)
  if (has_sub && (is.null(from) || is.null(index))) {
    cli::cli_abort(c(
      "{.arg from} and {.arg index} must both be supplied together.",
      "i" = "{.arg from} names the already-registered multi-column track; {.arg index} (1-based) picks a column."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }
  if (!has_ext && !has_sub) {
    cli::cli_abort(c(
      "Connect-existing mode needs either {.arg fileExtension} (adopt a whole file) ",
      "or {.arg from}/{.arg index} (name one column of a registered track)."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }
  # Note: fileExtension and from/index are NOT mutually exclusive here — a
  # from/index sub-column reference may also carry the fileExtension of the
  # underlying multi-column file (see the "(from/index) addresses a
  # sub-column" test, which supplies both together).

  track_def <- list(name = name)
  if (!is.null(columnName)) track_def$columnName <- columnName
  if (has_ext) track_def$fileExtension <- fileExtension
  if (has_sub) {
    track_def$from <- from
    track_def$index <- as.integer(index)
  }
  if (!is.null(dots$generator)) {
    track_def$generator <- dots$generator
  }

  .upsert_ssff_track_definition(corpus_obj, track_def, overwrite = overwrite)
  invisible(corpus_obj)
}
