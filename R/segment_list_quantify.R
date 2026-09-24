#' @include segment_list_classes.R reindeer_lazy_segment_list.R
NULL

#' Extract acoustic measurements from segments
#'
#' Apply a DSP function (typically from `superassp`) to every segment
#' in a `segment_list` and get one row of measurements back per segment
#' — or per time point when `.at` is given. Parameters are picked from
#' speaker metadata (`Age`, `Gender`) unless overridden through `...`.
#' For lazy pipelines `quantify()` records the request and runs it at
#' the next [collect()].
#'
#' @param object A `segment_list` (eager) or `lazy_segment_list`.
#' @param dsp_function A DSP function. Common choices: `superassp::trk_formant_forest`
#'   (formants), `superassp::trk_pitch_ksv` (pitch), `superassp::trk_rms`
#'   (intensity), `superassp::trk_dft_spectrum`.
#' @param ... Forwarded to the DSP function. Values you pass win over
#'   metadata-derived ones (`nominalF1`, `windowSize`, ...).
#' @param .at Relative time points to sample, each in `[0, 1]`. A scalar
#'   gives one row per segment (e.g. `0.5` for midpoint); a vector
#'   multiplies rows (e.g. `c(0.2, 0.5, 0.8)`).
#' @param .use_metadata Look up DSP parameters from speaker metadata.
#'   Default `TRUE`. See [dsp_parameters()] to preview.
#' @param .use_cache Reuse persistent results when the cache key
#'   matches. Default `FALSE`. See [inspect_cache()] for what's stored.
#'   When enabled, the returned tibble gains a `.cache_status` column
#'   (`"hit"` / `"miss"`).
#' @param .cache_dir,.cache_format Where and how to persist cached
#'   results.
#' @param .parallel,.workers Run segments concurrently via a
#'   multi-session future plan. Default: on, with `detectCores() - 1`.
#' @param .verbose Print a per-step progress summary.
#' @param .optimize Use optimized computation (default `TRUE`; turn off
#'   only for debugging).
#' @return An [extended_segment_list]: every column of the input
#'   `segment_list` (see [query()] for the column inventory), plus one
#'   column per DSP output column produced by `dsp_function` (consult
#'   the function's own help — for example `superassp::trk_formant_forest` adds
#'   `F1`, `F2`, `F3`, `B1`, `B2`, `B3`). When `.at` is a vector, an
#'   extra `.time_point` column records which relative time each row
#'   came from. When `.use_cache = TRUE`, a `.cache_status` column
#'   reports `"hit"` or `"miss"` per row. The DSP function and its
#'   added columns are also recorded on the object as the S7 properties
#'   `@dsp_function` and `@dsp_columns`.
#' @family signal
#' @seealso [biographize()], [dsp_parameters()], [inspect_cache()]
#' @examplesIf interactive()
#' corp <- demo_corpus()
#' segs <- query(corp, "Phonetic =~ [aeiou]", lazy = FALSE)
#'
#' # Formants at the midpoint
#' quantify(segs, superassp::trk_formant_forest, .at = 0.5)
#'
#' # Pitch contour: 11 evenly spaced points
#' quantify(segs, superassp::trk_pitch_ksv, .at = seq(0, 1, 0.1))
#'
#' # Override metadata-derived parameters
#' quantify(segs, superassp::trk_formant_forest, nominalF1 = 500, windowSize = 20)
#' @usage quantify(object, ...)
#' @name quantify
#' @export
quantify <- S7::new_generic("quantify", "object")

#' @rdname quantify
#' @usage NULL
#' @name quantify.segment_list
S7::method(quantify, segment_list) <- function(object, dsp_function, ...,
                                                .at = NULL,
                                                .use_metadata = TRUE,
                                                .verbose = FALSE,
                                                .parallel = TRUE,
                                                .workers = NULL,
                                                .use_cache = FALSE,
                                                .cache_dir = NULL,
                                                .cache_format = c("auto", "qs", "rds"),
                                                .optimize = TRUE) {

  # Input validation with assertthat
  assertthat::assert_that(
    S7::S7_inherits(object, segment_list),
    msg = "object must be a segment_list"
  )
  if (is.list(dsp_function) && !is.function(dsp_function)) {
    cli::cli_abort(c(
      "{.arg dsp_function} must be a function or a character vector of registered track names.",
      "x" = "Got a {.cls {class(dsp_function)}}."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }

  assertthat::assert_that(
    is.function(dsp_function) || is.character(dsp_function),
    msg = "dsp_function must be a function or character string"
  )
  assertthat::assert_that(
    assertthat::is.flag(.use_metadata),
    assertthat::is.flag(.verbose),
    assertthat::is.flag(.parallel),
    assertthat::is.flag(.use_cache),
    assertthat::is.flag(.optimize),
    msg = "Logical flags must be TRUE or FALSE"
  )

  if (is.character(dsp_function)) {
    return(.quantify_segment_list_by_name(
      object, dsp_function, ...,
      .at = .at, .verbose = .verbose, .parallel = .parallel,
      .workers = .workers, .use_cache = .use_cache,
      .cache_dir = .cache_dir, .cache_format = match.arg(.cache_format)
    ))
  }

  # Match cache format argument
  .cache_format <- match.arg(.cache_format)

  # The caller's expression is only available here, at the boundary, where
  # `dsp_function` is still the caller's promise. It identifies the routine in
  # cache keys and in the result's metadata.
  dsp_name <- .dsp_name_from_expr(substitute(dsp_function), dsp_function)

  if (nrow(object) == 0) {
    if (.verbose) cli::cli_alert_warning("Empty segment list")
    return(extended_segment_list(data = as.data.frame(object)))
  }

  # Validate .at parameter
  if (!is.null(.at)) {
    assertthat::assert_that(
      is.numeric(.at),
      msg = ".at must be numeric"
    )
    assertthat::assert_that(
      all(.at >= 0 & .at <= 1),
      msg = ".at values must be between 0 and 1"
    )
  }

  # Validate .workers if provided
  if (!is.null(.workers)) {
    assertthat::assert_that(
      assertthat::is.count(.workers),
      msg = ".workers must be a positive integer"
    )
  }

  # Try to get corpus from db_path (PHASE 1: Cached corpus loading)
  corpus_obj <- get_corpus_cached(object, NULL)

  if (is.null(corpus_obj)) {
    cli::cli_abort(c(
      "Cannot access corpus",
      "x" = "Unable to load corpus from db_path: {object@db_path}",
      "i" = "Make sure the segment_list has a valid db_path property"
    ))
  }

  # Get mediafileExtension from config
  media_ext <- corpus_obj@config$mediafileExtension %||% "wav"

  # Get metadata if requested
  metadata_by_bundle <- NULL
  dsp_params_base <- list(...)

  if (.use_metadata) {
    if (.verbose) cli::cli_alert_info("Fetching metadata for {nrow(object)} segments")

    con <- get_corpus_connection(corpus_obj)

    # Get unique bundles from segment list
    unique_bundles <- unique(as.data.frame(object)[, c("session", "bundle")])

    # Fetch only needed metadata from correct table (metadata_bundle).
    # Filtering in SQL keeps the transfer proportional to the query result
    # rather than to the whole corpus; the composite key matches the expression
    # used by biographize().
    db_uuid <- corpus_obj@.uuid
    ub_keys <- unique(paste(unique_bundles$session, unique_bundles$bundle, sep = "||"))
    placeholders <- paste(rep("?", length(ub_keys)), collapse = ", ")
    all_bundle_meta <- DBI::dbGetQuery(con, paste0(
      "SELECT session, bundle, field_name AS key, field_value AS value, field_type AS value_type
       FROM metadata_bundle
       WHERE db_uuid = ? AND (session || '||' || bundle) IN (", placeholders, ")"),
      params = c(list(db_uuid), as.list(ub_keys)))
    # Semi-join: keep only rows matching unique_bundles
    meta_keys <- paste(all_bundle_meta$session, all_bundle_meta$bundle, sep = "\x01")
    ub_keys <- paste(unique_bundles$session, unique_bundles$bundle, sep = "\x01")
    all_bundle_meta <- all_bundle_meta[meta_keys %in% ub_keys, ]
    # Pivot wider using data.table::dcast
    if (nrow(all_bundle_meta) > 0) {
      dt_meta <- data.table::as.data.table(all_bundle_meta)
      metadata_by_bundle <- data.table::dcast(dt_meta, session + bundle ~ key, value.var = "value")
      metadata_by_bundle <- tibble::as_tibble(metadata_by_bundle)
    } else {
      metadata_by_bundle <- tibble::tibble(session = character(), bundle = character())
    }
  }

  # Convert to data frame for processing
  seg_df <- as.data.frame(object)

  # Join with metadata if available
  if (!is.null(metadata_by_bundle)) {
    seg_df <- merge(seg_df, metadata_by_bundle, by = c("session", "bundle"), all.x = TRUE)
  }

  # Resolve effective DSP params per segment (list-column). Metadata-derived
  # norms are computed per bundle (single-row contract) and merged under any
  # user-supplied overrides; segments whose bundle has no norm row fall back
  # to the user params alone.
  if (.use_metadata && !is.null(metadata_by_bundle) && nrow(metadata_by_bundle) > 0) {
    per_bundle <- .derive_dsp_params_per_bundle(
      dsp_fun = dsp_function,
      metadata = metadata_by_bundle,
      metadata_fields = c("Gender", "Age"),
      user_params = dsp_params_base
    )
    lookup <- stats::setNames(
      per_bundle$dsp_params,
      paste(per_bundle$session, per_bundle$bundle, sep = "\x01")
    )
    seg_key <- paste(seg_df$session, seg_df$bundle, sep = "\x01")
    seg_df$seg_params <- lapply(seg_key, function(k) {
      p <- lookup[[k]]
      if (is.null(p)) dsp_params_base else p
    })
  } else {
    seg_df$seg_params <- rep(list(dsp_params_base), nrow(seg_df))
  }

  # On older superassp builds the trk_* wrappers expose no tunable formals, so
  # nothing can be derived and the DSP runs on its own defaults. Say so, rather
  # than letting a metadata-driven pipeline report parameters that were never
  # applied. Once per session.
  if (.use_metadata &&
      !is.null(metadata_by_bundle) && nrow(metadata_by_bundle) > 0 &&
      all(c("Age", "Gender") %in% names(metadata_by_bundle)) &&
      all(lengths(seg_df$seg_params) == 0L) &&
      !isTRUE(getOption("reindeer.norm_warning_shown", FALSE))) {
    options(reindeer.norm_warning_shown = TRUE)
    cli::cli_warn(
      c("Age/Gender metadata is present but no DSP parameters could be derived.",
        i = "This DSP routine exposes no tunable formals, so its defaults are used.",
        i = "superassp >= 3.0.0 exposes them; older builds do not.",
        i = "Reinstall it from GitHub: {.run remotes::install_github(\"humlab-speech/superassp\")}."),
      class = c("reindeer_metadata_warning", "reindeer_warning"))
  }

  # PHASE 2: Choose processing strategy based on optimize flag and available packages
  # The vectorized executor is the only one that reads and writes the persistent
  # cache, so a cache-enabled call routes there whatever its size. Previously a
  # `.use_cache = TRUE` request for 21-100 segments was accepted and silently
  # ignored, because that band used the parallel executor.
  if ((.optimize && nrow(seg_df) > 100) || .use_cache) {
    # Use vectorized processing for large datasets
    if (.verbose) {
      cli::cli_alert_info("Using optimized vectorized processing")
    }

    # Setup persistent cache if requested
    cache_conn <- if (.use_cache) {
      resolved_cache_dir <- .cache_dir %||% corpus_obj@.cache_dir
      .get_persistent_cache_connection(resolved_cache_dir)
    } else {
      NULL
    }

    # PHASE 2: Vectorized batch processing
    results_list <- .process_segments_vectorized(
      seg_df, corpus_obj, dsp_function, dsp_params_base,
      media_ext, .at, .verbose, .use_cache, cache_conn, .cache_format,
      dsp_name = dsp_name
    )

  } else if (.parallel && nrow(seg_df) > 20) {
    # Use parallel I/O for medium to large datasets
    if (.verbose) {
      cli::cli_alert_info("Using parallel I/O processing")
    }

    if (is.null(.workers)) {
      .workers <- max(1, parallel::detectCores() - 1)
    }

    # PHASE 2: Parallel I/O processing
    results_list <- .process_parallel_io(
      seg_df, corpus_obj, dsp_function, dsp_params_base,
      media_ext, .at, .workers, .verbose
    )

  } else {
    # Fall back to sequential processing for small datasets
    if (.verbose) {
      cli::cli_alert_info("Using sequential processing")
    }

    # PHASE 1: File-batch processing (already optimized)
    results_list <- .process_by_file_batch(
      seg_df, corpus_obj, dsp_function, dsp_params_base,
      media_ext, .at, .verbose
    )
  }

  if (.verbose) {
    cli::cli_progress_done()
  }

  # Remove NULL results
  results <- Filter(Negate(is.null), results_list)

  if (length(results) == 0) {
    if (.verbose) {
      cli::cli_alert_warning("No results generated")
    }
    if (nrow(object) > 0) {
      cli::cli_warn(
        c("quantify() produced no measurements: the {nrow(object)} input segment{?s} came back unchanged.",
          i = "The DSP routine returned nothing for every segment, so no columns were added.",
          i = "Call with {.code .verbose = TRUE} to see the per-segment progress."),
        class = c("reindeer_quantify_warning", "reindeer_warning"))
    }
    return(extended_segment_list(data = as.data.frame(object)))
  }

  # Combine all results efficiently
  # PHASE 2: Use data.table for faster binding if available
  combined <- if (length(results) > 100) {
    data.table::rbindlist(results, fill = TRUE) |>
      tibble::as_tibble()
  } else {
    data.table::rbindlist(results, fill = TRUE) |>
      tibble::as_tibble()
  }

  if (.verbose) {
    n_segs <- length(unique(combined$start_item_id))
    n_rows <- nrow(combined)
    cli::cli_alert_success("Processed {n_segs} segment{?s} ({n_rows} row{?s} total)")
  }

  # Identify DSP columns (those not in original segment_list)
  segment_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate", "signal_file",
    "file_exists", "cache_key", "cached_result", "file_group_id",
    "seg_params", "seg_params_digest",
    "result"
  )

  # Also exclude metadata columns if present
  metadata_pattern <- "^(Gender|Age|windowSize)"

  dsp_cols <- setdiff(
    names(combined),
    c(segment_cols, grep(metadata_pattern, names(combined), value = TRUE))
  )

  # Function name recorded on the result (same identity used in cache keys)
  dsp_fun_name <- dsp_name

  # Create extended_segment_list
  result <- extended_segment_list(
    data = combined,
    db_uuid = object@db_uuid,
    db_path = object@db_path,
    dsp_function = dsp_fun_name,
    dsp_columns = dsp_cols
  )

  .record_step(result, object, "quantify", sys.call(-1L))
}

#' Quantify method for lazy_segment_list — defer DSP until collect()
#'
#' When called on a lazy segment list, `quantify()` does not execute DSP.
#' Instead it appends a "quantify" entry to the lazy chain's
#' `post_transforms`; the DSP is run at `collect()` time on the
#' materialized `segment_list`. This lets users write a single pipeline
#' (`query(corp, ..., lazy = TRUE) |> scout() |> quantify(...) |>
#' collect()`) without paying for DSP unless / until the result is needed.
#'
#' @param object A `lazy_segment_list` (from `query(corp, query, lazy = TRUE)`).
#' @param dsp_function A DSP function from `superassp` or similar.
#' @param ... Additional arguments passed to the DSP function at `collect()` time.
#' @return The same `lazy_segment_list` with a deferred quantify step.
#' @name quantify.lazy_segment_list
#' @usage NULL
S7::method(quantify, lazy_segment_list) <- function(object, dsp_function, ...) {
  if (!is.function(dsp_function) && !is.character(dsp_function)) {
    cli::cli_abort("{.arg dsp_function} must be a function or character string")
  }
  spec <- list(
    type = "quantify",
    dsp_function = dsp_function,
    args = list(...)
  )
  object@query_parts$post_transforms <- c(
    object@query_parts$post_transforms,
    list(spec)
  )
  invisible(object)
}

#' Character-track-name branch of quantify.segment_list
#'
#' Looks each name up in the corpus' `ssffTrackDefinitions`. If the track
#' has files on disk (registered via `quantify(corp, .using = fn)`), reads
#' the segment's window directly with `superassp::read_track()`. If the
#' track was registered without computation (connect-existing mode with a
#' `generator` recipe but no files — e.g. `suggestCaching == FALSE` at
#' registration) and no file exists for a bundle, recomputes via the
#' stored `generator$function`/`generator$package` recipe instead.
#'
#' @noRd
.quantify_segment_list_by_name <- function(object, track_names, ...,
                                           .at, .verbose, .parallel, .workers,
                                           .use_cache, .cache_dir, .cache_format) {
  corpus_obj <- get_corpus_cached(object, NULL)
  if (is.null(corpus_obj)) {
    cli::cli_abort(c(
      "Cannot access corpus",
      "x" = "Unable to load corpus from db_path: {object@db_path}"
    ), class = c("reindeer_corpus_access_error", "reindeer_error"))
  }

  dbConfig <- load_DBconfig(corpus_obj)
  all_tracks <- dbConfig$ssffTrackDefinitions %||% list()
  all_names <- vapply(all_tracks, function(t) t$name %||% "", character(1))

  unknown <- setdiff(track_names, all_names)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unregistered track name{?s}: {.val {unknown}}.",
      "i" = "Register with {.code quantify(corpus, .using = fn)} or {.code quantify(corpus, name = ..., fileExtension = ...)} first.",
      "i" = "Currently registered: {.val {all_names}}."
    ), class = c("reindeer_unknown_track_error", "reindeer_error"))
  }

  seg_df <- as.data.frame(object)
  media_ext <- corpus_obj@config$mediafileExtension %||% "wav"
  time_points <- if (is.null(.at)) list(NA_real_) else as.list(.at)

  results <- list()
  for (tn in track_names) {
    track_def <- all_tracks[[match(tn, all_names)]]
    for (i in seq_len(nrow(seg_df))) {
      seg <- seg_df[i, ]
      signal_dir <- file.path(corpus_obj@basePath, paste0(seg$session, session.suffix),
                              paste0(seg$bundle, bundle.dir.suffix))
      track_file <- list.files(signal_dir,
                               pattern = paste0("^", seg$bundle, "\\.", track_def$fileExtension, "$"),
                               full.names = TRUE)

      for (tp in time_points) {
        if (is.na(tp)) {
          begin <- seg$start / 1000; end <- seg$end / 1000
        } else {
          t_abs <- (seg$start + tp * (seg$end - seg$start)) / 1000
          begin <- t_abs; end <- t_abs
        }

        row_data <- NULL
        if (length(track_file) == 1L && file.exists(track_file)) {
          col_arg <- if (!is.null(track_def$from)) track_def$from else track_def$columnName %||% tn
          track_data <- tryCatch(
            superassp::read_track(track_file, begin = begin, end = end, tracks = col_arg),
            error = function(e) NULL
          )
          if (!is.null(track_data) && length(track_data) >= 1) {
            mat <- track_data[[1]]
            if (!is.null(track_def$index)) mat <- mat[, track_def$index, drop = FALSE]
            row_data <- as.data.frame(mat[nrow(mat), , drop = FALSE])
            names(row_data) <- if (ncol(row_data) > 1) {
              paste0(tn, seq_len(ncol(row_data)))
            } else {
              tn
            }
          }
        }

        if (is.null(row_data) && !is.null(track_def$generator)) {
          gen <- track_def$generator
          if (is.null(gen$package) || is.na(gen$package)) {
            cli::cli_abort("Track {.val {tn}} has no on-disk file and no resolvable {.arg generator$package} to recompute from.")
          }
          dsp_fun <- tryCatch(get(gen$`function`, envir = asNamespace(gen$package)),
                              error = function(e) NULL)
          if (is.null(dsp_fun)) {
            cli::cli_abort("Cannot resolve {.code {gen$package}::{gen$`function`}} to recompute {.val {tn}}.")
          }
          signal_file <- list.files(signal_dir, pattern = paste0("^", seg$bundle, "\\.", media_ext, "$"),
                                    full.names = TRUE)
          if (length(signal_file) != 1L) next
          computed <- tryCatch(
            do.call(dsp_fun, c(list(listOfFiles = signal_file), gen$args %||% list(),
                               list(toFile = FALSE, verbose = FALSE))),
            error = function(e) NULL
          )
          if (!is.null(computed) && is.list(computed) && length(computed) >= 1) {
            mat <- computed[[1]]
            sample_rate <- attr(computed, "sampleRate") %||% 100
            start_time  <- attr(computed, "startTime") %||% 0
            frame_idx <- round((begin - start_time) * sample_rate) + 1L
            frame_idx <- max(1L, min(nrow(mat), frame_idx))
            row_data <- as.data.frame(mat[frame_idx, , drop = FALSE])
            names(row_data) <- tn
          }
        }

        if (!is.null(row_data)) {
          key <- paste(i, tp, sep = "|")
          if (is.null(results[[key]])) {
            results[[key]] <- cbind(seg, row_data)
            if (!is.na(tp)) results[[key]]$.time_point <- tp
          } else {
            results[[key]] <- cbind(results[[key]], row_data)
          }
        }
      }
    }
  }

  if (length(results) == 0) {
    if (.verbose) cli::cli_alert_warning("No results generated")
    return(extended_segment_list(data = seg_df))
  }

  combined <- data.table::rbindlist(results, fill = TRUE) |> tibble::as_tibble()
  dsp_cols <- setdiff(names(combined), names(seg_df))

  result <- extended_segment_list(
    data = combined, db_uuid = object@db_uuid, db_path = object@db_path,
    dsp_function = paste(track_names, collapse = "|"), dsp_columns = dsp_cols
  )
  .record_step(result, object, "quantify", sys.call(-2L))
}
