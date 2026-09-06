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
#' @param dsp_function A DSP function. Common choices: `superassp::forest`
#'   (formants), `superassp::ksvF0` (pitch), `superassp::rmsana`
#'   (intensity), `superassp::dftSpectrum`.
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
#'   the function's own help — for example `superassp::forest` adds
#'   `F1`, `F2`, `F3`, `B1`, `B2`, `B3`). When `.at` is a vector, an
#'   extra `.time_point` column records which relative time each row
#'   came from. When `.use_cache = TRUE`, a `.cache_status` column
#'   reports `"hit"` or `"miss"` per row. The DSP function and its
#'   added columns are also recorded on the object as the S7 properties
#'   `@dsp_function` and `@dsp_columns`.
#' @family signal
#' @seealso [enrich()], [dsp_parameters()], [inspect_cache()]
#' @examplesIf interactive()
#' corp <- demo_corpus()
#' segs <- query(corp, "Phonetic =~ [aeiou]", lazy = FALSE)
#'
#' # Formants at the midpoint
#' quantify(segs, superassp::forest, .at = 0.5)
#'
#' # Pitch contour: 11 evenly spaced points
#' quantify(segs, superassp::ksvF0, .at = seq(0, 1, 0.1))
#'
#' # Override metadata-derived parameters
#' quantify(segs, superassp::forest, nominalF1 = 500, windowSize = 20)
#' @usage
#' quantify(object, dsp_function, ..., .at = NULL, .use_metadata = TRUE,
#'   .use_cache = FALSE, .cache_dir = NULL, .cache_format = c("auto", "qs", "rds"),
#'   .parallel = TRUE, .workers = NULL, .verbose = FALSE, .optimize = TRUE)
#' @name quantify
#' @export
quantify <- S7::new_generic("quantify", "object")

#' @rdname quantify
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

  # Match cache format argument
  .cache_format <- match.arg(.cache_format)

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

    # Fetch only needed metadata from correct table (metadata_bundle)
    db_uuid <- corpus_obj@.uuid
    all_bundle_meta <- DBI::dbGetQuery(con,
      "SELECT session, bundle, field_name AS key, field_value AS value, field_type AS value_type
       FROM metadata_bundle WHERE db_uuid = ?",
      params = list(db_uuid))
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

  # PHASE 2: Choose processing strategy based on optimize flag and available packages
  if (.optimize && nrow(seg_df) > 100) {
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
      media_ext, .at, .verbose, .use_cache, cache_conn, .cache_format
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

  # Get function name
  dsp_fun_name <- tryCatch({
    if (is.function(dsp_function)) {
      # Try to get function name
      fun_name <- deparse(substitute(dsp_function))
      if (length(fun_name) == 1 && !grepl("^function", fun_name)) {
        fun_name
      } else {
        "custom_function"
      }
    } else {
      as.character(dsp_function)
    }
  }, error = function(e) "unknown")

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
