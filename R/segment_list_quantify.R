#' Quantify generic - Apply DSP to segments
#' @param object The object to quantify
#' @param ... Additional arguments passed to methods
#' @name quantify
#' @export
quantify <- S7::new_generic("quantify", "object")

#' Quantify method for segment_list - Apply DSP to query results
#'
#' Applies a DSP function to all segments in a segment_list, extracting
#' acoustic measurements for each segment. This is equivalent to emuR::get_trackdata()
#' but allows for custom DSP functions with metadata-driven parameters.
#'
#' @param object A segment_list object (from query/query)
#' @param dsp_function A DSP function from superassp or similar
#' @param ... Additional arguments passed to the DSP function
#' @param .at Optional vector of relative time points (0-1) to extract from track
#' @param .use_metadata Logical; whether to use bundle metadata for parameter derivation
#' @param .verbose Logical; show progress messages
#' @param .parallel Logical; use parallel processing (default TRUE)
#' @param .workers Number of parallel workers (default: parallel::detectCores() - 1)
#' @param .use_cache Logical; enable result caching (default FALSE)
#' @param .cache_dir Character; cache directory path (default: \code{corpus@.cache_dir}, i.e. \code{basePath/.quantify_cache})
#' @param .cache_format Character; serialization format - "auto" (uses qs if available,
#'        otherwise base serialize), "qs" (faster, smaller, requires qs package),
#'        or "rds" (base R serialize, slower, larger). Default: "auto"
#' @param .optimize Logical; use optimized processing (default TRUE)
#'
#' @return An extended_segment_list with segment information and DSP-derived measurements
#'
#' @examplesIf interactive()
#' segs <- query(corpus, "Phonetic == t")
#' formants <- quantify(segs, superassp::forest)
#'
#' # Extract at specific time points
#' formants_at_midpoint <- quantify(segs, superassp::forest, .at = 0.5)
#' formants_three_points <- quantify(segs, superassp::forest, .at = c(0.2, 0.5, 0.8))
#'
#' # Disable parallel processing
#' formants <- quantify(segs, superassp::forest, .parallel = FALSE)
#'
#' # Enable caching with qs format (faster, recommended)
#' formants <- quantify(segs, superassp::forest, .use_cache = TRUE)
#'
#' # Force base R serialization
#' formants <- quantify(segs, superassp::forest, .use_cache = TRUE, .cache_format = "rds")
#'
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

    # Derive DSP parameters from metadata where applicable
    if (.use_metadata && nrow(metadata_by_bundle) > 0) {
      # This happens once for all segments sharing metadata
      dsp_params_base <- derive_dsp_parameters(
        dsp_fun = dsp_function,
        metadata = metadata_by_bundle,
        metadata_fields = c("Gender", "Age"),
        user_params = dsp_params_base
      )
    }
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
