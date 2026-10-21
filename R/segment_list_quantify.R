#' @include segment_list_classes.R reindeer_lazy_segment_list.R
NULL

#' Extract acoustic measurements from segments
#'
#' Apply a DSP function (typically from `superassp`) to every segment
#' in a `segment_list` and get one row of measurements back per segment
#' - or per time point when `.at` is given. Parameters are picked from
#' speaker metadata (`Age`, `Gender`) unless overridden through `...`.
#' For lazy pipelines `quantify()` records the request and runs it at
#' the next [collect()].
#'
#' @param object A `segment_list` (eager) or `lazy_segment_list`.
#' @param ... Forwarded to the DSP function (segment_list method) or to
#'   the SSFF track generator (corpus method). Values you pass win over
#'   metadata-derived ones (`nominalF1`, `windowSize`, ...). See the
#'   "Method arguments" section below for the named arguments each
#'   method accepts ahead of `...`.
#' @section Method arguments - segment_list:
#' \describe{
#'   \item{`dsp_function`}{A DSP function. Common choices:
#'     `superassp::trk_formant_forest` (formants), `superassp::trk_pitch_ksv`
#'     (pitch), `superassp::trk_rms` (intensity), `superassp::trk_dft_spectrum`.}
#'   \item{`.at`}{Relative time points to sample, each in `[0, 1]`. Each
#'     selects the frame nearest to `start + .at * (end - start)` (emuR's
#'     `get_trackdata(cut =)`; no interpolation). A scalar
#'     gives one row per segment (e.g. `0.5` for midpoint); a vector
#'     multiplies rows (e.g. `c(0.2, 0.5, 0.8)`). `NULL` (default) returns
#'     every frame inside each segment.}
#'   \item{`.use_metadata`}{Look up DSP parameters from speaker metadata.
#'     Default `TRUE`. See [dsp_parameters()] to preview.}
#'   \item{`.use_cache`}{Reuse persistent results when the cache key
#'     matches. Default `TRUE`. `summary()` on the corpus shows what's stored.
#'     When enabled, the returned tibble gains a `.cache_status` column
#'     (`"hit"` / `"miss"`).}
#'   \item{`.cache_dir`, `.cache_format`}{Where and how to persist cached
#'     results.}
#'   \item{`.parallel`, `.workers`}{Deprecated and ignored. Parallelism
#'     follows the caller's [future::plan()]; the default is sequential.}
#'   \item{`.verbose`}{Print a per-step progress summary.}
#'   \item{`.optimize`}{Deprecated and ignored.}
#'   \item{`.mode`}{`"bundle"` (default) analyses each bundle's whole file once
#'     with that speaker's parameters and slices every segment out of the
#'     result; `"window"` calls the routine once per segment on just its
#'     window. Routines that do not return a time-indexed track always run
#'     per segment. The default matches emuR (whole-file tracks, sliced per
#'     segment). Windowed analysis loses left context, so pitch, spectral and
#'     intensity values differ from `"bundle"` (see `dev/WHOLE_FILE_FIDELITY.md`
#'     in the source repository); formant routines such as
#'     `trk_formant_forest` agree in both modes.}
#' }
#' @details When `dsp_function` is a character vector of registered
#'   track names, each bundle's track file is read once (as
#'   `emuR::get_trackdata()` does: `quantify(segs, "fm", .at = 0.5)` equals
#'   `get_trackdata(db, segs, "fm", cut = 0.5)`); value columns are named
#'   after the track (`fm1` to `fm4`). A bundle without the file is an error.
#'   `.use_cache`, `.parallel`, `.workers`, `.cache_dir`, and `.cache_format`
#'   have no effect there.
#' @return A [segment_list] with the `dsp_function` and `dsp_columns` properties set: every column of the input
#'   `segment_list` (see [query()] for the column inventory), plus one
#'   column per DSP output column produced by `dsp_function` (consult
#'   the function's own help - for example `superassp::trk_formant_forest` adds
#'   `F1_Hz` to `F5_Hz` and `B1_Hz` to `B5_Hz`; the unit is part of the name). When `.at` is a vector, an
#'   extra `.time_point` column records which relative time each row
#'   came from. When `.use_cache = TRUE`, a `.cache_status` column
#'   reports `"hit"` or `"miss"` per row. The DSP function and its
#'   added columns are also recorded on the object as the S7 properties
#'   `@dsp_function` and `@dsp_columns`.
#' @family signal
#' @seealso [biographize()], [dsp_parameters()], [clear_cache()]
#' @examplesIf requireNamespace("superassp", quietly = TRUE)
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
                                                .parallel = NULL,
                                                .workers = NULL,
                                                .use_cache = TRUE,
                                                .cache_dir = NULL,
                                                .cache_format = c("auto", "qs", "rds"),
                                                .optimize = NULL,
                                                .mode = c("bundle", "window")) {
  .mode <- match.arg(.mode)

  .warn_ignored_parallel_args(.parallel = .parallel, .workers = .workers,
                              .optimize = .optimize)
  .parallel <- TRUE
  .workers <- NULL
  .optimize <- TRUE

  # Input validation 
  .assert(
    S7::S7_inherits(object, segment_list),
    msg = "object must be a segment_list"
  )
  if (is.list(dsp_function) && !is.function(dsp_function)) {
    cli::cli_abort(c(
      "{.arg dsp_function} must be a function or a character vector of registered track names.",
      "x" = "Got a {.cls {class(dsp_function)}}."
    ), class = c("reindeer_track_validation_error", "reindeer_error"))
  }

  .assert(
    is.function(dsp_function) || is.character(dsp_function),
    msg = "dsp_function must be a function or character string"
  )
  .assert(
    .is_flag(.use_metadata),
    .is_flag(.verbose),
    .is_flag(.use_cache),
    msg = "Logical flags must be TRUE or FALSE"
  )

  if (is.character(dsp_function)) {
    return(.quantify_segment_list_by_name(
      object, dsp_function, ...,
      .at = .at, .verbose = .verbose, .use_cache = .use_cache,
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
    .assert(
      is.numeric(.at),
      msg = ".at must be numeric"
    )
    .assert(
      all(.at >= 0 & .at <= 1),
      msg = ".at values must be between 0 and 1"
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
  media_ext <- corpus_obj@config$mediafileExtension %or_empty% "wav"

  # Get metadata if requested
  metadata_by_bundle <- NULL
  dsp_params_base <- list(...)

  if (.use_metadata) {
    if (.verbose) cli::cli_alert_info("Fetching metadata for {nrow(object)} segments")

    unique_bundles <- unique(as.data.frame(object)[, c("session", "bundle")])
    metadata_by_bundle <- .speaker_metadata(corpus_obj, unique_bundles,
                                            c("Age", "Gender"))
  }

  # Convert to data frame for processing
  seg_df <- as.data.frame(object)

  # Row identity: output is restored to input order at the end.
  seg_df$.row_id <- seq_len(nrow(seg_df))

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

  # Once per call: Age/Gender are known but this routine can receive none of
  # the derived norms, so it runs on its own defaults. Say so.
  if (.use_metadata &&
      !is.null(metadata_by_bundle) && nrow(metadata_by_bundle) > 0 &&
      all(c("Age", "Gender") %in% names(metadata_by_bundle)) &&
      all(vapply(seg_df$seg_params,
                 function(p) length(attr(p, "derived")) == 0L, logical(1)))) {
    cli::cli_warn(
      c("Age/Gender metadata is present but {.val {dsp_name}} accepts none of the derived DSP parameters.",
        i = "It runs on its own defaults. See {.fn dsp_parameters} for the norms and {.code formals()} of the routine for what it accepts."),
      class = c("reindeer_metadata_warning", "reindeer_warning"))
  }

  # One executor for every call: per bundle file, analyse once (or per window)
  # and slice; the persistent cache is read and written per segment.
  cache_conn <- if (.use_cache) {
    .get_persistent_cache_connection(.cache_dir %or_empty% corpus_obj@.cache_dir)
  } else {
    NULL
  }
  results_list <- .process_segments_vectorized(
    seg_df, corpus_obj, dsp_function, dsp_params_base,
    media_ext, .at, .verbose, .use_cache, cache_conn, .cache_format,
    dsp_name = dsp_name, .mode = .mode
  )

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

  # Restore input order (stable within a segment's own rows), then drop the
  # bookkeeping columns: the output is input columns + track columns only.
  if (".row_id" %in% names(combined)) {
    combined <- combined[order(combined$.row_id), , drop = FALSE]
  }
  internal <- c(".row_id", "signal_file", "file_exists", "cache_key", "cached_result",
                "file_group_id", "seg_params", "seg_params_digest", "result")
  combined <- combined[, setdiff(names(combined), internal), drop = FALSE]

  # Identify DSP columns (those not in original segment_list)
  segment_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate", "signal_file", ".cache_status",
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

#' Quantify method for lazy_segment_list - defer DSP until collect()
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
  invisible(.fork_lazy(object, "post", spec))
}

#' Character-track-name branch of quantify.segment_list
#'
#' Looks each name up in the corpus' `ssffTrackDefinitions`, reads each
#' bundle's track file once with `superassp::read_track()` (zeros kept, as in
#' emuR) and slices and selects frames exactly as function mode does. A
#' missing or unreadable file is an error, never a recompute.
#'
#' @noRd
.quantify_segment_list_by_name <- function(object, track_names, ...,
                                           .at, .verbose,
                                           .use_cache, .cache_dir, .cache_format) {
  corpus_obj <- get_corpus_cached(object, NULL)
  if (is.null(corpus_obj)) {
    cli::cli_abort(c(
      "Cannot access corpus",
      "x" = "Unable to load corpus from db_path: {object@db_path}"
    ), class = c("reindeer_corpus_access_error", "reindeer_error"))
  }

  dbConfig <- load_DBconfig(corpus_obj)
  all_tracks <- dbConfig$ssffTrackDefinitions %or_empty% list()
  all_names <- vapply(all_tracks, function(t) t$name %or_empty% "", character(1))

  unknown <- setdiff(track_names, all_names)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unregistered track name{?s}: {.val {unknown}}.",
      "i" = "Register with {.code quantify(corpus, .using = fn)} or {.code quantify(corpus, name = ..., file_extension = ...)} first.",
      "i" = "Currently registered: {.val {all_names}}."
    ), class = c("reindeer_unknown_track_error", "reindeer_error"))
  }

  if (is.null(.at) && length(track_names) > 1L) {
    cli::cli_abort(c(
      "Full trajectories ({.code .at = NULL}) read one track per call.",
      "i" = "Pass {.arg .at}, or quantify {.val {track_names}} one at a time."
    ), class = c("reindeer_quantify_error", "reindeer_error"))
  }

  dots <- list(...)
  if (length(dots) > 0) {
    cli::cli_abort(c(
      "Extra argument{?s} {.val {names(dots)}} have no effect when {.arg dsp_function} names registered tracks.",
      "i" = "Registered-track reads don't take DSP parameters - pass the DSP function itself instead of a character vector if you need that."
    ), class = c("reindeer_quantify_unused_arg_error", "reindeer_error"))
  }

  seg_df <- as.data.frame(object)
  bundle_dir <- file.path(corpus_obj@basePath, paste0(seg_df$session, session.suffix),
                          paste0(seg_df$bundle, bundle.dir.suffix))

  per_track <- lapply(track_names, function(tn) {
    def <- all_tracks[[match(tn, all_names)]]
    files <- file.path(bundle_dir, paste0(seg_df$bundle, ".", def$fileExtension))
    missing <- unique(files[!file.exists(files)])
    if (length(missing) > 0L) {
      cli::cli_abort(c(
        "Track {.val {tn}} has no {.file .{def$fileExtension}} file in {length(missing)} bundle{?s}.",
        "x" = "Missing: {.file {utils::head(basename(missing), 5)}}",
        "i" = "Compute it with {.code quantify(corpus, .using = <dsp function>)}, or quantify the segments with the DSP function itself."
      ), class = c("reindeer_missing_track_file_error", "reindeer_error"))
    }
    column <- def$from %or_empty% (def$columnName %or_empty% tn)
    out <- vector("list", nrow(seg_df))
    for (f in unique(files)) {
      idx <- which(files == f)
      tab <- .read_track_table(f, tn, column, def$index)
      slices <- .slice_table(tab, seg_df$start[idx], seg_df$end[idx])
      out[idx] <- Map(function(sl, i) {
        if (is.null(sl)) return(NULL)
        cbind(.seg = i, .dsp_result_to_df(sl, .at, seg_df$start[i], seg_df$end[i]))
      }, slices, idx)
    }
    empty <- sum(vapply(out, is.null, logical(1)))
    if (empty > 0L) {
      cli::cli_warn("{empty} of {nrow(seg_df)} segment{?s} contain no {.val {tn}} frame and {?was/were} dropped.",
                    class = c("reindeer_quantify_warning", "reindeer_warning"))
    }
    data.table::rbindlist(out)
  })

  res <- per_track[[1]]
  for (k in seq_along(per_track)[-1]) {
    res <- merge(res, per_track[[k]], by = c(".seg", ".time_point"), all = TRUE,
                 suffixes = c("", paste0("_", track_names[k])), sort = TRUE)
  }
  if (nrow(res) == 0L) {
    cli::cli_abort("No segment contains a frame of {.val {track_names}}.",
                   class = c("reindeer_quantify_error", "reindeer_error"))
  }

  combined <- tibble::as_tibble(cbind(seg_df[res$.seg, , drop = FALSE],
                                      as.data.frame(res)[setdiff(names(res), ".seg")]))
  dsp_cols <- setdiff(names(combined), names(seg_df))

  result <- extended_segment_list(
    data = combined, db_uuid = object@db_uuid, db_path = object@db_path,
    dsp_function = paste(track_names, collapse = "|"), dsp_columns = dsp_cols
  )
  .record_step(result, object, "quantify", sys.call(-2L))
}

#' Read one SSFF track file into a frame table (`frame_time` + value columns)
#' @noRd
.read_track_table <- function(file, name, column, index = NULL) {
  obj <- tryCatch(
    superassp::read_track(file, zero_to_na = FALSE, tracks = column),
    error = function(e) {
      cli::cli_abort("Cannot read column {.val {column}} of track {.val {name}} from {.file {file}}.",
                     parent = e, class = c("reindeer_track_read_error", "reindeer_error"))
    })
  mat <- as.matrix(obj[[1]])
  if (!is.null(index)) mat <- mat[, index, drop = FALSE]
  vals <- as.data.frame(mat)
  names(vals) <- if (ncol(vals) > 1L) paste0(name, seq_len(ncol(vals))) else name
  frame_time <- attr(obj, "startTime") + (seq_len(nrow(vals)) - 1) / attr(obj, "sampleRate")
  cbind(frame_time = frame_time, vals)
}
