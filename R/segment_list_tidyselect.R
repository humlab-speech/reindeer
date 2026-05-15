# ==============================================================================
# Tidyselect helpers for segment_list / extended_segment_list
# ==============================================================================
#
# `metadata_cols()`, `track_cols()`, `signal_cols()` are tidyselect helpers
# users can call inside dplyr::select(), pivot_tracks_longer(), etc. They
# inspect the data via tidyselect's `peek_data()` and partition columns into
# (a) required segment_list columns, (b) DSP-derived track columns, and
# (c) other metadata-style columns.

# Default regex for wide-form track column names produced by `quantify(.at=)`.
# Pattern: <track>_<rel_time> where rel_time is a decimal in [0,1].
.default_track_pattern <- "_[0-9]+(\\.[0-9]+)?$"

# Internal: get the names of DSP-derived columns from a segment_list, falling
# back to a regex match on the column name when the @dsp_columns slot is
# unavailable (e.g. plain tibble).
.detect_track_cols <- function(data, pattern = NULL) {
  nms <- names(data)
  required <- .required_segment_cols()
  candidates <- setdiff(nms, required)

  # If data is an extended_segment_list, trust its @dsp_columns slot.
  if (S7::S7_inherits(data, extended_segment_list)) {
    declared <- tryCatch(data@dsp_columns, error = function(e) character())
    declared <- intersect(declared, candidates)
    if (length(declared) > 0) return(declared)
  }

  # Otherwise fall back to regex on wide-form names.
  pat <- if (is.null(pattern)) .default_track_pattern else pattern
  hits <- grep(pat, candidates, value = TRUE)
  # Plus any list-columns (nested per-segment time series)
  list_cols <- candidates[vapply(candidates, function(c) is.list(data[[c]]),
                                  logical(1))]
  unique(c(hits, list_cols))
}

#' Select required segment_list columns
#'
#' Tidyselect helper for the 16 columns that every `segment_list` must carry
#' (`labels`, `start`, `end`, `db_uuid`, `session`, `bundle`, item IDs, level,
#' attribute, sequence indices, type, sample bounds, sample rate). Use it
#' inside `dplyr::select()` or any other tidyselect-aware verb to keep or
#' drop the segment columns as a group.
#'
#' @return A character vector of column names present in the current
#'   tidyselect context.
#' @examplesIf interactive()
#' segs |> dplyr::select(segment_cols())          # keep only required cols
#' segs |> dplyr::select(-segment_cols())         # drop them
#' @export
segment_cols <- function() {
  data <- tidyselect::peek_data(fn = "segment_cols")
  intersect(.required_segment_cols(), names(data))
}

#' Select metadata-style columns
#'
#' Tidyselect helper for columns that are neither required segment columns
#' (see [segment_cols()]) nor DSP-derived track columns (see [track_cols()]).
#' Typically this captures speaker/bundle metadata that `biographize()` or
#' `enrich()` joined onto the segment_list.
#'
#' @return Character vector of column names.
#' @examplesIf interactive()
#' segs |> dplyr::select(metadata_cols())
#' @export
metadata_cols <- function() {
  data <- tidyselect::peek_data(fn = "metadata_cols")
  required <- intersect(.required_segment_cols(), names(data))
  tracks <- .detect_track_cols(data)
  setdiff(names(data), c(required, tracks))
}

#' Select DSP-derived track columns
#'
#' Tidyselect helper matching wide-form measurement columns produced by
#' [quantify()] with a `.at` argument (e.g. `F1_0.0`, `F1_0.5`, `F1_1.0`)
#' and list-column tracks (per-segment time series). When given a custom
#' `pattern` argument, the regex overrides the default.
#'
#' @param pattern Optional regex used to detect wide-form track names. The
#'   default matches the `_<numeric>` suffix produced by `quantify(.at=)`.
#' @return Character vector of column names.
#' @examplesIf interactive()
#' segs |> dplyr::select(track_cols())                # all tracks
#' segs |> dplyr::select(track_cols("^F\\d_"))         # only formants
#' @export
track_cols <- function(pattern = NULL) {
  data <- tidyselect::peek_data(fn = "track_cols")
  .detect_track_cols(data, pattern)
}

#' Select signal/list columns
#'
#' Tidyselect helper for list-column track data (raw per-segment vectors of
#' values, e.g. one F0 contour per segment). Complement of the wide-form
#' track columns selected by [track_cols()] with the default pattern.
#'
#' @return Character vector of column names.
#' @export
signal_cols <- function() {
  data <- tidyselect::peek_data(fn = "signal_cols")
  required <- intersect(.required_segment_cols(), names(data))
  candidates <- setdiff(names(data), required)
  candidates[vapply(candidates, function(c) is.list(data[[c]]), logical(1))]
}
