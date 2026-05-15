# ==============================================================================
# nest_by_* helpers for segment_list / extended_segment_list
# ==============================================================================
#
# Sugar over dplyr's group_by + nest workflow. Returns a tibble with one row
# per group and a list-column whose entries are segment_list slices. The
# segment_list class is preserved inside each list cell, and provenance is
# carried as a single nest step on the outer tibble.

# Internal: split a segment_list into per-group sub-segment_lists.
.split_segment_list <- function(seg, by) {
  if (!by %in% names(seg)) {
    cli::cli_abort("{.arg by} {.val {by}} not found in segment_list columns")
  }
  proxy <- if (S7::S7_inherits(seg, segment_list)) {
    .vec_proxy_segment_list(seg)
  } else {
    tibble::as_tibble(seg)
  }
  groups <- split(seq_len(nrow(proxy)), proxy[[by]], drop = TRUE)
  parts <- lapply(groups, function(idx) {
    slice <- proxy[idx, , drop = FALSE]
    if (S7::S7_inherits(seg, extended_segment_list)) {
      extended_segment_list(
        slice,
        db_uuid = seg@db_uuid,
        db_path = seg@db_path,
        dsp_function = seg@dsp_function,
        dsp_columns = seg@dsp_columns
      )
    } else if (S7::S7_inherits(seg, segment_list)) {
      segment_list(slice, db_uuid = seg@db_uuid, db_path = seg@db_path)
    } else {
      tibble::as_tibble(slice)
    }
  })
  tibble::tibble(
    !!by := names(parts),
    data = unname(parts)
  )
}

# Internal: do the nest + record provenance.
.nest_by <- function(seg, by, .key = "data", verb = "nest") {
  out <- .split_segment_list(seg, by)
  names(out)[names(out) == "data"] <- .key

  # Carry segment_list metadata as attrs on the outer tibble.
  if (S7::S7_inherits(seg, segment_list)) {
    attr(out, "db_uuid") <- seg@db_uuid
    attr(out, "db_path") <- seg@db_path
  }
  prov <- attr(seg, "reindeer_provenance")
  attr(out, "reindeer_provenance") <- .append_provenance_row(
    prov, verb, sys.call(-1L),
    .nrow_or_na(seg), .nrow_or_na(out)
  )
  out
}

#' Nest a segment_list by speaker
#'
#' Returns a tibble with one row per speaker and a list-column of
#' per-speaker `segment_list` slices. The speaker column is detected
#' automatically (case-insensitive match on `Speaker`, `SpeakerID`,
#' `speaker`, `speaker_id`) unless `speaker_col` is provided.
#'
#' @param seg A `segment_list`.
#' @param .key Name for the list-column holding the per-group segment_lists.
#' @param speaker_col Name of the speaker column. When `NULL` (default),
#'   the function auto-detects from a list of conventional names.
#' @return A tibble with one row per speaker.
#' @examplesIf interactive()
#' segs |> biographize(corp) |> nest_by_speaker()
#' @export
nest_by_speaker <- function(seg, .key = "data", speaker_col = NULL) {
  if (is.null(speaker_col)) {
    candidates <- c("Speaker", "SpeakerID", "speaker", "speaker_id")
    hits <- intersect(candidates, names(seg))
    if (length(hits) == 0L) {
      cli::cli_abort(c(
        "No speaker column found.",
        "i" = "Run {.fn biographize} first, or pass {.arg speaker_col} explicitly."
      ))
    }
    speaker_col <- hits[1]
  }
  .nest_by(seg, speaker_col, .key = .key, verb = "nest_by_speaker")
}

#' Nest a segment_list by session
#'
#' @inheritParams nest_by_speaker
#' @return A tibble with one row per session.
#' @export
nest_by_session <- function(seg, .key = "data") {
  .nest_by(seg, "session", .key = .key, verb = "nest_by_session")
}

#' Nest a segment_list by bundle
#'
#' Bundles a segment_list by the (session, bundle) pair. The returned tibble
#' has one row per bundle and a list-column with the segment_list slice.
#'
#' @inheritParams nest_by_speaker
#' @return A tibble with one row per (session, bundle) combination.
#' @export
nest_by_bundle <- function(seg, .key = "data") {
  if (!all(c("session", "bundle") %in% names(seg))) {
    cli::cli_abort("segment_list missing {.field session} or {.field bundle}")
  }
  proxy <- if (S7::S7_inherits(seg, segment_list)) {
    .vec_proxy_segment_list(seg)
  } else {
    tibble::as_tibble(seg)
  }
  key <- paste(proxy$session, proxy$bundle, sep = "/")
  groups <- split(seq_len(nrow(proxy)), key, drop = TRUE)
  parts <- lapply(groups, function(idx) {
    slice <- proxy[idx, , drop = FALSE]
    if (S7::S7_inherits(seg, extended_segment_list)) {
      extended_segment_list(
        slice,
        db_uuid = seg@db_uuid,
        db_path = seg@db_path,
        dsp_function = seg@dsp_function,
        dsp_columns = seg@dsp_columns
      )
    } else if (S7::S7_inherits(seg, segment_list)) {
      segment_list(slice, db_uuid = seg@db_uuid, db_path = seg@db_path)
    } else {
      tibble::as_tibble(slice)
    }
  })
  keys <- strsplit(names(parts), "/", fixed = TRUE)
  out <- tibble::tibble(
    session = vapply(keys, `[[`, character(1), 1L),
    bundle  = vapply(keys, `[[`, character(1), 2L)
  )
  out[[.key]] <- unname(parts)
  if (S7::S7_inherits(seg, segment_list)) {
    attr(out, "db_uuid") <- seg@db_uuid
    attr(out, "db_path") <- seg@db_path
  }
  prov <- attr(seg, "reindeer_provenance")
  attr(out, "reindeer_provenance") <- .append_provenance_row(
    prov, "nest_by_bundle", sys.call(),
    .nrow_or_na(seg), .nrow_or_na(out)
  )
  out
}
