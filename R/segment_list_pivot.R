# ==============================================================================
# Track-aware pivot for segment_list / extended_segment_list
# ==============================================================================
#
# `quantify(.at = seq(0, 1, 0.1))` produces wide-form columns named like
# F1_0.0, F1_0.1 ... F1_1.0. Hand-pivoting these to long form is awkward
# because the time component is encoded in the column name. These verbs do
# the pivot in one step, preserving session/bundle/start/end and stripping
# the segment_list class (long form breaks the 1-row-per-segment invariant).
#
# List-column tracks (per-segment numeric vectors, no `.at`) are unnested
# into long form too: each segment yields one row per sample, with a
# `rel_time` column derived from equally-spaced positions in [0, 1].

# Internal: parse a wide track column name like "F1_0.5" into c("F1", 0.5).
# Returns NULL when the name doesn't match.
.parse_track_name <- function(nm, pattern = NULL) {
  pat <- if (is.null(pattern)) {
    "^(.*)_([0-9]+(?:\\.[0-9]+)?)$"
  } else {
    pattern
  }
  m <- regmatches(nm, regexec(pat, nm))[[1]]
  if (length(m) < 3) return(NULL)
  list(track = m[2], rel_time = suppressWarnings(as.numeric(m[3])))
}

# Internal: build a track_long tibble class for pretty downstream printing.
.as_track_long <- function(x, db_uuid = NULL, db_path = NULL,
                            provenance = NULL) {
  if (!inherits(x, "tbl_df")) x <- tibble::as_tibble(x)
  class(x) <- c("track_long", class(x))
  if (!is.null(db_uuid)) attr(x, "db_uuid") <- db_uuid
  if (!is.null(db_path)) attr(x, "db_path") <- db_path
  if (!is.null(provenance)) attr(x, "reindeer_provenance") <- provenance
  x
}

#' Pivot DSP track measurements to long form
#'
#' Convert wide-form track columns (e.g. `F1_0.0, F1_0.1, ..., F1_1.0`
#' produced by `quantify(.at = seq(0, 1, 0.1))`) into long form with a
#' `track`, `rel_time`, and `value` column. List-column tracks
#' (per-segment numeric vectors) are unnested into one row per sample.
#'
#' The long-form result is **not** a `segment_list` (one segment maps to
#' many rows). It is a `track_long` tibble that carries `db_uuid`,
#' `db_path`, and provenance as plain attributes for traceability.
#'
#' @param seg A `segment_list` or `extended_segment_list`.
#' @param cols Tidyselect specification of which columns to pivot. Defaults
#'   to [track_cols()].
#' @param names_to Name of the column receiving the track family (e.g.
#'   `"F1"`).
#' @param values_to Name of the column receiving the measurement value.
#' @param time_to Name of the column receiving the relative time (in [0, 1]
#'   for wide-form columns; positional index normalised to [0, 1] for
#'   list-column tracks).
#' @param .keep_metadata If `TRUE` (default), metadata columns
#'   (see [metadata_cols()]) are carried through.
#' @return A `track_long` tibble.
#' @examplesIf interactive()
#' segs <- query(corp, "Phonetic == V")
#' wide <- quantify(segs, superassp::forest, .at = seq(0, 1, 0.1))
#' long <- pivot_tracks_longer(wide)
#' head(long)
#' @export
pivot_tracks_longer <- function(seg,
                                 cols = NULL,
                                 names_to = "track",
                                 values_to = "value",
                                 time_to = "rel_time",
                                 .keep_metadata = TRUE) {
  if (!is.data.frame(seg)) {
    cli::cli_abort("{.arg seg} must be a segment_list or data.frame")
  }

  required <- intersect(.required_segment_cols(), names(seg))
  track_cols_vec <- if (is.null(cols)) {
    .detect_track_cols(seg)
  } else if (is.character(cols)) {
    intersect(cols, names(seg))
  } else {
    # tidyselect expression
    pos <- tidyselect::eval_select(rlang::enquo(cols), data = seg)
    names(pos)
  }

  if (length(track_cols_vec) == 0L) {
    cli::cli_abort(c(
      "No track columns detected to pivot.",
      "i" = "Did you forget to {.fn quantify}? Try passing {.arg cols} explicitly."
    ))
  }

  # Partition track columns into wide-form (numeric) and list-form.
  is_listcol <- vapply(track_cols_vec, function(c) is.list(seg[[c]]),
                       logical(1))
  wide_cols <- track_cols_vec[!is_listcol]
  list_cols <- track_cols_vec[is_listcol]

  keep_meta <- if (isTRUE(.keep_metadata)) {
    setdiff(names(seg), c(track_cols_vec))
  } else {
    required
  }
  keep_meta <- intersect(keep_meta, names(seg))

  pieces <- list()

  # Wide-form pivot: one row per (segment, track, rel_time).
  if (length(wide_cols) > 0L) {
    parsed <- lapply(wide_cols, .parse_track_name)
    valid <- !vapply(parsed, is.null, logical(1))
    if (!all(valid)) {
      # Drop unparseable wide cols silently — they may be scalar measurements
      # without a time suffix (e.g. mean F1).
      wide_cols <- wide_cols[valid]
      parsed <- parsed[valid]
    }
    if (length(wide_cols) > 0L) {
      proxy_w <- seg[, c(keep_meta, wide_cols), drop = FALSE]
      proxy_w <- tibble::as_tibble(proxy_w)
      # Manual long-pivot to avoid hard tidyr dep.
      stacked <- lapply(seq_along(wide_cols), function(i) {
        col <- wide_cols[i]
        info <- parsed[[i]]
        row <- proxy_w[, keep_meta, drop = FALSE]
        row[[names_to]] <- info$track
        row[[time_to]] <- info$rel_time
        row[[values_to]] <- proxy_w[[col]]
        row
      })
      pieces$wide <- do.call(rbind, stacked)
    }
  }

  # List-column pivot: one row per (segment, track, sample).
  if (length(list_cols) > 0L) {
    list_pieces <- lapply(list_cols, function(col) {
      vals <- seg[[col]]
      lens <- vapply(vals, length, integer(1))
      idx <- rep(seq_along(vals), lens)
      base <- seg[idx, keep_meta, drop = FALSE]
      base <- tibble::as_tibble(base)
      base[[names_to]] <- col
      base[[time_to]] <- unlist(lapply(lens, function(n) {
        if (n <= 1L) 0 else seq(0, 1, length.out = n)
      }), use.names = FALSE)
      base[[values_to]] <- unlist(vals, use.names = FALSE)
      base
    })
    pieces$list <- do.call(rbind, list_pieces)
  }

  out <- if (length(pieces) == 0L) {
    tibble::tibble()
  } else if (length(pieces) == 1L) {
    pieces[[1]]
  } else {
    # Align columns before rbind
    common <- Reduce(intersect, lapply(pieces, names))
    do.call(rbind, lapply(pieces, function(p) p[, common, drop = FALSE]))
  }

  db_uuid <- if (S7::S7_inherits(seg, segment_list)) seg@db_uuid else NULL
  db_path <- if (S7::S7_inherits(seg, segment_list)) seg@db_path else NULL
  prov <- attr(seg, "reindeer_provenance")

  out <- .as_track_long(out, db_uuid = db_uuid, db_path = db_path,
                        provenance = prov)
  # Record the pivot as a provenance step. Long form has more rows than the
  # input segment_list, so "rows_lost" is negative; that's expected and
  # diagnostic.
  attr(out, "reindeer_provenance") <- .append_provenance_row(
    prov, "pivot_tracks_longer", sys.call(),
    .nrow_or_na(seg), .nrow_or_na(out)
  )
  out
}

#' Pivot long-form track data back to wide form
#'
#' Inverse of [pivot_tracks_longer()]: takes a `track_long` tibble (or
#' anything with `track`, `rel_time`, `value` columns plus segment-identifying
#' columns) and reconstructs wide-form columns named `<track>_<rel_time>`.
#'
#' The result is a tibble, not a `segment_list` — pivoting back doesn't
#' recover the validator's required columns unless they were preserved on the
#' way in. When the input has all required segment columns plus a `db_uuid`
#' attribute, the result is upgraded to a `segment_list`.
#'
#' @param long A `track_long` tibble or compatible data frame.
#' @param names_from Name of the column holding the track family.
#' @param values_from Name of the column holding the measurement value.
#' @param time_from Name of the column holding the relative time.
#' @return A tibble or `segment_list`.
#' @examplesIf interactive()
#' long <- pivot_tracks_longer(wide)
#' restored <- pivot_tracks_wider(long)
#' @export
pivot_tracks_wider <- function(long,
                                names_from = "track",
                                values_from = "value",
                                time_from = "rel_time") {
  if (!is.data.frame(long)) {
    cli::cli_abort("{.arg long} must be a data.frame")
  }
  missing <- setdiff(c(names_from, values_from, time_from), names(long))
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg long} missing column{?s}: {.val {missing}}")
  }

  id_cols <- setdiff(names(long), c(names_from, values_from, time_from))

  new_names <- paste(long[[names_from]], long[[time_from]], sep = "_")
  long$.col <- new_names

  # Build wide via reshape (base R) to avoid hard tidyr/data.table dep.
  ids <- unique(long[, id_cols, drop = FALSE])
  ids$.row_id <- seq_len(nrow(ids))
  long_keyed <- merge(long, ids, by = id_cols, sort = FALSE)
  wide <- stats::reshape(
    data = long_keyed[, c(".row_id", ".col", values_from)],
    idvar = ".row_id",
    timevar = ".col",
    direction = "wide"
  )
  # reshape prepends "<values_from>." — strip it.
  prefix <- paste0(values_from, ".")
  names(wide) <- sub(paste0("^", prefix), "", names(wide))

  out <- merge(ids, wide, by = ".row_id", sort = FALSE)
  out$.row_id <- NULL
  out <- tibble::as_tibble(out)

  required <- .required_segment_cols()
  db_uuid <- attr(long, "db_uuid")
  db_path <- attr(long, "db_path")
  if (all(required %in% names(out)) &&
        is.character(db_uuid) && length(db_uuid) == 1L) {
    out <- segment_list(out, db_uuid = db_uuid,
                        db_path = db_path %||% "")
  }
  out
}

# Null-coalesce; rlang has %||%, but we don't import it elsewhere here.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Print method for track_long tibbles
#'
#' @param x A `track_long` object.
#' @param ... Passed to the tibble printer.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.track_long <- function(x, ...) {
  cli::cli_h2("Track-long table")
  n_tracks <- length(unique(x[["track"]]))
  cli::cli_alert_info("{nrow(x)} row{?s} × {ncol(x)} col{?s}; {n_tracks} track{?s}")
  NextMethod()
}
