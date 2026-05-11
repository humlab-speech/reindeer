# ==============================================================================
# vctrs + dplyr extension hooks for segment_list / extended_segment_list
# ==============================================================================
#
# These let tidyverse verbs (filter, mutate, select, arrange, slice) and base
# bracket subsetting return a segment_list with `db_uuid` / `db_path`
# preserved, instead of degrading to a plain tibble. The methods are registered
# at .onLoad time (see zzz.R) because their generic class names contain `::`.

#' @keywords internal
.vec_proxy_segment_list <- function(x, ...) {
  attrs_to_strip <- c("db_uuid", "db_path")
  out <- unclass(x)
  class(out) <- setdiff(class(x), c("reindeer::segment_list", "S7_object"))
  for (a in attrs_to_strip) attr(out, a) <- NULL
  out
}

#' @keywords internal
.vec_restore_segment_list <- function(x, to, ...) {
  out <- segment_list(x, db_uuid = to@db_uuid, db_path = to@db_path)
  prov <- attr(to, "reindeer_provenance")
  if (!is.null(prov)) attr(out, "reindeer_provenance") <- prov
  out
}

#' @keywords internal
.dplyr_reconstruct_segment_list <- function(data, template) {
  required_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate"
  )
  if (!all(required_cols %in% names(data))) {
    return(tibble::as_tibble(data))
  }
  out <- segment_list(data, db_uuid = template@db_uuid, db_path = template@db_path)
  .record_step(out, template, "dplyr_op", warn = FALSE)
}

#' @keywords internal
.vec_proxy_extended_segment_list <- function(x, ...) {
  out <- unclass(x)
  class(out) <- setdiff(
    class(x),
    c("reindeer::extended_segment_list", "reindeer::segment_list", "S7_object")
  )
  for (a in c("db_uuid", "db_path", "dsp_function", "dsp_columns")) {
    attr(out, a) <- NULL
  }
  out
}

#' @keywords internal
.vec_restore_extended_segment_list <- function(x, to, ...) {
  out <- extended_segment_list(
    x,
    db_uuid = to@db_uuid,
    db_path = to@db_path,
    dsp_function = to@dsp_function,
    dsp_columns = to@dsp_columns
  )
  prov <- attr(to, "reindeer_provenance")
  if (!is.null(prov)) attr(out, "reindeer_provenance") <- prov
  out
}

#' @keywords internal
.bracket_segment_list <- function(x, i, j, ..., drop = FALSE) {
  proxy <- .vec_proxy_segment_list(x)
  # Match tibble semantics: x[loc] selects columns (1-arg), x[i, ] rows (2-arg)
  n <- nargs() - !missing(drop)
  out <- if (n <= 1L) {
    proxy
  } else if (n == 2L) {
    proxy[i]
  } else if (missing(i) && missing(j)) {
    proxy
  } else if (missing(j)) {
    proxy[i, , drop = drop]
  } else if (missing(i)) {
    proxy[, j, drop = drop]
  } else {
    proxy[i, j, drop = drop]
  }
  if (!is.data.frame(out)) return(out)
  .dplyr_reconstruct_segment_list(out, x)
}

#' @keywords internal
.bracket_extended_segment_list <- function(x, i, j, ..., drop = FALSE) {
  proxy <- .vec_proxy_extended_segment_list(x)
  n <- nargs() - !missing(drop)
  out <- if (n <= 1L) {
    proxy
  } else if (n == 2L) {
    proxy[i]
  } else if (missing(i) && missing(j)) {
    proxy
  } else if (missing(j)) {
    proxy[i, , drop = drop]
  } else if (missing(i)) {
    proxy[, j, drop = drop]
  } else {
    proxy[i, j, drop = drop]
  }
  if (!is.data.frame(out)) return(out)
  .dplyr_reconstruct_extended_segment_list(out, x)
}

# ---------------------------------------------------------------------------
# *_join hooks: dplyr's reconstruct pipeline records "dplyr_op" for every
# verb. Joins deserve a named provenance step so users can see which kind
# of join dropped rows. These wrappers run the join on the tibble proxy,
# rebuild the segment_list, and log the join verb + row loss. warn=TRUE so
# loss above reindeer.loss_warn threshold fires a cli warning.
# ---------------------------------------------------------------------------

#' @keywords internal
.segment_list_join <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ..., keep = NULL,
                               .verb = "inner_join", .fn = dplyr::inner_join) {
  proxy <- .vec_proxy_segment_list(x)
  out <- .fn(proxy, y, by = by, copy = copy, suffix = suffix, ..., keep = keep)
  if (!is.data.frame(out)) return(out)
  required_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate"
  )
  if (!all(required_cols %in% names(out))) {
    return(tibble::as_tibble(out))
  }
  rebuilt <- segment_list(out, db_uuid = x@db_uuid, db_path = x@db_path)
  .record_step(rebuilt, x, .verb, warn = TRUE)
}

#' @keywords internal
.segment_list_filter_join <- function(x, y, by = NULL, copy = FALSE, ...,
                                      .verb = "anti_join",
                                      .fn = dplyr::anti_join) {
  proxy <- .vec_proxy_segment_list(x)
  out <- .fn(proxy, y, by = by, copy = copy, ...)
  if (!is.data.frame(out)) return(out)
  rebuilt <- segment_list(out, db_uuid = x@db_uuid, db_path = x@db_path)
  .record_step(rebuilt, x, .verb, warn = TRUE)
}

.left_join_segment_list  <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) .segment_list_join(x, y, by, copy, suffix, ..., keep = keep, .verb = "left_join",  .fn = dplyr::left_join)
.right_join_segment_list <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) .segment_list_join(x, y, by, copy, suffix, ..., keep = keep, .verb = "right_join", .fn = dplyr::right_join)
.inner_join_segment_list <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) .segment_list_join(x, y, by, copy, suffix, ..., keep = keep, .verb = "inner_join", .fn = dplyr::inner_join)
.full_join_segment_list  <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) .segment_list_join(x, y, by, copy, suffix, ..., keep = keep, .verb = "full_join",  .fn = dplyr::full_join)
.anti_join_segment_list  <- function(x, y, by = NULL, copy = FALSE, ...)        .segment_list_filter_join(x, y, by, copy, ..., .verb = "anti_join", .fn = dplyr::anti_join)
.semi_join_segment_list  <- function(x, y, by = NULL, copy = FALSE, ...)        .segment_list_filter_join(x, y, by, copy, ..., .verb = "semi_join", .fn = dplyr::semi_join)

#' @keywords internal
.dplyr_reconstruct_extended_segment_list <- function(data, template) {
  required_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate"
  )
  if (!all(required_cols %in% names(data))) {
    return(tibble::as_tibble(data))
  }
  remaining_dsp <- intersect(template@dsp_columns, names(data))
  out <- extended_segment_list(
    data,
    db_uuid = template@db_uuid,
    db_path = template@db_path,
    dsp_function = template@dsp_function,
    dsp_columns = remaining_dsp
  )
  .record_step(out, template, "dplyr_op", warn = FALSE)
}
