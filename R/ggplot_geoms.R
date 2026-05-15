# ==============================================================================
# ggplot2 helper geoms / annotations for segment_list data
# ==============================================================================
#
# These are thin wrappers around ggplot2 layer constructors that pick
# sensible defaults for the canonical segment_list / track_long shape
# (output of pivot_tracks_longer). They keep the user from having to
# memorise aes-mapping plumbing for the common views.

#' ggplot2 layer: formant trajectory lines
#'
#' Adds a `geom_line` layer over a `track_long` data frame, drawing one
#' line per (segment, formant) pair. Requires columns `rel_time`,
#' `value`, `track`, plus a segment identifier built from
#' `session`/`bundle`/`start`.
#'
#' @param mapping Default mapping (passed to `ggplot2::geom_line`).
#' @param data Default data (passed to `ggplot2::geom_line`).
#' @param formants Integer vector of formant numbers to keep.
#' @param ... Additional arguments to `ggplot2::geom_line`.
#' @return A `Layer` object.
#' @examplesIf interactive()
#' long <- pivot_tracks_longer(quantify_result)
#' ggplot2::ggplot(long) + geom_formant_trajectory()
#' @export
geom_formant_trajectory <- function(mapping = NULL, data = NULL,
                                     formants = 1:3, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .companion_abort("ggplot2", purpose = "geom_formant_trajectory")
  }
  if (is.null(data)) {
    layer_data <- function(d) {
      d <- d[grepl(paste0("^F[", paste(formants, collapse = ""), "]$"),
                    d$track), , drop = FALSE]
      d$.seg_id <- paste(d$session, d$bundle, d$start, sep = "/")
      d
    }
  } else {
    data <- data[grepl(paste0("^F[", paste(formants, collapse = ""), "]$"),
                         data$track), , drop = FALSE]
    data$.seg_id <- paste(data$session, data$bundle, data$start, sep = "/")
    layer_data <- NULL
  }
  ggplot2::geom_line(
    mapping = mapping %||% ggplot2::aes(
      x = .data$rel_time, y = .data$value,
      color = .data$track, group = .data$.seg_id
    ),
    data = if (!is.null(layer_data)) layer_data else data,
    ...
  )
}

#' ggplot2 layer: label tier strip
#'
#' Adds a horizontal label-tier strip to a ggplot. Pairs well with track
#' plots so that label boundaries are visible. Draws `geom_segment` for
#' each interval at `tier_y` and overlays `geom_text` with the label.
#'
#' @param mapping Default mapping.
#' @param data Default data.
#' @param tier_y Numeric y-position for the tier strip.
#' @param ... Forwarded to `ggplot2::geom_segment` / `geom_text`.
#' @return A list of ggplot2 `Layer` objects.
#' @export
geom_label_tier <- function(mapping = NULL, data = NULL, tier_y = 0, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .companion_abort("ggplot2", purpose = "geom_label_tier")
  }
  list(
    ggplot2::geom_segment(
      mapping = mapping %||% ggplot2::aes(
        x = .data$start, xend = .data$end,
        y = tier_y, yend = tier_y
      ),
      data = data, linewidth = 3, color = "steelblue", ...
    ),
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x = (.data$start + .data$end) / 2,
        y = tier_y, label = .data$labels
      ),
      data = data, vjust = -0.6, size = 3, ...
    )
  )
}

#' ggplot2 layer: pitch (F0) trajectory lines
#'
#' Adds a `geom_line` layer over a `track_long` data frame, drawing one
#' line per segment of the F0/pitch track.
#'
#' @inheritParams geom_formant_trajectory
#' @return A `Layer` object.
#' @export
geom_pitch_track <- function(mapping = NULL, data = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .companion_abort("ggplot2", purpose = "geom_pitch_track")
  }
  if (is.null(data)) {
    layer_data <- function(d) {
      d <- d[grepl("^(F0|pitch|f0)$", d$track, ignore.case = TRUE),
              , drop = FALSE]
      d$.seg_id <- paste(d$session, d$bundle, d$start, sep = "/")
      d
    }
  } else {
    data <- data[grepl("^(F0|pitch|f0)$", data$track, ignore.case = TRUE),
                  , drop = FALSE]
    data$.seg_id <- paste(data$session, data$bundle, data$start, sep = "/")
    layer_data <- NULL
  }
  ggplot2::geom_line(
    mapping = mapping %||% ggplot2::aes(
      x = .data$rel_time, y = .data$value, group = .data$.seg_id
    ),
    data = if (!is.null(layer_data)) layer_data else data,
    color = "firebrick", alpha = 0.4, ...
  )
}
