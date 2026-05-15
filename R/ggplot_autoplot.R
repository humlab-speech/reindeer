# ==============================================================================
# ggplot2 autoplot for segment_list / extended_segment_list
# ==============================================================================
#
# ggplot2 is in Suggests, not Imports, so these methods gracefully error if
# ggplot2 isn't installed. The S3 methods are registered in zzz.R against
# the S7 mangled class names "reindeer::segment_list" and
# "reindeer::extended_segment_list".

# Internal: require ggplot2 or abort with the missing-companion class.
.require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .companion_abort("ggplot2", purpose = "autoplot / geoms")
  }
}

# Internal: pick the most plausible track type given which columns exist.
.detect_autoplot_type <- function(object) {
  nms <- names(object)
  if (any(grepl("^F[12345]_", nms))) return("formants")
  if (any(grepl("^(F0|pitch|f0)_", nms, ignore.case = TRUE))) return("pitch")
  "labels"
}

# Internal: autoplot for a plain segment_list (no DSP). Only the labels view
# is supported here because there are no track columns to plot.
.autoplot_segment_list <- function(object, type = c("labels", "spectrogram"),
                                    ...) {
  .require_ggplot2()
  type <- match.arg(type)
  if (type == "spectrogram") {
    cli::cli_alert_info(
      paste0("Spectrogram view requires raw signal access via {.pkg superassp}; ",
             "falling back to labels view.")
    )
    type <- "labels"
  }
  plot_df <- tibble::tibble(
    session = object$session,
    bundle = object$bundle,
    start = object$start,
    end = object$end,
    label = object$labels,
    level = object$level
  )
  plot_df$xmid <- (plot_df$start + plot_df$end) / 2
  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$start, y = .data$level)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$end, yend = .data$level),
      linewidth = 4, color = "steelblue"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$xmid, label = .data$label),
      vjust = -0.6, size = 3
    ) +
    ggplot2::facet_wrap(~bundle, scales = "free_x") +
    ggplot2::labs(x = "Time (ms)", y = "Level",
                  title = "segment_list labels")
}

# Internal: autoplot for an extended_segment_list. Routes to one of:
#   formants  - per-segment F1..Fn trajectories
#   pitch     - per-segment F0 / pitch trajectory
#   labels    - label tier view (same as segment_list)
.autoplot_extended_segment_list <- function(object,
                                              type = c("auto", "formants",
                                                       "pitch", "spectrogram",
                                                       "labels"),
                                              ...) {
  .require_ggplot2()
  type <- match.arg(type)
  if (type == "auto") type <- .detect_autoplot_type(object)
  if (type == "spectrogram") {
    cli::cli_alert_info(
      paste0("Spectrogram view requires raw signal access via {.pkg superassp}; ",
             "falling back to labels view.")
    )
    return(.autoplot_segment_list(object, type = "labels", ...))
  }
  if (type == "labels") {
    return(.autoplot_segment_list(object, type = "labels", ...))
  }

  long <- pivot_tracks_longer(object, .keep_metadata = TRUE)
  if (type == "formants") {
    long <- long[grepl("^F[12345]$", long$track), , drop = FALSE]
    if (nrow(long) == 0L) {
      cli::cli_abort("No formant tracks (F1..F5) found to plot.")
    }
    long$.seg_id <- paste(long$session, long$bundle, long$start, sep = "/")
    return(
      ggplot2::ggplot(long,
        ggplot2::aes(x = .data$rel_time, y = .data$value,
                     color = .data$track, group = .data$.seg_id)) +
        ggplot2::geom_line(alpha = 0.4) +
        ggplot2::labs(x = "Relative time (0-1)", y = "Frequency (Hz)",
                      color = "Formant",
                      title = "Formant trajectories")
    )
  }
  if (type == "pitch") {
    long <- long[grepl("^(F0|pitch|f0)$", long$track,
                       ignore.case = TRUE), , drop = FALSE]
    if (nrow(long) == 0L) {
      cli::cli_abort("No pitch track (F0/pitch) found to plot.")
    }
    long$.seg_id <- paste(long$session, long$bundle, long$start, sep = "/")
    return(
      ggplot2::ggplot(long,
        ggplot2::aes(x = .data$rel_time, y = .data$value,
                     group = .data$.seg_id)) +
        ggplot2::geom_line(alpha = 0.4, color = "firebrick") +
        ggplot2::labs(x = "Relative time (0-1)", y = "F0 (Hz)",
                      title = "Pitch trajectories")
    )
  }
  cli::cli_abort("Unknown {.arg type}: {.val {type}}")
}
