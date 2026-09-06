# ==============================================================================
# PRINT, SUMMARY, AND GLIMPSE METHODS FOR SEGMENT_LIST
# ==============================================================================

#' Print method for segment_list with pillar formatting
#' @param x The segment_list object
#' @param ... Additional arguments (unused)
#' @param n Number of rows to show
#' @param width Width of the printed table
#' @name print.segment_list
.print_segment_list <- function(x, ..., n = NULL, width = NULL) {
  # Determine number of rows to show
  if (is.null(n)) {
    n <- getOption("pillar.print_max", 10)
  }

  # Header
  cli::cli_rule(
    left = cli::style_bold("segment_list"),
    right = "{cli::col_silver('{nrow(x)} segment{?s}')}"
  )

  if (nrow(x) == 0) {
    cli::cli_alert_warning("Empty segment list")
    return(invisible(x))
  }

  # Quick stats
  n_sessions <- length(unique(x$session))
  n_bundles <- length(unique(x$bundle))
  levels <- unique(x$level)
  types <- unique(x$type)

  cli::cli_text("")
  cli::cli_text(
    "{cli::col_blue(n_sessions)} session{?s}, ",
    "{cli::col_blue(n_bundles)} bundle{?s}, ",
    "level{?s}: {.val {paste(levels, collapse = ', ')}}"
  )

  # Duration range
  durations <- x$end - x$start
  cli::cli_text(
    "Duration: {round(min(durations), 1)}\u2013{round(max(durations), 1)} ms ",
    "(total: {round(sum(durations)/1000, 2)} s)"
  )

  # Label distribution (top 5)
  label_counts <- sort(table(x$labels), decreasing = TRUE)
  top_n <- min(5, length(label_counts))
  if (top_n > 0) {
    top_labels <- names(label_counts)[1:top_n]
    top_counts <- as.integer(label_counts[1:top_n])
    label_str <- paste0(top_labels, " (", top_counts, ")", collapse = ", ")
    cli::cli_text("Top labels: {.val {label_str}}")
  }

  cli::cli_rule()
  cli::cli_text("")

  # Use tibble for nice data display
  tbl <- tibble::as_tibble(as.data.frame(x))
  print(tbl, n = n, width = width, ...)

  invisible(x)
}

#' Summary method for segment_list
#' @param object The segment_list object
#' @param ... Additional arguments (unused)
#' @name summary.segment_list
.summary_segment_list <- function(object, ...) {
  cli::cli_h1("Segment List Summary")

  # Database info
  cli::cli_div(theme = list(rule = list(`margin-top` = 1)))
  cli::cli_h2("Database")
  cli::cli_dl(c(
    "UUID" = object@db_uuid,
    "Path" = if (nchar(object@db_path) > 0) object@db_path else "{.emph (not set)}",
    "Segments" = as.character(nrow(object))
  ))

  if (nrow(object) == 0) {
    cli::cli_alert_warning("Empty segment list")
    return(invisible(object))
  }

  # Structure
  cli::cli_h2("Structure")
  cli::cli_dl(c(
    "Levels" = paste(unique(object$level), collapse = ", "),
    "Types" = paste(unique(object$type), collapse = ", "),
    "Attributes" = paste(unique(object$attribute), collapse = ", "),
    "Sessions" = as.character(length(unique(object$session))),
    "Bundles" = as.character(length(unique(object$bundle)))
  ))

  # Temporal characteristics
  cli::cli_h2("Temporal Characteristics")
  durations <- object$end - object$start

  cli::cli_dl(c(
    "Duration range" = sprintf("%.2f \u2013 %.2f ms", min(durations), max(durations)),
    "Mean duration" = sprintf("%.2f ms", mean(durations)),
    "Median duration" = sprintf("%.2f ms", median(durations)),
    "Total duration" = sprintf("%.2f s", sum(durations) / 1000)
  ))

  if (length(unique(object$sample_rate)) == 1) {
    cli::cli_text("Sample rate: {.val {sprintf('%.0f Hz', unique(object$sample_rate))}}")
  } else {
    cli::cli_text(
      "Sample rate: {.val {sprintf('%.0f \u2013 %.0f Hz', min(object$sample_rate), max(object$sample_rate))}}"
    )
  }

  # Label distribution
  cli::cli_h2("Label Distribution")
  label_counts <- sort(table(object$labels), decreasing = TRUE)
  n_show <- min(10, length(label_counts))

  if (n_show > 0) {
    label_df <- tibble::tibble(
      Label = names(label_counts)[1:n_show],
      Count = as.integer(label_counts[1:n_show]),
      Pct = sprintf("%.1f%%", 100 * as.numeric(label_counts[1:n_show]) / nrow(object))
    )

    # Use tibble for nice table formatting
    print(label_df)

    if (length(label_counts) > n_show) {
      cli::cli_text("{.emph ... and {length(label_counts) - n_show} more label{?s}}")
    }
  }

  # Session/Bundle distribution
  cli::cli_h2("Distribution")
  session_counts <- table(object$session)
  cli::cli_text(
    "Segments per session: mean={round(mean(session_counts), 1)}, ",
    "range={min(session_counts)}\u2013{max(session_counts)}"
  )

  invisible(object)
}

# Implementation function for segment_list
glimpse_segment_list_impl <- function(x, width = NULL, ...) {
  cli::cli_h2("segment_list [{nrow(x)} \u00d7 {ncol(x)}]")

  if (nrow(x) == 0) {
    cli::cli_alert_warning("Empty")
    return(invisible(x))
  }

  # Quick summary
  cli::cli_text(
    "{length(unique(x$session))} session{?s}, ",
    "{length(unique(x$bundle))} bundle{?s}, ",
    "{length(unique(x$level))} level{?s}"
  )

  # Show column structure
  df <- as.data.frame(x)

  # Show first few values for each column
  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    col_type <- class(col_data)[1]

    # Format sample values
    if (is.numeric(col_data)) {
      sample_vals <- head(col_data, 5)
      sample_str <- paste(round(sample_vals, 2), collapse = ", ")
    } else {
      sample_vals <- head(as.character(col_data), 5)
      sample_str <- paste(sample_vals, collapse = ", ")
    }

    cli::cli_text("{.field {col_name}} {.emph <{col_type}>}: {sample_str}...")
  }

  invisible(x)
}

# ==============================================================================
# EXTENDED_SEGMENT_LIST PRINT, SUMMARY, AND GLIMPSE METHODS
# ==============================================================================

#' Print method for extended_segment_list with pillar formatting
#'
#' @param x The object.
#' @param ... Additional arguments (unused)
#' @param n The number of rows to show
#' @param width The width of the printed table.
#' @name print.extended_segment_list
#'
.print_extended_segment_list <- function(x, ..., n = NULL, width = NULL) {
  # Determine number of rows to show
  if (is.null(n)) {
    n <- getOption("pillar.print_max", 10)
  }

  # Header
  cli::cli_rule(
    left = cli::style_bold("extended_segment_list"),
    right = "{cli::col_silver('{nrow(x)} row{?s}')}"
  )

  if (nrow(x) == 0) {
    cli::cli_alert_warning("Empty extended segment list")
    return(invisible(x))
  }

  # Segment counts
  n_unique_segs <- length(unique(x$start_item_id))
  rows_per_seg <- nrow(x) / n_unique_segs

  cli::cli_text("")
  cli::cli_text(
    "{cli::col_blue(n_unique_segs)} segment{?s} \u00d7 ",
    "{cli::col_blue(round(rows_per_seg, 1))} point{?s}/seg = ",
    "{cli::col_blue(nrow(x))} row{?s}"
  )

  # DSP info
  if (nchar(x@dsp_function) > 0) {
    cli::cli_text("DSP: {.fn {x@dsp_function}}")
  }

  if (length(x@dsp_columns) > 0) {
    n_dsp_cols <- length(x@dsp_columns)
    cli::cli_text(
      "Measurements: {cli::col_green(n_dsp_cols)} column{?s} ",
      "({.val {paste(head(x@dsp_columns, 5), collapse = ', ')}}",
      "{if (n_dsp_cols > 5) '...' else ''})"
    )
  }

  # Quick stats on DSP columns
  if (length(x@dsp_columns) > 0) {
    numeric_cols <- x@dsp_columns[sapply(x@dsp_columns, function(col) {
      col %in% names(x) && is.numeric(x[[col]])
    })]

    if (length(numeric_cols) > 0) {
      ranges_str <- sapply(head(numeric_cols, 3), function(col) {
        vals <- x[[col]]
        sprintf("%s: %.1f\u2013%.1f", col, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE))
      })
      cli::cli_text("Ranges: {.val {paste(ranges_str, collapse = ', ')}}")
    }
  }

  cli::cli_rule()
  cli::cli_text("")

  # Use tibble for nice data display
  tbl <- tibble::as_tibble(as.data.frame(x))
  print(tbl, n = n, width = width, ...)

  invisible(x)
}

#' Summary method for extended_segment_list
#' @param object The extended_segment_list object
#' @param ... Additional arguments (unused)
#' @name summary.extended_segment_list
.summary_extended_segment_list <- function(object, ...) {
  cli::cli_h1("Extended Segment List Summary")

  # Database info
  cli::cli_h2("Database")
  cli::cli_dl(c(
    "UUID" = object@db_uuid,
    "Path" = if (nchar(object@db_path) > 0) object@db_path else "{.emph (not set)}",
    "Total rows" = as.character(nrow(object))
  ))

  if (nrow(object) == 0) {
    cli::cli_alert_warning("Empty extended segment list")
    return(invisible(object))
  }

  # Segment structure
  n_unique_segs <- length(unique(object$start_item_id))
  rows_per_seg <- nrow(object) / n_unique_segs

  cli::cli_h2("Structure")
  cli::cli_dl(c(
    "Unique segments" = as.character(n_unique_segs),
    "Rows per segment" = sprintf("%.2f", rows_per_seg),
    "Levels" = paste(unique(object$level), collapse = ", "),
    "Sessions" = as.character(length(unique(object$session))),
    "Bundles" = as.character(length(unique(object$bundle)))
  ))

  # DSP information
  if (nchar(object@dsp_function) > 0 || length(object@dsp_columns) > 0) {
    cli::cli_h2("DSP Processing")

    if (nchar(object@dsp_function) > 0) {
      cli::cli_text("Function: {.fn {object@dsp_function}}")
    }

    if (length(object@dsp_columns) > 0) {
      cli::cli_text("Measurement columns ({length(object@dsp_columns)}):")
      cli::cli_ul(object@dsp_columns)
    }
  }

  # Temporal characteristics (on unique segments)
  cli::cli_h2("Temporal Characteristics")
  unique_segs <- object[!duplicated(object$start_item_id), ]
  durations <- unique_segs$end - unique_segs$start

  cli::cli_dl(c(
    "Duration range" = sprintf("%.2f \u2013 %.2f ms", min(durations), max(durations)),
    "Mean duration" = sprintf("%.2f ms", mean(durations)),
    "Total duration" = sprintf("%.2f s", sum(durations) / 1000)
  ))

  # DSP measurement statistics
  if (length(object@dsp_columns) > 0) {
    cli::cli_h2("Measurement Statistics")

    for (col in head(object@dsp_columns, 10)) {
      if (col %in% names(object) && is.numeric(object[[col]])) {
        vals <- object[[col]]
        vals_clean <- vals[!is.na(vals)]

        if (length(vals_clean) > 0) {
          cli::cli_text(
            "{.field {col}}: ",
            "range={sprintf('%.2f\u2013%.2f', min(vals_clean), max(vals_clean))}, ",
            "mean={sprintf('%.2f', mean(vals_clean))}, ",
            "sd={sprintf('%.2f', sd(vals_clean))}, ",
            "NA={sum(is.na(vals))}"
          )
        }
      }
    }

    if (length(object@dsp_columns) > 10) {
      cli::cli_text("{.emph ... and {length(object@dsp_columns) - 10} more column{?s}}")
    }
  }

  invisible(object)
}

# Implementation function for extended_segment_list
glimpse_extended_segment_list_impl <- function(x, width = NULL, ...) {
  cli::cli_h2("extended_segment_list [{nrow(x)} \u00d7 {ncol(x)}]")

  if (nrow(x) == 0) {
    cli::cli_alert_warning("Empty")
    return(invisible(x))
  }

  # Quick summary
  n_unique_segs <- length(unique(x$start_item_id))
  cli::cli_text(
    "{n_unique_segs} segment{?s}, ",
    "{length(x@dsp_columns)} DSP column{?s}"
  )

  if (nchar(x@dsp_function) > 0) {
    cli::cli_text("DSP function: {.fn {x@dsp_function}}")
  }

  cli::cli_text("")

  # Show column structure with emphasis on DSP columns
  df <- as.data.frame(x)

  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    col_type <- class(col_data)[1]

    # Mark DSP columns
    is_dsp <- col_name %in% x@dsp_columns
    prefix <- if (is_dsp) cli::col_green("\u2605") else " "

    # Format sample values
    if (is.numeric(col_data)) {
      sample_vals <- head(col_data, 5)
      sample_str <- paste(round(sample_vals, 2), collapse = ", ")
    } else {
      sample_vals <- head(as.character(col_data), 5)
      sample_str <- paste(sample_vals, collapse = ", ")
    }

    cli::cli_text("{prefix} {.field {col_name}} {.emph <{col_type}>}: {sample_str}...")
  }

  cli::cli_text("")
  cli::cli_text("{cli::col_green('\u2605')} = DSP measurement column")

  invisible(x)
}
