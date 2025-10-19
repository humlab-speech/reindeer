#' Segment List S7 Class
#' 
#' An S7 class representing a segment list returned by EMU queries.
#' This class extends tibble with validation to ensure compatibility
#' with emuR::query() results.
#' 
#' @section Structure:
#' A segment_list must contain the following columns:
#' - labels: Character vector of segment labels
#' - start: Numeric start time in milliseconds
#' - end: Numeric end time in milliseconds  
#' - db_uuid: Character UUID of database
#' - session: Character session name
#' - bundle: Character bundle name
#' - start_item_id: Integer start item ID
#' - end_item_id: Integer end item ID
#' - level: Character level name
#' - attribute: Character attribute name
#' - start_item_seq_idx: Integer start sequence index
#' - end_item_seq_idx: Integer end sequence index
#' - type: Character type (SEGMENT, EVENT, ITEM)
#' - sample_start: Integer sample start
#' - sample_end: Integer sample end
#' - sample_rate: Numeric sample rate
#' 
#' @examples
#' \dontrun{
#' # Query returns a segment_list
#' segs <- query(corpus, "Phonetic == t")
#' 
#' # Apply DSP to segments
#' result <- quantify(segs, dsp_function)
#' }

library(S7)

#' @export
segment_list <- S7::new_class(
  "segment_list",
  parent = class_data.frame,
  properties = list(
    db_uuid = class_character,
    db_path = class_character
  ),
  validator = function(self) {
    # Required columns from emuR::query() results
    required_cols <- c(
      "labels", "start", "end", "db_uuid", "session", "bundle",
      "start_item_id", "end_item_id", "level", "attribute",
      "start_item_seq_idx", "end_item_seq_idx", "type",
      "sample_start", "sample_end", "sample_rate"
    )
    
    missing_cols <- setdiff(required_cols, names(self))
    if (length(missing_cols) > 0) {
      return(paste0(
        "segment_list missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ))
    }
    
    # Check column types
    if (!is.character(self$labels)) {
      return("'labels' must be character")
    }
    if (!is.numeric(self$start) || !is.numeric(self$end)) {
      return("'start' and 'end' must be numeric")
    }
    if (!is.character(self$db_uuid) || !is.character(self$session) || 
        !is.character(self$bundle)) {
      return("'db_uuid', 'session', and 'bundle' must be character")
    }
    
    NULL
  },
  constructor = function(data, db_uuid = NULL, db_path = NULL) {
    # Convert to data.frame if needed
    if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
      data <- as.data.frame(data)
    } else if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
    
    # Extract db_uuid from data if not provided
    if (is.null(db_uuid) && "db_uuid" %in% names(data)) {
      db_uuid_vals <- unique(data$db_uuid)
      if (length(db_uuid_vals) > 1) {
        warning("Multiple db_uuids found; using first")
        db_uuid <- as.character(db_uuid_vals[1])
      } else {
        db_uuid <- as.character(db_uuid_vals[1])
      }
    }
    
    if (is.null(db_uuid) || length(db_uuid) == 0) {
      db_uuid <- ""
    }
    
    if (is.null(db_path) || length(db_path) == 0) {
      db_path <- ""
    }
    
    S7::new_object(
      .parent = data,
      db_uuid = as.character(db_uuid),
      db_path = as.character(db_path)
    )
  }
)

#' Print method for segment_list
#' @export
S7::method(print, segment_list) <- function(x, ..., n = 10) {
  # Header with styling
  cli::cli_rule(left = "Segment List", right = "{nrow(x)} segment{?s}")
  
  if (nrow(x) > 0) {
    # Database info
    cli::cli_alert_info("Database: {.val {x@db_uuid}}")
    if (nchar(x@db_path) > 0) {
      cli::cli_alert_info("Path: {.path {x@db_path}}")
    }
    
    # Key statistics in a compact format
    cli::cli_text("")
    cli::cli_text("{.strong Levels:} {paste(unique(x$level), collapse = ', ')}")
    cli::cli_text("{.strong Types:} {paste(unique(x$type), collapse = ', ')}")
    cli::cli_text("{.strong Sessions:} {length(unique(x$session))}")
    cli::cli_text("{.strong Bundles:} {length(unique(x$bundle))}")
    
    # Duration statistics
    durations <- x$end - x$start
    cli::cli_text("{.strong Duration:} {round(min(durations), 1)} - {round(max(durations), 1)} ms (total: {round(sum(durations)/1000, 2)} s)")
    
    # Label distribution
    label_counts <- table(x$labels)
    top_labels <- head(sort(label_counts, decreasing = TRUE), 5)
    if (length(top_labels) > 0) {
      label_summary <- paste(names(top_labels), " (", top_labels, ")", sep = "", collapse = ", ")
      cli::cli_text("{.strong Top labels:} {label_summary}")
      if (length(label_counts) > 5) {
        cli::cli_text("{.emph ... and {length(label_counts) - 5} more}")
      }
    }
    
    cli::cli_text("")
    cli::cli_rule()
    
    # Show data
    print(tibble::as_tibble(as.data.frame(x)), n = n, ...)
  } else {
    cli::cli_alert_warning("Empty segment list")
  }
  
  invisible(x)
}

#' Summary method for segment_list  
#' @export
S7::method(summary, segment_list) <- function(object, ...) {
  cli::cli_h1("Segment List Summary")
  
  # Database info
  cli::cli_h2("Database Information")
  cli::cli_dl(c(
    "UUID" = object@db_uuid,
    "Path" = if (nchar(object@db_path) > 0) object@db_path else "(not specified)",
    "Segments" = as.character(nrow(object))
  ))
  
  if (nrow(object) > 0) {
    # Structure info
    cli::cli_h2("Structure")
    cli::cli_dl(c(
      "Levels" = paste(unique(object$level), collapse = ", "),
      "Types" = paste(unique(object$type), collapse = ", "),
      "Attributes" = paste(unique(object$attribute), collapse = ", "),
      "Sessions" = as.character(length(unique(object$session))),
      "Bundles" = as.character(length(unique(object$bundle)))
    ))
    
    # Temporal info
    cli::cli_h2("Temporal Characteristics")
    durations <- object$end - object$start
    cli::cli_dl(c(
      "Duration range" = sprintf("%.3f - %.3f ms", min(durations), max(durations)),
      "Mean duration" = sprintf("%.3f ms", mean(durations)),
      "Median duration" = sprintf("%.3f ms", median(durations)),
      "Total duration" = sprintf("%.3f s", sum(durations) / 1000),
      "Sample rate" = if (length(unique(object$sample_rate)) == 1) {
        sprintf("%.0f Hz", unique(object$sample_rate))
      } else {
        sprintf("%.0f - %.0f Hz", min(object$sample_rate), max(object$sample_rate))
      }
    ))
    
    # Label distribution
    cli::cli_h2("Label Distribution")
    label_counts <- sort(table(object$labels), decreasing = TRUE)
    n_show <- min(10, length(label_counts))
    
    if (n_show > 0) {
      label_df <- data.frame(
        Label = names(label_counts)[1:n_show],
        Count = as.integer(label_counts[1:n_show]),
        Percentage = sprintf("%.1f%%", 100 * as.numeric(label_counts[1:n_show]) / nrow(object))
      )
      print(knitr::kable(label_df, format = "simple", align = c("l", "r", "r")))
      
      if (length(label_counts) > n_show) {
        cli::cli_text("{.emph ... and {length(label_counts) - n_show} more label{?s}}")
      }
    }
    
    # Session/Bundle breakdown
    cli::cli_h2("Distribution by Session/Bundle")
    session_counts <- table(object$session)
    bundle_counts <- table(object$bundle)
    cli::cli_dl(c(
      "Segments per session" = sprintf("%.1f (range: %d - %d)", 
                                        mean(session_counts), 
                                        min(session_counts), 
                                        max(session_counts)),
      "Unique bundles" = as.character(length(unique(object$bundle)))
    ))
  } else {
    cli::cli_alert_warning("Empty segment list")
  }
  
  invisible(object)
}

#' Convert data.frame to segment_list
#' @export
as_segment_list <- function(x, db_uuid = NULL, db_path = NULL) {
  if (inherits(x, "segment_list")) {
    return(x)
  }
  segment_list(x, db_uuid = db_uuid, db_path = db_path)
}

#' Check if object is a segment_list
#' @export
is_segment_list <- function(x) {
  inherits(x, "segment_list")
}

#' Extended Segment List S7 Class
#' 
#' An S7 class representing a segment list with DSP-derived measurements.
#' This extends segment_list with additional columns from DSP processing
#' (e.g., formants, pitch, intensity).
#' 
#' @section Structure:
#' An extended_segment_list contains all segment_list columns plus
#' additional columns added by DSP processing via quantify().
#' 
#' @examples
#' \dontrun{
#' # Query and quantify
#' segs <- query(corpus, "Phonetic == t")
#' extended <- quantify(segs, superassp::forest)
#' 
#' # Extended segment list contains formant measurements
#' print(extended)
#' summary(extended)
#' }
#' 
#' @export
extended_segment_list <- S7::new_class(
  "extended_segment_list",
  parent = segment_list,
  properties = list(
    dsp_function = class_character,
    dsp_columns = class_character
  ),
  validator = function(self) {
    # Inherits validation from segment_list
    NULL
  },
  constructor = function(data, db_uuid = NULL, db_path = NULL, 
                        dsp_function = "", dsp_columns = character(0)) {
    # Convert to data.frame if needed
    if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
      data <- as.data.frame(data)
    } else if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
    
    # Extract db_uuid from data if not provided
    if (is.null(db_uuid) && "db_uuid" %in% names(data)) {
      db_uuid_vals <- unique(data$db_uuid)
      if (length(db_uuid_vals) > 1) {
        warning("Multiple db_uuids found; using first")
        db_uuid <- as.character(db_uuid_vals[1])
      } else {
        db_uuid <- as.character(db_uuid_vals[1])
      }
    }
    
    if (is.null(db_uuid) || length(db_uuid) == 0) {
      db_uuid <- ""
    }
    
    if (is.null(db_path) || length(db_path) == 0) {
      db_path <- ""
    }
    
    S7::new_object(
      .parent = data,
      db_uuid = as.character(db_uuid),
      db_path = as.character(db_path),
      dsp_function = as.character(dsp_function),
      dsp_columns = as.character(dsp_columns)
    )
  }
)

#' Print method for extended_segment_list
#' @export
S7::method(print, extended_segment_list) <- function(x, ..., n = 10) {
  # Header with styling
  cli::cli_rule(left = "Extended Segment List", right = "{nrow(x)} row{?s}")
  
  if (nrow(x) > 0) {
    # Database info
    cli::cli_alert_info("Database: {.val {x@db_uuid}}")
    if (nchar(x@db_path) > 0) {
      cli::cli_alert_info("Path: {.path {x@db_path}}")
    }
    
    # DSP info
    if (nchar(x@dsp_function) > 0) {
      cli::cli_alert_info("DSP function: {.fn {x@dsp_function}}")
    }
    if (length(x@dsp_columns) > 0) {
      cli::cli_alert_info("DSP columns: {paste(x@dsp_columns, collapse = ', ')}")
    }
    
    # Key statistics in a compact format
    cli::cli_text("")
    
    # Count unique segments (by start_item_id)
    n_unique_segments <- length(unique(x$start_item_id))
    rows_per_segment <- nrow(x) / n_unique_segments
    
    cli::cli_text("{.strong Unique segments:} {n_unique_segments}")
    cli::cli_text("{.strong Total rows:} {nrow(x)} ({round(rows_per_segment, 1)} per segment)")
    cli::cli_text("{.strong Levels:} {paste(unique(x$level), collapse = ', ')}")
    cli::cli_text("{.strong Sessions:} {length(unique(x$session))}")
    cli::cli_text("{.strong Bundles:} {length(unique(x$bundle))}")
    
    # Show numeric column ranges if DSP columns exist
    if (length(x@dsp_columns) > 0) {
      cli::cli_text("")
      cli::cli_text("{.strong DSP measurement ranges:}")
      for (col in x@dsp_columns) {
        if (col %in% names(x) && is.numeric(x[[col]])) {
          vals <- x[[col]]
          cli::cli_text("  {col}: {round(min(vals, na.rm = TRUE), 2)} - {round(max(vals, na.rm = TRUE), 2)}")
        }
      }
    }
    
    cli::cli_text("")
    cli::cli_rule()
    
    # Show data
    print(tibble::as_tibble(as.data.frame(x)), n = n, ...)
  } else {
    cli::cli_alert_warning("Empty extended segment list")
  }
  
  invisible(x)
}

#' Summary method for extended_segment_list
#' @export
S7::method(summary, extended_segment_list) <- function(object, ...) {
  cli::cli_h1("Extended Segment List Summary")
  
  # Database info
  cli::cli_h2("Database Information")
  cli::cli_dl(c(
    "UUID" = object@db_uuid,
    "Path" = if (nchar(object@db_path) > 0) object@db_path else "(not specified)",
    "Total rows" = as.character(nrow(object))
  ))
  
  if (nrow(object) > 0) {
    # Count unique segments
    n_unique_segments <- length(unique(object$start_item_id))
    rows_per_segment <- nrow(object) / n_unique_segments
    
    # Structure info
    cli::cli_h2("Structure")
    cli::cli_dl(c(
      "Unique segments" = as.character(n_unique_segments),
      "Rows per segment" = sprintf("%.2f", rows_per_segment),
      "Levels" = paste(unique(object$level), collapse = ", "),
      "Types" = paste(unique(object$type), collapse = ", "),
      "Sessions" = as.character(length(unique(object$session))),
      "Bundles" = as.character(length(unique(object$bundle)))
    ))
    
    # DSP info
    if (nchar(object@dsp_function) > 0 || length(object@dsp_columns) > 0) {
      cli::cli_h2("DSP Information")
      info_list <- list()
      if (nchar(object@dsp_function) > 0) {
        info_list[["Function"]] <- object@dsp_function
      }
      if (length(object@dsp_columns) > 0) {
        info_list[["Columns"]] <- paste(object@dsp_columns, collapse = ", ")
      }
      cli::cli_dl(info_list)
    }
    
    # Temporal info (computed on unique segments)
    cli::cli_h2("Temporal Characteristics")
    unique_segs <- object[!duplicated(object$start_item_id), ]
    durations <- unique_segs$end - unique_segs$start
    cli::cli_dl(c(
      "Duration range" = sprintf("%.3f - %.3f ms", min(durations), max(durations)),
      "Mean duration" = sprintf("%.3f ms", mean(durations)),
      "Total duration" = sprintf("%.3f s", sum(durations) / 1000)
    ))
    
    # DSP measurement statistics
    if (length(object@dsp_columns) > 0) {
      cli::cli_h2("DSP Measurements")
      
      for (col in object@dsp_columns) {
        if (col %in% names(object) && is.numeric(object[[col]])) {
          vals <- object[[col]]
          vals_clean <- vals[!is.na(vals)]
          
          if (length(vals_clean) > 0) {
            cli::cli_h3(col)
            cli::cli_dl(c(
              "Range" = sprintf("%.2f - %.2f", min(vals_clean), max(vals_clean)),
              "Mean" = sprintf("%.2f", mean(vals_clean)),
              "Median" = sprintf("%.2f", median(vals_clean)),
              "SD" = sprintf("%.2f", sd(vals_clean)),
              "NA count" = as.character(sum(is.na(vals)))
            ))
          }
        }
      }
    }
    
    # Label distribution (on unique segments)
    cli::cli_h2("Label Distribution (unique segments)")
    label_counts <- sort(table(unique_segs$labels), decreasing = TRUE)
    n_show <- min(5, length(label_counts))
    
    if (n_show > 0) {
      label_df <- data.frame(
        Label = names(label_counts)[1:n_show],
        Count = as.integer(label_counts[1:n_show]),
        Percentage = sprintf("%.1f%%", 100 * as.numeric(label_counts[1:n_show]) / n_unique_segments)
      )
      print(knitr::kable(label_df, format = "simple", align = c("l", "r", "r")))
      
      if (length(label_counts) > n_show) {
        cli::cli_text("{.emph ... and {length(label_counts) - n_show} more label{?s}}")
      }
    }
  } else {
    cli::cli_alert_warning("Empty extended segment list")
  }
  
  invisible(object)
}

#' Check if object is an extended_segment_list
#' @export
is_extended_segment_list <- function(x) {
  inherits(x, "extended_segment_list")
}

#' Quantify generic - Apply DSP to segments
#' @export
quantify <- S7::new_generic("quantify", "object")

#' Quantify method for segment_list - Apply DSP to query results
#' 
#' Applies a DSP function to all segments in a segment_list, extracting
#' acoustic measurements for each segment. This is equivalent to emuR::get_trackdata()
#' but allows for custom DSP functions with metadata-driven parameters.
#' 
#' @param object A segment_list object (from ask_for/query)
#' @param dsp_function A DSP function from superassp or similar
#' @param ... Additional arguments passed to the DSP function
#' @param .at Optional vector of relative time points (0-1) to extract from track
#' @param .use_metadata Logical; whether to use bundle metadata for parameter derivation
#' @param .verbose Logical; show progress messages
#' @param .parallel Logical; use parallel processing (default TRUE)
#' @param .workers Number of parallel workers (default: parallel::detectCores() - 1)
#' @param .use_cache Logical; enable result caching (default FALSE)
#' @param .cache_dir Character; cache directory path (default: tempdir()/reindeer_cache)
#' @param .cache_format Character; serialization format - "auto" (uses qs if available, 
#'        otherwise base serialize), "qs" (faster, smaller, requires qs package), 
#'        or "rds" (base R serialize, slower, larger). Default: "auto"
#' @param .optimize Logical; use optimized processing (default TRUE)
#' 
#' @return An extended_segment_list with segment information and DSP-derived measurements
#' 
#' @examples
#' \dontrun{
#' segs <- ask_for(corpus, "Phonetic == t")
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
#' }
#' 
#' @export
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
  
  # Match cache format argument
  .cache_format <- match.arg(.cache_format)
  
  if (!inherits(object, "segment_list")) {
    cli::cli_abort("{.arg object} must be a segment_list")
  }
  
  if (nrow(object) == 0) {
    if (.verbose) cli::cli_alert_warning("Empty segment list")
    return(tibble::tibble())
  }
  
  # Validate .at parameter
  if (!is.null(.at)) {
    if (!is.numeric(.at) || any(.at < 0) || any(.at > 1)) {
      cli::cli_abort("{.arg .at} must be numeric values between 0 and 1")
    }
  }
  
  # Try to get corpus from db_path (PHASE 1: Cached corpus loading)
  corpus_obj <- .get_corpus_cached(object, NULL)
  
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
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # Get unique bundles from segment list
    unique_bundles <- unique(as.data.frame(object)[, c("session", "bundle")])
    
    # Fetch only needed metadata
    metadata_by_bundle <- DBI::dbGetQuery(con, 
      "SELECT session, bundle, key, value, value_type 
       FROM bundle_metadata") %>%
      dplyr::semi_join(unique_bundles, by = c("session", "bundle")) %>%
      tidyr::pivot_wider(names_from = key, values_from = value)
  }
  
  # Convert to data frame for processing
  seg_df <- as.data.frame(object)
  
  # Join with metadata if available
  if (!is.null(metadata_by_bundle)) {
    seg_df <- seg_df %>%
      dplyr::left_join(metadata_by_bundle, by = c("session", "bundle"))
    
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
  if (.optimize && requireNamespace("data.table", quietly = TRUE) && nrow(seg_df) > 100) {
    # Use vectorized processing for large datasets
    if (.verbose) {
      cli::cli_alert_info("Using optimized vectorized processing")
    }
    
    # Setup persistent cache if requested
    cache_conn <- if (.use_cache) {
      .get_persistent_cache_connection(.cache_dir)
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
  results <- purrr::compact(results_list)
  
  if (length(results) == 0) {
    if (.verbose) {
      cli::cli_alert_warning("No results generated")
    }
    return(tibble::tibble())
  }
  
  # Combine all results efficiently
  # PHASE 2: Use data.table for faster binding if available
  combined <- if (requireNamespace("data.table", quietly = TRUE) && length(results) > 100) {
    data.table::rbindlist(results, fill = TRUE) |>
      tibble::as_tibble()
  } else {
    dplyr::bind_rows(results)
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
  
  result
}

