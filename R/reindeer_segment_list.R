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
S7::method(print, segment_list) <- function(x, ...) {
  cat("Segment List\n")
  cat("============\n")
  cat(sprintf("Database: %s\n", x@db_uuid))
  cat(sprintf("Segments: %d\n", nrow(x)))
  if (nrow(x) > 0) {
    cat(sprintf("Level: %s\n", paste(unique(x$level), collapse = ", ")))
    cat(sprintf("Type: %s\n", paste(unique(x$type), collapse = ", ")))
    cat("\n")
  }
  print(tibble::as_tibble(as.data.frame(x)))
  invisible(x)
}

#' Summary method for segment_list  
#' @export
S7::method(summary, segment_list) <- function(object, ...) {
  cat("Segment List Summary\n")
  cat("===================\n")
  cat(sprintf("Database UUID: %s\n", object@db_uuid))
  cat(sprintf("Number of segments: %d\n", nrow(object)))
  
  if (nrow(object) > 0) {
    cat(sprintf("Levels: %s\n", paste(unique(object$level), collapse = ", ")))
    cat(sprintf("Types: %s\n", paste(unique(object$type), collapse = ", ")))
    cat(sprintf("Sessions: %d\n", length(unique(object$session))))
    cat(sprintf("Bundles: %d\n", length(unique(object$bundle))))
    cat(sprintf("Duration range: %.3f - %.3f ms\n", 
                min(object$end - object$start),
                max(object$end - object$start)))
    cat(sprintf("Total duration: %.3f s\n", sum(object$end - object$start) / 1000))
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
#' }
#' 
#' @export
S7::method(quantify, segment_list) <- function(object, dsp_function, ..., 
                                                .at = NULL,
                                                .use_metadata = TRUE,
                                                .verbose = FALSE,
                                                .parallel = TRUE,
                                                .workers = NULL) {
  
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
  
  # Try to get corpus from db_path
  corpus_obj <- NULL
  if (!is.null(object@db_path) && nchar(object@db_path) > 0) {
    if (dir.exists(object@db_path)) {
      if (.verbose) cli::cli_alert_info("Loading corpus from {object@db_path}")
      tryCatch({
        corpus_obj <- corpus(object@db_path, verbose = FALSE)
      }, error = function(e) {
        if (.verbose) {
          cli::cli_alert_warning("Could not load corpus: {conditionMessage(e)}")
        }
      })
    }
  }
  
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
  
  # Setup parallel processing
  if (.parallel) {
    if (is.null(.workers)) {
      .workers <- max(1, parallel::detectCores() - 1)
    }
    
    if (.verbose) {
      cli::cli_alert_info("Using parallel processing with {.workers} worker{?s}")
    }
    
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = .workers)
  }
  
  # Convert to data frame for processing
  seg_df <- as.data.frame(object)
  
  # Join with metadata if available
  if (!is.null(metadata_by_bundle)) {
    seg_df <- seg_df %>%
      dplyr::left_join(metadata_by_bundle, by = c("session", "bundle"))
  }
  
  # Define processing function for each segment
  process_segment <- function(i) {
    seg <- seg_df[i, , drop = FALSE]
    
    # Get signal file path
    signal_file <- file.path(
      corpus_obj@basePath,
      paste0(seg$session, "_ses"),
      paste0(seg$bundle, "_bndl"),
      paste0(seg$bundle, ".", media_ext)
    )
    
    if (!file.exists(signal_file)) {
      if (.verbose) {
        cli::cli_alert_warning("Signal file not found: {signal_file}")
      }
      return(NULL)
    }
    
    # Derive DSP parameters from metadata if requested
    user_params <- list(...)
    dsp_params <- user_params
    
    if (.use_metadata && !is.null(metadata_by_bundle)) {
      dsp_params <- derive_dsp_parameters(
        dsp_fun = dsp_function,
        metadata = seg,
        metadata_fields = c("Gender", "Age"),
        user_params = user_params
      )
    }
    
    # Apply DSP function
    tryCatch({
      result <- do.call(dsp_function, c(
        list(
          listOfFiles = signal_file,
          beginTime = seg$start / 1000,  # Convert ms to seconds
          endTime = seg$end / 1000
        ),
        dsp_params,
        list(toFile = FALSE, verbose = FALSE)
      ))
      
      # Handle different result types
      result_df <- if (inherits(result, "AsspDataObj")) {
        # SSFF track - extract values
        track_data <- wrassp::as.data.frame.AsspDataObj(result)
        
        # Extract at specific time points if requested
        if (!is.null(.at)) {
          n_frames <- nrow(track_data)
          frame_indices <- pmax(1, pmin(n_frames, round(.at * n_frames)))
          track_data <- track_data[frame_indices, , drop = FALSE]
          track_data$.time_point <- .at
        }
        
        track_data
      } else if (is.data.frame(result)) {
        result
      } else if (is.list(result)) {
        as.data.frame(result)
      } else {
        data.frame(value = result)
      }
      
      # Add segment info to each row
      # If .at is specified and multiple points, replicate segment info
      n_result_rows <- nrow(result_df)
      seg_replicated <- seg[rep(1, n_result_rows), , drop = FALSE]
      rownames(seg_replicated) <- NULL
      
      combined <- dplyr::bind_cols(
        tibble::as_tibble(seg_replicated),
        tibble::as_tibble(result_df)
      )
      
      combined
    }, error = function(e) {
      if (.verbose) {
        cli::cli_alert_warning("Error processing segment {i}: {conditionMessage(e)}")
      }
      NULL
    })
  }
  
  # Process all segments
  if (.verbose) {
    cli::cli_progress_bar("Processing segments", total = nrow(seg_df))
  }
  
  if (.parallel) {
    results <- furrr::future_map(
      seq_len(nrow(seg_df)),
      process_segment,
      .progress = .verbose,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    results <- lapply(seq_len(nrow(seg_df)), process_segment)
  }
  
  if (.verbose) {
    cli::cli_progress_done()
  }
  
  # Remove NULL results
  results <- purrr::compact(results)
  
  if (length(results) == 0) {
    if (.verbose) {
      cli::cli_alert_warning("No results generated")
    }
    return(tibble::tibble())
  }
  
  # Combine all results
  combined <- dplyr::bind_rows(results)
  
  if (.verbose) {
    n_segs <- length(unique(combined$start_item_id))
    n_rows <- nrow(combined)
    cli::cli_alert_success("Processed {n_segs} segment{?s} ({n_rows} row{?s} total)")
  }
  
  combined
}

