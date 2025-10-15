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
#' @param segment_list A segment_list object (from ask_for/query)
#' @param dsp_function A DSP function from superassp or similar
#' @param ... Additional arguments passed to the DSP function
#' @param .use_metadata Logical; whether to use bundle metadata for parameter derivation
#' @param .verbose Logical; show progress messages
#' @param .parallel Logical; use parallel processing (default TRUE)
#' @param .workers Number of parallel workers (default: parallel::detectCores() - 1)
#' 
#' @return A data frame with segment information and DSP-derived measurements
#' 
#' @examples
#' \dontrun{
#' segs <- query(corpus, "Phonetic == t")
#' formants <- quantify(segs, superassp::forest)
#' 
#' # Disable parallel processing
#' formants <- quantify(segs, superassp::forest, .parallel = FALSE)
#' }
#' 
#' @export
S7::method(quantify, segment_list) <- function(object, dsp_function, ..., 
                                                .use_metadata = TRUE,
                                                .verbose = FALSE,
                                                .parallel = TRUE,
                                                .workers = NULL) {
  
  if (!inherits(object, "segment_list")) {
    cli::cli_abort("{.arg object} must be a segment_list")
  }
  
  if (nrow(object) == 0) {
    cli::cli_alert_warning("Empty segment list")
    return(tibble::tibble())
  }
  
  # Try to get corpus connection from db_path
  corpus_obj <- NULL
  if (!is.null(object@db_path) && object@db_path != "") {
    basePath <- dirname(object@db_path)
    if (dir.exists(basePath)) {
      tryCatch({
        corpus_obj <- corpus(basePath, verbose = FALSE)
      }, error = function(e) {
        if (.verbose) {
          cli::cli_alert_info("Could not load corpus from db_path")
        }
      })
    }
  }
  
  # Get metadata if requested and pre-join with segments - optimized
  bundle_metadata <- NULL
  object_with_meta <- object
  if (.use_metadata && !is.null(corpus_obj)) {
    con <- get_corpus_connection(corpus_obj)
    
    # Only fetch metadata for the specific bundles in this segment list
    unique_bundles <- unique(as.data.frame(object)[, c("session", "bundle")])
    
    # More efficient: fetch only needed metadata
    bundle_metadata <- DBI::dbReadTable(con, "bundle_metadata") %>%
      dplyr::semi_join(unique_bundles, by = c("session", "bundle"))
    
    DBI::dbDisconnect(con)
    
    # Pre-join metadata for efficiency
    object_with_meta <- as.data.frame(object) %>%
      dplyr::left_join(bundle_metadata, by = c("session", "bundle"))
  }
  
  # Determine number of workers
  if (.parallel) {
    if (is.null(.workers)) {
      .workers <- max(1, parallel::detectCores() - 1)
    }
    
    if (.verbose) {
      cli::cli_alert_info("Using parallel processing with {.workers} worker{?s}")
    }
    
    # Set up parallel processing
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = .workers)
  }
  
  # Process segments
  if (.verbose) {
    cli::cli_progress_bar("Processing segments", total = nrow(object_with_meta))
  }
  
  # Define processing function
  process_segment <- function(i, seg_data, dsp_function, use_metadata, 
                              user_params, db_path, verbose = FALSE) {
    seg <- seg_data[i, ]
    
    # Derive parameters from metadata if available
    dsp_params <- user_params
    if (use_metadata && !is.null(seg$Gender)) {
      dsp_params <- derive_dsp_parameters(
        dsp_fun = dsp_function,
        metadata = seg,
        metadata_fields = c("Gender", "Age"),
        user_params = user_params
      )
    }
    
    # Get signal file path
    signal_file <- file.path(
      dirname(dirname(db_path)),
      paste0(seg$session, "_ses"),
      paste0(seg$bundle, "_bndl"),
      paste0(seg$bundle, ".wav")  # TODO: get from config
    )
    
    if (!file.exists(signal_file)) {
      return(NULL)
    }
    
    # Apply DSP function to segment
    tryCatch({
      result <- do.call(dsp_function, c(
        list(
          listOfFiles = signal_file,
          beginTime = seg$start / 1000,  # Convert ms to s
          endTime = seg$end / 1000
        ),
        dsp_params,
        list(toFile = FALSE, verbose = FALSE)
      ))
      
      # Combine with segment info
      result_df <- if (is.data.frame(result)) {
        result
      } else if (is.list(result)) {
        as.data.frame(result)
      } else {
        data.frame(value = result)
      }
      
      result_df <- dplyr::bind_cols(
        tibble::as_tibble(seg),
        result_df
      )
      
      result_df
    }, error = function(e) {
      NULL
    })
  }
  
  # Execute processing (parallel or sequential)
  if (.parallel) {
    results <- furrr::future_map(
      seq_len(nrow(object_with_meta)),
      process_segment,
      seg_data = object_with_meta,
      dsp_function = dsp_function,
      use_metadata = .use_metadata,
      user_params = list(...),
      db_path = object@db_path,
      verbose = FALSE,
      .progress = .verbose,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    results <- list()
    for (i in seq_len(nrow(object_with_meta))) {
      results[[i]] <- process_segment(
        i, object_with_meta, dsp_function, .use_metadata,
        list(...), object@db_path, FALSE
      )
      if (.verbose) {
        cli::cli_progress_update()
      }
    }
  }
  
  if (.verbose) {
    cli::cli_progress_done()
  }
  
  # Remove NULL results and combine
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
    cli::cli_alert_success("Processed {nrow(combined)} segment{?s}")
  }
  
  combined
}

