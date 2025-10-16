# Helper functions for tidy_trackdata optimizations
# These provide caching and efficient data access patterns

# Environment for caching corpus and handle objects
.tidy_cache <- new.env(parent = emptyenv())

# Environment for caching quantify results
.quantify_cache <- new.env(parent = emptyenv())

#' Get or create corpus from segment list (with caching)
#' @noRd
.get_corpus_cached <- function(.segments, .from = NULL) {
  # If .from is provided and is a corpus, use it directly
  if (!is.null(.from) && S7::S7_inherits(.from, reindeer::corpus)) {
    return(.from)
  }
  
  # Try to get from segment list
  if (S7::S7_inherits(.segments, reindeer::segment_list)) {
    db_path <- S7::prop(.segments, "db_path")
    if (nzchar(db_path) && dir.exists(db_path)) {
      # Check cache
      cache_key <- paste0("corpus_", db_path)
      if (exists(cache_key, envir = .tidy_cache)) {
        return(get(cache_key, envir = .tidy_cache))
      }
      
      # Create and cache
      corp <- corpus(db_path)
      assign(cache_key, corp, envir = .tidy_cache)
      return(corp)
    }
  }
  
  cli::cli_abort(c(
    "Cannot determine corpus",
    "x" = "Please provide a corpus via {.arg .from} argument or ensure segment list has valid db_path"
  ))
}

#' Get or create emuR handle (with caching)
#' @noRd
.get_handle_cached <- function(corp) {
  cache_key <- paste0("handle_", corp@basePath)
  
  if (exists(cache_key, envir = .tidy_cache)) {
    handle <- get(cache_key, envir = .tidy_cache)
    # Validate connection is still active
    if (DBI::dbIsValid(handle$connection)) {
      return(handle)
    }
  }
  
  # Load and cache
  handle <- emuR::load_emuDB(corp@basePath, verbose = FALSE)
  assign(cache_key, handle, envir = .tidy_cache)
  return(handle)
}

#' Clear the tidy trackdata cache
#' @export
clear_tidy_cache <- function() {
  # Close any database connections
  for (obj_name in ls(envir = .tidy_cache)) {
    obj <- get(obj_name, envir = .tidy_cache)
    if (inherits(obj, "emuDBhandle") && !is.null(obj$connection)) {
      try(DBI::dbDisconnect(obj$connection), silent = TRUE)
    }
  }
  
  # Clear environment
  rm(list = ls(envir = .tidy_cache), envir = .tidy_cache)
  rm(list = ls(envir = .quantify_cache), envir = .quantify_cache)
  invisible(NULL)
}

#' Generate cache key for quantify results
#' @noRd
.make_quantify_cache_key <- function(segment_info, dsp_function, params) {
  # Create a unique key based on segment and parameters
  key_parts <- c(
    segment_info$session,
    segment_info$bundle, 
    as.character(segment_info$start),
    as.character(segment_info$end),
    deparse(substitute(dsp_function))[1],
    digest::digest(params, algo = "xxhash64")
  )
  paste(key_parts, collapse = "_")
}

#' Get cached quantify result
#' @noRd
.get_quantify_cache <- function(cache_key) {
  if (exists(cache_key, envir = .quantify_cache)) {
    get(cache_key, envir = .quantify_cache)
  } else {
    NULL
  }
}

#' Set quantify result in cache
#' @noRd
.set_quantify_cache <- function(cache_key, result, max_cache_size_mb = 500) {
  # Check cache size and clear if too large
  cache_size_bytes <- sum(vapply(ls(envir = .quantify_cache), function(x) {
    object.size(get(x, envir = .quantify_cache))
  }, numeric(1)))
  
  if (cache_size_bytes > max_cache_size_mb * 1024^2) {
    # Remove oldest 50% of cache entries
    cache_keys <- ls(envir = .quantify_cache)
    to_remove <- cache_keys[1:max(1, length(cache_keys) %/% 2)]
    rm(list = to_remove, envir = .quantify_cache)
  }
  
  assign(cache_key, result, envir = .quantify_cache)
  invisible(NULL)
}

#' Convert segment_list to data.frame efficiently
#' @noRd
.seglist_to_df <- function(.segments) {
  if (S7::S7_inherits(.segments, reindeer::segment_list)) {
    as.data.frame(S7::S7_data(.segments))
  } else {
    as.data.frame(.segments)
  }
}

#' Batch process segments by session (for parallel processing)
#' @noRd
.batch_by_session <- function(seglist_df) {
  split(seglist_df, seglist_df$session)
}

#' Batch process segments by bundle (for parallel processing)
#' @noRd
.batch_by_bundle <- function(seglist_df) {
  split(seglist_df, interaction(seglist_df$session, seglist_df$bundle, drop = TRUE))
}

#' Optimized batch processor for large segment lists
#' Groups segments by audio file to minimize I/O operations
#' @noRd
.process_by_file_batch <- function(seg_df, corpus_obj, dsp_function, dsp_params, 
                                   media_ext, .at = NULL, .verbose = FALSE) {
  # Group by audio file
  seg_df$signal_file <- file.path(
    corpus_obj@basePath,
    paste0(seg_df$session, "_ses"),
    paste0(seg_df$bundle, "_bndl"),
    paste0(seg_df$bundle, ".", media_ext)
  )
  
  # Split by file
  file_groups <- split(seg_df, seg_df$signal_file)
  
  # Process each file's segments together
  results <- lapply(file_groups, function(file_segs) {
    signal_file <- unique(file_segs$signal_file)[1]
    
    if (!file.exists(signal_file)) {
      if (.verbose) {
        cli::cli_alert_warning("Signal file not found: {basename(signal_file)}")
      }
      return(NULL)
    }
    
    # Process all segments from this file
    segment_results <- lapply(seq_len(nrow(file_segs)), function(i) {
      seg <- file_segs[i, , drop = FALSE]
      
      tryCatch({
        result <- do.call(dsp_function, c(
          list(
            listOfFiles = signal_file,
            beginTime = seg$start / 1000,
            endTime = seg$end / 1000
          ),
          dsp_params,
          list(toFile = FALSE, verbose = FALSE)
        ))
        
        # Handle different result types
        result_df <- if (inherits(result, "AsspDataObj")) {
          track_data <- wrassp::as.data.frame.AsspDataObj(result)
          
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
        
        # Add segment info
        n_result_rows <- nrow(result_df)
        seg_replicated <- seg[rep(1, n_result_rows), , drop = FALSE]
        rownames(seg_replicated) <- NULL
        
        dplyr::bind_cols(
          tibble::as_tibble(seg_replicated),
          tibble::as_tibble(result_df)
        )
      }, error = function(e) {
        if (.verbose) {
          cli::cli_alert_warning("Error processing segment: {conditionMessage(e)}")
        }
        NULL
      })
    })
    
    # Combine results from this file
    purrr::compact(segment_results)
  })
  
  # Flatten nested list
  unlist(results, recursive = FALSE)
}
