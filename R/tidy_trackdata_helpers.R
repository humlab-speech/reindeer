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
  # Handle lazy evaluation first - explicitly check class
  if ("lazy_segment_list" %in% class(.segments)) {
    # Force collection
    .segments <- reindeer::collect(.segments)
  }
  
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

#' ============================================================================
#' PHASE 2: Advanced Optimizations
#' ============================================================================

#' Get or create persistent SQLite cache for quantify results
#' @noRd
..get_persistent_cache_connection <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "reindeer_cache")
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  cache_file <- file.path(cache_dir, "quantify_cache.sqlite")
  cache_key <- paste0("persistent_cache_", cache_file)
  
  if (exists(cache_key, envir = .tidy_cache)) {
    conn <- get(cache_key, envir = .tidy_cache)
    if (DBI::dbIsValid(conn)) {
      return(conn)
    }
  }
  
  # Create new connection
  conn <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  
  # Create cache table if it doesn't exist
  if (!"cache" %in% DBI::dbListTables(conn)) {
    DBI::dbExecute(conn, "
      CREATE TABLE cache (
        cache_key TEXT PRIMARY KEY,
        result_blob BLOB,
        format TEXT DEFAULT 'rds',
        created_at INTEGER,
        accessed_at INTEGER,
        size_bytes INTEGER
      )
    ")
    
    DBI::dbExecute(conn, "
      CREATE INDEX idx_accessed_at ON cache(accessed_at)
    ")
    
    DBI::dbExecute(conn, "
      CREATE INDEX idx_format ON cache(format)
    ")
  } else {
    # Migrate existing cache table to add format column if needed
    columns <- DBI::dbListFields(conn, "cache")
    if (!"format" %in% columns) {
      DBI::dbExecute(conn, "ALTER TABLE cache ADD COLUMN format TEXT DEFAULT 'rds'")
      DBI::dbExecute(conn, "CREATE INDEX idx_format ON cache(format)")
    }
  }
  
  assign(cache_key, conn, envir = .tidy_cache)
  return(conn)
}

#' Get result from persistent cache
#' @noRd
.get_persistent_cache <- function(cache_key, conn) {
  query <- "SELECT result_blob, format FROM cache WHERE cache_key = ?"
  result <- DBI::dbGetQuery(conn, query, params = list(cache_key))
  
  if (nrow(result) > 0) {
    # Update access time
    DBI::dbExecute(conn, 
      "UPDATE cache SET accessed_at = ? WHERE cache_key = ?",
      params = list(as.integer(Sys.time()), cache_key)
    )
    
    # Get format (default to 'rds' for backward compatibility)
    format <- result$format[1]
    if (is.null(format) || is.na(format)) {
      format <- "rds"
    }
    
    # Deserialize based on format
    blob <- result$result_blob[[1]]
    
    data <- tryCatch({
      if (format == "qs" && requireNamespace("qs", quietly = TRUE)) {
        qs::qdeserialize(blob)
      } else {
        unserialize(blob)
      }
    }, error = function(e) {
      # If deserialization fails, try the other method as fallback
      tryCatch({
        if (format == "qs") {
          # qs failed, try base unserialize
          unserialize(blob)
        } else {
          # base failed, try qs if available
          if (requireNamespace("qs", quietly = TRUE)) {
            qs::qdeserialize(blob)
          } else {
            stop(e)
          }
        }
      }, error = function(e2) {
        # Both failed - return NULL to trigger recomputation
        NULL
      })
    })
    
    return(data)
  } else {
    NULL
  }
}

#' Set result in persistent cache
#' @noRd
.set_persistent_cache <- function(cache_key, result, conn, max_cache_size_mb = 1000,
                                   format = c("auto", "qs", "rds")) {
  format <- match.arg(format)
  
  # Determine format to use
  if (format == "auto") {
    format <- if (requireNamespace("qs", quietly = TRUE)) "qs" else "rds"
  }
  
  # Serialize result based on format
  result_blob <- if (format == "qs" && requireNamespace("qs", quietly = TRUE)) {
    qs::qserialize(result, preset = "fast")
  } else {
    serialize(result, NULL)
  }
  
  size_bytes <- length(result_blob)
  current_time <- as.integer(Sys.time())
  
  # Check total cache size
  total_size_query <- "SELECT SUM(size_bytes) as total FROM cache"
  total_size <- DBI::dbGetQuery(conn, total_size_query)$total
  
  if (!is.null(total_size) && total_size > max_cache_size_mb * 1024^2) {
    # Remove oldest 25% of entries
    remove_query <- "
      DELETE FROM cache 
      WHERE cache_key IN (
        SELECT cache_key FROM cache 
        ORDER BY accessed_at ASC 
        LIMIT (SELECT COUNT(*) / 4 FROM cache)
      )
    "
    DBI::dbExecute(conn, remove_query)
  }
  
  # Insert or replace with format marker
  DBI::dbExecute(conn, "
    INSERT OR REPLACE INTO cache (cache_key, result_blob, format, created_at, accessed_at, size_bytes)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(cache_key, list(result_blob), format, current_time, current_time, size_bytes))
  
  invisible(NULL)
}

#' Vectorized segment processing using data.table
#' @noRd
.process_segments_vectorized <- function(seg_df, corpus_obj, dsp_function, dsp_params,
                                         media_ext, .at = NULL, .verbose = FALSE,
                                         use_cache = TRUE, cache_conn = NULL,
                                         cache_format = "auto") {
  
  # Convert to data.table for faster operations
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(seg_df)
    
    # Add file paths vectorized
    dt[, signal_file := file.path(
      corpus_obj@basePath,
      paste0(session, "_ses"),
      paste0(bundle, "_bndl"),
      paste0(bundle, ".", media_ext)
    )]
    
    # Filter existing files
    dt[, file_exists := file.exists(signal_file)]
    dt_valid <- dt[file_exists == TRUE]
    
    if (nrow(dt_valid) == 0) {
      return(list())
    }
    
    # Generate cache keys if caching enabled
    if (use_cache && !is.null(cache_conn)) {
      dt_valid[, cache_key := {
        vapply(seq_len(.N), function(i) {
          .make_quantify_cache_key(
            list(session = session[i], bundle = bundle[i], 
                 start = start[i], end = end[i]),
            dsp_function,
            dsp_params
          )
        }, character(1))
      }]
      
      # Check cache for existing results
      dt_valid[, cached_result := lapply(cache_key, function(k) {
        .get_persistent_cache(k, cache_conn)
      })]
      
      # Separate cached and uncached
      dt_cached <- dt_valid[!vapply(cached_result, is.null, logical(1))]
      dt_uncached <- dt_valid[vapply(cached_result, is.null, logical(1))]
      
      if (.verbose && nrow(dt_cached) > 0) {
        cli::cli_alert_success("Found {nrow(dt_cached)} cached result{?s}")
      }
    } else {
      dt_uncached <- dt_valid
      dt_cached <- data.table::data.table()
    }
    
    # Process uncached segments
    if (nrow(dt_uncached) > 0) {
      # Group by file for efficient I/O
      dt_uncached[, file_group_id := .GRP, by = signal_file]
      
      results_list <- dt_uncached[, {
        file <- unique(signal_file)[1]
        
        # Process all segments from this file at once
        segment_results <- lapply(seq_len(.N), function(i) {
          tryCatch({
            result <- do.call(dsp_function, c(
              list(
                listOfFiles = file,
                beginTime = start[i] / 1000,
                endTime = end[i] / 1000
              ),
              dsp_params,
              list(toFile = FALSE, verbose = FALSE)
            ))
            
            # Store in cache if enabled
            if (use_cache && !is.null(cache_conn)) {
              .set_persistent_cache(cache_key[i], result, cache_conn, format = cache_format)
            }
            
            result
          }, error = function(e) {
            if (.verbose) {
              cli::cli_alert_warning("Error: {conditionMessage(e)}")
            }
            NULL
          })
        })
        
        list(results = segment_results)
      }, by = file_group_id]
      
      # Combine results with segment info
      dt_uncached[, result := unlist(results_list$results, recursive = FALSE)]
    } else {
      dt_uncached[, result := list()]
    }
    
    # Combine cached and uncached
    if (nrow(dt_cached) > 0) {
      dt_cached[, result := cached_result]
      dt_all <- data.table::rbindlist(list(dt_cached, dt_uncached), fill = TRUE)
    } else {
      dt_all <- dt_uncached
    }
    
    # Convert results to tibble format
    result_list <- dt_all[, {
      if (!is.null(result[[1]])) {
        result_df <- if (inherits(result[[1]], "AsspDataObj")) {
          track_data <- wrassp::as.data.frame.AsspDataObj(result[[1]])
          
          if (!is.null(.at)) {
            n_frames <- nrow(track_data)
            frame_indices <- pmax(1, pmin(n_frames, round(.at * n_frames)))
            track_data <- track_data[frame_indices, , drop = FALSE]
            track_data$.time_point <- .at
          }
          
          track_data
        } else if (is.data.frame(result[[1]])) {
          result[[1]]
        } else if (is.list(result[[1]])) {
          as.data.frame(result[[1]])
        } else {
          data.frame(value = result[[1]])
        }
        
        # Add segment info
        n_result_rows <- nrow(result_df)
        seg_info <- .SD[rep(1, n_result_rows)]
        
        dplyr::bind_cols(
          tibble::as_tibble(seg_info),
          tibble::as_tibble(result_df)
        )
      } else {
        NULL
      }
    }, by = seq_len(nrow(dt_all))]
    
    # Return list of tibbles
    result_list[!vapply(result_list$V1, is.null, logical(1))]$V1
    
  } else {
    # Fall back to non-data.table implementation
    .process_by_file_batch(seg_df, corpus_obj, dsp_function, dsp_params, 
                          media_ext, .at, .verbose)
  }
}

#' Memory-mapped SSFF file reader for large files
#' @noRd
.read_ssff_mmap <- function(file_path, beginTime = 0, endTime = NULL, field = NULL) {
  # For very large files, consider memory mapping
  # This is a placeholder for more sophisticated memory-mapped I/O
  
  # Check file size
  file_size <- file.info(file_path)$size
  
  # Use memory mapping for files > 100 MB
  if (file_size > 100 * 1024^2 && requireNamespace("bigstatsr", quietly = TRUE)) {
    # TODO: Implement true memory mapping for SSFF files
    # For now, fall back to standard reading
    wrassp::read.AsspDataObj(file_path, begin = beginTime, end = endTime)
  } else {
    wrassp::read.AsspDataObj(file_path, begin = beginTime, end = endTime)
  }
}

#' Parallel audio file processing with true parallel I/O
#' @noRd
.process_parallel_io <- function(seg_df, corpus_obj, dsp_function, dsp_params,
                                 media_ext, .at = NULL, .cores = NULL, .verbose = FALSE) {
  
  if (!requireNamespace("future", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg future} not available, falling back to sequential processing")
    return(.process_segments_vectorized(seg_df, corpus_obj, dsp_function, dsp_params,
                                       media_ext, .at, .verbose, use_cache = FALSE))
  }
  
  # Set up parallel processing
  if (is.null(.cores)) {
    .cores <- max(1, future::availableCores() - 1)
  }
  
  # Split segments by file for optimal I/O
  seg_df$signal_file <- file.path(
    corpus_obj@basePath,
    paste0(seg_df$session, "_ses"),
    paste0(seg_df$bundle, "_bndl"),
    paste0(seg_df$bundle, ".", media_ext)
  )
  
  file_groups <- split(seg_df, seg_df$signal_file)
  
  # Set up future plan
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  future::plan(future::multisession, workers = .cores)
  
  # Process files in parallel
  results <- future.apply::future_lapply(file_groups, function(file_segs) {
    signal_file <- unique(file_segs$signal_file)[1]
    
    if (!file.exists(signal_file)) {
      return(NULL)
    }
    
    # Process segments from this file
    lapply(seq_len(nrow(file_segs)), function(i) {
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
        
        # Handle result formatting
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
          cli::cli_alert_warning("Error: {conditionMessage(e)}")
        }
        NULL
      })
    })
  }, future.seed = TRUE)
  
  # Flatten and return
  unlist(unlist(results, recursive = FALSE), recursive = FALSE)
}
