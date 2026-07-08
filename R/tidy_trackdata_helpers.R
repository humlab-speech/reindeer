# Helper functions for tidy_trackdata optimizations
# These provide caching and efficient data access patterns

# Environment for caching corpus and handle objects
.tidy_cache <- new.env(parent = emptyenv())

# Environment for caching quantify results
.quantify_cache <- new.env(parent = emptyenv())

#' Get or create corpus from a segment list (companion-package contract)
#'
#' Internal helper exposed for companion packages (`erodex`) that need to
#' reconstruct the backing corpus from a `segment_list`. Not a user verb;
#' end users start from a `corpus()` object directly.
#'
#' @param .segments A `segment_list` (or `lazy_segment_list`) to recover the
#'   backing corpus from.
#' @param .from Optional `corpus` to use directly instead of reconstructing.
#' @return A `corpus` object.
#' @keywords internal
#' @export
get_corpus_cached <- function(.segments, .from = NULL) {
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
  
  # Create native handle (no emuR dependency)
  handle <- list(
    dbName = corp@dbName,
    basePath = corp@basePath,
    connection = get_connection(corp),
    UUID = corp@.uuid
  )
  class(handle) <- "emuDBhandle"
  assign(cache_key, handle, envir = .tidy_cache)
  return(handle)
}

#' Clear the tidy trackdata cache
#' @keywords internal
#' @noRd
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
#'
#' Includes the segment scope (session/bundle/start/end), the DSP
#' function name, every parameter that affects the DSP output, AND
#' the `.at` extraction points. Without `.at` in the key, two calls
#' with different time-point requests share a cache entry and return
#' the wrong timepoints. Bundle metadata (Age/Gender) is folded in
#' automatically because `derive_dsp_parameters()` bakes those
#' fields into `params` before the key is produced.
#'
#' @noRd
.make_quantify_cache_key <- function(segment_info, dsp_function, params,
                                     .at = NULL,
                                     .precomputed = NULL) {
  # `.precomputed` is the list returned by `.precompute_cache_key_parts()`:
  # call sites that loop over many segments should compute it ONCE outside
  # the loop instead of paying digest cost per row.
  pre <- .precomputed %||% .precompute_cache_key_parts(dsp_function, params, .at)
  key_parts <- c(
    segment_info$session,
    segment_info$bundle,
    as.character(segment_info$start),
    as.character(segment_info$end),
    pre$dsp_name,
    pre$params_digest,
    pre$at_digest
  )
  paste(key_parts, collapse = "_")
}

#' Precompute the loop-invariant parts of a quantify cache key.
#'
#' The DSP function name + params digest + .at digest don't change across
#' segments in a single quantify() call, but `.make_quantify_cache_key()`
#' was recomputing them for every row. Hoist them out and reuse.
#'
#' @param dsp_function The DSP function symbol the caller passed in. The
#'   caller must wrap with `substitute()` so we can recover its source name.
#' @noRd
.precompute_cache_key_parts <- function(dsp_function, params, .at = NULL) {
  list(
    dsp_name = deparse(substitute(dsp_function))[1],
    params_digest = digest::digest(params, algo = "xxhash64"),
    at_digest = if (is.null(.at)) "NA" else digest::digest(.at, algo = "xxhash64")
  )
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
  if (S7::S7_inherits(.segments, lazy_segment_list)) {
    # Force collection
    .segments <- collect(.segments)
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
  
  # Hoist file existence checks out of the per-group loop — one vectorised
  # filesystem hit beats N system calls when batches cluster on the same
  # missing files. Drops missing-file groups before the lapply runs.
  unique_files <- unique(seg_df$signal_file)
  exists_map <- stats::setNames(file.exists(unique_files), unique_files)
  if (.verbose) {
    missing_files <- names(exists_map)[!exists_map]
    for (mf in missing_files) {
      cli::cli_alert_warning("Signal file not found: {basename(mf)}")
    }
  }
  seg_df <- seg_df[exists_map[seg_df$signal_file], , drop = FALSE]
  if (nrow(seg_df) == 0L) return(list())

  # Split by file
  file_groups <- split(seg_df, seg_df$signal_file)

  # Process each file's segments together
  results <- lapply(file_groups, function(file_segs) {
    signal_file <- unique(file_segs$signal_file)[1]
    
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
          track_data <- as.data.frame(result)
          
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
        
        cbind(
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
    Filter(Negate(is.null), segment_results)
  })
  
  # Flatten nested list
  unlist(results, recursive = FALSE)
}

# ============================================================================
# PHASE 2: Advanced Optimizations
# ============================================================================

#' Get or create persistent SQLite cache for quantify results
#' 
#' @noRd
#' @keywords internal
.get_persistent_cache_connection <- function(cache_dir = NULL, verbose = TRUE) {
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

  # Check cache size if it exists and verbose is enabled
  if (file.exists(cache_file) && verbose) {
    tryCatch({
      cache_size_info <- check_cache_size(
        cache_file,
        cache_type = "Quantify/enrich persistent",
        warn_threshold = "500 MB",
        max_threshold = "2 GB",
        verbose = TRUE
      )
    }, error = function(e) {
      # Silently ignore errors in cache size checking
      NULL
    })
  }
  
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
            cli::cli_abort("Deserialization failed: {conditionMessage(e)}", parent = e)
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
  
  if (isTRUE(total_size > max_cache_size_mb * 1024^2)) {
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
    
    # Generate cache keys if caching enabled.
    # Precompute the loop-invariant key parts ONCE — params/.at digests don't
    # change across segments and digesting per-row was visible at 10k+ rows.
    if (use_cache && !is.null(cache_conn)) {
      key_pre <- .precompute_cache_key_parts(dsp_function, dsp_params, .at)
      dt_valid[, cache_key := {
        vapply(seq_len(.N), function(i) {
          .make_quantify_cache_key(
            list(session = session[i], bundle = bundle[i],
                 start = start[i], end = end[i]),
            dsp_function,
            dsp_params,
            .at = .at,
            .precomputed = key_pre
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
      if (nrow(dt_cached) > 0) {
        dt_cached[, .cache_status := "hit"]
      }
      if (nrow(dt_uncached) > 0) {
        dt_uncached[, .cache_status := "miss"]
      }

      if (.verbose && nrow(dt_cached) > 0) {
        cli::cli_alert_success("Found {nrow(dt_cached)} cached result{?s}")
      }
    } else {
      dt_uncached <- dt_valid
      dt_uncached[, .cache_status := NA_character_]
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
          track_data <- as.data.frame(result[[1]])
          
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
        
        cbind(
          tibble::as_tibble(seg_info),
          tibble::as_tibble(result_df)
        )
      } else {
        NULL
      }
    }, by = seq_len(nrow(dt_all))]
    
    # Return list of tibbles
    result_list[!vapply(result_list$V1, is.null, logical(1))]$V1
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
          track_data <- as.data.frame(result)
          
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
        
        cbind(
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
