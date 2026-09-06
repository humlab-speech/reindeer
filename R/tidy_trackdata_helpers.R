# Helper functions for tidy_trackdata optimizations
# These provide caching and efficient data access patterns

# Environment for caching corpus and handle objects
.tidy_cache <- new.env(parent = emptyenv())

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
#' Precompute the loop-invariant parts of a quantify cache key.
#'
#' The DSP function name and .at digest don't change across segments in a
#' single quantify() call; hoist them out and reuse. The params digest is
#' computed per row (effective params vary by bundle), so it is not hoisted.
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
    
    # Process all segments from this file. The DSP call's invariant
    # arguments (file path, params, toFile/verbose) are built once per file;
    # only the per-segment time window is re-supplied. Decoding the signal
    # once and slicing is deliberately not done: the windowed superassp API
    # ties analysis context to the requested window, so a whole-file pass
    # would change boundary-adjacent results.
    params_file <- file_segs$seg_params[[1]] %||% dsp_params
    dsp_call_base <- c(
      list(listOfFiles = signal_file),
      params_file,
      list(toFile = FALSE, verbose = FALSE)
    )
    segment_results <- lapply(seq_len(nrow(file_segs)), function(i) {
      seg <- file_segs[i, , drop = FALSE]
      
      tryCatch({
        result <- do.call(dsp_function, c(
          dsp_call_base,
          list(beginTime = seg$start / 1000, endTime = seg$end / 1000)
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
    
    # Deserialize based on format (with cross-format fallback)
    return(.deserialize_cache_blob(result$result_blob[[1]], format))
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
    format <- if (requireNamespace("qs2", quietly = TRUE)) "qs" else "rds"
  }
  
  # Serialize result based on format
  result_blob <- if (format == "qs" && requireNamespace("qs2", quietly = TRUE)) {
    qs2::qs_serialize(result)
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

#' Deserialize a cache blob, trying the recorded format first and falling
#' back to the other serialization format on failure.
#' @noRd
.deserialize_cache_blob <- function(blob, format) {
  if (is.null(format) || is.na(format)) format <- "rds"
  tryCatch({
    if (format == "qs" && requireNamespace("qs2", quietly = TRUE)) {
      qs2::qs_deserialize(blob)
    } else {
      unserialize(blob)
    }
  }, error = function(e) {
    tryCatch({
      if (format == "qs") {
        unserialize(blob)
      } else if (requireNamespace("qs2", quietly = TRUE)) {
        qs2::qs_deserialize(blob)
      } else {
        NULL
      }
    }, error = function(e2) NULL)
  })
}

#' Batch read from the persistent cache.
#'
#' Fetches all keys with a small number of chunked `IN` queries (chunked to
#' stay under SQLite's bind-parameter limit) and touches `accessed_at` once
#' per chunk, instead of one SELECT + UPDATE per key. Returns a named list of
#' deserialized results keyed by `cache_key`; keys absent from the cache are
#' omitted.
#' @noRd
.get_persistent_cache_batch <- function(cache_keys, conn) {
  cache_keys <- unique(cache_keys)
  if (length(cache_keys) == 0L) return(stats::setNames(list(), character()))

  out_keys <- character()
  out_vals <- list()
  chunk_size <- 500L
  starts <- seq.int(1L, length(cache_keys), by = chunk_size)
  now <- as.integer(Sys.time())

  for (s in starts) {
    ch <- cache_keys[s:min(s + chunk_size - 1L, length(cache_keys))]
    ph <- paste(rep("?", length(ch)), collapse = ", ")
    rows <- DBI::dbGetQuery(
      conn,
      sprintf("SELECT cache_key, result_blob, format FROM cache WHERE cache_key IN (%s)", ph),
      params = as.list(ch)
    )
    if (nrow(rows) > 0L) {
      out_keys <- c(out_keys, rows$cache_key)
      out_vals <- c(out_vals, lapply(seq_len(nrow(rows)), function(i) {
        .deserialize_cache_blob(rows$result_blob[[i]], rows$format[i])
      }))
      # One batched access-time touch per chunk.
      hph <- paste(rep("?", nrow(rows)), collapse = ", ")
      DBI::dbExecute(
        conn,
        sprintf("UPDATE cache SET accessed_at = ? WHERE cache_key IN (%s)", hph),
        params = c(list(now), as.list(rows$cache_key))
      )
    }
  }
  stats::setNames(out_vals, out_keys)
}

#' Batch write to the persistent cache.
#'
#' Serializes every entry once, evicts once if over budget, then inserts all
#' rows inside a single transaction — replacing the per-row SELECT-SUM +
#' INSERT pattern that ran two statements per miss.
#' @noRd
.set_persistent_cache_batch <- function(entries, conn, max_cache_size_mb = 1000,
                                        format = c("auto", "qs", "rds")) {
  if (length(entries) == 0L) return(invisible(NULL))
  format <- match.arg(format)
  if (format == "auto") {
    format <- if (requireNamespace("qs2", quietly = TRUE)) "qs" else "rds"
  }

  use_qs <- format == "qs" && requireNamespace("qs2", quietly = TRUE)
  blobs <- lapply(entries, function(e) {
    if (use_qs) qs2::qs_serialize(e$result) else serialize(e$result, NULL)
  })
  sizes <- vapply(blobs, length, integer(1))
  now <- as.integer(Sys.time())

  # Evict oldest 25% when over budget (once per batch, not per insert).
  total <- DBI::dbGetQuery(conn, "SELECT SUM(size_bytes) AS total FROM cache")$total
  if (isTRUE(total > max_cache_size_mb * 1024^2)) {
    DBI::dbExecute(conn, "
      DELETE FROM cache
      WHERE cache_key IN (
        SELECT cache_key FROM cache
        ORDER BY accessed_at ASC
        LIMIT (SELECT COUNT(*) / 4 FROM cache)
      )
    ")
  }

  DBI::dbWithTransaction(conn, {
    for (i in seq_along(entries)) {
      DBI::dbExecute(conn, "
        INSERT OR REPLACE INTO cache (cache_key, result_blob, format, created_at, accessed_at, size_bytes)
        VALUES (?, ?, ?, ?, ?, ?)
      ", params = list(entries[[i]]$cache_key, list(blobs[[i]]), format, now, now, sizes[i]))
    }
  })
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
    
    # Generate cache keys if caching enabled. The params digest is computed
    # per row: effective DSP params now vary by bundle (Age/Gender), so it is
    # no longer loop-invariant. Everything else (dsp name, .at) is hoisted.
    if (use_cache && !is.null(cache_conn)) {
      key_pre <- .precompute_cache_key_parts(dsp_function, list(), .at)
      dt_valid[, seg_params_digest := vapply(seg_params, function(p) {
        digest::digest(p, algo = "xxhash64")
      }, character(1))]
      dt_valid[, cache_key := paste(
        session, bundle, as.character(start), as.character(end),
        key_pre$dsp_name, seg_params_digest, key_pre$at_digest,
        sep = "_"
      )]
      
      # Batch cache lookup — a handful of chunked IN queries instead of one
      # SELECT + UPDATE per row.
      cached_map <- .get_persistent_cache_batch(dt_valid$cache_key, cache_conn)
      dt_valid[, cached_result := lapply(cache_key, function(k) cached_map[[k]])]
      
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
        
        # Process all segments from this file at once. Rows in a file group
        # share one bundle, hence one params list; build the invariant call
        # args once and only re-supply the per-segment window.
        params_file <- seg_params[[1]]
        dsp_call_base <- c(
          list(listOfFiles = file),
          params_file,
          list(toFile = FALSE, verbose = FALSE)
        )
        segment_results <- lapply(seq_len(.N), function(i) {
          tryCatch({
            result <- do.call(dsp_function, c(
              dsp_call_base,
              list(beginTime = start[i] / 1000, endTime = end[i] / 1000)
            ))
            
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

      # Batch-write all misses in one transaction instead of per row.
      if (use_cache && !is.null(cache_conn) && nrow(dt_uncached) > 0L) {
        non_null <- !vapply(dt_uncached$result, is.null, logical(1))
        if (any(non_null)) {
          entries <- Map(
            function(k, r) list(cache_key = k, result = r),
            dt_uncached$cache_key[non_null],
            dt_uncached$result[non_null]
          )
          .set_persistent_cache_batch(entries, cache_conn, format = cache_format)
        }
      }
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
    
    # Process segments from this file. Invariant call args built once per
    # file (single-bundle file group); only the per-segment window varies.
    params_file <- file_segs$seg_params[[1]] %||% dsp_params
    dsp_call_base <- c(
      list(listOfFiles = signal_file),
      params_file,
      list(toFile = FALSE, verbose = FALSE)
    )
    lapply(seq_len(nrow(file_segs)), function(i) {
      seg <- file_segs[i, , drop = FALSE]
      
      tryCatch({
        result <- do.call(dsp_function, c(
          dsp_call_base,
          list(beginTime = seg$start / 1000, endTime = seg$end / 1000)
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
