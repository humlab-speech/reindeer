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
#' @noRd
get_corpus_cached <- function(.segments, .from = NULL) {
  if (!is.null(.from) && S7::S7_inherits(.from, reindeer::corpus)) {
    return(.from)
  }
  db_path <- if (S7::S7_inherits(.segments, reindeer::segment_list)) {
    S7::prop(.segments, "db_path")
  } else ""
  if (nzchar(db_path) && dir.exists(db_path)) {
    # Reuse the corpus (and its open SQLite connection) while someone still
    # holds it; otherwise open a fresh one. Nothing is pinned for the session.
    live <- .live_corpus(db_path)
    if (!is.null(live)) return(live)
    return(corpus(db_path))
  }
  cli::cli_abort(c(
    "Cannot determine corpus",
    "x" = "Please provide a corpus via {.arg .from} argument or ensure segment list has valid db_path"
  ))
}

# basePath -> weak reference to the corpus. The key is the corpus' connection
# environment, so the registry never keeps a corpus (or its connection) alive.
.corpus_registry <- new.env(parent = emptyenv())

.register_corpus <- function(corp) {
  assign(normalizePath(corp@basePath, mustWork = FALSE),
         rlang::new_weakref(key = corp@.connection, value = corp),
         envir = .corpus_registry)
  invisible(corp)
}

.live_corpus <- function(base_path) {
  key <- normalizePath(base_path, mustWork = FALSE)
  if (!exists(key, envir = .corpus_registry, inherits = FALSE)) return(NULL)
  rlang::wref_value(get(key, envir = .corpus_registry, inherits = FALSE))
}

#' Get or create emuR handle (with caching)
#' @noRd
.get_handle_cached <- function(corp) {
  # Built on demand around the corpus' own connection: nothing is pinned.
  handle <- list(
    dbName = corp@dbName,
    basePath = corp@basePath,
    connection = get_connection(corp),
    UUID = corp@.uuid
  )
  class(handle) <- "emuDBhandle"
  handle
}
#' Stable name for a DSP function, for cache keys and result metadata
#'
#' Callers capture the unevaluated expression at their own boundary (where the
#' promise still exists) and hand it here. An anonymous closure is identified by
#' its formals and body, so two different closures can never share a cache entry.
#'
#' @param expr The caller's unevaluated argument, from `substitute()`
#' @param value The evaluated function (or character name)
#' @return Character scalar
#' @noRd
.dsp_name_from_expr <- function(expr, value) {
  if (is.character(value)) {
    return(paste(value, collapse = "|"))
  }
  nm <- tryCatch(paste(deparse(expr), collapse = ""), error = function(e) NULL)
  if (!is.null(nm) && nzchar(nm) && !grepl("^(function|NULL|\\()", nm)) {
    return(nm)
  }
  tryCatch(
    paste0("anonymous_",
           digest::digest(list(formals(value), body(value)), algo = "xxhash64")),
    error = function(e) "unknown")
}

#' Bump to invalidate every existing quantify cache entry.
#' @noRd
.cache_schema <- 3L

#' Identity of a DSP routine for cache keys
#'
#' Independent of the name the caller used: the package and its version (for
#' namespace functions) plus a digest of formals and body. `f <- forest` and
#' `forest` are the same routine; `forest` and `ksv` never are.
#' @noRd
.fn_identity <- function(fn) {
  if (!is.function(fn)) return(paste(as.character(fn), collapse = "|"))
  env <- environment(fn)
  pkg <- if (!is.null(env) && isNamespace(env)) getNamespaceName(env) else NA_character_
  ver <- if (is.na(pkg)) NA_character_ else as.character(utils::packageVersion(pkg))
  # A closure's behaviour also depends on what its body refers to outside the
  # function: captured values (probe_dsp(1) vs probe_dsp(2)) and user-defined
  # helper functions (digested one level deep: formals + body). Namespace and
  # base functions are covered by package name + version of the callee.
  captured <- NULL
  if (is.na(pkg) && !is.null(env)) {
    used <- sort(unique(all.names(body(fn))))
    found <- mget(used, envir = env, inherits = TRUE, ifnotfound = list(NULL))
    keep <- vapply(found, function(o) {
      if (is.null(o)) return(FALSE)
      if (!is.function(o)) return(TRUE)
      e <- environment(o)
      !is.null(e) && !isNamespace(e) && !identical(e, baseenv())
    }, logical(1))
    captured <- lapply(found[keep], function(o)
      if (is.function(o)) list(formals(o), body(o)) else o)
    # the locals of fn itself would also match by name; they are in body()
    captured <- captured[setdiff(names(captured), names(formals(fn)))]
  }
  digest::digest(list(pkg, ver, formals(fn), body(fn), captured), algo = "xxhash64")
}

#' Signal file mtime + size, one string per file
#' @noRd
.file_stamp <- function(paths) {
  info <- file.info(paths)
  paste(as.numeric(info$mtime), info$size, sep = ":")
}

#' Cache key components for a quantify result
#'
#' The `.at` digest and the DSP identity are loop-invariant for a call; the
#' params digest is computed per row (effective params vary by bundle), so it is
#' not hoisted.
#'
#' @param dsp_name Character name identifying the DSP routine. Pass the real
#'   name; deriving it from the argument would capture the local variable name
#'   instead, which is how every entry ended up labelled "dsp_function".
#' @noRd
.precompute_cache_key_parts <- function(dsp_name, params, .at = NULL, fn_id = NULL) {
  list(
    # The caller's name for the routine is cosmetic; identity is what keys.
    dsp_name = paste0(
      if (!is.null(fn_id)) fn_id
      else if (is.null(dsp_name) || !nzchar(dsp_name)) "unknown" else dsp_name,
      "#v", .cache_schema),
    params_digest = digest::digest(params, algo = "xxhash64"),
    at_digest = if (is.null(.at)) "NA" else digest::digest(.at, algo = "xxhash64")
  )
}
#' Flatten per-file segment results into one list of data frames
#'
#' `results` holds one element per signal file; each element is the list
#' produced by the per-segment loop, i.e. data frames with occasional NULLs
#' where a file or segment failed.
#'
#' The flattening must stop at that level. `unlist()` recurses into the data
#' frames themselves, so calling it twice turns every per-segment tibble into
#' its columns and the caller's `rbindlist()` then fails with "Item 1 of input
#' is not a data.frame, data.table or list" - which is what happened for every
#' quantify() call in the 21-100 segment band.
#'
#' @param results List of per-file results
#' @return Flat list of data frames, NULLs removed
#' @noRd
.flatten_segment_results <- function(results) {
  if (length(results) == 0) {
    return(list())
  }
  flat <- do.call(c, lapply(results, function(x) {
    if (is.null(x)) list() else unname(x)
  }))
  Filter(Negate(is.null), flat)
}

#' Analyse ONE whole signal file; return its frames as a table, or NULL
#'
#' Only time-indexed tracks (real `AsspDataObj`s carrying their own time base)
#' can be sliced per segment later. Anything else - tables, lists, fakes, or a
#' failed call - returns NULL and is analysed per window instead.
#' @noRd
.dsp_whole_table <- function(dsp_function, file, params) {
  whole <- tryCatch(
    do.call(dsp_function, c(list(listOfFiles = file), params,
                            list(toFile = FALSE, verbose = FALSE))),
    error = function(e) NULL)
  if (!inherits(whole, "AsspDataObj") || is.null(attr(whole, "sampleRate")) ||
      is.null(attr(whole, "startTime"))) {
    return(NULL)
  }
  tab <- as.data.frame(whole)
  tm <- suppressWarnings(as.numeric(tab[["frame_time"]]))
  if (length(tm) != nrow(tab) || anyNA(tm)) return(NULL)
  tab
}

#' Cut each segment's frames out of a whole-file table
#' @return One element per segment: a `reindeer_track_slice`, or NULL if no
#'   frame falls inside the segment.
#' @noRd
.slice_table <- function(tab, starts_ms, ends_ms) {
  tm <- as.numeric(tab[["frame_time"]])
  lapply(seq_along(starts_ms), function(i) {
    keep <- which(tm >= starts_ms[i] / 1000 & tm <= ends_ms[i] / 1000)
    if (length(keep) == 0L) return(NULL)
    structure(tab[keep, , drop = FALSE], class = c("reindeer_track_slice", "data.frame"))
  })
}

#' Run the DSP routine once per segment window of ONE signal file
#' @return A list with one element per segment (NULL where nothing came out).
#' @noRd
.run_dsp_windows <- function(dsp_function, file, params, starts_ms, ends_ms, verbose = FALSE) {
  base <- c(list(listOfFiles = file), params, list(toFile = FALSE, verbose = FALSE))
  lapply(seq_along(starts_ms), function(i) {
    tryCatch(
      do.call(dsp_function, c(base, list(beginTime = starts_ms[i] / 1000,
                                         endTime = ends_ms[i] / 1000))),
      error = function(e) {
        if (verbose) cli::cli_alert_warning("Error: {conditionMessage(e)}")
        NULL
      })
  })
}

#' Turn one DSP return value into a data frame, applying `.at`
#'
#' `.at` (fractions of the segment) selects the NEAREST frame of a track; no
#' interpolation. It only makes sense for track outputs, so asking for it on a
#' routine that returns a plain list or vector aborts instead of being ignored.
#' @noRd
.dsp_result_to_df <- function(result, .at = NULL) {
  if (inherits(result, "AsspDataObj") || inherits(result, "reindeer_track_slice")) {
    track_data <- if (inherits(result, "AsspDataObj")) as.data.frame(result) else {
      class(result) <- "data.frame"
      result
    }
    if (!is.null(.at)) {
      n_frames <- nrow(track_data)
      frame_indices <- pmax(1, pmin(n_frames, round(.at * n_frames)))
      track_data <- track_data[frame_indices, , drop = FALSE]
      track_data$.time_point <- .at
    }
    return(track_data)
  }
  if (!is.null(.at) && (is.atomic(result) || identical(class(result), "list"))) {
    # Plain lists (lst_* routines) and bare vectors have no frames to select;
    # classed objects (data frames, JsonTrackObj, ...) may be tracks and pass through.
    cli::cli_abort(
      c("{.arg .at} selects a frame of a track, but the routine returned a {.cls {class(result)[1]}}.",
        "i" = "Drop {.arg .at} for routines that return one value set per segment."),
      class = c("reindeer_quantify_error", "reindeer_error"))
  }
  if (is.data.frame(result)) result
  else if (is.list(result)) as.data.frame(result)
  else data.frame(value = result)
}

#' Optimized batch processor for large segment lists
#' Groups segments by audio file to minimize I/O operations
#' @noRd
# Bind segment bookkeeping columns with DSP result columns, renaming any
# DSP column whose name collides with a segment column (e.g. a track's own
# `sample_rate` vs the segment's cached one) so rbindlist() downstream never
# sees duplicate names.
.bind_seg_and_result <- function(seg_df, result_df) {
  dup <- intersect(names(seg_df), names(result_df))
  if (length(dup) > 0) {
    is_dup <- names(result_df) %in% dup
    names(result_df)[is_dup] <- paste0(names(result_df)[is_dup], "_dsp")
  }
  cbind(tibble::as_tibble(seg_df), tibble::as_tibble(result_df))
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
#' rows inside a single transaction - replacing the per-row SELECT-SUM +
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

#' Warn when the quantify cache has grown past 500 MB
#' @noRd
.warn_if_cache_large <- function(cache_conn) {
  path <- tryCatch(DBI::dbGetInfo(cache_conn)$dbname, error = function(e) NULL)
  if (is.null(path) || !file.exists(path) || file.size(path) < 500 * 1024^2) return(invisible())
  check_cache_size(dirname(path), cache_type = "Quantify", verbose = TRUE)
  invisible()
}

#' The quantify executor
#'
#' Per bundle file: in `"bundle"` mode the whole file is analysed once and the
#' flattened frame table is what the persistent cache stores (one entry per
#' bundle, keyed by routine identity, speaker parameters and file stamp, so
#' `.at` and the segment set can change without a miss); each segment is then
#' a slice of that table. Files whose routine does not return a sliceable
#' track, and `"window"` mode, are analysed per segment, with one cache entry
#' per segment.
#' @noRd
.process_segments_vectorized <- function(seg_df, corpus_obj, dsp_function, dsp_params,
                                         media_ext, .at = NULL, .verbose = FALSE,
                                         use_cache = TRUE, cache_conn = NULL,
                                         cache_format = "auto", dsp_name = NULL,
                                         .mode = "bundle") {
  dt <- data.table::as.data.table(seg_df)
  dt[, signal_file := file.path(
    corpus_obj@basePath, paste0(session, "_ses"), paste0(bundle, "_bndl"),
    paste0(bundle, ".", media_ext))]
  dt[, file_exists := file.exists(signal_file)]
  dt_valid <- dt[file_exists == TRUE]
  n <- nrow(dt_valid)
  if (n == 0) return(list())

  caching <- use_cache && !is.null(cache_conn)
  fn_id <- .fn_identity(dsp_function)
  results <- vector("list", n)
  status <- rep(NA_character_, n)
  resolved <- rep(FALSE, n)
  by_file <- split(seq_len(n), dt_valid$signal_file)
  stamps <- .file_stamp(names(by_file))

  # ---- bundle level: one table per file -------------------------------------
  if (identical(.mode, "bundle")) {
    first <- vapply(by_file, `[`, integer(1), 1L)
    params <- dt_valid$seg_params[first]
    gkeys <- vapply(seq_along(by_file), function(k) {
      digest::digest(list("bundle-table", dt_valid$session[first[k]], dt_valid$bundle[first[k]],
                          fn_id, params[[k]], stamps[k], .cache_schema), algo = "xxhash64")
    }, character(1))
    tabs <- vector("list", length(by_file))
    cached <- if (caching) .get_persistent_cache_batch(gkeys, cache_conn) else list()
    for (k in seq_along(by_file)) tabs[k] <- list(cached[[gkeys[k]]])
    miss <- which(vapply(tabs, is.null, logical(1)))
    if (length(miss) > 0L) {
      computed <- future.apply::future_lapply(miss, function(k) {
        .dsp_whole_table(dsp_function, names(by_file)[k], params[[k]])
      }, future.seed = TRUE)
      for (j in seq_along(miss)) tabs[miss[j]] <- list(computed[[j]])
      ok <- miss[!vapply(computed, is.null, logical(1))]
      if (caching && length(ok) > 0L) {
        .set_persistent_cache_batch(
          Map(function(k) list(cache_key = gkeys[k], result = tabs[[k]]), ok),
          cache_conn, format = cache_format)
        .warn_if_cache_large(cache_conn)
      }
    }
    for (k in seq_along(by_file)) {
      if (is.null(tabs[[k]])) next
      idx <- by_file[[k]]
      results[idx] <- .slice_table(tabs[[k]], dt_valid$start[idx], dt_valid$end[idx])
      status[idx] <- if (caching) (if (k %in% miss) "miss" else "hit") else NA_character_
      resolved[idx] <- TRUE
    }
  }

  # ---- segment level: windowed analysis, per-segment cache -------------------
  pending <- which(!resolved)
  if (length(pending) > 0L) {
    sub <- dt_valid[pending]
    if (caching) {
      key_pre <- .precompute_cache_key_parts(dsp_name, list(), .at, fn_id = fn_id)
      sp_digest <- vapply(sub$seg_params, function(p) digest::digest(p, algo = "xxhash64"),
                          character(1))
      st <- stats::setNames(stamps, names(by_file))[sub$signal_file]
      keys <- paste(sub$session, sub$bundle, as.character(sub$start), as.character(sub$end),
                    key_pre$dsp_name, sp_digest, key_pre$at_digest, st, sep = "_")
      seg_cached <- .get_persistent_cache_batch(keys, cache_conn)
      hit <- !vapply(keys, function(k) is.null(seg_cached[[k]]), logical(1))
      for (j in which(hit)) results[pending[j]] <- list(seg_cached[[keys[j]]])
      status[pending] <- ifelse(hit, "hit", "miss")
      todo <- which(!hit)
    } else {
      keys <- NULL
      todo <- seq_along(pending)
    }
    if (length(todo) > 0L) {
      files <- unique(sub$signal_file[todo])
      groups <- lapply(files, function(f) {
        idx <- todo[sub$signal_file[todo] == f]
        list(idx = idx, file = f, params = sub$seg_params[[idx[1]]],
             starts = sub$start[idx], ends = sub$end[idx])
      })
      group_results <- future.apply::future_lapply(groups, function(gr) {
        .run_dsp_windows(dsp_function, gr$file, gr$params, gr$starts, gr$ends, .verbose)
      }, future.seed = TRUE)
      for (g in seq_along(groups)) {
        # `[<-` with list(NULL) keeps the slot of a failed segment
        results[pending[groups[[g]]$idx]] <- group_results[[g]]
      }
      if (caching) {
        done <- todo[!vapply(results[pending[todo]], is.null, logical(1))]
        if (length(done) > 0L) {
          .set_persistent_cache_batch(
            Map(function(k, r) list(cache_key = k, result = r), keys[done], results[pending[done]]),
            cache_conn, format = cache_format)
        }
      }
    }
  }

  dt_valid[, .cache_status := status]

  # ---- assemble: one rbindlist for all results -------------------------------
  frames <- lapply(results, function(r) if (is.null(r)) NULL else .dsp_result_to_df(r, .at))
  have <- which(!vapply(frames, is.null, logical(1)))
  if (length(have) == 0L) return(list())
  nrows <- vapply(frames[have], nrow, integer(1))
  seg_info <- dt_valid[rep(have, times = nrows)]
  res <- data.table::rbindlist(frames[have], fill = TRUE)
  dup <- intersect(names(seg_info), names(res))
  if (length(dup) > 0L) data.table::setnames(res, dup, paste0(dup, "_dsp"))
  list(tibble::as_tibble(cbind(seg_info, res)))
}

