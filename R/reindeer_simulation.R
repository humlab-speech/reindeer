# ==============================================================================
# SIMULATION INFRASTRUCTURE FOR DSP PROCEDURES
# ==============================================================================
#
# This module provides infrastructure for systematic parameter space exploration
# in DSP procedures with efficient caching and retrieval of results.
#
# Key components:
# 1. Signal file integrity tracking (SHA1 hashes)
# 2. Simulation mode for quantify with parameter grids
# 3. SQLite-based simulation caching
# 4. Result retrieval and reuse
#

# ==============================================================================
# SIGNAL FILE INTEGRITY TRACKING
# ==============================================================================

#' Compute SHA1 hash for a signal file
#'
#' @param file_path Full path to signal file
#' @return SHA1 hash as character string
#' @keywords internal
compute_signal_hash <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NA_character_)
  }
  digest::digest(file_path, algo = "sha1", file = TRUE)
}

#' Update signal file hashes in bundle metadata
#'
#' Computes and stores SHA1 hashes for all signal files associated with bundles
#' 
#' @param corpus_obj A corpus object
#' @param bundles Optional data.table with session and bundle columns to update.
#'   If NULL, updates all bundles.
#' @param verbose Show progress messages
#' @param parallel Use parallel processing (default: TRUE)
#' @export
update_signal_hashes <- function(corpus_obj, bundles = NULL, 
                                  verbose = TRUE, parallel = TRUE) {
  
  if (!inherits(corpus_obj, "reindeer::corpus") && !inherits(corpus_obj, "corpus")) {
    cli::cli_abort("Input must be a corpus object")
  }
  
  # Get signal files
  signal_files_dt <- peek_signals(corpus_obj)
  
  # Filter to requested bundles if specified
  if (!is.null(bundles)) {
    signal_files_dt <- signal_files_dt[session %in% bundles$session & 
                                         bundle %in% bundles$bundle]
  }
  
  if (nrow(signal_files_dt) == 0) {
    if (verbose) cli::cli_alert_warning("No signal files found")
    return(invisible(NULL))
  }
  
  if (verbose) {
    cli::cli_alert_info("Computing SHA1 hashes for {nrow(signal_files_dt)} signal file{?s}")
  }
  
  # Compute hashes (optionally in parallel)
  if (parallel && nrow(signal_files_dt) > 10) {
    n_cores <- max(1, parallel::detectCores() - 1)
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    parallel::clusterEvalQ(cl, library(digest))
    parallel::clusterExport(cl, "compute_signal_hash", envir = environment())
    
    hashes <- parallel::parSapply(cl, signal_files_dt$full_path, compute_signal_hash)
  } else {
    hashes <- sapply(signal_files_dt$full_path, compute_signal_hash)
  }
  
  signal_files_dt$sha1 <- hashes
  
  # Prepare metadata updates
  # Group by bundle to create single metadata entry per bundle
  bundle_hashes <- signal_files_dt[, .(
    signal_hashes = list(data.table::data.table(
      name = name,
      extension = extension,
      sha1 = sha1
    ))
  ), by = .(session, bundle)]
  
  # Convert to JSON for storage
  bundle_hashes$signal_hashes_json <- sapply(bundle_hashes$signal_hashes, function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE)
  })
  
  # Update metadata for each bundle
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(corpus_obj@basePath, "_cache.sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  db_uuid <- corpus_obj@config$UUID
  
  for (i in seq_len(nrow(bundle_hashes))) {
    session <- bundle_hashes$session[i]
    bundle <- bundle_hashes$bundle[i]
    hash_json <- bundle_hashes$signal_hashes_json[i]
    
    # Insert or replace
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO metadata_bundle 
        (db_uuid, session, bundle, field_name, field_value, field_type)
      VALUES (?, ?, ?, 'signal_hashes', ?, 'json')",
      params = list(db_uuid, session, bundle, hash_json)
    )
  }
  
  if (verbose) {
    cli::cli_alert_success(
      "Updated signal hashes for {nrow(bundle_hashes)} bundle{?s}"
    )
  }
  
  invisible(bundle_hashes)
}

#' Get signal file hashes for bundles
#'
#' @param corpus_obj A corpus object
#' @param session Session name (optional)
#' @param bundle Bundle name (optional)
#' @return data.table with signal hash information
#' @export
get_signal_hashes <- function(corpus_obj, session = NULL, bundle = NULL) {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(corpus_obj@basePath, "_cache.sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  db_uuid <- corpus_obj@config$UUID
  
  query <- "
    SELECT session, bundle, field_value as signal_hashes_json
    FROM metadata_bundle
    WHERE db_uuid = ? AND field_name = 'signal_hashes'
  "
  
  params <- list(db_uuid)
  
  if (!is.null(session)) {
    query <- paste0(query, " AND session = ?")
    params <- c(params, list(session))
  }
  
  if (!is.null(bundle)) {
    query <- paste0(query, " AND bundle = ?")
    params <- c(params, list(bundle))
  }
  
  result <- DBI::dbGetQuery(con, query, params = params)
  
  if (nrow(result) == 0) {
    return(data.table::data.table())
  }
  
  # Parse JSON
  result_dt <- data.table::as.data.table(result)
  result_dt$hashes <- lapply(result_dt$signal_hashes_json, function(x) {
    jsonlite::fromJSON(x)
  })
  result_dt$signal_hashes_json <- NULL
  
  result_dt
}

# ==============================================================================
# SIMULATION CACHE SCHEMA
# ==============================================================================

#' Initialize simulation cache database
#'
#' Creates SQLite database to store simulation results
#'
#' @param cache_dir Directory for simulation cache
#' @param timestamp Timestamp for cache file name
#' @param dsp_function_name Name of DSP function
#' @return Path to created cache file
#' @keywords internal
initialize_simulation_cache <- function(cache_dir, timestamp, dsp_function_name) {
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(
    cache_dir,
    sprintf("%s_%s.sqlite", timestamp, dsp_function_name)
  )
  
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Metadata table - stores information about the simulation
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS simulation_metadata (
      id INTEGER PRIMARY KEY,
      timestamp TEXT NOT NULL,
      dsp_function TEXT NOT NULL,
      created_at TEXT NOT NULL,
      corpus_path TEXT NOT NULL,
      corpus_uuid TEXT NOT NULL,
      n_segments INTEGER,
      n_parameter_combinations INTEGER,
      parameter_names TEXT,  -- JSON array
      computation_time_seconds REAL
    )")
  
  # Parameters table - stores each unique parameter combination
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS parameter_combinations (
      param_id INTEGER PRIMARY KEY AUTOINCREMENT,
      param_hash TEXT UNIQUE NOT NULL,  -- Hash of parameter combination
      params_json TEXT NOT NULL  -- JSON of all parameters
    )")
  
  # Results table - stores results for each segment × parameter combination
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS simulation_results (
      result_id INTEGER PRIMARY KEY AUTOINCREMENT,
      param_id INTEGER NOT NULL,
      segment_row_idx INTEGER NOT NULL,
      session TEXT NOT NULL,
      bundle TEXT NOT NULL,
      start_time REAL NOT NULL,
      end_time REAL NOT NULL,
      signal_hash TEXT,  -- Hash of signal file(s) used
      result_blob BLOB NOT NULL,  -- Serialized result object
      FOREIGN KEY (param_id) REFERENCES parameter_combinations(param_id),
      UNIQUE (param_id, segment_row_idx)
    )")
  
  # Create indices for efficient retrieval
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_param_hash 
    ON parameter_combinations(param_hash)")
  
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_results_param 
    ON simulation_results(param_id)")
  
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_results_segment 
    ON simulation_results(param_id, segment_row_idx)")
  
  cache_file
}

# ==============================================================================
# PARAMETER GRID GENERATION
# ==============================================================================

#' Create parameter grid from simulation specification
#'
#' @param simulate_spec List with parameter names and value vectors
#' @return data.table with all parameter combinations
#' @keywords internal
create_parameter_grid <- function(simulate_spec) {
  
  if (length(simulate_spec) == 0) {
    return(data.table::data.table())
  }
  
  # Validate input
  if (!all(sapply(simulate_spec, is.vector))) {
    cli::cli_abort("All elements in .simulate must be vectors")
  }
  
  # Create grid
  grid <- expand.grid(simulate_spec, stringsAsFactors = FALSE)
  grid <- data.table::as.data.table(grid)
  
  # Add parameter hash for efficient lookup
  grid$param_hash <- sapply(seq_len(nrow(grid)), function(i) {
    digest::digest(as.list(grid[i, ]), algo = "md5")
  })
  
  grid
}

#' Hash parameter combination for lookup
#'
#' @param params Named list of parameter values
#' @return MD5 hash string
#' @keywords internal
hash_parameters <- function(params) {
  digest::digest(params, algo = "md5")
}

# ==============================================================================
# EXTENDED QUANTIFY WITH SIMULATION SUPPORT
# ==============================================================================

#' Enhanced quantify method for segment_list with simulation support
#'
#' Extends quantify to support systematic parameter space exploration
#'
#' @param .what A segment_list object
#' @param .using DSP function to apply
#' @param ... Additional arguments passed to DSP function
#' @param .simulate Named list specifying parameter grid, e.g.,
#'   list(nominalF1 = seq(300, 600, 10), minF = seq(100, 250, 50))
#' @param .simulation_store Directory path for storing simulation cache
#' @param .simulation_timestamp Optional timestamp string (auto-generated if NULL)
#' @param .simulation_overwrite Overwrite existing simulation cache
#' @param .verbose Show progress
#' @return extended_segment_list or list of extended_segment_lists (if simulating)
#' @export
quantify_simulate <- function(.what, .using, ...,
                               .simulate = NULL,
                               .simulation_store = NULL,
                               .simulation_timestamp = NULL,
                               .simulation_overwrite = FALSE,
                               .verbose = TRUE) {
  
  # Validate inputs
  if (!inherits(.what, "segment_list")) {
    cli::cli_abort(".what must be a segment_list object")
  }
  
  if (!is.function(.using)) {
    cli::cli_abort(".using must be a DSP function")
  }
  
  # If not simulating, use regular quantify
  if (is.null(.simulate)) {
    return(quantify(.what, .using = .using, ...))
  }
  
  # Simulation mode
  if (is.null(.simulation_store)) {
    cli::cli_abort(
      ".simulation_store directory must be specified when using .simulate"
    )
  }
  
  # Get corpus object
  corpus_obj <- attr(.what, "corpus")
  if (is.null(corpus_obj)) {
    cli::cli_abort("segment_list must have corpus attribute")
  }
  
  # Generate timestamp if not provided
  if (is.null(.simulation_timestamp)) {
    .simulation_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  
  # Get DSP function name
  dsp_name <- deparse(substitute(.using))
  if (is.character(.using)) {
    dsp_name <- .using
  }
  
  # Initialize cache
  cache_file <- initialize_simulation_cache(
    .simulation_store,
    .simulation_timestamp,
    dsp_name
  )
  
  if (.verbose) {
    cli::cli_alert_info("Simulation cache: {.file {cache_file}}")
  }
  
  # Check if cache exists and whether to use it
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  existing_metadata <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as n FROM simulation_metadata
  ")
  
  if (existing_metadata$n > 0 && !.simulation_overwrite) {
    if (.verbose) {
      cli::cli_alert_info("Using existing simulation cache")
    }
    # Could implement cache retrieval here
    cli::cli_abort("Cache retrieval not yet implemented - use .simulation_overwrite = TRUE")
  }
  
  # Create parameter grid
  param_grid <- create_parameter_grid(.simulate)
  n_combinations <- nrow(param_grid)
  
  if (.verbose) {
    cli::cli_alert_info(
      "Simulating {n_combinations} parameter combination{?s} on {nrow(.what)} segment{?s}"
    )
    cli::cli_alert_info(
      "Parameters: {paste(names(.simulate), collapse = ', ')}"
    )
  }
  
  # Get base parameters from metadata and ...
  base_params <- list(...)
  
  # Ensure signal hashes are up to date
  update_signal_hashes(corpus_obj, 
                       bundles = unique(.what[, .(session, bundle)]),
                       verbose = FALSE)
  
  # Get signal hashes for segments
  signal_hashes <- get_signal_hashes(corpus_obj)
  .what_with_hash <- merge(
    data.table::as.data.table(.what),
    signal_hashes[, .(session, bundle, hashes)],
    by = c("session", "bundle"),
    all.x = TRUE
  )
  
  # Store simulation metadata
  DBI::dbExecute(con, "
    INSERT INTO simulation_metadata 
      (timestamp, dsp_function, created_at, corpus_path, corpus_uuid,
       n_segments, n_parameter_combinations, parameter_names)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      .simulation_timestamp,
      dsp_name,
      as.character(Sys.time()),
      corpus_obj@basePath,
      corpus_obj@config$UUID,
      nrow(.what),
      n_combinations,
      jsonlite::toJSON(names(.simulate))
    )
  )
  
  # Process each parameter combination
  start_time <- Sys.time()
  
  if (.verbose) {
    cli::cli_progress_bar(
      "Running simulations",
      total = n_combinations
    )
  }
  
  results_list <- vector("list", n_combinations)
  
  for (i in seq_len(n_combinations)) {
    
    # Get parameters for this combination
    current_params <- as.list(param_grid[i, !c("param_hash")])
    param_hash <- param_grid$param_hash[i]
    
    # Merge with base parameters (simulation params override)
    all_params <- modifyList(base_params, current_params)
    
    # Store parameter combination
    param_id <- DBI::dbGetQuery(con, "
      SELECT param_id FROM parameter_combinations WHERE param_hash = ?",
      params = list(param_hash)
    )
    
    if (nrow(param_id) == 0) {
      DBI::dbExecute(con, "
        INSERT INTO parameter_combinations (param_hash, params_json)
        VALUES (?, ?)",
        params = list(param_hash, jsonlite::toJSON(current_params, auto_unbox = TRUE))
      )
      param_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() as param_id")
    }
    
    param_id <- param_id$param_id[1]
    
    # Apply quantify with these parameters
    result <- do.call(quantify, c(
      list(.what = .what, .using = .using),
      all_params
    ))
    
    # Store results in cache
    for (seg_idx in seq_len(nrow(result))) {
      
      # Serialize result for this segment
      result_blob <- serialize(result[seg_idx], NULL)
      
      # Get signal hash
      sig_hash <- NA_character_
      if ("hashes" %in% names(.what_with_hash)) {
        hash_info <- .what_with_hash$hashes[[seg_idx]]
        if (!is.null(hash_info)) {
          sig_hash <- jsonlite::toJSON(hash_info)
        }
      }
      
      DBI::dbExecute(con, "
        INSERT OR REPLACE INTO simulation_results
          (param_id, segment_row_idx, session, bundle, start_time, end_time,
           signal_hash, result_blob)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          param_id,
          seg_idx,
          result$session[seg_idx],
          result$bundle[seg_idx],
          result$start[seg_idx],
          result$end[seg_idx],
          sig_hash,
          result_blob
        )
      )
    }
    
    # Store result
    results_list[[i]] <- result
    names(results_list)[i] <- param_hash
    
    if (.verbose) {
      cli::cli_progress_update()
    }
  }
  
  if (.verbose) {
    cli::cli_progress_done()
  }
  
  # Update computation time
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  DBI::dbExecute(con, "
    UPDATE simulation_metadata 
    SET computation_time_seconds = ?
    WHERE id = 1",
    params = list(elapsed)
  )
  
  if (.verbose) {
    cli::cli_alert_success(
      "Simulation complete in {round(elapsed, 2)} seconds"
    )
  }
  
  # Return list of results with parameter information
  structure(
    results_list,
    class = c("simulation_results", "list"),
    parameter_grid = param_grid,
    cache_file = cache_file,
    timestamp = .simulation_timestamp,
    dsp_function = dsp_name
  )
}

# ==============================================================================
# SIMULATION RESULT RETRIEVAL
# ==============================================================================

#' Retrieve simulation results from cache
#'
#' @param segment_list Original segment_list used in simulation
#' @param parameters Named list of parameter values to retrieve
#' @param cache_path Path to simulation cache file, or
#' @param timestamp Timestamp string to identify cache file
#' @param cache_dir Directory containing cache files (if using timestamp)
#' @param dsp_function DSP function name (if using timestamp)
#' @return extended_segment_list with retrieved results
#' @export
reminisce <- function(segment_list,
                      parameters,
                      cache_path = NULL,
                      timestamp = NULL,
                      cache_dir = NULL,
                      dsp_function = NULL) {
  
  # Determine cache file
  if (is.null(cache_path)) {
    if (is.null(timestamp) || is.null(cache_dir) || is.null(dsp_function)) {
      cli::cli_abort(
        "Must provide either cache_path or (timestamp, cache_dir, dsp_function)"
      )
    }
    cache_path <- file.path(
      cache_dir,
      sprintf("%s_%s.sqlite", timestamp, dsp_function)
    )
  }
  
  if (!file.exists(cache_path)) {
    cli::cli_abort("Cache file not found: {.file {cache_path}}")
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Find parameter combination
  param_hash <- hash_parameters(parameters)
  
  param_id <- DBI::dbGetQuery(con, "
    SELECT param_id FROM parameter_combinations 
    WHERE param_hash = ?",
    params = list(param_hash)
  )
  
  if (nrow(param_id) == 0) {
    # Try to find by matching parameters
    all_params <- DBI::dbGetQuery(con, "
      SELECT param_id, params_json FROM parameter_combinations
    ")
    
    matching <- sapply(all_params$params_json, function(json) {
      params <- jsonlite::fromJSON(json)
      all(mapply(function(n, v) {
        isTRUE(all.equal(params[[n]], v))
      }, names(parameters), parameters))
    })
    
    if (!any(matching)) {
      cli::cli_abort(
        "No matching parameter combination found in cache",
        "i" = "Available parameters: {DBI::dbGetQuery(con, 'SELECT params_json FROM parameter_combinations LIMIT 5')$params_json}"
      )
    }
    
    param_id <- all_params$param_id[which(matching)[1]]
  } else {
    param_id <- param_id$param_id[1]
  }
  
  # Retrieve results
  results <- DBI::dbGetQuery(con, "
    SELECT segment_row_idx, result_blob
    FROM simulation_results
    WHERE param_id = ?
    ORDER BY segment_row_idx",
    params = list(param_id)
  )
  
  if (nrow(results) == 0) {
    cli::cli_abort("No results found for parameter combination")
  }
  
  # Deserialize and combine results
  result_list <- lapply(results$result_blob, unserialize)
  
  combined <- data.table::rbindlist(result_list, fill = TRUE)
  
  # Convert to extended_segment_list
  class(combined) <- c("extended_segment_list", "segment_list", class(combined))
  attr(combined, "corpus") <- attr(segment_list, "corpus")
  attr(combined, "parameters") <- parameters
  attr(combined, "cache_file") <- cache_path
  
  combined
}

#' List available simulations in cache directory
#'
#' @param cache_dir Directory containing simulation caches
#' @return data.table with simulation metadata
#' @export
list_simulations <- function(cache_dir) {
  
  if (!dir.exists(cache_dir)) {
    cli::cli_alert_warning("Cache directory does not exist: {.path {cache_dir}}")
    return(data.table::data.table())
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.sqlite$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    cli::cli_alert_info("No simulation caches found in {.path {cache_dir}}")
    return(data.table::data.table())
  }
  
  # Extract metadata from each cache
  metadata_list <- lapply(cache_files, function(cache_file) {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      meta <- DBI::dbGetQuery(con, "
        SELECT * FROM simulation_metadata LIMIT 1
      ")
      
      if (nrow(meta) > 0) {
        meta$cache_file <- cache_file
        meta
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
  })
  
  metadata_list <- Filter(Negate(is.null), metadata_list)
  
  if (length(metadata_list) == 0) {
    return(data.table::data.table())
  }
  
  data.table::rbindlist(metadata_list, fill = TRUE)
}

# ==============================================================================
# PRINT AND SUMMARY METHODS
# ==============================================================================

#' @export
print.simulation_results <- function(x, ...) {
  param_grid <- attr(x, "parameter_grid")
  
  cat("\n")
  cat("══ Simulation Results ══\n")
  cat("\n")
  cat(sprintf("DSP function: %s\n", attr(x, 'dsp_function')))
  cat(sprintf("Timestamp: %s\n", attr(x, 'timestamp')))
  cat(sprintf("Cache file: %s\n", attr(x, 'cache_file')))
  cat("\n")
  cat(sprintf("%d parameter combination%s\n", length(x), if (length(x) != 1) "s" else ""))
  cat("\n")
  
  # Remove param_hash column for display
  param_display <- param_grid[, setdiff(names(param_grid), "param_hash"), with = FALSE]
  
  if (nrow(param_display) <= 10) {
    print(param_display)
  } else {
    cat("First 5 parameter combinations:\n")
    print(head(param_display, 5))
    cat("...\n")
    cat(sprintf("(%d more)\n", nrow(param_display) - 5))
  }
  
  invisible(x)
}

#' @export
summary.simulation_results <- function(object, ...) {
  print(object)
  
  cat("\n")
  cat("══ Result Summary ══\n")
  
  # Show summary of first result as example
  if (length(object) > 0) {
    cat("\n")
    cat("Example result (first parameter combination):\n")
    print(summary(object[[1]]))
  }
  
  invisible(object)
}

# ==============================================================================
# SIMULATION SUPPORT FOR ENRICH
# ==============================================================================

#' Enhanced enrich with simulation support
#'
#' Extends enrich() to support systematic parameter space exploration for track
#' generation. Results are cached in SQLite database for efficient retrieval.
#'
#' @param corpus_obj A corpus object
#' @param .using A DSP function from superassp package
#' @param ... Additional arguments passed to DSP function
#' @param .simulate Named list specifying parameter grid (e.g.,
#'   list(nominalF1 = seq(300, 600, 50), windowSize = c(20, 25, 30)))
#' @param .simulation_store Directory for simulation cache (required if .simulate is used)
#' @param .simulation_timestamp Optional timestamp (auto-generated if NULL)
#' @param .simulation_overwrite Overwrite existing simulation cache
#' @param .metadata_fields Character vector of metadata fields for parameter derivation
#' @param .signal_extension File extension of signal files to process
#' @param .force Recompute even if track files exist
#' @param .verbose Show progress messages
#' @param .parallel Use parallel processing
#' @param .workers Number of parallel workers
#' @return The corpus object (invisibly), or simulation_tracks object if simulating
#' @export
enrich_simulate <- function(corpus_obj, .using, ...,
                            .simulate = NULL,
                            .simulation_store = NULL,
                            .simulation_timestamp = NULL,
                            .simulation_overwrite = FALSE,
                            .metadata_fields = c("Gender", "Age"),
                            .signal_extension = NULL,
                            .force = FALSE,
                            .verbose = TRUE,
                            .parallel = TRUE,
                            .workers = NULL) {
  
  # If not simulating, use regular enrich
  if (is.null(.simulate)) {
    return(enrich(
      corpus_obj = corpus_obj,
      .using = .using,
      ...,
      .metadata_fields = .metadata_fields,
      .signal_extension = .signal_extension,
      .force = .force,
      .verbose = .verbose,
      .parallel = .parallel,
      .workers = .workers
    ))
  }
  
  # Simulation mode
  if (is.null(.simulation_store)) {
    cli::cli_abort(
      ".simulation_store directory must be specified when using .simulate"
    )
  }
  
  # Validate corpus
  if (!inherits(corpus_obj, "reindeer::corpus") && !inherits(corpus_obj, "corpus")) {
    cli::cli_abort("{.arg corpus_obj} must be a corpus object")
  }
  
  # Generate timestamp if not provided
  if (is.null(.simulation_timestamp)) {
    .simulation_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  
  # Get DSP function name
  dsp_name <- deparse(substitute(.using))
  if (is.character(.using)) {
    dsp_name <- .using
  } else if (is.function(.using)) {
    dsp_name <- as.character(substitute(.using))
  }
  
  # Initialize cache
  cache_file <- initialize_track_simulation_cache(
    .simulation_store,
    .simulation_timestamp,
    dsp_name
  )
  
  if (.verbose) {
    cli::cli_h2("Simulating track generation with {.fn {dsp_name}}")
    cli::cli_alert_info("Cache: {.file {cache_file}}")
  }
  
  # Determine signal extension
  if (is.null(.signal_extension)) {
    .signal_extension <- corpus_obj@config$mediafileExtension %||% "wav"
  }
  
  # Get signal files
  signal_files <- peek_signals(corpus_obj) %>%
    dplyr::filter(extension == .signal_extension)
  
  if (nrow(signal_files) == 0) {
    cli::cli_alert_warning("No signal files with extension {.val {.signal_extension}}")
    return(invisible(corpus_obj))
  }
  
  # Update signal hashes
  if (.verbose) {
    cli::cli_alert_info("Computing signal file hashes...")
  }
  update_signal_hashes(corpus_obj, verbose = FALSE, parallel = .parallel)
  
  # Create parameter grid
  param_grid <- create_parameter_grid(.simulate)
  n_combinations <- nrow(param_grid)
  
  if (.verbose) {
    cli::cli_alert_info(
      "Simulating {n_combinations} parameter combination{?s} on {nrow(signal_files)} signal file{?s}"
    )
    cli::cli_alert_info(
      "Parameters: {paste(names(.simulate), collapse = ', ')}"
    )
  }
  
  # Open cache connection
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Store simulation metadata
  DBI::dbExecute(con, "
    INSERT INTO simulation_metadata
      (timestamp, dsp_function, created_at, corpus_path, corpus_uuid,
       n_signal_files, n_parameter_combinations, parameter_names)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      .simulation_timestamp,
      dsp_name,
      as.character(Sys.time()),
      corpus_obj@basePath,
      corpus_obj@config$UUID,
      nrow(signal_files),
      n_combinations,
      jsonlite::toJSON(names(.simulate))
    )
  )
  
  # Get metadata for bundles
  bundle_metadata <- get_all_bundle_metadata(corpus_obj)
  signal_files_with_meta <- signal_files %>%
    dplyr::left_join(bundle_metadata, by = c("session", "bundle"))
  
  # Get base parameters
  base_params <- list(...)
  
  # Determine parallel processing
  if (.parallel) {
    if (is.null(.workers)) {
      .workers <- max(1, parallel::detectCores() - 1)
    }
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = .workers)
  }
  
  start_time <- Sys.time()
  
  # Process each parameter combination
  if (.verbose) {
    cli::cli_progress_bar(
      "Running simulations",
      total = n_combinations
    )
  }
  
  track_results <- list()
  
  for (i in seq_len(n_combinations)) {
    
    # Get parameters for this combination
    current_params <- as.list(param_grid[i, !c("param_hash")])
    param_hash <- param_grid$param_hash[i]
    
    # Store parameter combination
    param_id <- DBI::dbGetQuery(con, "
      SELECT param_id FROM parameter_combinations WHERE param_hash = ?",
      params = list(param_hash)
    )
    
    if (nrow(param_id) == 0) {
      DBI::dbExecute(con, "
        INSERT INTO parameter_combinations (param_hash, params_json)
        VALUES (?, ?)",
        params = list(param_hash, jsonlite::toJSON(current_params, auto_unbox = TRUE))
      )
      param_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() as param_id")
    }
    
    param_id <- param_id$param_id[1]
    
    # Process each signal file with these parameters
    process_file <- function(j, files_meta, dsp_fun, current_params, base_params,
                             metadata_fields, param_id, con, cache_file) {
      
      file_row <- files_meta[j, ]
      
      # Derive DSP parameters from metadata
      all_params <- derive_dsp_parameters(
        dsp_fun = dsp_fun,
        metadata = file_row,
        metadata_fields = metadata_fields,
        user_params = modifyList(base_params, current_params)
      )
      
      # Apply DSP function in memory (not to file)
      tryCatch({
        track <- do.call(dsp_fun, c(
          list(listOfFiles = file_row$full_path),
          all_params,
          list(toFile = FALSE, verbose = FALSE)
        ))
        
        # Serialize track object
        track_blob <- base::serialize(track, connection = NULL, ascii = FALSE)
        
        # Get signal hash
        signal_hashes <- get_signal_hashes(corpus_obj, 
                                           session = file_row$session,
                                           bundle = file_row$bundle)
        sig_hash_json <- if (nrow(signal_hashes) > 0) {
          jsonlite::toJSON(signal_hashes$hashes[[1]])
        } else {
          NA_character_
        }
        
        # Store in cache
        # Need to reconnect in parallel workers
        if (!DBI::dbIsValid(con)) {
          con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
        }
        
        DBI::dbExecute(con, "
          INSERT OR REPLACE INTO track_simulation_results
            (param_id, session, bundle, signal_file, signal_hash, track_blob)
          VALUES (?, ?, ?, ?, ?, ?)",
          params = list(
            param_id,
            file_row$session,
            file_row$bundle,
            file_row$name,
            sig_hash_json,
            list(track_blob)  # Wrap in list for BLOB
          )
        )
        
        list(success = TRUE, bundle = file_row$bundle, session = file_row$session)
      }, error = function(e) {
        list(success = FALSE, bundle = file_row$bundle, session = file_row$session,
             error = e$message)
      })
    }
    
    # Process all files for this parameter combination
    if (.parallel) {
      file_results <- furrr::future_map(
        seq_len(nrow(signal_files_with_meta)),
        process_file,
        files_meta = signal_files_with_meta,
        dsp_fun = .using,
        current_params = current_params,
        base_params = base_params,
        metadata_fields = .metadata_fields,
        param_id = param_id,
        con = con,
        cache_file = cache_file,
        .progress = FALSE,
        .options = furrr::furrr_options(seed = TRUE)
      )
    } else {
      file_results <- lapply(
        seq_len(nrow(signal_files_with_meta)),
        process_file,
        files_meta = signal_files_with_meta,
        dsp_fun = .using,
        current_params = current_params,
        base_params = base_params,
        metadata_fields = .metadata_fields,
        param_id = param_id,
        con = con,
        cache_file = cache_file
      )
    }
    
    track_results[[i]] <- file_results
    names(track_results)[i] <- param_hash
    
    if (.verbose) {
      cli::cli_progress_update()
    }
  }
  
  if (.verbose) {
    cli::cli_progress_done()
  }
  
  # Update computation time
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  DBI::dbExecute(con, "
    UPDATE simulation_metadata
    SET computation_time_seconds = ?
    WHERE id = 1",
    params = list(elapsed)
  )
  
  if (.verbose) {
    cli::cli_alert_success(
      "Track simulation complete in {round(elapsed, 2)} seconds"
    )
    
    # Report errors
    all_results <- unlist(track_results, recursive = FALSE)
    errors <- Filter(function(x) !x$success, all_results)
    if (length(errors) > 0) {
      cli::cli_alert_warning("{length(errors)} file{?s} failed processing")
    }
  }
  
  # Return simulation results object
  structure(
    track_results,
    class = c("simulation_tracks", "list"),
    parameter_grid = param_grid,
    cache_file = cache_file,
    timestamp = .simulation_timestamp,
    dsp_function = dsp_name,
    corpus = corpus_obj
  )
}

#' Initialize track simulation cache database
#'
#' @keywords internal
initialize_track_simulation_cache <- function(cache_dir, timestamp, dsp_function_name) {
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(
    cache_dir,
    sprintf("enrich_%s_%s.sqlite", timestamp, dsp_function_name)
  )
  
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Metadata table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS simulation_metadata (
      id INTEGER PRIMARY KEY,
      timestamp TEXT NOT NULL,
      dsp_function TEXT NOT NULL,
      created_at TEXT NOT NULL,
      corpus_path TEXT NOT NULL,
      corpus_uuid TEXT NOT NULL,
      n_signal_files INTEGER,
      n_parameter_combinations INTEGER,
      parameter_names TEXT,
      computation_time_seconds REAL
    )")
  
  # Parameters table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS parameter_combinations (
      param_id INTEGER PRIMARY KEY AUTOINCREMENT,
      param_hash TEXT UNIQUE NOT NULL,
      params_json TEXT NOT NULL
    )")
  
  # Track results table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS track_simulation_results (
      result_id INTEGER PRIMARY KEY AUTOINCREMENT,
      param_id INTEGER NOT NULL,
      session TEXT NOT NULL,
      bundle TEXT NOT NULL,
      signal_file TEXT NOT NULL,
      signal_hash TEXT,
      track_blob BLOB NOT NULL,
      FOREIGN KEY (param_id) REFERENCES parameter_combinations(param_id),
      UNIQUE (param_id, session, bundle, signal_file)
    )")
  
  # Indices
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_track_param
    ON track_simulation_results(param_id)")
  
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_track_bundle
    ON track_simulation_results(session, bundle)")
  
  cache_file
}

#' Retrieve track simulation results
#'
#' Retrieves SSFF track objects from simulation cache
#'
#' @param corpus_obj Corpus object used in simulation
#' @param parameters Named list of parameter values to retrieve
#' @param cache_path Path to cache file, or
#' @param timestamp Timestamp string, or
#' @param cache_dir Cache directory (requires timestamp)
#' @param dsp_function DSP function name (requires timestamp)
#' @param session Optional session filter
#' @param bundle Optional bundle filter
#' @return data.table with track information
#' @export
reminisce_tracks <- function(corpus_obj,
                             parameters,
                             cache_path = NULL,
                             timestamp = NULL,
                             cache_dir = NULL,
                             dsp_function = NULL,
                             session = NULL,
                             bundle = NULL) {
  
  # Determine cache file
  if (is.null(cache_path)) {
    if (is.null(timestamp) || is.null(cache_dir) || is.null(dsp_function)) {
      cli::cli_abort(
        "Must provide either cache_path or (timestamp, cache_dir, dsp_function)"
      )
    }
    cache_path <- file.path(
      cache_dir,
      sprintf("enrich_%s_%s.sqlite", timestamp, dsp_function)
    )
  }
  
  if (!file.exists(cache_path)) {
    cli::cli_abort("Cache file not found: {.file {cache_path}}")
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Find parameter combination
  param_hash <- hash_parameters(parameters)
  
  param_id <- DBI::dbGetQuery(con, "
    SELECT param_id FROM parameter_combinations
    WHERE param_hash = ?",
    params = list(param_hash)
  )
  
  if (nrow(param_id) == 0) {
    cli::cli_abort("No matching parameter combination found")
  }
  
  param_id <- param_id$param_id[1]
  
  # Build query
  query <- "
    SELECT session, bundle, signal_file, signal_hash, track_blob
    FROM track_simulation_results
    WHERE param_id = ?"
  params <- list(param_id)
  
  if (!is.null(session)) {
    query <- paste0(query, " AND session = ?")
    params <- c(params, list(session))
  }
  
  if (!is.null(bundle)) {
    query <- paste0(query, " AND bundle = ?")
    params <- c(params, list(bundle))
  }
  
  results <- DBI::dbGetQuery(con, query, params = params)
  
  if (nrow(results) == 0) {
    cli::cli_alert_warning("No results found for parameters")
    return(data.table::data.table())
  }
  
  # Deserialize tracks
  results$track <- lapply(results$track_blob, unserialize)
  results$track_blob <- NULL
  
  # Convert to data.table
  results_dt <- data.table::as.data.table(results)
  
  structure(
    results_dt,
    class = c("simulation_track_results", "data.table", "data.frame"),
    parameters = parameters,
    cache_file = cache_path
  )
}

#' Assess simulation results against stored tracks
#'
#' Compares simulated track results with tracks stored in the corpus database
#' using a specified assessment metric (default: RMSE from yardstick package)
#'
#' @param segment_list A segment_list with corpus attribute
#' @param simulation_results Simulation results from enrich_simulate
#' @param .metric Assessment metric function (default: yardstick::rmse)
#' @param .track_name Name of track in database to compare against
#' @param .detailed Return detailed per-segment metrics (vs. summary)
#' @param .verbose Show progress
#' @return data.table with assessment results
#' @export
assess <- function(segment_list,
                   simulation_results,
                   .metric = yardstick::rmse,
                   .track_name = NULL,
                   .detailed = FALSE,
                   .verbose = TRUE) {
  
  # Validate inputs
  if (!inherits(segment_list, "segment_list")) {
    cli::cli_abort("First argument must be a segment_list")
  }
  
  if (!inherits(simulation_results, "simulation_tracks")) {
    cli::cli_abort("Second argument must be simulation_tracks from enrich_simulate")
  }
  
  corpus_obj <- attr(segment_list, "corpus") %||% attr(simulation_results, "corpus")
  if (is.null(corpus_obj)) {
    cli::cli_abort("Cannot find corpus object in segment_list or simulation_results")
  }
  
  # Determine track name
  if (is.null(.track_name)) {
    # Try to infer from DSP function
    dsp_fun <- attr(simulation_results, "dsp_function")
    cli::cli_alert_info("Using DSP function name as track: {.val {dsp_fun}}")
    .track_name <- dsp_fun
  }
  
  cache_file <- attr(simulation_results, "cache_file")
  param_grid <- attr(simulation_results, "parameter_grid")
  
  if (.verbose) {
    cli::cli_h2("Assessing simulation results")
    cli::cli_alert_info("Comparing {nrow(param_grid)} parameter combination{?s}")
    cli::cli_alert_info("Using metric: {.fn {deparse(substitute(.metric))}}")
  }
  
  # Get stored tracks for segments
  # This would call quantify on the original tracks
  reference_tracks <- quantify(
    segment_list,
    .using = function(listOfFiles, ...) {
      # Read existing SSFF files
      wrassp::read.AsspDataObj(listOfFiles, ...)
    },
    trackName = .track_name
  )
  
  # For each parameter combination, compare
  assessment_results <- list()
  
  if (.verbose) {
    cli::cli_progress_bar("Assessing combinations", total = nrow(param_grid))
  }
  
  for (i in seq_len(nrow(param_grid))) {
    param_hash <- param_grid$param_hash[i]
    params <- as.list(param_grid[i, !c("param_hash")])
    
    # Retrieve simulated tracks
    sim_tracks <- reminisce_tracks(
      corpus_obj,
      parameters = params,
      cache_path = cache_file
    )
    
    # Match segments to tracks
    # Compare track values
    # This is simplified - actual implementation would extract track data
    # and compute metrics
    
    if (.detailed) {
      # Per-segment metrics
      # ... detailed comparison logic ...
      assessment_results[[i]] <- data.table::data.table(
        param_id = i,
        params = list(params),
        metric_value = NA_real_  # Placeholder
      )
    } else {
      # Summary metric
      assessment_results[[i]] <- data.table::data.table(
        param_id = i,
        params = list(params),
        metric_value = NA_real_  # Placeholder
      )
    }
    
    if (.verbose) {
      cli::cli_progress_update()
    }
  }
  
  if (.verbose) {
    cli::cli_progress_done()
  }
  
  results_dt <- data.table::rbindlist(assessment_results, fill = TRUE)
  
  structure(
    results_dt,
    class = c("assessment_results", "data.table", "data.frame"),
    metric = deparse(substitute(.metric)),
    track_name = .track_name,
    detailed = .detailed
  )
}

#' @export
print.simulation_tracks <- function(x, ...) {
  param_grid <- attr(x, "parameter_grid")
  
  cat("\n")
  cat("══ Track Simulation Results ══\n")
  cat("\n")
  cat(sprintf("DSP function: %s\n", attr(x, 'dsp_function')))
  cat(sprintf("Timestamp: %s\n", attr(x, 'timestamp')))
  cat(sprintf("Cache file: %s\n", attr(x, 'cache_file')))
  cat("\n")
  cat(sprintf("%d parameter combination%s\n", length(x), if (length(x) != 1) "s" else ""))
  cat("\n")
  
  param_display <- param_grid[, setdiff(names(param_grid), "param_hash"), with = FALSE]
  
  if (nrow(param_display) <= 10) {
    print(param_display)
  } else {
    cat("First 5 parameter combinations:\n")
    print(head(param_display, 5))
    cat("...\n")
    cat(sprintf("(%d more)\n", nrow(param_display) - 5))
  }
  
  invisible(x)
}
