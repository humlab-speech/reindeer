# ==============================================================================
# SIMULATION INFRASTRUCTURE FOR DSP PROCEDURES
# ==============================================================================
#
# This module provides infrastructure for systematic parameter space exploration
# in DSP procedures with efficient caching and retrieval of results.
#
# Key components:
# 1. Signal file integrity tracking (SHA1 hashes)
# 2. Simulation mode for quantify/enrich with parameter grids
# 3. Preprocessing support for media file transformation before DSP
# 4. SQLite-based simulation caching
# 5. Result retrieval and reuse
#
# ==============================================================================
# PREPROCESSING EXAMPLES
# ==============================================================================
#
# The simulation system supports preprocessing media files before DSP analysis.
# This allows systematic exploration of how media transformations affect results.
#
# Example 1: Simulate formant analysis with different sample rates
# \dontrun{
# library(superassp)
# library(reindeer)
#
# # Load corpus and query segments
# corp <- corpus("path/to/db_emuDB")
# segments <- ask_for(corp, "Phonetic == a")
#
# # Simulate with sample rate preprocessing
# results <- quantify_simulate(
#   segments,
#   .using = superassp::forest,
#   .simulate = list(
#     nominalF1 = seq(500, 900, 100)  # DSP parameter grid
#   ),
#   .prep_function = superassp::prep_recode,
#   .prep_simulate = list(
#     sample_rate = c(16000, 22050, 44100),  # Prep parameter grid
#     format = "wav"
#   ),
#   .simulation_store = "simulations/formants"
# )
#
# # This creates 3 * 5 = 15 combinations:
# # - 3 sample rates × 5 nominalF1 values
# # Each segment is processed with all 15 combinations
#
# print(results)
# # Shows both DSP and prep parameters
# }
#
# Example 2: Enrich corpus with pitch tracks using different codecs
# \dontrun{
# # Simulate track generation with preprocessing
# track_sim <- enrich_simulate(
#   corp,
#   .using = superassp::ksvF0,
#   .simulate = list(
#     minF = c(50, 75, 100),
#     maxF = c(300, 400, 500)
#   ),
#   .prep_function = superassp::prep_recode,
#   .prep_simulate = list(
#     format = c("wav", "flac", "mp3"),
#     bit_rate = c(128000, 320000)
#   ),
#   .simulation_store = "simulations/pitch"
# )
#
# # This creates 3 * 3 * 3 * 2 = 54 combinations:
# # - 3 minF × 3 maxF × 3 formats × 2 bit rates
# }
#
# Example 3: Preprocessing only (no DSP parameter variation)
# \dontrun{
# # Test effect of sample rate alone
# results <- quantify_simulate(
#   segments,
#   .using = superassp::forest,
#   nominalF1 = 700,  # Fixed DSP parameter
#   .prep_function = superassp::prep_recode,
#   .prep_simulate = list(
#     sample_rate = c(8000, 16000, 22050, 32000, 44100)
#   ),
#   .simulation_store = "simulations/samplerate_test"
# )
# }
#
# Example 4: DSP parameters only (no preprocessing)
# \dontrun{
# # Traditional simulation without preprocessing
# results <- quantify_simulate(
#   segments,
#   .using = superassp::forest,
#   .simulate = list(
#     nominalF1 = seq(400, 1000, 50),
#     windowShift = c(5, 10)
#   ),
#   .simulation_store = "simulations/traditional"
# )
# }
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
  
  if (!S7::S7_inherits(corpus_obj, reindeer::corpus)) {
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
      prep_function TEXT,  -- Preprocessing function name (optional)
      created_at TEXT NOT NULL,
      corpus_path TEXT NOT NULL,
      corpus_uuid TEXT NOT NULL,
      n_segments INTEGER,
      n_parameter_combinations INTEGER,
      parameter_names TEXT,  -- JSON array of DSP parameter names
      prep_parameter_names TEXT,  -- JSON array of prep parameter names
      computation_time_seconds REAL
    )")
  
  # Parameters table - stores each unique parameter combination
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS parameter_combinations (
      param_id INTEGER PRIMARY KEY AUTOINCREMENT,
      param_hash TEXT UNIQUE NOT NULL,  -- Hash of parameter combination
      params_json TEXT NOT NULL,  -- JSON of DSP parameters
      prep_params_json TEXT  -- JSON of prep parameters (optional)
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
#' @param prep_simulate_spec List with prep parameter names and value vectors (optional)
#' @return data.table with all parameter combinations
#' @keywords internal
create_parameter_grid <- function(simulate_spec, prep_simulate_spec = NULL) {

  if (length(simulate_spec) == 0 && (is.null(prep_simulate_spec) || length(prep_simulate_spec) == 0)) {
    return(data.table::data.table())
  }

  # Validate input
  if (length(simulate_spec) > 0 && !all(sapply(simulate_spec, is.vector))) {
    cli::cli_abort("All elements in .simulate must be vectors")
  }

  if (!is.null(prep_simulate_spec) && length(prep_simulate_spec) > 0 &&
      !all(sapply(prep_simulate_spec, is.vector))) {
    cli::cli_abort("All elements in .prep_simulate must be vectors")
  }

  # Create grids
  dsp_grid <- if (length(simulate_spec) > 0) {
    expand.grid(simulate_spec, stringsAsFactors = FALSE)
  } else {
    data.frame(dummy = 1)  # Placeholder for when only prep params vary
  }

  prep_grid <- if (!is.null(prep_simulate_spec) && length(prep_simulate_spec) > 0) {
    expand.grid(prep_simulate_spec, stringsAsFactors = FALSE)
  } else {
    data.frame(dummy = 1)  # Placeholder for when only DSP params vary
  }

  # Combine grids (full outer product)
  combined_grid <- merge(dsp_grid, prep_grid, by = NULL)

  # Remove dummy columns
  combined_grid$dummy.x <- NULL
  combined_grid$dummy.y <- NULL

  grid <- data.table::as.data.table(combined_grid)

  # Separate DSP and prep params
  dsp_param_names <- if (length(simulate_spec) > 0) names(simulate_spec) else character(0)
  prep_param_names <- if (!is.null(prep_simulate_spec) && length(prep_simulate_spec) > 0) {
    names(prep_simulate_spec)
  } else {
    character(0)
  }

  # Add parameter hash for efficient lookup (hash includes both DSP and prep params)
  grid$param_hash <- sapply(seq_len(nrow(grid)), function(i) {
    dsp_params <- if (length(dsp_param_names) > 0) {
      as.list(grid[i, ..dsp_param_names])
    } else {
      list()
    }
    prep_params <- if (length(prep_param_names) > 0) {
      as.list(grid[i, ..prep_param_names])
    } else {
      list()
    }
    digest::digest(list(dsp = dsp_params, prep = prep_params), algo = "md5")
  })

  # Add metadata about which params are DSP vs prep
  attr(grid, "dsp_params") <- dsp_param_names
  attr(grid, "prep_params") <- prep_param_names

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
#' Extends quantify to support systematic parameter space exploration with
#' optional preprocessing of media files before DSP analysis.
#'
#' @param .what A segment_list object
#' @param .using DSP function to apply
#' @param ... Additional arguments passed to DSP function
#' @param .simulate Named list specifying DSP parameter grid, e.g.,
#'   list(nominalF1 = seq(300, 600, 10), minF = seq(100, 250, 50))
#' @param .prep_function Optional preprocessing function to apply to media before DSP.
#'   Should accept listOfFiles parameter and return audio data compatible with DSP function.
#'   See superassp::prep_recode() for example.
#' @param .prep_simulate Named list specifying prep parameter grid, e.g.,
#'   list(sample_rate = c(16000, 22050, 44100), format = c("wav", "mp3"))
#' @param .simulation_store Directory path for storing simulation cache
#' @param .simulation_timestamp Optional timestamp string (auto-generated if NULL)
#' @param .simulation_overwrite Overwrite existing simulation cache
#' @param .verbose Show progress
#' @return extended_segment_list or list of extended_segment_lists (if simulating)
#' @export
