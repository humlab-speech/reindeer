#' Retrieve cached simulation results
#'
#' Reads back quantification results stored by
#' \code{\link{quantify_simulate}} from a simulation cache.
#'
#' @param segment_list A segment_list to look up in the cache
#' @param parameters Named list of DSP parameters identifying the combination
#' @param cache_path Direct path to the simulation SQLite file
#' @param timestamp Timestamp used when creating the simulation
#' @param cache_dir Directory containing simulation caches
#' @param dsp_function Name of the DSP function
#' @return A data.table of cached measurements
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
  result_list <- lapply(results$result_blob, function(blob) {
    tryCatch({
      if (requireNamespace("qs", quietly = TRUE)) {
        qs::qdeserialize(blob)
      } else {
        unserialize(blob)
      }
    }, error = function(e) {
      # Fallback: try alternate deserialization method
      tryCatch(unserialize(blob), error = function(e2) {
        if (requireNamespace("qs", quietly = TRUE)) qs::qdeserialize(blob)
        else cli::cli_abort("Deserialization failed: {conditionMessage(e)}", parent = e)
      })
    })
  })
  
  combined <- data.table::rbindlist(result_list, fill = TRUE)
  
  # Convert to extended_segment_list using S7 constructor when possible,
  # falling back to manual class tagging for compatibility
  tryCatch({
    combined <- extended_segment_list(data = as.data.frame(combined))
  }, error = function(e) {
    # If S7 constructor fails (e.g., missing required columns), use namespaced names
    class(combined) <<- c("reindeer::extended_segment_list", "reindeer::segment_list", class(combined))
  })
  attr(combined, "parameters") <- parameters
  attr(combined, "cache_file") <- cache_path
  
  combined
}

#' List available simulations in cache directory
#'
#' @param cache_dir Directory containing simulation caches
#' @return tibble with simulation metadata
#' @export
list_simulations <- function(cache_dir) {
  
  if (!dir.exists(cache_dir)) {
    cli::cli_alert_warning("Cache directory does not exist: {.path {cache_dir}}")
    return(tibble::tibble())
  }

  cache_files <- list.files(cache_dir, pattern = "\\.sqlite$", full.names = TRUE)

  if (length(cache_files) == 0) {
    cli::cli_alert_info("No simulation caches found in {.path {cache_dir}}")
    return(tibble::tibble())
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
    return(tibble::tibble())
  }

  tibble::as_tibble(data.table::rbindlist(metadata_list, fill = TRUE))
}

# ==============================================================================
# PRINT AND SUMMARY METHODS
# ==============================================================================

#' @export
print.simulation_results <- function(x, ...) {
  param_grid <- attr(x, "parameter_grid")
  dsp_params <- attr(x, "dsp_params")
  prep_params <- attr(x, "prep_params")
  prep_function <- attr(x, "prep_function")

  cli::cli_text("")
  cli::cli_rule("Simulation Results")
  cli::cli_text("")
  cat(sprintf("DSP function: %s\n", attr(x, 'dsp_function')))

  if (!is.null(prep_function)) {
    cat(sprintf("Prep function: %s\n", prep_function))
  }

  cat(sprintf("Timestamp: %s\n", attr(x, 'timestamp')))
  cat(sprintf("Cache file: %s\n", attr(x, 'cache_file')))
  cat("\n")
  cat(sprintf("%d parameter combination%s\n", length(x), if (length(x) != 1) "s" else ""))
  cat("\n")

  # Remove param_hash column for display
  param_display <- param_grid[, setdiff(names(param_grid), "param_hash"), with = FALSE]

  # Show DSP and prep parameters separately if both exist
  if (length(dsp_params) > 0 && length(prep_params) > 0) {
    cat("DSP Parameters:\n")
    dsp_display <- param_display[, ..dsp_params]
    if (nrow(dsp_display) <= 10) {
      print(dsp_display)
    } else {
      print(head(dsp_display, 5))
      cat(sprintf("... (%d more combinations)\n", nrow(dsp_display) - 5))
    }

    cat("\nPrep Parameters:\n")
    prep_display <- param_display[, ..prep_params]
    if (nrow(prep_display) <= 10) {
      print(unique(prep_display))
    } else {
      print(head(unique(prep_display), 5))
      cat(sprintf("... (%d more combinations)\n", nrow(unique(prep_display)) - 5))
    }
  } else {
    # Show all parameters together
    if (nrow(param_display) <= 10) {
      print(param_display)
    } else {
      cat("First 5 parameter combinations:\n")
      print(head(param_display, 5))
      cat("...\n")
      cat(sprintf("(%d more)\n", nrow(param_display) - 5))
    }
  }

  invisible(x)
}

#' @export
summary.simulation_results <- function(object, ...) {
  print(object)
  
  cat("\n")
  cat("\u2550\u2550 Result Summary \u2550\u2550\n")
  
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
#' generation with optional preprocessing of media files. Results are cached in
#' SQLite database for efficient retrieval.
#'
#' Initialize a track simulation cache
#'
#' Creates a new SQLite cache file for track-based simulation results.
#'
#' @param cache_dir Directory for simulation caches
#' @param timestamp Timestamp string for cache file naming
#' @param dsp_function_name Name of the DSP function
#' @return Path to the created cache file
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
      prep_function TEXT,  -- Preprocessing function name (optional)
      created_at TEXT NOT NULL,
      corpus_path TEXT NOT NULL,
      corpus_uuid TEXT NOT NULL,
      n_signal_files INTEGER,
      n_parameter_combinations INTEGER,
      parameter_names TEXT,  -- JSON array of DSP parameter names
      prep_parameter_names TEXT,  -- JSON array of prep parameter names
      computation_time_seconds REAL
    )")
  
  # Parameters table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS parameter_combinations (
      param_id INTEGER PRIMARY KEY AUTOINCREMENT,
      param_hash TEXT UNIQUE NOT NULL,
      params_json TEXT NOT NULL,  -- JSON of DSP parameters
      prep_params_json TEXT  -- JSON of prep parameters (optional)
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
  results$track <- lapply(results$track_blob, function(blob) {
    tryCatch({
      if (requireNamespace("qs", quietly = TRUE)) {
        qs::qdeserialize(blob)
      } else {
        unserialize(blob)
      }
    }, error = function(e) {
      tryCatch(unserialize(blob), error = function(e2) {
        if (requireNamespace("qs", quietly = TRUE)) qs::qdeserialize(blob)
        else cli::cli_abort("Deserialization failed: {conditionMessage(e)}", parent = e)
      })
    })
  })
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
