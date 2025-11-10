quantify_simulate <- function(.what, .using, ...,
                               .simulate = NULL,
                               .prep_function = NULL,
                               .prep_simulate = NULL,
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
  if (is.null(.simulate) && is.null(.prep_simulate)) {
    return(quantify(.what, .using = .using, ...))
  }

  # Validate prep function if prep simulation is requested
  if (!is.null(.prep_simulate) && is.null(.prep_function)) {
    cli::cli_abort(".prep_function must be provided when using .prep_simulate")
  }

  if (!is.null(.prep_function) && !is.function(.prep_function)) {
    cli::cli_abort(".prep_function must be a function")
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

  # Get prep function name if provided
  prep_name <- if (!is.null(.prep_function)) {
    prep_fn_name <- deparse(substitute(.prep_function))
    if (is.character(.prep_function)) {
      .prep_function
    } else {
      prep_fn_name
    }
  } else {
    NULL
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

  # Check cache size if it exists
  if (file.exists(cache_file) && .verbose) {
    tryCatch({
      cache_size_info <- check_cache_size(
        cache_file,
        cache_type = "Simulation",
        warn_threshold = "500 MB",
        max_threshold = "2 GB",
        verbose = TRUE
      )
    }, error = function(e) {
      # Silently ignore errors in cache size checking
      NULL
    })
  }

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
  param_grid <- create_parameter_grid(.simulate %||% list(), .prep_simulate)
  n_combinations <- nrow(param_grid)

  dsp_param_names <- attr(param_grid, "dsp_params")
  prep_param_names <- attr(param_grid, "prep_params")

  if (.verbose) {
    cli::cli_alert_info(
      "Simulating {n_combinations} parameter combination{?s} on {nrow(.what)} segment{?s}"
    )
    if (length(dsp_param_names) > 0) {
      cli::cli_alert_info(
        "DSP parameters: {paste(dsp_param_names, collapse = ', ')}"
      )
    }
    if (length(prep_param_names) > 0) {
      cli::cli_alert_info(
        "Prep parameters: {paste(prep_param_names, collapse = ', ')}"
      )
    }
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
      (timestamp, dsp_function, prep_function, created_at, corpus_path, corpus_uuid,
       n_segments, n_parameter_combinations, parameter_names, prep_parameter_names)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      .simulation_timestamp,
      dsp_name,
      prep_name,
      as.character(Sys.time()),
      corpus_obj@basePath,
      corpus_obj@config$UUID,
      nrow(.what),
      n_combinations,
      jsonlite::toJSON(dsp_param_names),
      jsonlite::toJSON(prep_param_names %||% character(0))
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

    # Separate DSP and prep parameters
    current_dsp_params <- if (length(dsp_param_names) > 0) {
      current_params[dsp_param_names]
    } else {
      list()
    }

    current_prep_params <- if (length(prep_param_names) > 0) {
      current_params[prep_param_names]
    } else {
      list()
    }

    # Merge DSP params with base parameters (simulation params override)
    all_dsp_params <- modifyList(base_params, current_dsp_params)

    # Store parameter combination
    param_id <- DBI::dbGetQuery(con, "
      SELECT param_id FROM parameter_combinations WHERE param_hash = ?",
      params = list(param_hash)
    )

    if (nrow(param_id) == 0) {
      DBI::dbExecute(con, "
        INSERT INTO parameter_combinations (param_hash, params_json, prep_params_json)
        VALUES (?, ?, ?)",
        params = list(
          param_hash,
          jsonlite::toJSON(current_dsp_params, auto_unbox = TRUE),
          jsonlite::toJSON(current_prep_params, auto_unbox = TRUE)
        )
      )
      param_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() as param_id")
    }

    param_id <- param_id$param_id[1]

    # Apply preprocessing if prep function is provided
    # For quantify, preprocessing happens at the segment/file level
    # so we pass it as a parameter to quantify which will handle it
    if (!is.null(.prep_function)) {
      # Create a wrapper DSP function that applies preprocessing first
      wrapped_dsp <- function(listOfFiles, ...) {
        # Apply prep function with current prep params
        prep_args <- c(list(listOfFiles = listOfFiles), current_prep_params)
        processed_audio <- do.call(.prep_function, prep_args)

        # Now apply DSP function to preprocessed audio
        # The DSP function should accept the processed audio data
        .using(processed_audio, ...)
      }

      result <- do.call(quantify, c(
        list(.what = .what, .using = wrapped_dsp),
        all_dsp_params
      ))
    } else {
      # No preprocessing, use DSP function directly
      result <- do.call(quantify, c(
        list(.what = .what, .using = .using),
        all_dsp_params
      ))
    }
    
    # Store results in cache
    for (seg_idx in seq_len(nrow(result))) {
      
      # Serialize result for this segment using qs for better performance
      result_blob <- qs::qserialize(result[seg_idx], preset = "fast")
      
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
    dsp_function = dsp_name,
    prep_function = prep_name,
    dsp_params = dsp_param_names,
    prep_params = prep_param_names
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
enrich_simulate <- function(corpus_obj, .using, ...,
                            .simulate = NULL,
                            .prep_function = NULL,
                            .prep_simulate = NULL,
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
  if (is.null(.simulate) && is.null(.prep_simulate)) {
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

  # Validate prep function if prep simulation is requested
  if (!is.null(.prep_simulate) && is.null(.prep_function)) {
    cli::cli_abort(".prep_function must be provided when using .prep_simulate")
  }

  if (!is.null(.prep_function) && !is.function(.prep_function)) {
    cli::cli_abort(".prep_function must be a function")
  }

  # Simulation mode
  if (is.null(.simulation_store)) {
    cli::cli_abort(
      ".simulation_store directory must be specified when using .simulate or .prep_simulate"
    )
  }
  
  # Validate corpus
  if (!S7::S7_inherits(corpus_obj, reindeer::corpus)) {
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

  # Get prep function name if provided
  prep_name <- if (!is.null(.prep_function)) {
    prep_fn_name <- deparse(substitute(.prep_function))
    if (is.character(.prep_function)) {
      .prep_function
    } else {
      prep_fn_name
    }
  } else {
    NULL
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
  param_grid <- create_parameter_grid(.simulate %||% list(), .prep_simulate)
  n_combinations <- nrow(param_grid)

  dsp_param_names <- attr(param_grid, "dsp_params")
  prep_param_names <- attr(param_grid, "prep_params")

  if (.verbose) {
    cli::cli_alert_info(
      "Simulating {n_combinations} parameter combination{?s} on {nrow(signal_files)} signal file{?s}"
    )
    if (length(dsp_param_names) > 0) {
      cli::cli_alert_info(
        "DSP parameters: {paste(dsp_param_names, collapse = ', ')}"
      )
    }
    if (length(prep_param_names) > 0) {
      cli::cli_alert_info(
        "Prep parameters: {paste(prep_param_names, collapse = ', ')}"
      )
    }
  }
  
  # Open cache connection
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Store simulation metadata
  DBI::dbExecute(con, "
    INSERT INTO simulation_metadata
      (timestamp, dsp_function, prep_function, created_at, corpus_path, corpus_uuid,
       n_signal_files, n_parameter_combinations, parameter_names, prep_parameter_names)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      .simulation_timestamp,
      dsp_name,
      prep_name,
      as.character(Sys.time()),
      corpus_obj@basePath,
      corpus_obj@config$UUID,
      nrow(signal_files),
      n_combinations,
      jsonlite::toJSON(dsp_param_names),
      jsonlite::toJSON(prep_param_names %||% character(0))
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

    # Separate DSP and prep parameters
    current_dsp_params <- if (length(dsp_param_names) > 0) {
      current_params[dsp_param_names]
    } else {
      list()
    }

    current_prep_params <- if (length(prep_param_names) > 0) {
      current_params[prep_param_names]
    } else {
      list()
    }

    # Store parameter combination
    param_id <- DBI::dbGetQuery(con, "
      SELECT param_id FROM parameter_combinations WHERE param_hash = ?",
      params = list(param_hash)
    )

    if (nrow(param_id) == 0) {
      DBI::dbExecute(con, "
        INSERT INTO parameter_combinations (param_hash, params_json, prep_params_json)
        VALUES (?, ?, ?)",
        params = list(
          param_hash,
          jsonlite::toJSON(current_dsp_params, auto_unbox = TRUE),
          jsonlite::toJSON(current_prep_params, auto_unbox = TRUE)
        )
      )
      param_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() as param_id")
    }

    param_id <- param_id$param_id[1]

    # Process each signal file with these parameters
    process_file <- function(j, files_meta, dsp_fun, prep_fun, current_dsp_params,
                             current_prep_params, base_params,
                             metadata_fields, param_id, con, cache_file) {

      file_row <- files_meta[j, ]

      # Derive DSP parameters from metadata
      all_dsp_params <- derive_dsp_parameters(
        dsp_fun = dsp_fun,
        metadata = file_row,
        metadata_fields = metadata_fields,
        user_params = modifyList(base_params, current_dsp_params)
      )

      # Apply preprocessing and DSP function in memory (not to file)
      tryCatch({
        # Apply prep function first if provided
        input_for_dsp <- if (!is.null(prep_fun)) {
          # Apply prep function with prep parameters
          prep_args <- c(list(listOfFiles = file_row$full_path), current_prep_params)
          do.call(prep_fun, prep_args)
        } else {
          # No preprocessing, use file path directly
          file_row$full_path
        }

        # Apply DSP function to preprocessed (or original) input
        track <- do.call(dsp_fun, c(
          list(listOfFiles = input_for_dsp),
          all_dsp_params,
          list(toFile = FALSE, verbose = FALSE)
        ))
        
        # Serialize track object using qs for better performance
        track_blob <- qs::qserialize(track, preset = "fast")
        
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
        prep_fun = .prep_function,
        current_dsp_params = current_dsp_params,
        current_prep_params = current_prep_params,
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
        prep_fun = .prep_function,
        current_dsp_params = current_dsp_params,
        current_prep_params = current_prep_params,
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
    prep_function = prep_name,
    dsp_params = dsp_param_names,
    prep_params = prep_param_names,
    corpus = corpus_obj
  )
}

#' Initialize track simulation cache database
#'
#' @keywords internal
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
  dsp_params <- attr(x, "dsp_params")
  prep_params <- attr(x, "prep_params")
  prep_function <- attr(x, "prep_function")

  cat("\n")
  cat("══ Track Simulation Results ══\n")
  cat("\n")
  cat(sprintf("DSP function: %s\n", attr(x, 'dsp_function')))

  if (!is.null(prep_function)) {
    cat(sprintf("Prep function: %s\n", prep_function))
  }

  cat(sprintf("Timestamp: %s\n", attr(x, 'timestamp')))
  cat(sprintf("Cache file: %s\n", attr(x, 'cache_file')))
  cat("\n")
  cat(sprintf("%d parameter combination%s\n", length(x), if (length(x) != 1) "s" else ""))
  cat("\n")

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
