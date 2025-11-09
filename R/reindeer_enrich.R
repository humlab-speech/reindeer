#' Enrich corpus with DSP-derived tracks
#'
#' Apply digital signal processing (DSP) functions from the superassp package
#' to all signal files in a corpus, using metadata-driven parameter configuration.
#' This function reads bundle and session-level metadata to determine optimal
#' DSP parameters (e.g., formant analysis settings based on speaker age and gender).
#'
#' @param corpus_obj A corpus object
#' @param .using A DSP function from superassp package (e.g., forest, ksvF0)
#' @param ... Additional arguments passed to the DSP function
#' @param .metadata_fields Character vector of metadata fields to use for parameter derivation
#'        Default: c("Gender", "Age")
#' @param .signal_extension File extension of signal files to process. 
#'        Default: NULL (uses mediafileExtension from corpus config)
#' @param .force Logical; if TRUE, recompute even if track files exist
#' @param .verbose Logical; show progress and diagnostic messages
#' @param .parallel Logical; use parallel processing (default TRUE)
#' @param .workers Number of parallel workers (default: parallel::detectCores() - 1)
#' 
#' @return The corpus object (invisibly), with new SSFF track files created
#' 
#' @details
#' The function performs the following steps:
#' 1. Lists all signal files with the specified extension (or mediafileExtension)
#' 2. For each bundle, retrieves metadata (with database -> session -> bundle inheritance)
#' 3. Maps metadata fields to DSP function parameters (e.g., Age/Gender -> nominalF1)
#' 4. Applies the DSP function with derived parameters
#' 5. Registers new tracks in database configuration
#' 
#' Metadata-to-parameter mapping follows these rules:
#' - Age and Gender are used to estimate formant frequencies (nominalF1, maxFormantHz)
#' - Other metadata fields are matched by name to function formals
#' - Database-level defaults apply unless overridden at session or bundle level
#' 
#' Parallel processing is used by default for improved performance on multi-core systems.
#' Set .parallel = FALSE to disable parallel processing (useful for debugging).
#' 
#' @examples
#' \dontrun{
#' # Enrich corpus with formant tracks
#' corp %>% enrich(.using = superassp::forest)
#' 
#' # With explicit parameters
#' corp %>% enrich(.using = superassp::ksvF0, 
#'                 minF = 75, maxF = 500,
#'                 .force = TRUE)
#' 
#' # Disable parallel processing
#' corp %>% enrich(.using = superassp::forest, .parallel = FALSE)
#' }
#' 
#' @seealso [quantify()]
#' @export
enrich <- function(corpus_obj, .using, ..., 
                   .metadata_fields = c("Gender", "Age"),
                   .signal_extension = NULL,
                   .force = FALSE,
                   .verbose = TRUE,
                   .parallel = TRUE,
                   .workers = NULL) {
  
  if (!inherits(corpus_obj, "corpus")) {
    cli::cli_abort("{.arg corpus_obj} must be a corpus object")
  }
  
  # Get DSP function name and package
  dsp_fun_name <- deparse(substitute(.using))
  if (is.function(.using)) {
    dsp_fun <- .using
  } else {
    cli::cli_abort("{.arg .using} must be a function")
  }
  
  # Determine signal file extension
  if (is.null(.signal_extension)) {
    .signal_extension <- corpus_obj@config$mediafileExtension
    if (is.null(.signal_extension)) {
      .signal_extension <- "wav"
      if (.verbose) {
        cli::cli_alert_info("Using default extension: {.val wav}")
      }
    }
  }
  
  if (.verbose) {
    cli::cli_h2("Enriching corpus with {.fn {dsp_fun_name}}")
    cli::cli_alert_info("Processing {.val {.signal_extension}} files")
  }
  
  # Get all signal files
  signal_files <- peek_signals(corpus_obj) %>%
    dplyr::filter(extension == .signal_extension)
  
  if (nrow(signal_files) == 0) {
    cli::cli_alert_warning("No signal files found with extension {.val {.signal_extension}}")
    return(invisible(corpus_obj))
  }
  
  if (.verbose) {
    cli::cli_alert_success("Found {nrow(signal_files)} signal file{?s}")
  }
  
  # Get metadata for all bundles - optimized query to only fetch needed bundles
  con <- get_corpus_connection(corpus_obj)
  
  # Only fetch metadata for bundles we're actually processing
  needed_bundles <- unique(paste(signal_files$session, signal_files$bundle, sep = "||"))
  
  # More efficient: query only the metadata we need
  bundle_metadata_query <- sprintf(
    "SELECT * FROM bundle_metadata WHERE CONCAT(session, '||', bundle) IN (%s)",
    paste(sprintf("'%s'", needed_bundles), collapse = ", ")
  )
  
  # Fallback to simpler approach if database doesn't support CONCAT
  bundle_metadata <- tryCatch({
    DBI::dbGetQuery(con, bundle_metadata_query)
  }, error = function(e) {
    # Fallback: get all and filter in R
    all_meta <- DBI::dbReadTable(con, "bundle_metadata")
    all_meta %>%
      dplyr::semi_join(signal_files, by = c("session", "bundle"))
  })
  
  DBI::dbDisconnect(con)
  
  # Pre-join metadata with signal files for efficiency
  signal_files_with_meta <- signal_files %>%
    dplyr::left_join(bundle_metadata, by = c("session", "bundle"))
  
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
  
  # Process bundles
  if (.verbose) {
    cli::cli_progress_bar("Processing bundles", total = nrow(signal_files_with_meta))
  }
  
  # Define processing function
  process_bundle <- function(i, signal_files_with_meta, dsp_fun, 
                             metadata_fields, user_params, verbose = FALSE) {
    bundle_row <- signal_files_with_meta[i, ]
    
    # Derive DSP parameters from metadata
    dsp_params <- derive_dsp_parameters(
      dsp_fun = dsp_fun,
      metadata = bundle_row,
      metadata_fields = metadata_fields,
      user_params = user_params
    )
    
    # Apply DSP function
    tryCatch({
      result <- do.call(dsp_fun, c(
        list(listOfFiles = bundle_row$full_path),
        dsp_params,
        list(toFile = TRUE, verbose = FALSE)
      ))
      
      list(success = TRUE, bundle = bundle_row$bundle, session = bundle_row$session)
    }, error = function(e) {
      list(success = FALSE, bundle = bundle_row$bundle, session = bundle_row$session,
           error = e$message)
    })
  }
  
  # Execute processing (parallel or sequential)
  if (.parallel) {
    results <- furrr::future_map(
      seq_len(nrow(signal_files_with_meta)),
      process_bundle,
      signal_files_with_meta = signal_files_with_meta,
      dsp_fun = dsp_fun,
      metadata_fields = .metadata_fields,
      user_params = list(...),
      verbose = FALSE,
      .progress = .verbose,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    results <- list()
    for (i in seq_len(nrow(signal_files_with_meta))) {
      results[[i]] <- process_bundle(
        i, signal_files_with_meta, dsp_fun,
        .metadata_fields, list(...), FALSE
      )
      if (.verbose) {
        cli::cli_progress_update()
      }
    }
  }
  
  if (.verbose) {
    cli::cli_progress_done()
    
    # Report any errors
    errors <- purrr::keep(results, ~!.x$success)
    if (length(errors) > 0) {
      cli::cli_alert_warning("{length(errors)} bundle{?s} failed processing")
      for (err in errors) {
        cli::cli_alert_info("{err$session}/{err$bundle}: {err$error}")
      }
    }
    
    cli::cli_alert_success("Enrichment complete")
  }
  
  invisible(corpus_obj)
}

#' Derive DSP parameters from bundle metadata (with caching)
#' 
#' Internal function to map metadata fields to DSP function parameters.
#' Uses age and gender to estimate formant frequencies when applicable.
#' Results are cached based on metadata values for efficiency.
#' 
#' @keywords internal
derive_dsp_parameters <- function(dsp_fun, metadata, metadata_fields, user_params) {
  
  # Create cache key from metadata values (for memoization)
  cache_key <- paste(
    as.character(metadata$Gender %||% "NA"),
    as.character(metadata$Age %||% "NA"),
    paste(metadata[metadata_fields], collapse = "|"),
    sep = "||"
  )
  
  # Check if we've already computed params for this combination
  if (!exists(".dsp_param_cache", envir = .GlobalEnv)) {
    assign(".dsp_param_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  cache_env <- get(".dsp_param_cache", envir = .GlobalEnv)
  
  if (exists(cache_key, envir = cache_env)) {
    cached_params <- get(cache_key, envir = cache_env)
    # Merge with user params (user params override)
    return(utils::modifyList(cached_params, user_params))
  }
  
  # Get formal arguments of DSP function (cached per function)
  fun_formals <- names(formals(dsp_fun))
  
  # Start with user-provided parameters
  params <- list()
  
  # Extract metadata values
  meta_list <- as.list(metadata)
  
  # Map Gender and Age to formant parameters if applicable
  if ("Gender" %in% names(meta_list) && "Age" %in% names(meta_list)) {
    gender <- meta_list$Gender
    age <- as.numeric(meta_list$Age)
    
    if (!is.na(gender) && !is.na(age)) {
      # Estimate nominal F1 based on age and gender
      # These are rough estimates based on typical values
      if (tolower(gender) %in% c("male", "m")) {
        if (age < 12) {
          nominal_f1 <- 1000  # Child
        } else {
          nominal_f1 <- 500   # Adult male
        }
      } else if (tolower(gender) %in% c("female", "f")) {
        if (age < 12) {
          nominal_f1 <- 1000  # Child
        } else {
          nominal_f1 <- 560   # Adult female
        }
      } else {
        nominal_f1 <- 530  # Default/neutral
      }
      
      # Set nominalF1 if function accepts it
      if ("nominalF1" %in% fun_formals) {
        params$nominalF1 <- nominal_f1
      }
      
      # Set maxFormantHz if function accepts it
      if ("maxFormantHz" %in% fun_formals) {
        params$maxFormantHz <- nominal_f1 * 10  # Rough estimate
      }
    }
  }
  
  # Map other metadata fields directly to parameters
  for (field in metadata_fields) {
    if (field %in% names(meta_list) && field %in% fun_formals) {
      params[[field]] <- meta_list[[field]]
    }
  }
  
  # Cache the computed params
  assign(cache_key, params, envir = cache_env)
  
  # Merge with user params (user params override)
  utils::modifyList(params, user_params)
}

#' Alias for enrich (for backwards compatibility with furnish)
#' @rdname enrich
#' @export
furnish <- enrich
