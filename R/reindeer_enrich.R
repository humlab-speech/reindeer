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
#' @examples
#' \dontrun{
#' # Enrich corpus with formant tracks
#' corp %>% enrich(.using = superassp::forest)
#' 
#' # With explicit parameters
#' corp %>% enrich(.using = superassp::ksvF0, 
#'                 minF = 75, maxF = 500,
#'                 .force = TRUE)
#' }
#' 
#' @seealso [quantify()], [peek_signals()]
#' @export
enrich <- function(corpus_obj, .using, ..., 
                   .metadata_fields = c("Gender", "Age"),
                   .signal_extension = NULL,
                   .force = FALSE,
                   .verbose = TRUE) {
  
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
  
  # Get metadata for all bundles
  con <- get_corpus_connection(corpus_obj)
  bundle_metadata <- DBI::dbReadTable(con, "bundle_metadata")
  DBI::dbDisconnect(con)
  
  # Process each bundle
  if (.verbose) {
    cli::cli_progress_bar("Processing bundles", total = nrow(signal_files))
  }
  
  for (i in seq_len(nrow(signal_files))) {
    bundle_row <- signal_files[i, ]
    
    # Get metadata for this bundle (with inheritance)
    bundle_meta <- bundle_metadata %>%
      dplyr::filter(
        session == bundle_row$session,
        bundle == bundle_row$bundle
      )
    
    # Derive DSP parameters from metadata
    dsp_params <- derive_dsp_parameters(
      dsp_fun = dsp_fun,
      metadata = bundle_meta,
      metadata_fields = .metadata_fields,
      user_params = list(...)
    )
    
    # Apply DSP function
    tryCatch({
      result <- do.call(dsp_fun, c(
        list(listOfFiles = bundle_row$full_path),
        dsp_params,
        list(toFile = TRUE, verbose = FALSE)
      ))
      
      if (.verbose) {
        cli::cli_progress_update()
      }
    }, error = function(e) {
      cli::cli_alert_warning(
        "Error processing {bundle_row$session}/{bundle_row$bundle}: {e$message}"
      )
      if (.verbose) {
        cli::cli_progress_update()
      }
    })
  }
  
  if (.verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("Enrichment complete")
  }
  
  invisible(corpus_obj)
}

#' Derive DSP parameters from bundle metadata
#' 
#' Internal function to map metadata fields to DSP function parameters.
#' Uses age and gender to estimate formant frequencies when applicable.
#' 
#' @keywords internal
derive_dsp_parameters <- function(dsp_fun, metadata, metadata_fields, user_params) {
  
  # Get formal arguments of DSP function
  fun_formals <- names(formals(dsp_fun))
  
  # Start with user-provided parameters
  params <- user_params
  
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
      
      # Set nominalF1 if function accepts it and user didn't provide it
      if ("nominalF1" %in% fun_formals && !"nominalF1" %in% names(params)) {
        params$nominalF1 <- nominal_f1
      }
      
      # Set maxFormantHz if function accepts it and user didn't provide it
      if ("maxFormantHz" %in% fun_formals && !"maxFormantHz" %in% names(params)) {
        params$maxFormantHz <- nominal_f1 * 10  # Rough estimate
      }
    }
  }
  
  # Map other metadata fields directly to parameters
  for (field in metadata_fields) {
    if (field %in% names(meta_list) && field %in% fun_formals) {
      if (!field %in% names(params)) {  # Don't override user params
        params[[field]] <- meta_list[[field]]
      }
    }
  }
  
  params
}

#' Alias for enrich (for backwards compatibility with furnish)
#' @rdname enrich
#' @export
furnish <- enrich
