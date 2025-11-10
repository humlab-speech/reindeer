# ==============================================================================
# REINDEER ANNOTATION FUNCTIONS - PYTHON/TRANSCRIPTION SYSTEM INTEGRATION
# ==============================================================================
#
# This file provides automatic annotation functions that:
# 1. Use Python/Parselmouth instead of external Praat calls
# 2. Return Suggestion objects (don't modify database directly)
# 3. Work with the reindeer transcription system (draft/assess/prepare/transcribe)
# 4. Are thread-safe for parallel processing
#

#' @import reticulate
#' @import dplyr
#' @import cli

# Initialize Python environment for annotations
.annotation_py_env <- NULL

#' Initialize Python environment for annotations
#' @keywords internal
init_annotation_python <- function() {
  if (is.null(.annotation_py_env)) {
    # Source the Python annotation wrappers
    python_script <- system.file("python", "annotation_wrappers.py", 
                                 package = "reindeer", mustWork = TRUE)
    
    .annotation_py_env <<- reticulate::import_from_path(
      "annotation_wrappers",
      path = dirname(python_script)
    )
    
    cli::cli_alert_success("Initialized Python annotation environment")
  }
  
  invisible(.annotation_py_env)
}


# ==============================================================================
# DRAFT: PERIOD ANNOTATION
# ==============================================================================

#' Draft period annotations using Python/Parselmouth
#'
#' Creates EVENT-level transcription suggestions for glottal periods detected
#' from acoustic signal. Uses Parselmouth (Python) for processing, replacing
#' external Praat calls.
#'
#' This function returns a `SuggestedEvents` object that can be assessed,
#' corrected, and transcribed using the reindeer transcription system.
#'
#' @param corpus A corpus object
#' @param bundles A bundle_list or data.frame with session and bundle columns
#' @param level_name Name of the annotation level to create (default: "Periods")
#' @param minimum_f0 Minimum F0 for period detection (Hz)
#' @param maximum_f0 Maximum F0 for period detection (Hz)
#' @param window_shape Window shape for signal extraction
#' @param interpolation Intensity interpolation method
#' @param relative_width Relative width of extraction window
#' @param parallel Use parallel processing (default: FALSE)
#' @param n_cores Number of cores for parallel processing
#'
#' @return A `SuggestedEvents` object containing period annotations
#'
#' @export
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/ae_emuDB")
#' bundles <- corp[".*", ".*"]  # All bundles
#' 
#' # Draft period annotations
#' suggestions <- draft_periods(corp, bundles)
#' 
#' # Assess and transcribe
#' assessment <- assess(suggestions)
#' print(assessment)
#' 
#' prepare(suggestions)
#' log <- transcribe(suggestions)
#' }
draft_periods <- function(
    corpus,
    bundles,
    level_name = "Periods",
    minimum_f0 = 75.0,
    maximum_f0 = 600.0,
    window_shape = "Gaussian1",
    interpolation = "cubic",
    relative_width = 1.0,
    parallel = FALSE,
    n_cores = parallel::detectCores() - 1
) {
  
  # Initialize Python
  py_env <- init_annotation_python()
  
  # Get signal files for bundles
  signal_files <- signal_files(corpus) %>%
    dplyr::semi_join(bundles, by = c("session", "bundle")) %>%
    dplyr::filter(extension == corpus@config$mediafileExtension)
  
  if (nrow(signal_files) == 0) {
    cli::cli_abort("No signal files found for specified bundles")
  }
  
  cli::cli_alert_info("Drafting period annotations for {nrow(signal_files)} bundle(s)...")
  
  # Call Python batch processing
  results_df <- py_env$periods_batch(
    sound_files = signal_files$full_path,
    session_names = signal_files$session,
    bundle_names = signal_files$bundle,
    minimum_f0 = minimum_f0,
    maximum_f0 = maximum_f0,
    window_shape = window_shape,
    interpolation = interpolation,
    relative_width = relative_width
  )
  
  # Convert to R data frame
  suggestions_df <- as.data.frame(results_df)
  
  # Create SuggestedEvents objects for each bundle
  suggestions_list <- list()
  
  for (i in seq_len(nrow(bundles))) {
    sess <- bundles$session[i]
    bund <- bundles$bundle[i]
    
    bundle_suggestions <- suggestions_df %>%
      dplyr::filter(session == sess, bundle == bund) %>%
      dplyr::select(start_time, end_time, label)
    
    if (nrow(bundle_suggestions) > 0) {
      suggestions_list[[length(suggestions_list) + 1]] <- SuggestedEvents(
        corpus = corpus,
        session = sess,
        bundle = bund,
        level_name = level_name,
        suggestions = bundle_suggestions,
        event_categories = unique(bundle_suggestions$label[bundle_suggestions$label != ""])
      )
    }
  }
  
  cli::cli_alert_success("Created {length(suggestions_list)} period annotation suggestion(s)")
  
  # Return list of suggestions (or single if only one bundle)
  if (length(suggestions_list) == 1) {
    return(suggestions_list[[1]])
  } else {
    return(suggestions_list)
  }
}


# ==============================================================================
# DRAFT: INTSINT/MOMEL ANNOTATION
# ==============================================================================

#' Draft INTSINT/MOMEL intonation annotations
#'
#' Creates EVENT-level transcription suggestions for INTSINT tones using the
#' MOMEL/INTSINT framework. Uses Python/Parselmouth implementation.
#'
#' @param corpus A corpus object
#' @param bundles A bundle_list or data.frame with session and bundle columns
#' @param level_name Name of the annotation level (default: "Intsint")
#' @param window_length Analysis window length (ms)
#' @param minimum_f0 Minimum F0 (Hz)
#' @param maximum_f0 Maximum F0 (Hz)
#' @param pitch_span Pitch span in octaves (1.5 = normal, 2.5 = expressive)
#' @param maximum_error Maximum error for MOMEL
#' @param reduced_window_length Reduced analysis window (ms)
#' @param minimal_distance Minimal distance between targets (ms)
#' @param minimal_frequency_ratio Minimal frequency ratio
#'
#' @return A `SuggestedEvents` object or list of objects
#' 
#' @export
draft_intsint_momel <- function(
    corpus,
    bundles,
    level_name = "Intsint",
    window_length = 30,
    minimum_f0 = 60,
    maximum_f0 = 750,
    pitch_span = 1.5,
    maximum_error = 1.04,
    reduced_window_length = 20,
    minimal_distance = 20,
    minimal_frequency_ratio = 0.05
) {
  
  # Initialize Python
  py_env <- init_annotation_python()
  
  # Get signal files
  signal_files <- signal_files(corpus) %>%
    dplyr::semi_join(bundles, by = c("session", "bundle")) %>%
    dplyr::filter(extension == corpus@config$mediafileExtension)
  
  if (nrow(signal_files) == 0) {
    cli::cli_abort("No signal files found for specified bundles")
  }
  
  cli::cli_alert_info("Drafting INTSINT/MOMEL annotations for {nrow(signal_files)} bundle(s)...")
  
  # Call Python batch processing
  results_df <- py_env$intsint_momel_batch(
    sound_files = signal_files$full_path,
    session_names = signal_files$session,
    bundle_names = signal_files$bundle,
    window_length = as.integer(window_length),
    minimum_f0 = as.integer(minimum_f0),
    maximum_f0 = as.integer(maximum_f0),
    pitch_span = pitch_span,
    maximum_error = maximum_error,
    reduced_window_length = as.integer(reduced_window_length),
    minimal_distance = as.integer(minimal_distance),
    minimal_frequency_ratio = minimal_frequency_ratio
  )
  
  # Convert to R data frame
  suggestions_df <- as.data.frame(results_df)
  
  # Define INTSINT categories
  absolute_tones <- c("T", "M", "B")
  relative_tones <- c("H", "U", "S", "D", "L")
  all_tones <- c(absolute_tones, relative_tones)
  
  # Create suggestions for each bundle
  suggestions_list <- list()
  
  for (i in seq_len(nrow(bundles))) {
    sess <- bundles$session[i]
    bund <- bundles$bundle[i]
    
    bundle_suggestions <- suggestions_df %>%
      dplyr::filter(session == sess, bundle == bund) %>%
      dplyr::select(start_time, end_time, label)
    
    if (nrow(bundle_suggestions) > 0) {
      suggestions_list[[length(suggestions_list) + 1]] <- SuggestedEvents(
        corpus = corpus,
        session = sess,
        bundle = bund,
        level_name = level_name,
        suggestions = bundle_suggestions,
        event_categories = all_tones
      )
    }
  }
  
  cli::cli_alert_success("Created {length(suggestions_list)} INTSINT annotation suggestion(s)")
  
  # Return list or single suggestion
  if (length(suggestions_list) == 1) {
    return(suggestions_list[[1]])
  } else {
    return(suggestions_list)
  }
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Check if Python annotation environment is available
#' @export
has_python_annotations <- function() {
  tryCatch({
    init_annotation_python()
    TRUE
  }, error = function(e) {
    FALSE
  })
}


#' Test Python annotation functions
#' @param test_file Path to test audio file
#' @export
test_python_annotations <- function(test_file) {
  if (!file.exists(test_file)) {
    cli::cli_abort("Test file not found: {test_file}")
  }
  
  py_env <- init_annotation_python()
  
  cli::cli_h2("Testing Period Annotation")
  periods <- py_env$annotate_periods_single(test_file)
  print(head(periods))
  
  cli::cli_h2("Testing INTSINT/MOMEL Annotation")
  tryCatch({
    intsint <- py_env$annotate_intsint_momel_single(test_file)
    print(head(intsint))
  }, error = function(e) {
    cli::cli_alert_warning("INTSINT/MOMEL test failed: {conditionMessage(e)}")
  })
  
  invisible(list(periods = periods))
}


# ==============================================================================
# PARALLEL PROCESSING NOTES
# ==============================================================================

# Thread Safety:
# - Python functions process each file independently
# - No shared state between Parselmouth objects  
# - Safe for use with parallel::mclapply or future::future_map
# - Each worker gets its own Python interpreter session via reticulate
#
# Recommended usage:
#   future::plan("multisession", workers = 4)
#   results <- furrr::future_map(bundles, ~draft_periods(corpus, .x))
