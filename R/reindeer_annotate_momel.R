#' Automatic annotation of intonation using MOMEL and INTSINT labels (Python implementation)
#'
#' This function creates annotation suggestions for utterances using the MOMEL/INTSINT 
#' framework implemented in Python with Parselmouth. It returns a suggestion object
#' that can be assessed and transcribed using the transcription system.
#'
#' The Python implementation eliminates dependencies on external Praat scripts,
#' Perl scripts, and compiled C binaries, providing a pure Python/Parselmouth solution
#' that is more portable and easier to maintain.
#'
#' @param corpus A corpus object created with [corpus()]
#' @param bundle_list A bundle_list object (or segment_list from ask_for/query)
#' @param windowSize The window size (in ms) used when extracting f0
#' @param minF The low end of the fundamental frequency range
#' @param maxF The high end of the fundamental frequency range
#' @param pitchSpan The maximum pitch span to consider (in octaves)
#' @param maximumError The maximum error allowed by MOMEL
#' @param reducWindowSize The size of the reduced analysis window
#' @param minimalDistance The minimal distance between targets (in ms)
#' @param minimalFrequencyRatio The minimal frequency ratio for target reduction
#' @param intsint_level The name of the transcription level for INTSINT labels
#' @param momel_level Optional name for MOMEL target level
#' @param time_step Time step for pitch extraction (in seconds)
#' @param verbose Should progress be displayed?
#'
#' @return An EventSuggestion object for the transcription system
#' @export
#'
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/ae_emuDB")
#' bundles <- corp[".*", ".*"]
#' suggestions <- draft_momel_intsint(corp, bundles)
#' assess(suggestions)
#' prepare(corp, suggestions)
#' log <- transcribe(corp, suggestions)
#' }
draft_momel_intsint <- function(corpus,
                               bundle_list,
                               windowSize = 30,
                               minF = 60,
                               maxF = 750,
                               pitchSpan = 1.5,
                               maximumError = 1.04,
                               reducWindowSize = 20,
                               minimalDistance = 20,
                               minimalFrequencyRatio = 0.05,
                               intsint_level = "Intsint",
                               momel_level = NULL,
                               time_step = 0.01,
                               .force_overwrite = FALSE,
                               verbose = TRUE) {
  
  # Store parameters for caching
  draft_params <- list(
    windowSize = windowSize,
    minF = minF,
    maxF = maxF,
    pitchSpan = pitchSpan,
    maximumError = maximumError,
    reducWindowSize = reducWindowSize,
    minimalDistance = minimalDistance,
    minimalFrequencyRatio = minimalFrequencyRatio,
    intsint_level = intsint_level,
    momel_level = momel_level,
    time_step = time_step
  )

  # Get or create cache
  cache_info <- get_draft_cache(
    corpus,
    "draft_momel_intsint",
    draft_params,
    force_overwrite = .force_overwrite,
    verbose = verbose
  )

  con <- cache_info$con
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Ensure Python module is available
  if (!reticulate::py_module_available("parselmouth")) {
    stop("Parselmouth not available. Install with: pip install praat-parselmouth")
  }

  # Import Python module
  momel_intsint <- reticulate::import_from_path(
    "momel_intsint",
    path = system.file("python", package = "reindeer", mustWork = TRUE)
  )

  if (verbose) {
    if (!cache_info$is_new) {
      cli::cli_alert_info(
        "Resuming from cache: {cache_info$n_completed} bundle{?s} already completed"
      )
    }
    cli::cli_progress_step("Processing {nrow(bundle_list)} bundle{?s}")
  }
  
  # Get signal files for the bundles
  signal_files <- signal_files(corpus)
  
  # Join with bundle list
  bundles_to_process <- bundle_list %>%
    dplyr::left_join(signal_files, by = c("session", "bundle"))
  
  # Check for media file extension
  media_ext <- corpus$config$mediafileExtension
  bundles_with_audio <- bundles_to_process %>%
    dplyr::filter(extension == media_ext)
  
  if (nrow(bundles_with_audio) == 0) {
    stop("No audio files found with extension '", media_ext, "'")
  }
  
  # Update total bundles in cache
  DBI::dbExecute(con, "
    UPDATE draft_metadata
    SET n_bundles_total = ?
    WHERE id = 1",
    params = list(nrow(bundles_with_audio))
  )

  # Process each bundle
  all_annotations <- list()

  if (verbose) {
    n_to_process <- sum(!sapply(seq_len(nrow(bundles_with_audio)), function(i) {
      bundle_info <- bundles_with_audio[i, ]
      is_bundle_cached(con, bundle_info$session, bundle_info$bundle, intsint_level, intsint_level)
    }))

    cli::cli_progress_bar(
      "Processing bundles ({n_to_process} new)",
      total = nrow(bundles_with_audio)
    )
  }

  for (i in seq_len(nrow(bundles_with_audio))) {
    bundle_info <- bundles_with_audio[i, ]
    sound_file <- bundle_info$full_path

    # Check if already cached
    if (is_bundle_cached(con, bundle_info$session, bundle_info$bundle, intsint_level, intsint_level)) {
      if (verbose) {
        cli::cli_progress_update()
      }
      next
    }

    tryCatch({
      # Process with Python
      result <- momel_intsint$process_momel_intsint(
        sound_file = sound_file,
        window_length = as.integer(windowSize),
        min_f0 = as.numeric(minF),
        max_f0 = as.numeric(maxF),
        pitch_span = as.numeric(pitchSpan),
        max_error = as.numeric(maximumError),
        reduced_window_length = as.integer(reducWindowSize),
        minimal_distance = as.integer(minimalDistance),
        minimal_frequency_ratio = as.numeric(minimalFrequencyRatio),
        time_step = as.numeric(time_step)
      )
      
      # Extract results
      intsint_targets <- result[[1]]
      range_val <- result[[2]]
      key_val <- result[[3]]
      
      # Convert Python list to R data frame
      if (length(intsint_targets) > 0) {
        annotations <- data.frame(
          session = bundle_info$session,
          bundle = bundle_info$bundle,
          level = intsint_level,
          attribute = intsint_level,
          start = sapply(intsint_targets, function(t) t$time * 1000),  # Convert to ms
          labels = sapply(intsint_targets, function(t) t$tone),
          momel_target = sapply(intsint_targets, function(t) t$target),
          intsint_estimate = sapply(intsint_targets, function(t) t$estimate),
          range = range_val,
          key = key_val,
          stringsAsFactors = FALSE
        )

        # Store in cache
        store_draft_annotations(
          con,
          session = bundle_info$session,
          bundle = bundle_info$bundle,
          level_name = intsint_level,
          level_type = "EVENT",
          attribute_name = intsint_level,
          annotations = annotations,
          parameters = draft_params,
          error_occurred = FALSE
        )

        all_annotations[[i]] <- annotations
      }

    }, error = function(e) {
      # Store error in cache
      store_draft_annotations(
        con,
        session = bundle_info$session,
        bundle = bundle_info$bundle,
        level_name = intsint_level,
        level_type = "EVENT",
        attribute_name = intsint_level,
        annotations = NULL,
        parameters = draft_params,
        error_occurred = TRUE,
        error_message = e$message
      )

      if (verbose) {
        cli::cli_alert_warning(
          "Failed to process {bundle_info$session}/{bundle_info$bundle}: {e$message}"
        )
      }
    })

    if (verbose) {
      cli::cli_progress_update()
    }
  }
  
  if (verbose) {
    cli::cli_progress_done()
  }

  # Mark cache as completed
  mark_draft_completed(con)

  # Retrieve all successful annotations from cache (includes previously cached + newly generated)
  cached_results <- retrieve_draft_annotations(con, level_name = intsint_level)

  if (nrow(cached_results) == 0 || all(cached_results$error_occurred)) {
    stop("No annotations were generated successfully")
  }

  # Filter successful results and extract annotations
  successful_results <- cached_results[!cached_results$error_occurred, ]
  all_annotations <- successful_results$annotations

  # Combine all annotations
  suggestions_df <- dplyr::bind_rows(all_annotations)
  
  # Create EventSuggestion object
  suggestion <- EventSuggestion(
    annotations = suggestions_df %>%
      dplyr::select(session, bundle, level, attribute, start, labels),
    level_name = intsint_level,
    level_type = "EVENT",
    attribute_definitions = c(intsint_level),
    legal_labels = c("T", "M", "B", "H", "U", "S", "D", "L"),
    label_groups = list(
      Absolute_tones = c("T", "M", "B"),
      Relative_tones = c("H", "U", "S", "D", "L")
    ),
    metadata = list(
      method = "MOMEL-INTSINT",
      implementation = "Python/Parselmouth",
      parameters = list(
        windowSize = windowSize,
        minF = minF,
        maxF = maxF,
        pitchSpan = pitchSpan,
        maximumError = maximumError,
        reducWindowSize = reducWindowSize,
        minimalDistance = minimalDistance,
        minimalFrequencyRatio = minimalFrequencyRatio,
        time_step = time_step
      ),
      additional_data = suggestions_df %>%
        dplyr::select(session, bundle, start, momel_target, 
                     intsint_estimate, range, key)
    )
  )
  
  if (verbose) {
    cli::cli_alert_success(
      "Generated {nrow(suggestions_df)} INTSINT annotation{?s}"
    )
  }
  
  return(suggestion)
}


#' @rdname draft_momel_intsint
#' @export
annotate_momel_intsint <- function(corpus,
                                   bundle_list,
                                   ...,
                                   assess_first = TRUE,
                                   verbose = TRUE) {
  
  # Draft suggestions
  suggestions <- draft_momel_intsint(corpus, bundle_list, ..., verbose = verbose)
  
  # Assess
  if (assess_first) {
    assessment <- assess(suggestions, verbose = verbose)
    
    if (!assessment$passed) {
      stop("Assessment failed. Please review issues before transcribing.")
    }
  }
  
  # Prepare (create level if needed)
  prepare(corpus, suggestions, verbose = verbose)
  
  # Transcribe
  log <- transcribe(corpus, suggestions, verbose = verbose)
  
  if (verbose) {
    cli::cli_alert_success("MOMEL-INTSINT transcription complete")
  }
  
  return(log)
}
