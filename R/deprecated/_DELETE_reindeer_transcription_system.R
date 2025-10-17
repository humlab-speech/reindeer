# ============================================================================
# DEPRECATED FILE - MARKED FOR DELETION
# ============================================================================
# 
# This file is no longer used by the reindeer package and will be removed.
# See R/deprecated/README.md for replacement files.
#
# DO NOT USE FUNCTIONS FROM THIS FILE
# 
# Date marked: 2025-10-16
# ============================================================================

# ==============================================================================
# COMPREHENSIVE TRANSCRIPTION SYSTEM FOR REINDEER
# ==============================================================================
#
# This module provides a complete workflow for automatic transcription:
#   - draft(): Create transcription suggestions from automatic annotation
#   - assess(): Validate suggestions against database constraints
#   - correct(): Manually adjust specific suggestions
#   - prepare(): Create/update levels and labeltypes as needed
#   - transcribe(): Apply suggestions to database with logging
#   - TranscriptionLog: Track and reverse changes
#

library(S7)
library(cli)
library(DBI)
library(dplyr)
library(tidyr)

# ==============================================================================
# BASE CLASSES
# ==============================================================================

#' Suggestion - Base class for transcription suggestions
#' 
#' @export
Suggestion <- S7::new_class(
  "Suggestion",
  properties = list(
    corpus = S7::class_any,  # corpus object
    session = S7::class_character,
    bundle = S7::class_character,
    level_name = S7::class_character,
    level_type = S7::class_character,  # "SEGMENT", "EVENT", or "ITEM"
    attribute_name = S7::class_character,
    suggestions = S7::class_data.frame,
    min_duration = S7::class_numeric,
    remove_empty = S7::class_logical,
    assessed = S7::class_logical,
    assessment_results = S7::class_any
  ),
  constructor = function(corpus, session, bundle, level_name, level_type,
                        attribute_name = level_name, suggestions,
                        min_duration = 0.01, remove_empty = TRUE) {
    
    # Validate inputs
    if (!S7::S7_inherits(corpus) || !grepl("corpus", class(corpus)[1])) {
      cli::cli_abort("corpus must be a corpus object")
    }
    
    if (!level_type %in% c("SEGMENT", "EVENT", "ITEM")) {
      cli::cli_abort("level_type must be one of: SEGMENT, EVENT, ITEM")
    }
    
    # Ensure required columns
    required_cols <- c("start_time", "end_time", "label")
    missing_cols <- setdiff(required_cols, names(suggestions))
    if (length(missing_cols) > 0) {
      cli::cli_abort("suggestions missing required columns: {missing_cols}")
    }
    
    # Sort by start time
    suggestions <- suggestions[order(suggestions$start_time), ]
    
    S7::new_object(
      S7::S7_object(),
      corpus = corpus,
      session = session,
      bundle = bundle,
      level_name = level_name,
      level_type = level_type,
      attribute_name = attribute_name,
      suggestions = suggestions,
      min_duration = min_duration,
      remove_empty = remove_empty,
      assessed = FALSE,
      assessment_results = NULL
    )
  }
)

#' SuggestedSegments - Segment-level transcription suggestions
#' @export
SuggestedSegments <- S7::new_class(
  "SuggestedSegments",
  parent = Suggestion,
  properties = list(
    phonetic_alphabet = S7::class_character
  ),
  constructor = function(corpus, session, bundle, level_name, suggestions,
                        attribute_name = level_name, phonetic_alphabet = "IPA",
                        min_duration = 0.01, remove_empty = TRUE) {
    
    # Add duration if not present
    if (!"duration" %in% names(suggestions)) {
      suggestions$duration <- suggestions$end_time - suggestions$start_time
    }
    
    obj <- Suggestion(corpus, session, bundle, level_name, "SEGMENT",
                     attribute_name, suggestions, min_duration, remove_empty)
    
    S7::new_object(obj, phonetic_alphabet = phonetic_alphabet)
  }
)

#' SuggestedEvents - Event-level transcription suggestions
#' @export
SuggestedEvents <- S7::new_class(
  "SuggestedEvents",
  parent = Suggestion,
  properties = list(
    event_categories = S7::class_character
  ),
  constructor = function(corpus, session, bundle, level_name, suggestions,
                        attribute_name = level_name, event_categories = character(0),
                        min_duration = 0, remove_empty = TRUE) {
    
    obj <- Suggestion(corpus, session, bundle, level_name, "EVENT",
                     attribute_name, suggestions, min_duration, remove_empty)
    
    S7::new_object(obj, event_categories = event_categories)
  }
)

#' SuggestedItems - Item-level transcription suggestions
#' @export
SuggestedItems <- S7::new_class(
  "SuggestedItems",
  parent = Suggestion,
  properties = list(
    confidence_scores = S7::class_numeric
  ),
  constructor = function(corpus, session, bundle, level_name, suggestions,
                        attribute_name = level_name, confidence_scores = numeric(0),
                        min_duration = 0.05, remove_empty = TRUE) {
    
    # Add confidence if provided
    if (length(confidence_scores) > 0 && length(confidence_scores) == nrow(suggestions)) {
      suggestions$confidence <- confidence_scores
    }
    
    obj <- Suggestion(corpus, session, bundle, level_name, "ITEM",
                     attribute_name, suggestions, min_duration, remove_empty)
    
    S7::new_object(obj, confidence_scores = confidence_scores)
  }
)

# ==============================================================================
# TRANSCRIPTION LOG CLASS
# ==============================================================================

#' TranscriptionLog - Tracks changes made by transcription
#' 
#' Stores information about transcription operations to enable rollback
#' 
#' @export
TranscriptionLog <- S7::new_class(
  "TranscriptionLog",
  properties = list(
    corpus = S7::class_any,
    session = S7::class_character,
    bundle = S7::class_character,
    level_name = S7::class_character,
    timestamp = S7::class_any,
    operation = S7::class_character,  # "create_level", "add_items", etc.
    items_added = S7::class_data.frame,
    items_removed = S7::class_data.frame,
    n_items_added = S7::class_integer,
    n_items_modified = S7::class_integer,
    n_items_removed = S7::class_integer,
    level_created = S7::class_logical,
    attribute_created = S7::class_logical,
    backup_path = S7::class_character,
    success = S7::class_logical,
    error_message = S7::class_character
  ),
  constructor = function(corpus, session, bundle, level_name, operation = "transcribe") {
    S7::new_object(
      S7::S7_object(),
      corpus = corpus,
      session = session,
      bundle = bundle,
      level_name = level_name,
      timestamp = Sys.time(),
      operation = operation,
      items_added = data.frame(),
      items_removed = data.frame(),
      n_items_added = 0L,
      n_items_modified = 0L,
      n_items_removed = 0L,
      level_created = FALSE,
      attribute_created = FALSE,
      backup_path = character(0),
      success = FALSE,
      error_message = character(0)
    )
  }
)

# ==============================================================================
# DRAFT METHOD
# ==============================================================================

#' Draft - Create transcription suggestions using automatic annotation
#'
#' @param corpus A corpus object
#' @param annotation_func A function that performs automatic annotation
#' @param session Session name (or regex pattern)
#' @param bundle Bundle name (or regex pattern)
#' @param level_name Target level name
#' @param level_type Level type: "SEGMENT", "EVENT", or "ITEM"
#' @param ... Additional arguments passed to annotation_func
#' 
#' @returns A Suggestion object (or subclass)
#' @export
draft <- S7::new_generic("draft", "corpus")

S7::method(draft, S7::class_any) <- function(corpus, annotation_func, session, bundle,
                                             level_name, level_type = "SEGMENT", ...) {
  
  if (!inherits(corpus, "corpus")) {
    cli::cli_abort("{.arg corpus} must be a corpus object")
  }
  
  if (!is.function(annotation_func)) {
    cli::cli_abort("{.arg annotation_func} must be a function")
  }
  
  # Get bundle list matching patterns
  bundles <- corpus[session, bundle]
  
  if (nrow(bundles) == 0) {
    cli::cli_warn("No bundles match the specified session/bundle patterns")
    return(NULL)
  }
  
  if (nrow(bundles) > 1) {
    cli::cli_warn("Multiple bundles matched; only processing first one")
    bundles <- bundles[1, ]
  }
  
  session_name <- bundles$session_name[1]
  bundle_name <- bundles$bundle_name[1]
  
  cli::cli_h2("Drafting transcription suggestions")
  cli::cli_alert_info("Session: {session_name}, Bundle: {bundle_name}")
  cli::cli_alert_info("Target level: {level_name} ({level_type})")
  
  # Run annotation function
  cli::cli_alert_info("Running annotation function...")
  suggestions_df <- tryCatch({
    annotation_func(corpus, session_name, bundle_name, ...)
  }, error = function(e) {
    cli::cli_abort("Annotation function failed: {e$message}")
  })
  
  # Validate output
  if (!is.data.frame(suggestions_df)) {
    cli::cli_abort("Annotation function must return a data.frame")
  }
  
  required_cols <- c("start_time", "end_time", "label")
  missing <- setdiff(required_cols, names(suggestions_df))
  if (length(missing) > 0) {
    cli::cli_abort("Annotation output missing required columns: {missing}")
  }
  
  # Create appropriate suggestion class
  suggestion <- switch(level_type,
    "SEGMENT" = SuggestedSegments(corpus, session_name, bundle_name, 
                                  level_name, suggestions_df, ...),
    "EVENT" = SuggestedEvents(corpus, session_name, bundle_name,
                             level_name, suggestions_df, ...),
    "ITEM" = SuggestedItems(corpus, session_name, bundle_name,
                           level_name, suggestions_df, ...),
    cli::cli_abort("Unknown level_type: {level_type}")
  )
  
  cli::cli_alert_success("Created {nrow(suggestions_df)} suggestion{?s}")
  
  suggestion
}

# ==============================================================================
# ASSESS METHOD
# ==============================================================================

#' Assess - Validate transcription suggestions
#'
#' Performs sanity checks on suggestions:
#' - Level/attribute existence
#' - Overlap detection
#' - Duration validation
#' - Bundle timing constraints
#' 
#' @param suggestion A Suggestion object
#' @param verbose Show detailed assessment information
#' 
#' @returns Updated suggestion object with assessment results
#' @export
assess <- S7::new_generic("assess", "suggestion")

S7::method(assess, Suggestion) <- function(suggestion, verbose = TRUE) {
  
  if (verbose) {
    cli::cli_h2("Assessing transcription suggestions")
  }
  
  errors <- character(0)
  warnings <- character(0)
  info <- list()
  
  # Get database connection
  con <- get_corpus_connection(suggestion@corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Check bundle exists
  bundle_query <- sprintf(
    "SELECT * FROM bundle WHERE db_uuid = '%s' AND session = '%s' AND name = '%s'",
    suggestion@corpus@.uuid, suggestion@session, suggestion@bundle
  )
  bundle_info <- DBI::dbGetQuery(con, bundle_query)
  
  if (nrow(bundle_info) == 0) {
    errors <- c(errors, sprintf("Bundle '%s' in session '%s' does not exist",
                               suggestion@bundle, suggestion@session))
    suggestion@assessment_results <- list(errors = errors, warnings = warnings, info = info)
    suggestion@assessed <- TRUE
    return(suggestion)
  }
  
  info$sample_rate <- bundle_info$sample_rate[1]
  info$bundle_exists <- TRUE
  
  # Check level exists
  level_def <- get_levelDefinition_from_config(suggestion@corpus@config, suggestion@level_name)
  
  if (is.null(level_def)) {
    warnings <- c(warnings, sprintf("Level '%s' does not exist - will need to be created",
                                   suggestion@level_name))
    info$level_exists <- FALSE
    info$level_needs_creation <- TRUE
  } else {
    info$level_exists <- TRUE
    info$level_needs_creation <- FALSE
    
    # Check type matches
    if (level_def$type != suggestion@level_type) {
      errors <- c(errors, sprintf("Level type mismatch: level '%s' is type '%s' but suggestions are '%s'",
                                 suggestion@level_name, level_def$type, suggestion@level_type))
    }
    
    # Check attribute exists
    attr_names <- sapply(level_def$attributeDefinitions, function(x) x$name)
    if (!suggestion@attribute_name %in% attr_names) {
      warnings <- c(warnings, sprintf("Attribute '%s' does not exist on level '%s' - will need to be created",
                                     suggestion@attribute_name, suggestion@level_name))
      info$attribute_needs_creation <- TRUE
    } else {
      info$attribute_needs_creation <- FALSE
    }
  }
  
  # Validate suggestions
  if (nrow(suggestion@suggestions) == 0) {
    if (suggestion@remove_empty) {
      info$empty_suggestions <- TRUE
      warnings <- c(warnings, "No suggestions to apply (will be skipped)")
    } else {
      errors <- c(errors, "No suggestions provided and remove_empty is FALSE")
    }
  } else {
    info$empty_suggestions <- FALSE
    
    # Check for negative durations
    neg_dur <- suggestion@suggestions$end_time <= suggestion@suggestions$start_time
    if (any(neg_dur)) {
      errors <- c(errors, sprintf("%d suggestions have end_time <= start_time", sum(neg_dur)))
    }
    
    # Check minimum duration
    durations <- suggestion@suggestions$end_time - suggestion@suggestions$start_time
    too_short <- durations < suggestion@min_duration
    if (any(too_short)) {
      warnings <- c(warnings, sprintf("%d suggestions shorter than minimum duration (%.3f s)",
                                     sum(too_short), suggestion@min_duration))
      info$short_suggestions <- sum(too_short)
    }
    
    # Check for overlaps within suggestions
    if (nrow(suggestion@suggestions) > 1) {
      for (i in 1:(nrow(suggestion@suggestions) - 1)) {
        if (suggestion@suggestions$end_time[i] > suggestion@suggestions$start_time[i + 1]) {
          errors <- c(errors, sprintf("Overlapping suggestions at rows %d and %d", i, i + 1))
        }
      }
    }
    
    # Check bundle timing constraints (get bundle duration from audio file)
    audio_file <- file.path(suggestion@corpus@basePath,
                           paste0(suggestion@session, "_ses"),
                           paste0(suggestion@bundle, "_bndl"),
                           paste0(suggestion@bundle, ".", suggestion@corpus@config$mediafileExtension))
    
    if (file.exists(audio_file)) {
      tryCatch({
        audio_info <- wrassp::read.AsspDataObj(audio_file, begin = 0, end = 0)
        bundle_duration_sec <- attr(audio_info, "endRecord") / info$sample_rate
        
        # Check if any suggestions exceed bundle duration
        beyond <- suggestion@suggestions$end_time > bundle_duration_sec
        if (any(beyond)) {
          errors <- c(errors, sprintf("%d suggestions extend beyond bundle duration (%.3f s)",
                                     sum(beyond), bundle_duration_sec))
        }
        
        info$bundle_duration <- bundle_duration_sec
      }, error = function(e) {
        warnings <- c(warnings, sprintf("Could not read audio file duration: %s", e$message))
      })
    } else {
      warnings <- c(warnings, "Audio file not found - cannot validate bundle duration")
    }
    
    # Check for overlaps with existing annotations (if level exists)
    if (info$level_exists) {
      existing_query <- sprintf(
        "SELECT i.*, l.label FROM items i
         LEFT JOIN labels l ON i.db_uuid = l.db_uuid AND i.session = l.session AND 
                              i.bundle = l.bundle AND i.item_id = l.item_id
         WHERE i.db_uuid = '%s' AND i.session = '%s' AND i.bundle = '%s' AND i.level = '%s'",
        suggestion@corpus@.uuid, suggestion@session, suggestion@bundle, suggestion@level_name
      )
      existing_items <- DBI::dbGetQuery(con, existing_query)
      
      if (nrow(existing_items) > 0) {
        if (suggestion@level_type == "SEGMENT") {
          # Convert samples to time
          existing_items$start_time <- existing_items$sample_start / info$sample_rate
          existing_items$end_time <- (existing_items$sample_start + existing_items$sample_dur) / info$sample_rate
          
          # Check for overlaps
          overlap_count <- 0
          for (i in 1:nrow(suggestion@suggestions)) {
            overlaps <- (existing_items$start_time < suggestion@suggestions$end_time[i]) &
                       (existing_items$end_time > suggestion@suggestions$start_time[i])
            overlap_count <- overlap_count + sum(overlaps)
          }
          
          if (overlap_count > 0) {
            errors <- c(errors, sprintf("%d suggestions overlap with existing annotations", overlap_count))
          }
        } else {
          # For ITEM/EVENT, overlaps might be intentional (multiple attributes)
          warnings <- c(warnings, sprintf("Level already has %d existing items", nrow(existing_items)))
        }
      }
    }
  }
  
  # Store assessment results
  suggestion@assessment_results <- list(
    errors = errors,
    warnings = warnings,
    info = info,
    timestamp = Sys.time()
  )
  suggestion@assessed <- TRUE
  
  # Display results
  if (verbose) {
    if (length(errors) > 0) {
      cli::cli_alert_danger("Assessment found {length(errors)} error{?s}:")
      for (err in errors) {
        cli::cli_bullets(c("x" = err))
      }
    }
    
    if (length(warnings) > 0) {
      cli::cli_alert_warning("Assessment found {length(warnings)} warning{?s}:")
      for (warn in warnings) {
        cli::cli_bullets(c("!" = warn))
      }
    }
    
    if (length(errors) == 0 && length(warnings) == 0) {
      cli::cli_alert_success("Assessment passed with no issues")
    }
    
    # Show info
    if (length(info) > 0) {
      cli::cli_h3("Assessment summary:")
      if (!is.null(info$level_exists)) {
        cli::cli_bullets(c("i" = "Level exists: {info$level_exists}"))
      }
      if (!is.null(info$level_needs_creation) && info$level_needs_creation) {
        cli::cli_bullets(c("i" = "Level will be created"))
      }
      if (!is.null(info$attribute_needs_creation) && info$attribute_needs_creation) {
        cli::cli_bullets(c("i" = "Attribute will be created"))
      }
      if (!is.null(info$bundle_duration)) {
        cli::cli_bullets(c("i" = "Bundle duration: {round(info$bundle_duration, 3)} s"))
      }
      if (!is.null(info$short_suggestions) && info$short_suggestions > 0) {
        cli::cli_bullets(c("i" = "{info$short_suggestions} suggestions will be filtered (too short)"))
      }
    }
  }
  
  suggestion
}

# ==============================================================================
# CORRECT METHOD
# ==============================================================================

#' Correct - Manually adjust specific suggestions
#'
#' @param suggestion A Suggestion object
#' @param index Row index/indices to correct
#' @param start_time New start time (optional)
#' @param end_time New end time (optional)
#' @param label New label (optional)
#' 
#' @returns Updated suggestion object
#' @export
correct <- S7::new_generic("correct", "suggestion")

S7::method(correct, Suggestion) <- function(suggestion, index, start_time = NULL,
                                           end_time = NULL, label = NULL) {
  
  if (any(index < 1 | index > nrow(suggestion@suggestions))) {
    cli::cli_abort("Index out of range: must be between 1 and {nrow(suggestion@suggestions)}")
  }
  
  # Apply corrections
  if (!is.null(start_time)) {
    suggestion@suggestions$start_time[index] <- start_time
  }
  
  if (!is.null(end_time)) {
    suggestion@suggestions$end_time[index] <- end_time
  }
  
  if (!is.null(label)) {
    suggestion@suggestions$label[index] <- label
  }
  
  # Validate after correction
  if (!is.null(start_time) || !is.null(end_time)) {
    if (any(suggestion@suggestions$end_time[index] <= suggestion@suggestions$start_time[index])) {
      cli::cli_warn("Correction resulted in negative duration")
    }
  }
  
  # Mark as needing reassessment
  suggestion@assessed <- FALSE
  
  cli::cli_alert_success("Corrected {length(index)} suggestion{?s}")
  
  suggestion
}

# ==============================================================================
# PREPARE METHOD - Create/update levels and attributes
# ==============================================================================

#' Prepare - Create levels/attributes needed for transcription
#'
#' @param suggestion A Suggestion object
#' @param force Skip confirmation prompts
#' @param verbose Show progress messages
#' 
#' @returns Updated corpus configuration
#' @export
prepare <- S7::new_generic("prepare", "suggestion")

S7::method(prepare, Suggestion) <- function(suggestion, force = FALSE, verbose = TRUE) {
  
  # Run assessment if not done
  if (!suggestion@assessed) {
    suggestion <- assess(suggestion, verbose = FALSE)
  }
  
  results <- suggestion@assessment_results
  
  if (length(results$errors) > 0 && !force) {
    cli::cli_abort(c("Cannot prepare due to assessment errors:",
                    results$errors,
                    "i" = "Use force=TRUE to proceed anyway"))
  }
  
  if (verbose) {
    cli::cli_h2("Preparing database for transcription")
  }
  
  changes_made <- FALSE
  
  # Create level if needed
  if (results$info$level_needs_creation) {
    if (!force) {
      response <- readline(sprintf("Create level '%s' of type '%s'? (y/n): ",
                                  suggestion@level_name, suggestion@level_type))
      if (!tolower(response) %in% c("y", "yes")) {
        cli::cli_abort("User cancelled level creation")
      }
    }
    
    if (verbose) {
      cli::cli_alert_info("Creating level: {suggestion@level_name}")
    }
    
    # Add level to configuration
    suggestion@corpus@config <- add_level_to_config(
      suggestion@corpus@config,
      level_name = suggestion@level_name,
      level_type = suggestion@level_type,
      attribute_name = suggestion@attribute_name
    )
    
    changes_made <- TRUE
  }
  
  # Create attribute if needed
  if (!is.null(results$info$attribute_needs_creation) && results$info$attribute_needs_creation) {
    if (!force) {
      response <- readline(sprintf("Create attribute '%s' on level '%s'? (y/n): ",
                                  suggestion@attribute_name, suggestion@level_name))
      if (!tolower(response) %in% c("y", "yes")) {
        cli::cli_abort("User cancelled attribute creation")
      }
    }
    
    if (verbose) {
      cli::cli_alert_info("Creating attribute: {suggestion@attribute_name}")
    }
    
    suggestion@corpus@config <- add_attribute_to_level(
      suggestion@corpus@config,
      level_name = suggestion@level_name,
      attribute_name = suggestion@attribute_name
    )
    
    changes_made <- TRUE
  }
  
  # Write updated configuration
  if (changes_made) {
    config_path <- file.path(suggestion@corpus@basePath,
                            paste0(suggestion@corpus@dbName, "_DBconfig.json"))
    
    if (verbose) {
      cli::cli_alert_info("Writing updated configuration")
    }
    
    jsonlite::write_json(suggestion@corpus@config, config_path, 
                        pretty = TRUE, auto_unbox = TRUE)
    
    # Rewrite annotation files if level was created
    if (results$info$level_needs_creation) {
      if (verbose) {
        cli::cli_alert_info("Rewriting annotation files")
      }
      
      rewrite_annotations_parallel(suggestion@corpus, verbose = verbose)
    }
    
    if (verbose) {
      cli::cli_alert_success("Database prepared successfully")
    }
  } else {
    if (verbose) {
      cli::cli_alert_success("No preparation needed - database ready")
    }
  }
  
  suggestion@corpus@config
}

# ==============================================================================
# TRANSCRIBE METHOD - Apply suggestions to database
# ==============================================================================

#' Transcribe - Apply transcription suggestions to database
#'
#' @param suggestion A Suggestion object
#' @param force Skip confirmations and ignore errors
#' @param verbose Show progress messages
#' 
#' @returns TranscriptionLog object
#' @export
transcribe <- S7::new_generic("transcribe", "suggestion")

S7::method(transcribe, Suggestion) <- function(suggestion, force = FALSE, verbose = TRUE) {
  
  # Create log
  log <- TranscriptionLog(suggestion@corpus, suggestion@session, suggestion@bundle,
                         suggestion@level_name)
  
  # Run assessment if not done
  if (!suggestion@assessed) {
    suggestion <- assess(suggestion, verbose = FALSE)
  }
  
  results <- suggestion@assessment_results
  
  if (length(results$errors) > 0 && !force) {
    log@success <- FALSE
    log@error_message <- paste(results$errors, collapse = "; ")
    cli::cli_abort(c("Cannot transcribe due to assessment errors:",
                    results$errors,
                    "i" = "Run prepare() first or use force=TRUE"))
  }
  
  if (verbose) {
    cli::cli_h2("Transcribing suggestions to database")
  }
  
  # Filter out suggestions that are too short
  valid_suggestions <- suggestion@suggestions[
    (suggestion@suggestions$end_time - suggestion@suggestions$start_time) >= suggestion@min_duration,
  ]
  
  if (nrow(valid_suggestions) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No valid suggestions to transcribe")
    }
    log@success <- TRUE
    return(log)
  }
  
  if (verbose && nrow(valid_suggestions) < nrow(suggestion@suggestions)) {
    cli::cli_alert_info("Filtered {nrow(suggestion@suggestions) - nrow(valid_suggestions)} suggestions (too short)")
  }
  
  # Get database connection
  con <- get_corpus_connection(suggestion@corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    # Get bundle info for sample rate
    bundle_query <- sprintf(
      "SELECT sample_rate FROM bundle WHERE db_uuid = '%s' AND session = '%s' AND name = '%s'",
      suggestion@corpus@.uuid, suggestion@session, suggestion@bundle
    )
    bundle_info <- DBI::dbGetQuery(con, bundle_query)
    sample_rate <- bundle_info$sample_rate[1]
    
    # Convert times to samples
    if (suggestion@level_type == "SEGMENT") {
      valid_suggestions$sample_start <- round((valid_suggestions$start_time + 0.5/sample_rate) * sample_rate)
      valid_suggestions$sample_end <- round((valid_suggestions$end_time - 0.5/sample_rate) * sample_rate)
      valid_suggestions$sample_dur <- valid_suggestions$sample_end - valid_suggestions$sample_start
    } else if (suggestion@level_type == "EVENT") {
      valid_suggestions$sample_point <- round((valid_suggestions$start_time / 1000) * sample_rate)
    }
    
    # Get next available item IDs
    max_id_query <- sprintf(
      "SELECT COALESCE(MAX(item_id), 0) as max_id FROM items 
       WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s'",
      suggestion@corpus@.uuid, suggestion@session, suggestion@bundle
    )
    max_id <- DBI::dbGetQuery(con, max_id_query)$max_id[1]
    
    item_ids <- (max_id + 1):(max_id + nrow(valid_suggestions))
    
    # Prepare items data
    items_to_add <- data.frame(
      db_uuid = suggestion@corpus@.uuid,
      session = suggestion@session,
      bundle = suggestion@bundle,
      item_id = item_ids,
      level = suggestion@level_name,
      type = suggestion@level_type,
      seq_idx = 1:nrow(valid_suggestions),
      sample_rate = sample_rate
    )
    
    if (suggestion@level_type == "SEGMENT") {
      items_to_add$sample_point <- NA_integer_
      items_to_add$sample_start <- valid_suggestions$sample_start
      items_to_add$sample_dur <- valid_suggestions$sample_dur
    } else if (suggestion@level_type == "EVENT") {
      items_to_add$sample_point <- valid_suggestions$sample_point
      items_to_add$sample_start <- NA_integer_
      items_to_add$sample_dur <- NA_integer_
    } else {  # ITEM
      items_to_add$sample_point <- NA_integer_
      items_to_add$sample_start <- NA_integer_
      items_to_add$sample_dur <- NA_integer_
    }
    
    # Insert items
    if (verbose) {
      cli::cli_alert_info("Inserting {nrow(items_to_add)} items")
    }
    
    DBI::dbWriteTable(con, "items", items_to_add, append = TRUE)
    
    # Prepare labels data
    labels_to_add <- data.frame(
      db_uuid = suggestion@corpus@.uuid,
      session = suggestion@session,
      bundle = suggestion@bundle,
      item_id = item_ids,
      label_idx = 1,  # Assuming single label per item
      name = suggestion@attribute_name,
      label = valid_suggestions$label
    )
    
    # Insert labels
    DBI::dbWriteTable(con, "labels", labels_to_add, append = TRUE)
    
    log@items_added <- items_to_add
    log@n_items_added <- as.integer(nrow(items_to_add))
    log@n_items_modified <- 0L
    log@n_items_removed <- 0L
    log@success <- TRUE
    
    # Rewrite annotation file for this bundle
    if (verbose) {
      cli::cli_alert_info("Updating annotation file")
    }
    
    rewrite_bundle_annotation(suggestion@corpus, suggestion@session, 
                             suggestion@bundle, con, verbose = FALSE)
    
    if (verbose) {
      cli::cli_alert_success("Successfully transcribed {nrow(valid_suggestions)} items")
    }
    
  }, error = function(e) {
    log@success <- FALSE
    log@error_message <- e$message
    cli::cli_abort("Transcription failed: {e$message}")
  })
  
  log
}

# ==============================================================================
# TRANSCRIPTIONLOG METHODS
# ==============================================================================

#' Summary method for TranscriptionLog
#' @export
S7::method(summary, TranscriptionLog) <- function(object, ...) {
  cli::cli_h2("Transcription Log")
  cli::cli_bullets(c(
    "i" = "Timestamp: {format(object@timestamp)}",
    "i" = "Session: {object@session}",
    "i" = "Bundle: {object@bundle}",
    "i" = "Level: {object@level_name}",
    "i" = "Operation: {object@operation}",
    "i" = "Success: {object@success}"
  ))
  
  if (!object@success && length(object@error_message) > 0) {
    cli::cli_alert_danger("Error: {object@error_message}")
  }
  
  if (nrow(object@items_added) > 0) {
    cli::cli_alert_info("Items added: {nrow(object@items_added)}")
  }
  
  if (nrow(object@items_removed) > 0) {
    cli::cli_alert_info("Items removed: {nrow(object@items_removed)}")
  }
  
  if (object@level_created) {
    cli::cli_alert_info("Level created: {object@level_name}")
  }
  
  if (object@attribute_created) {
    cli::cli_alert_info("Attribute created")
  }
  
  invisible(object)
}

#' Reverse - Roll back transcription changes
#' @export
reverse <- S7::new_generic("reverse", "log")

S7::method(reverse, TranscriptionLog) <- function(log, verbose = TRUE) {
  
  if (!log@success) {
    cli::cli_warn("Cannot reverse unsuccessful transcription")
    return(invisible(NULL))
  }
  
  if (verbose) {
    cli::cli_h2("Reversing transcription")
  }
  
  # Get database connection
  con <- get_corpus_connection(log@corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    # Remove added items
    if (nrow(log@items_added) > 0) {
      if (verbose) {
        cli::cli_alert_info("Removing {nrow(log@items_added)} items")
      }
      
      item_ids <- paste0("(", paste(log@items_added$item_id, collapse = ","), ")")
      
      # Remove labels
      DBI::dbExecute(con, sprintf(
        "DELETE FROM labels WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s' AND item_id IN %s",
        log@corpus@.uuid, log@session, log@bundle, item_ids
      ))
      
      # Remove items
      DBI::dbExecute(con, sprintf(
        "DELETE FROM items WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s' AND item_id IN %s",
        log@corpus@.uuid, log@session, log@bundle, item_ids
      ))
    }
    
    # Rewrite annotation file
    if (verbose) {
      cli::cli_alert_info("Updating annotation file")
    }
    
    rewrite_bundle_annotation(log@corpus, log@session, log@bundle, con, verbose = FALSE)
    
    if (verbose) {
      cli::cli_alert_success("Transcription reversed successfully")
    }
    
  }, error = function(e) {
    cli::cli_abort("Failed to reverse transcription: {e$message}")
  })
  
  invisible(NULL)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get level definition from config
#' @keywords internal
get_levelDefinition_from_config <- function(config, level_name) {
  for (level in config$levelDefinitions) {
    if (level$name == level_name) {
      return(level)
    }
  }
  NULL
}

#' Add level to configuration
#' @keywords internal
add_level_to_config <- function(config, level_name, level_type, attribute_name) {
  new_level <- list(
    name = level_name,
    type = level_type,
    attributeDefinitions = list(
      list(name = attribute_name, type = "STRING")
    )
  )
  
  config$levelDefinitions <- c(config$levelDefinitions, list(new_level))
  config
}

#' Add attribute to level
#' @keywords internal
add_attribute_to_level <- function(config, level_name, attribute_name) {
  for (i in seq_along(config$levelDefinitions)) {
    if (config$levelDefinitions[[i]]$name == level_name) {
      new_attr <- list(name = attribute_name, type = "STRING")
      config$levelDefinitions[[i]]$attributeDefinitions <- c(
        config$levelDefinitions[[i]]$attributeDefinitions,
        list(new_attr)
      )
      break
    }
  }
  config
}

#' Rewrite all annotations in parallel
#' @keywords internal
rewrite_annotations_parallel <- function(corpus, verbose = TRUE) {
  con <- get_corpus_connection(corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  bundles <- DBI::dbGetQuery(con, sprintf(
    "SELECT session, name as bundle FROM bundle WHERE db_uuid = '%s'",
    corpus@.uuid
  ))
  
  if (verbose) {
    cli::cli_progress_bar("Rewriting annotations", total = nrow(bundles))
  }
  
  for (i in 1:nrow(bundles)) {
    rewrite_bundle_annotation(corpus, bundles$session[i], bundles$bundle[i], 
                             con, verbose = FALSE)
    if (verbose) {
      cli::cli_progress_update()
    }
  }
  
  if (verbose) {
    cli::cli_progress_done()
  }
}

#' Rewrite annotation file for a single bundle
#' @keywords internal
rewrite_bundle_annotation <- function(corpus, session, bundle, con, verbose = TRUE) {
  # Load annotation data from cache
  items <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM items WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s' ORDER BY level, seq_idx",
    corpus@.uuid, session, bundle
  ))
  
  labels <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM labels WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s'",
    corpus@.uuid, session, bundle
  ))
  
  links <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM links WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s'",
    corpus@.uuid, session, bundle
  ))
  
  # Convert to annotation JSON structure
  annot_json <- create_annotation_json(items, labels, links, corpus@config)
  
  # Write to file
  annot_path <- file.path(corpus@basePath,
                         paste0(session, "_ses"),
                         paste0(bundle, "_bndl"),
                         paste0(bundle, "_annot.json"))
  
  jsonlite::write_json(annot_json, annot_path, pretty = TRUE, auto_unbox = TRUE)
  
  # Update MD5 in cache
  new_md5 <- tools::md5sum(annot_path)
  DBI::dbExecute(con, sprintf(
    "UPDATE bundle SET md5_annot_json = '%s' WHERE db_uuid = '%s' AND session = '%s' AND name = '%s'",
    new_md5, corpus@.uuid, session, bundle
  ))
  
  if (verbose) {
    cli::cli_alert_success("Updated annotation file for {session}/{bundle}")
  }
}

#' Create annotation JSON structure from database tables
#' @keywords internal
create_annotation_json <- function(items, labels, links, config) {
  # Group by level
  levels_list <- list()
  
  if (nrow(items) > 0) {
    for (level_name in unique(items$level)) {
      level_items <- items[items$level == level_name, ]
      level_labels <- labels[labels$item_id %in% level_items$item_id, ]
      
      items_list <- lapply(1:nrow(level_items), function(i) {
        item <- level_items[i, ]
        item_labels <- level_labels[level_labels$item_id == item$item_id, ]
        
        item_json <- list(
          id = item$item_id,
          samplePoint = if (!is.na(item$sample_point)) item$sample_point else NULL,
          sampleStart = if (!is.na(item$sample_start)) item$sample_start else NULL,
          sampleDur = if (!is.na(item$sample_dur)) item$sample_dur else NULL,
          labels = lapply(1:nrow(item_labels), function(j) {
            list(
              name = item_labels$name[j],
              value = item_labels$label[j]
            )
          })
        )
        
        # Remove NULL fields
        item_json[!sapply(item_json, is.null)]
      })
      
      levels_list[[level_name]] <- list(
        name = level_name,
        items = items_list
      )
    }
  }
  
  # Add links
  links_list <- list()
  if (nrow(links) > 0) {
    links_list <- lapply(1:nrow(links), function(i) {
      list(
        fromID = links$from_id[i],
        toID = links$to_id[i],
        label = if (!is.na(links$label[i])) links$label[i] else NULL
      )
    })
  }
  
  list(
    name = items$bundle[1],
    annotates = items$bundle[1],
    sampleRate = items$sample_rate[1],
    levels = unname(levels_list),
    links = links_list
  )
}

# ==============================================================================
# PRINT METHODS
# ==============================================================================

#' @export
S7::method(print, Suggestion) <- function(x, ...) {
  cli::cli_h2("{S7::class_name(x)}")
  cli::cli_bullets(c(
    "i" = "Session: {x@session}",
    "i" = "Bundle: {x@bundle}",
    "i" = "Level: {x@level_name} ({x@level_type})",
    "i" = "Attribute: {x@attribute_name}",
    "i" = "Suggestions: {nrow(x@suggestions)}",
    "i" = "Assessed: {x@assessed}"
  ))
  
  if (nrow(x@suggestions) > 0) {
    cli::cli_text("\nFirst few suggestions:")
    print(head(x@suggestions[, c("start_time", "end_time", "label")], 5))
  }
  
  if (x@assessed && !is.null(x@assessment_results)) {
    n_err <- length(x@assessment_results$errors)
    n_warn <- length(x@assessment_results$warnings)
    
    if (n_err > 0) {
      cli::cli_alert_danger("{n_err} error{?s} in assessment")
    }
    if (n_warn > 0) {
      cli::cli_alert_warning("{n_warn} warning{?s} in assessment")
    }
    if (n_err == 0 && n_warn == 0) {
      cli::cli_alert_success("Assessment passed")
    }
  }
  
  invisible(x)
}
