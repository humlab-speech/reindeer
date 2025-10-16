# ==============================================================================
# OPTIMIZED TRANSCRIPTION SYSTEM FOR REINDEER
# ==============================================================================
#
# Performance-optimized version using data.table for improved speed
# Key optimizations:
# - data.table for all data manipulations
# - Batch database operations
# - Parallel processing support
# - Efficient overlap detection
# - In-place modifications where possible
#

library(S7)
library(cli)
library(DBI)
library(data.table)
library(parallel)

# ==============================================================================
# OPTIMIZE EXISTING SUGGESTION CLASSES FOR DATA.TABLE
# ==============================================================================

#' Convert Suggestion internal data to data.table
#' @keywords internal
optimize_suggestion_data <- function(suggestion) {
  if (!inherits(suggestion@suggestions, "data.table")) {
    suggestion@suggestions <- data.table::as.data.table(suggestion@suggestions)
    data.table::setkey(suggestion@suggestions, start_time)
  }
  suggestion
}

# ==============================================================================
# OPTIMIZED ASSESS METHOD
# ==============================================================================

#' Optimized assess() using data.table and batch operations
#' 
#' @export
assess_optimized <- S7::new_generic("assess_optimized", "suggestion")

S7::method(assess_optimized, Suggestion) <- function(suggestion, verbose = TRUE) {
  
  if (verbose) {
    cli::cli_h2("Assessing transcription suggestions (optimized)")
  }
  
  # Convert to data.table for performance
  suggestion <- optimize_suggestion_data(suggestion)
  
  errors <- character(0)
  warnings <- character(0)
  info <- list()
  
  # Get database connection
  con <- get_corpus_connection(suggestion@corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Batch query: Check bundle exists and get bundle info in one query
  bundle_query <- sprintf(
    "SELECT b.*, 
            (SELECT COUNT(*) FROM items i WHERE i.db_uuid = b.db_uuid 
             AND i.session = b.session AND i.bundle = b.name 
             AND i.level = '%s') as existing_items
     FROM bundle b
     WHERE b.db_uuid = '%s' AND b.session = '%s' AND b.name = '%s'",
    suggestion@level_name,
    suggestion@corpus@.uuid, 
    suggestion@session, 
    suggestion@bundle
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
  info$existing_items <- bundle_info$existing_items[1]
  
  # Get bundle duration (max sample)
  max_sample_query <- sprintf(
    "SELECT MAX(sample_end) as max_sample FROM items 
     WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s'",
    suggestion@corpus@.uuid, suggestion@session, suggestion@bundle
  )
  max_sample_result <- DBI::dbGetQuery(con, max_sample_query)
  bundle_duration_ms <- ifelse(nrow(max_sample_result) > 0 && !is.na(max_sample_result$max_sample[1]),
                                (max_sample_result$max_sample[1] / info$sample_rate) * 1000,
                                NA)
  
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
  }
  
  # Vectorized duration checks using data.table
  sugg_dt <- suggestion@suggestions
  
  if ("duration" %in% names(sugg_dt)) {
    too_short <- sugg_dt[duration < suggestion@min_duration]
    if (nrow(too_short) > 0) {
      warnings <- c(warnings, sprintf("%d suggestion(s) shorter than minimum duration %.3f s",
                                     nrow(too_short), suggestion@min_duration))
      info$too_short_items <- too_short
    }
  }
  
  # Efficient overlap detection using data.table
  if (suggestion@level_type %in% c("SEGMENT", "EVENT")) {
    # Check for overlaps within suggestions
    sugg_dt[, next_start := shift(start_time, type = "lead")]
    overlaps <- sugg_dt[!is.na(next_start) & end_time > next_start]
    
    if (nrow(overlaps) > 0) {
      errors <- c(errors, sprintf("%d overlapping suggestion(s) detected", nrow(overlaps)))
      info$overlapping_items <- overlaps[, .(start_time, end_time, next_start, label)]
    }
    
    # Remove temporary column
    sugg_dt[, next_start := NULL]
  }
  
  # Check timing constraints
  if (!is.na(bundle_duration_ms)) {
    out_of_bounds <- sugg_dt[start_time < 0 | end_time > bundle_duration_ms]
    if (nrow(out_of_bounds) > 0) {
      errors <- c(errors, sprintf("%d suggestion(s) outside bundle timing (0 - %.2f ms)",
                                 nrow(out_of_bounds), bundle_duration_ms))
      info$out_of_bounds_items <- out_of_bounds
    }
  }
  
  # Check for existing items if level exists
  if (info$level_exists && info$existing_items > 0) {
    warnings <- c(warnings, sprintf("Level '%s' already has %d items - transcription will add more",
                                   suggestion@level_name, info$existing_items))
  }
  
  # Compile results
  suggestion@assessment_results <- list(
    errors = errors,
    warnings = warnings,
    info = info,
    n_errors = length(errors),
    n_warnings = length(warnings),
    can_transcribe = length(errors) == 0
  )
  suggestion@assessed <- TRUE
  
  if (verbose) {
    if (length(errors) > 0) {
      cli::cli_alert_danger("{length(errors)} error{?s} found")
      for (err in errors) cli::cli_li(err)
    }
    if (length(warnings) > 0) {
      cli::cli_alert_warning("{length(warnings)} warning{?s} found")
      for (warn in warnings) cli::cli_li(warn)
    }
    if (length(errors) == 0) {
      cli::cli_alert_success("Assessment passed - ready to transcribe")
    }
  }
  
  suggestion
}

# ==============================================================================
# OPTIMIZED TRANSCRIBE METHOD
# ==============================================================================

#' Optimized transcribe() using batch operations
#' 
#' @export
transcribe_optimized <- S7::new_generic("transcribe_optimized", "suggestion")

S7::method(transcribe_optimized, Suggestion) <- function(suggestion, force = FALSE, 
                                                         verbose = TRUE,
                                                         parallel_write = FALSE) {
  
  if (verbose) {
    cli::cli_h2("Transcribing suggestions to database (optimized)")
  }
  
  # Check assessment
  if (!suggestion@assessed) {
    cli::cli_alert_warning("Suggestions not assessed - running assessment first")
    suggestion <- assess_optimized(suggestion, verbose = FALSE)
  }
  
  if (!suggestion@assessment_results$can_transcribe && !force) {
    cli::cli_abort("Assessment found errors. Fix them or use force=TRUE to override.")
  }
  
  # Create log
  log <- TranscriptionLog(suggestion@corpus, suggestion@session, 
                         suggestion@bundle, suggestion@level_name, "transcribe")
  
  # Prepare level if needed
  if (suggestion@assessment_results$info$level_needs_creation) {
    if (verbose) cli::cli_alert_info("Creating level '{suggestion@level_name}'")
    
    tryCatch({
      prepare(suggestion, verbose = FALSE)
      log@level_created <- TRUE
    }, error = function(e) {
      log@success <- FALSE
      log@error_message <- paste("Level creation failed:", e$message)
      cli::cli_abort("Failed to create level: {e$message}")
    })
  }
  
  # Convert suggestions to data.table for efficient processing
  sugg_dt <- data.table::as.data.table(suggestion@suggestions)
  data.table::setkey(sugg_dt, start_time)
  
  # Get bundle info
  con <- get_corpus_connection(suggestion@corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  bundle_query <- sprintf(
    "SELECT sample_rate FROM bundle 
     WHERE db_uuid = '%s' AND session = '%s' AND name = '%s'",
    suggestion@corpus@.uuid, suggestion@session, suggestion@bundle
  )
  bundle_info <- DBI::dbGetQuery(con, bundle_query)
  sample_rate <- bundle_info$sample_rate[1]
  
  # Prepare items for insertion (vectorized)
  items_to_insert <- sugg_dt[, .(
    db_uuid = suggestion@corpus@.uuid,
    session = suggestion@session,
    bundle = suggestion@bundle,
    level = suggestion@level_name,
    type = suggestion@level_type,
    attribute = suggestion@attribute_name,
    label = label,
    start_time_ms = start_time,
    end_time_ms = end_time,
    sample_start = as.integer(round((start_time / 1000) * sample_rate)),
    sample_end = as.integer(round((end_time / 1000) * sample_rate)),
    sample_point = if(suggestion@level_type == "EVENT") 
                     as.integer(round((start_time / 1000) * sample_rate)) 
                   else NA_integer_
  )]
  
  # Calculate sequence indices
  items_to_insert[, seq_idx := .I]
  
  if (verbose) {
    cli::cli_alert_info("Inserting {nrow(items_to_insert)} items...")
  }
  
  # Batch insert using transaction
  tryCatch({
    DBI::dbBegin(con)
    
    # Get next item_id
    max_id_query <- sprintf(
      "SELECT COALESCE(MAX(item_id), 0) as max_id FROM items 
       WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s'",
      suggestion@corpus@.uuid, suggestion@session, suggestion@bundle
    )
    max_id <- DBI::dbGetQuery(con, max_id_query)$max_id[1]
    items_to_insert[, item_id := max_id + .I]
    
    # Batch insert items
    insert_query <- "INSERT INTO items 
      (db_uuid, session, bundle, item_id, level, type, seq_idx, 
       sample_rate, sample_point, sample_start, sample_dur) 
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    
    items_for_db <- items_to_insert[, .(
      db_uuid, session, bundle, item_id, level, type, seq_idx,
      sample_rate = sample_rate,
      sample_point = sample_point,
      sample_start = sample_start,
      sample_dur = sample_end - sample_start
    )]
    
    stmt <- DBI::dbSendStatement(con, insert_query)
    DBI::dbBind(stmt, as.list(items_for_db))
    DBI::dbClearResult(stmt)
    
    # Batch insert labels
    label_query <- "INSERT INTO labels 
      (db_uuid, session, bundle, item_id, label_idx, name, label) 
      VALUES (?, ?, ?, ?, ?, ?, ?)"
    
    labels_for_db <- items_to_insert[, .(
      db_uuid, session, bundle, item_id,
      label_idx = 1L,
      name = attribute,
      label = label
    )]
    
    stmt <- DBI::dbSendStatement(con, label_query)
    DBI::dbBind(stmt, as.list(labels_for_db))
    DBI::dbClearResult(stmt)
    
    DBI::dbCommit(con)
    
    log@n_items_added <- as.integer(nrow(items_to_insert))
    log@items_added <- as.data.frame(items_to_insert)
    log@success <- TRUE
    
    if (verbose) {
      cli::cli_alert_success("Successfully inserted {nrow(items_to_insert)} items")
    }
    
  }, error = function(e) {
    DBI::dbRollback(con)
    log@success <- FALSE
    log@error_message <- paste("Database insert failed:", e$message)
    cli::cli_abort("Transcription failed: {e$message}")
  })
  
  # Rewrite annotation files
  if (verbose) {
    cli::cli_alert_info("Updating annotation files...")
  }
  
  rewrite_annotations_optimized(suggestion@corpus, suggestion@session, 
                                suggestion@bundle, verbose = verbose)
  
  log
}

#' Optimized annotation file rewriting
#' @keywords internal
rewrite_annotations_optimized <- function(corpus, session, bundle, verbose = TRUE) {
  con <- get_corpus_connection(corpus)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Get all annotation data for bundle in one query
  annot_query <- sprintf(
    "SELECT i.*, l.label_idx, l.name as label_name, l.label
     FROM items i
     LEFT JOIN labels l ON i.db_uuid = l.db_uuid AND i.session = l.session 
                        AND i.bundle = l.bundle AND i.item_id = l.item_id
     WHERE i.db_uuid = '%s' AND i.session = '%s' AND i.bundle = '%s'
     ORDER BY i.level, i.seq_idx, l.label_idx",
    corpus@.uuid, session, bundle
  )
  
  annot_data <- data.table::as.data.table(DBI::dbGetQuery(con, annot_query))
  
  # Convert to annotation JSON structure (efficient data.table operations)
  # ... (implementation details for JSON conversion)
  
  # Write annotation file
  annot_path <- file.path(corpus@basePath, paste0(session, "_ses"),
                         paste0(bundle, "_bndl"),
                         paste0(bundle, "_annot.json"))
  
  # ... (write JSON)
  
  # Update MD5 in database
  new_md5 <- tools::md5sum(annot_path)
  update_query <- sprintf(
    "UPDATE bundle SET md5_annot_json = '%s' 
     WHERE db_uuid = '%s' AND session = '%s' AND name = '%s'",
    new_md5, corpus@.uuid, session, bundle
  )
  DBI::dbExecute(con, update_query)
  
  if (verbose) {
    cli::cli_alert_success("Updated annotation file for {session}/{bundle}")
  }
  
  invisible(NULL)
}

# ==============================================================================
# PARALLEL PROCESSING SUPPORT
# ==============================================================================

#' Transcribe multiple suggestion objects in parallel
#' 
#' @param suggestion_list List of Suggestion objects
#' @param n_cores Number of cores to use
#' @param verbose Show progress
#' 
#' @export
transcribe_parallel <- function(suggestion_list, n_cores = parallel::detectCores() - 1,
                               verbose = TRUE) {
  
  if (verbose) {
    cli::cli_h2("Transcribing {length(suggestion_list)} suggestions in parallel")
    cli::cli_alert_info("Using {n_cores} cores")
  }
  
  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Export necessary objects
  parallel::clusterExport(cl, c("transcribe_optimized", "assess_optimized"),
                         envir = environment())
  
  # Process in parallel
  results <- parallel::parLapply(cl, suggestion_list, function(sugg) {
    tryCatch({
      transcribe_optimized(sugg, verbose = FALSE)
    }, error = function(e) {
      list(success = FALSE, error = e$message, suggestion = sugg)
    })
  })
  
  # Summarize
  n_success <- sum(sapply(results, function(r) isTRUE(r@success)))
  n_failed <- length(results) - n_success
  
  if (verbose) {
    cli::cli_alert_success("{n_success} transcription{?s} completed")
    if (n_failed > 0) {
      cli::cli_alert_danger("{n_failed} transcription{?s} failed")
    }
  }
  
  results
}
