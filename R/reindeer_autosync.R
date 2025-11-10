#' Automatic Synchronization System for EMU Databases
#'
#' This module provides automatic synchronization between:
#' 1. _annot.json files and their .eaf equivalents
#' 2. Database structure/metadata and CMDI XML files
#'
#' The system uses file modification timestamps and MD5 checksums to detect changes
#' and trigger appropriate updates.

# ==============================================================================
# CONFIGURATION AND STATE MANAGEMENT
# ==============================================================================

#' Enable or disable automatic synchronization
#'
#' @param db_handle An emuDBhandle object
#' @param enable Logical; TRUE to enable, FALSE to disable
#' @param sync_eaf Logical; automatically sync EAF files when _annot.json changes
#' @param sync_cmdi Logical; automatically sync CMDI file when database/metadata changes
#' @param align_items Logical; for EAF sync, whether to align ITEMs with time info
#' @param cmdi_profile Character; CMDI profile to use ("media-corpus", "speech-corpus", "speech-corpus-dlu")
#' @param verbose Logical; show sync messages
#'
#' @export
enable_auto_sync <- function(db_handle, 
                              enable = TRUE,
                              sync_eaf = TRUE,
                              sync_cmdi = TRUE,
                              align_items = TRUE,
                              cmdi_profile = "speech-corpus",
                              verbose = TRUE) {
  
  emuR:::check_emuDBhandle(db_handle)
  
  # Store configuration in database directory
  sync_config_path <- file.path(db_handle$basePath, ".sync_config.json")
  
  config <- list(
    enabled = enable,
    sync_eaf = sync_eaf,
    sync_cmdi = sync_cmdi,
    align_items = align_items,
    cmdi_profile = cmdi_profile,
    verbose = verbose,
    last_cmdi_sync = Sys.time(),
    db_uuid = db_handle$UUID
  )
  
  jsonlite::write_json(config, sync_config_path, pretty = TRUE, auto_unbox = TRUE)
  
  if (verbose) {
    if (enable) {
      cli::cli_alert_success("Auto-sync enabled for {.path {db_handle$dbName}}")
      if (sync_eaf) cli::cli_alert_info("  EAF files will sync on _annot.json changes")
      if (sync_cmdi) cli::cli_alert_info("  CMDI will sync on database changes")
    } else {
      cli::cli_alert_warning("Auto-sync disabled for {.path {db_handle$dbName}}")
    }
  }
  
  # Initialize sync state tracking
  init_sync_state(db_handle)
  
  invisible(config)
}

#' Initialize synchronization state tracking
#'
#' @param db_handle An emuDBhandle object
#' @keywords internal
init_sync_state <- function(db_handle) {
  sync_state_path <- file.path(db_handle$basePath, ".sync_state.json")
  
  if (file.exists(sync_state_path)) {
    return(invisible(NULL))
  }
  
  # Create initial state
  state <- list(
    annot_checksums = list(),
    metadata_checksums = list(),
    last_full_scan = Sys.time(),
    db_uuid = db_handle$UUID
  )
  
  jsonlite::write_json(state, sync_state_path, pretty = TRUE, auto_unbox = TRUE)
  invisible(NULL)
}

#' Load sync configuration
#'
#' @param db_handle An emuDBhandle object
#' @return List with sync configuration, or NULL if not configured
#' @keywords internal
load_sync_config <- function(db_handle) {
  sync_config_path <- file.path(db_handle$basePath, ".sync_config.json")
  
  if (!file.exists(sync_config_path)) {
    return(NULL)
  }
  
  jsonlite::read_json(sync_config_path)
}

#' Load sync state
#'
#' @param db_handle An emuDBhandle object
#' @return List with sync state
#' @keywords internal
load_sync_state <- function(db_handle) {
  sync_state_path <- file.path(db_handle$basePath, ".sync_state.json")
  
  if (!file.exists(sync_state_path)) {
    init_sync_state(db_handle)
  }
  
  jsonlite::read_json(sync_state_path)
}

#' Save sync state
#'
#' @param db_handle An emuDBhandle object
#' @param state List with sync state
#' @keywords internal
save_sync_state <- function(db_handle, state) {
  sync_state_path <- file.path(db_handle$basePath, ".sync_state.json")
  jsonlite::write_json(state, sync_state_path, pretty = TRUE, auto_unbox = TRUE)
  invisible(NULL)
}

# ==============================================================================
# CHANGE DETECTION
# ==============================================================================

#' Detect changes in _annot.json files
#'
#' @param db_handle An emuDBhandle object
#' @return Data frame with changed bundles (session, bundle, annot_path)
#' @keywords internal
detect_annot_changes <- function(db_handle) {
  state <- load_sync_state(db_handle)
  
  # Get all _annot.json files
  annot_files <- list.files(
    db_handle$basePath,
    pattern = "_annot\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  changed <- list()
  
  for (annot_path in annot_files) {
    # Extract session and bundle names
    rel_path <- sub(paste0(db_handle$basePath, "/"), "", annot_path)
    parts <- strsplit(rel_path, "/")[[1]]
    
    if (length(parts) < 2) next
    
    session <- sub("_ses$", "", parts[1])
    bundle <- sub("_bndl$", "", parts[2])
    
    # Calculate MD5 checksum
    checksum <- as.character(tools::md5sum(annot_path))
    
    # Compare with stored checksum
    state_key <- paste0(session, ":", bundle)
    old_checksum <- state$annot_checksums[[state_key]]
    
    if (is.null(old_checksum) || old_checksum != checksum) {
      changed[[length(changed) + 1]] <- list(
        session = session,
        bundle = bundle,
        annot_path = annot_path,
        old_checksum = old_checksum,
        new_checksum = checksum
      )
      
      # Update state
      state$annot_checksums[[state_key]] <- checksum
    }
  }
  
  # Save updated state
  save_sync_state(db_handle, state)
  
  if (length(changed) == 0) {
    return(NULL)
  }
  
  # Convert to data frame
  do.call(rbind, lapply(changed, function(x) {
    data.frame(
      session = x$session,
      bundle = x$bundle,
      annot_path = x$annot_path,
      stringsAsFactors = FALSE
    )
  }))
}

#' Detect changes in .meta_json files
#'
#' @param db_handle An emuDBhandle object
#' @return Data frame with changed metadata files
#' @keywords internal
detect_metadata_changes <- function(db_handle) {
  state <- load_sync_state(db_handle)
  
  # Get all .meta_json files
  meta_files <- list.files(
    db_handle$basePath,
    pattern = "^\\.meta_json$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  changed <- FALSE
  
  for (meta_path in meta_files) {
    checksum <- as.character(tools::md5sum(meta_path))
    
    # Use relative path as key
    rel_path <- sub(paste0(db_handle$basePath, "/"), "", meta_path)
    old_checksum <- state$metadata_checksums[[rel_path]]
    
    if (is.null(old_checksum) || old_checksum != checksum) {
      changed <- TRUE
      state$metadata_checksums[[rel_path]] <- checksum
    }
  }
  
  # Save updated state
  save_sync_state(db_handle, state)
  
  changed
}

#' Detect changes in database configuration
#'
#' @param db_handle An emuDBhandle object
#' @return Logical; TRUE if configuration changed
#' @keywords internal
detect_config_changes <- function(db_handle) {
  state <- load_sync_state(db_handle)
  
  config_path <- file.path(db_handle$basePath, paste0(db_handle$dbName, "_DBconfig.json"))
  
  if (!file.exists(config_path)) {
    return(FALSE)
  }
  
  checksum <- as.character(tools::md5sum(config_path))
  old_checksum <- state$metadata_checksums[["_DBconfig"]]
  
  if (is.null(old_checksum) || old_checksum != checksum) {
    state$metadata_checksums[["_DBconfig"]] <- checksum
    save_sync_state(db_handle, state)
    return(TRUE)
  }
  
  FALSE
}

# ==============================================================================
# SYNCHRONIZATION EXECUTION
# ==============================================================================

#' Synchronize all _annot.json files to EAF format
#'
#' @param db_handle An emuDBhandle object
#' @param changed_bundles Data frame with changed bundles, or NULL to check all
#' @param align_items Logical; whether to align ITEMs with time info
#' @param verbose Logical; show progress
#' @keywords internal
sync_annot_to_eaf <- function(db_handle, changed_bundles = NULL, align_items = TRUE, verbose = TRUE) {
  
  if (is.null(changed_bundles)) {
    changed_bundles <- detect_annot_changes(db_handle)
  }
  
  if (is.null(changed_bundles) || nrow(changed_bundles) == 0) {
    if (verbose) {
      cli::cli_alert_info("No _annot.json changes detected")
    }
    return(invisible(0))
  }
  
  n_synced <- 0
  errors <- list()
  
  if (verbose) {
    cli::cli_alert_info("Syncing {nrow(changed_bundles)} bundle(s) to EAF format...")
  }
  
  for (i in seq_len(nrow(changed_bundles))) {
    session <- changed_bundles$session[i]
    bundle <- changed_bundles$bundle[i]
    
    tryCatch({
      # Use reindeer's convert_emu_to_eaf function
      convert_emu_to_eaf(
        db_handle,
        session = session,
        bundle = bundle,
        align_items = align_items,
        overwrite = TRUE,
        verbose = FALSE
      )
      n_synced <- n_synced + 1
      
      if (verbose) {
        cli::cli_alert_success("  {session}/{bundle}")
      }
      
    }, error = function(e) {
      errors[[length(errors) + 1]] <- list(
        session = session,
        bundle = bundle,
        error = conditionMessage(e)
      )
      
      if (verbose) {
        cli::cli_alert_danger("  {session}/{bundle}: {conditionMessage(e)}")
      }
    })
  }
  
  if (verbose && length(errors) > 0) {
    cli::cli_alert_warning("Completed with {length(errors)} error(s)")
  }
  
  invisible(n_synced)
}

#' Synchronize database metadata to CMDI format
#'
#' @param db_handle An emuDBhandle object
#' @param force Logical; force sync even if no changes detected
#' @param profile Character; CMDI profile to use
#' @param verbose Logical; show progress
#' @keywords internal
sync_to_cmdi <- function(db_handle, force = FALSE, profile = "speech-corpus", verbose = TRUE) {
  
  # Check for changes
  config_changed <- detect_config_changes(db_handle)
  metadata_changed <- detect_metadata_changes(db_handle)
  
  # Check for session/bundle structure changes
  sessions <- emuR::list_sessions(db_handle)
  state <- load_sync_state(db_handle)
  
  session_count_key <- "session_count"
  old_session_count <- state$metadata_checksums[[session_count_key]]
  current_session_count <- nrow(sessions)
  
  structure_changed <- is.null(old_session_count) || old_session_count != current_session_count
  
  if (structure_changed) {
    state$metadata_checksums[[session_count_key]] <- current_session_count
    save_sync_state(db_handle, state)
  }
  
  if (!force && !config_changed && !metadata_changed && !structure_changed) {
    if (verbose) {
      cli::cli_alert_info("No database/metadata changes detected for CMDI")
    }
    return(invisible(FALSE))
  }
  
  if (verbose) {
    changes <- c()
    if (config_changed) changes <- c(changes, "configuration")
    if (metadata_changed) changes <- c(changes, "metadata")
    if (structure_changed) changes <- c(changes, "structure")
    
    cli::cli_alert_info("CMDI sync triggered by: {paste(changes, collapse = ', ')}")
  }
  
  # Load configuration to get metadata for CMDI
  config <- load_sync_config(db_handle)
  
  # Try to load existing CMDI to preserve user-provided metadata
  cmdi_path <- file.path(db_handle$basePath, paste0(db_handle$dbName, "_cmdi.xml"))
  existing_metadata <- list()
  
  if (file.exists(cmdi_path)) {
    # Extract key metadata from existing CMDI
    tryCatch({
      existing_metadata <- extract_cmdi_metadata(cmdi_path)
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("Could not read existing CMDI metadata")
      }
    })
  }
  
  # Generate/update CMDI
  tryCatch({
    create_cmdi_metadata(
      db_handle,
      output_file = cmdi_path,
      profile = profile,
      corpus_title = existing_metadata$corpus_title %||% db_handle$dbName,
      corpus_description = existing_metadata$corpus_description %||% 
        paste("EMU speech database:", db_handle$dbName),
      author = existing_metadata$author %||% "Database maintainer",
      institution = existing_metadata$institution %||% "Institution",
      contact_email = existing_metadata$contact_email %||% "contact@example.edu",
      license = existing_metadata$license %||% "CC-BY-4.0",
      availability = existing_metadata$availability %||% "available",
      include_placeholders = TRUE,
      verbose = FALSE
    )
    
    if (verbose) {
      cli::cli_alert_success("CMDI updated: {.path {basename(cmdi_path)}}")
    }
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_danger("CMDI sync failed: {conditionMessage(e)}")
    }
    return(invisible(FALSE))
  })
}

#' Extract metadata from existing CMDI file
#'
#' @param cmdi_path Path to CMDI XML file
#' @return List with extracted metadata
#' @keywords internal
extract_cmdi_metadata <- function(cmdi_path) {
  if (!file.exists(cmdi_path)) {
    return(list())
  }
  
  doc <- xml2::read_xml(cmdi_path)
  ns <- xml2::xml_ns(doc)
  
  # Use cmd namespace
  list(
    corpus_title = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:Name", ns)),
    corpus_description = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:Description", ns)),
    author = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:MdCreator", ns)),
    institution = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:Organization", ns)),
    contact_email = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:Email", ns)),
    license = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:License", ns)),
    availability = xml2::xml_text(xml2::xml_find_first(doc, ".//cmd:Availability", ns))
  )
}

# ==============================================================================
# MANUAL SYNC TRIGGERS
# ==============================================================================

#' Manually trigger synchronization
#'
#' @param db_handle An emuDBhandle object
#' @param sync_eaf Logical; sync EAF files
#' @param sync_cmdi Logical; sync CMDI file
#' @param force Logical; force sync even if no changes detected
#' @param verbose Logical; show progress
#'
#' @export
sync_database <- function(db_handle, 
                          sync_eaf = TRUE, 
                          sync_cmdi = TRUE,
                          force = FALSE,
                          verbose = TRUE) {
  
  emuR:::check_emuDBhandle(db_handle)
  
  config <- load_sync_config(db_handle)
  
  if (is.null(config)) {
    if (verbose) {
      cli::cli_alert_warning("Auto-sync not configured. Use {.fn enable_auto_sync} first.")
    }
    config <- list(
      align_items = TRUE,
      cmdi_profile = "speech-corpus"
    )
  }
  
  results <- list()
  
  # Sync EAF files
  if (sync_eaf) {
    if (verbose) {
      cli::cli_h2("Synchronizing EAF files")
    }
    
    changed_bundles <- if (force) {
      # Force sync all bundles
      sessions <- emuR::list_sessions(db_handle)
      all_bundles <- list()
      for (sess in sessions$name) {
        bundles <- emuR::list_bundles(db_handle, session = sess)
        if (nrow(bundles) > 0) {
          for (bndl in bundles$name) {
            annot_path <- file.path(
              db_handle$basePath,
              paste0(sess, "_ses"),
              paste0(bndl, "_bndl"),
              paste0(bndl, "_annot.json")
            )
            if (file.exists(annot_path)) {
              all_bundles[[length(all_bundles) + 1]] <- data.frame(
                session = sess,
                bundle = bndl,
                annot_path = annot_path,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
      if (length(all_bundles) > 0) {
        do.call(rbind, all_bundles)
      } else {
        NULL
      }
    } else {
      detect_annot_changes(db_handle)
    }
    
    results$eaf <- sync_annot_to_eaf(
      db_handle,
      changed_bundles = changed_bundles,
      align_items = config$align_items,
      verbose = verbose
    )
  }
  
  # Sync CMDI
  if (sync_cmdi) {
    if (verbose) {
      cli::cli_h2("Synchronizing CMDI metadata")
    }
    
    results$cmdi <- sync_to_cmdi(
      db_handle,
      force = force,
      profile = config$cmdi_profile %||% "speech-corpus",
      verbose = verbose
    )
  }
  
  invisible(results)
}

# ==============================================================================
# AUTOMATIC SYNC HOOKS
# ==============================================================================

#' Check and sync after database operations
#'
#' This function should be called after operations that modify the database
#'
#' @param db_handle An emuDBhandle object
#' @keywords internal
auto_sync_check <- function(db_handle) {
  config <- load_sync_config(db_handle)
  
  if (is.null(config) || !config$enabled) {
    return(invisible(NULL))
  }
  
  # Run sync quietly
  sync_database(
    db_handle,
    sync_eaf = config$sync_eaf,
    sync_cmdi = config$sync_cmdi,
    force = FALSE,
    verbose = config$verbose
  )
}

#' Wrapper for emuR functions that modify annotations
#'
#' This creates a wrapper that automatically syncs after annotation changes
#'
#' @param db_handle An emuDBhandle object
#' @param fun Function to wrap (e.g., emuR::set_levelCanvasesOrder)
#' @param ... Arguments to pass to the function
#' @keywords internal
with_auto_sync <- function(db_handle, fun, ...) {
  result <- fun(db_handle, ...)
  auto_sync_check(db_handle)
  result
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Load sync config from database path (non-handle version)
#' @keywords internal
load_sync_config_from_path <- function(basePath) {
  sync_config_path <- file.path(basePath, ".sync_config.json")
  
  if (!file.exists(sync_config_path)) {
    return(NULL)
  }
  
  jsonlite::read_json(sync_config_path)
}
