#' Auto-sync Wrappers for Database Modification Functions
#'
#' These functions wrap emuR database modification functions and trigger
#' automatic synchronization of EAF and CMDI files when changes occur.

# ==============================================================================
# BUNDLE METADATA UPDATES
# ==============================================================================

#' Write bundle metadata with auto-sync
#'
#' Updates .meta_json file for a bundle and triggers CMDI sync if enabled
#'
#' @param db_handle An emuDBhandle object
#' @param session Session name
#' @param bundle Bundle name
#' @param metadata List of metadata to write
#' @param merge Logical; merge with existing metadata (default TRUE)
#' @param trigger_sync Logical; trigger auto-sync after write (default TRUE)
#' @param verbose Logical; show messages
#'
#' @export
write_bundle_metadata <- function(db_handle,
                                   session,
                                   bundle,
                                   metadata,
                                   merge = TRUE,
                                   trigger_sync = TRUE,
                                   verbose = FALSE) {
  
  emuR:::check_emuDBhandle(db_handle)
  
  # Construct path to bundle directory
  bundle_dir <- file.path(
    db_handle$basePath,
    paste0(session, "_ses"),
    paste0(bundle, "_bndl")
  )
  
  if (!dir.exists(bundle_dir)) {
    stop("Bundle directory not found: ", bundle_dir)
  }
  
  meta_path <- file.path(bundle_dir, metadata.filename)
  
  # Load existing metadata if merging
  if (merge && file.exists(meta_path)) {
    existing <- jsonlite::read_json(meta_path, simplifyVector = FALSE)
    # Deep merge
    metadata <- modifyList(existing, metadata)
  }
  
  # Write metadata
  jsonlite::write_json(
    metadata,
    meta_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  if (verbose) {
    cli::cli_alert_success("Metadata written for {session}/{bundle}")
  }
  
  # Trigger auto-sync
  if (trigger_sync) {
    auto_sync_check(db_handle)
  }
  
  invisible(meta_path)
}

#' Write session metadata with auto-sync
#'
#' Updates .meta_json file for a session and triggers CMDI sync if enabled
#'
#' @param db_handle An emuDBhandle object
#' @param session Session name
#' @param metadata List of metadata to write
#' @param merge Logical; merge with existing metadata (default TRUE)
#' @param trigger_sync Logical; trigger auto-sync after write (default TRUE)
#' @param verbose Logical; show messages
#'
#' @export
write_session_metadata <- function(db_handle,
                                    session,
                                    metadata,
                                    merge = TRUE,
                                    trigger_sync = TRUE,
                                    verbose = FALSE) {
  
  emuR:::check_emuDBhandle(db_handle)
  
  # Construct path to session directory
  session_dir <- file.path(
    db_handle$basePath,
    paste0(session, "_ses")
  )
  
  if (!dir.exists(session_dir)) {
    stop("Session directory not found: ", session_dir)
  }
  
  meta_path <- file.path(session_dir, ".meta_json")
  
  # Load existing metadata if merging
  if (merge && file.exists(meta_path)) {
    existing <- jsonlite::read_json(meta_path, simplifyVector = FALSE)
    # Deep merge
    metadata <- modifyList(existing, metadata)
  }
  
  # Write metadata
  jsonlite::write_json(
    metadata,
    meta_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  if (verbose) {
    cli::cli_alert_success("Metadata written for session {session}")
  }
  
  # Trigger auto-sync
  if (trigger_sync) {
    auto_sync_check(db_handle)
  }
  
  invisible(meta_path)
}

# ==============================================================================
# ANNOTATION MODIFICATION WRAPPERS
# ==============================================================================

#' Save annotation with auto-sync
#'
#' Wrapper for saving annotations that triggers EAF sync
#'
#' @param db_handle An emuDBhandle object
#' @param session Session name
#' @param bundle Bundle name
#' @param annotation Annotation data (from emuR)
#' @param trigger_sync Logical; trigger auto-sync after save
#' @param verbose Logical; show messages
#'
#' @keywords internal
save_annotation_with_sync <- function(db_handle,
                                      session,
                                      bundle,
                                      annotation,
                                      trigger_sync = TRUE,
                                      verbose = FALSE) {
  
  # This assumes annotation is the loaded annotation data structure
  # that needs to be saved back to _annot.json
  
  annot_path <- file.path(
    db_handle$basePath,
    paste0(session, "_ses"),
    paste0(bundle, "_bndl"),
    paste0(bundle, "_annot.json")
  )
  
  # Save annotation
  jsonlite::write_json(
    annotation,
    annot_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  if (verbose) {
    cli::cli_alert_success("Annotation saved for {session}/{bundle}")
  }
  
  # Trigger auto-sync (will update EAF)
  if (trigger_sync) {
    config <- load_sync_config(db_handle)
    if (!is.null(config) && config$enabled && config$sync_eaf) {
      # Sync just this bundle
      changed <- data.frame(
        session = session,
        bundle = bundle,
        annot_path = annot_path,
        stringsAsFactors = FALSE
      )
      
      sync_annot_to_eaf(
        db_handle,
        changed_bundles = changed,
        align_items = config$align_items,
        verbose = verbose
      )
    }
  }
  
  invisible(annot_path)
}

# ==============================================================================
# DATABASE STRUCTURE MODIFICATION WRAPPERS
# ==============================================================================

#' Add session with auto-sync
#'
#' Wrapper for emuR::add_session that triggers CMDI sync
#'
#' @param db_handle An emuDBhandle object
#' @param name Session name
#' @param trigger_sync Logical; trigger auto-sync after adding
#' @param verbose Logical; show messages
#'
#' @export
add_session_with_sync <- function(db_handle,
                                   name,
                                   trigger_sync = TRUE,
                                   verbose = FALSE) {
  
  # Use emuR's internal function
  emuR:::add_sessionDBI(db_handle, sessionName = name)
  
  # Create session directory
  session_dir <- file.path(db_handle$basePath, paste0(name, "_ses"))
  if (!dir.exists(session_dir)) {
    dir.create(session_dir)
  }
  
  if (verbose) {
    cli::cli_alert_success("Session added: {name}")
  }
  
  # Trigger auto-sync
  if (trigger_sync) {
    config <- load_sync_config(db_handle)
    if (!is.null(config) && config$enabled && config$sync_cmdi) {
      sync_to_cmdi(
        db_handle,
        force = FALSE,
        profile = config$cmdi_profile,
        verbose = verbose
      )
    }
  }
  
  invisible(name)
}

#' Remove session with auto-sync
#'
#' Wrapper for session removal that triggers CMDI sync
#'
#' @param db_handle An emuDBhandle object
#' @param name Session name
#' @param trigger_sync Logical; trigger auto-sync after removal
#' @param verbose Logical; show messages
#'
#' @export
remove_session_with_sync <- function(db_handle,
                                      name,
                                      trigger_sync = TRUE,
                                      verbose = FALSE) {
  
  # Remove from database
  DBI::dbExecute(
    db_handle$connection,
    "DELETE FROM session WHERE db_uuid = ? AND name = ?",
    params = list(db_handle$UUID, name)
  )
  
  # Remove directory
  session_dir <- file.path(db_handle$basePath, paste0(name, "_ses"))
  if (dir.exists(session_dir)) {
    unlink(session_dir, recursive = TRUE)
  }
  
  if (verbose) {
    cli::cli_alert_success("Session removed: {name}")
  }
  
  # Trigger auto-sync
  if (trigger_sync) {
    config <- load_sync_config(db_handle)
    if (!is.null(config) && config$enabled && config$sync_cmdi) {
      sync_to_cmdi(
        db_handle,
        force = TRUE,  # Force because structure changed
        profile = config$cmdi_profile,
        verbose = verbose
      )
    }
  }
  
  invisible(NULL)
}

# ==============================================================================
# CONFIGURATION MODIFICATION WRAPPER
# ==============================================================================

#' Update database configuration with auto-sync
#'
#' Wrapper for configuration updates that triggers CMDI sync
#'
#' @param db_handle An emuDBhandle object
#' @param config New or modified configuration
#' @param trigger_sync Logical; trigger auto-sync after update
#' @param verbose Logical; show messages
#'
#' @export
update_config_with_sync <- function(db_handle,
                                     config,
                                     trigger_sync = TRUE,
                                     verbose = FALSE) {
  
  # Save configuration
  config_path <- file.path(
    db_handle$basePath,
    paste0(db_handle$dbName, "_DBconfig.json")
  )
  
  jsonlite::write_json(
    config,
    config_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  if (verbose) {
    cli::cli_alert_success("Database configuration updated")
  }
  
  # Reload database handle config
  db_handle$config <- config
  
  # Trigger auto-sync
  if (trigger_sync) {
    config_sync <- load_sync_config(db_handle)
    if (!is.null(config_sync) && config_sync$enabled && config_sync$sync_cmdi) {
      sync_to_cmdi(
        db_handle,
        force = TRUE,  # Force because config changed
        profile = config_sync$cmdi_profile,
        verbose = verbose
      )
    }
  }
  
  invisible(config)
}

# ==============================================================================
# BULK OPERATIONS
# ==============================================================================

#' Batch update bundle metadata
#'
#' Update metadata for multiple bundles at once
#'
#' @param db_handle An emuDBhandle object
#' @param updates List of lists, each with session, bundle, and metadata
#' @param trigger_sync Logical; trigger auto-sync after all updates
#' @param verbose Logical; show progress
#'
#' @export
batch_update_metadata <- function(db_handle,
                                   updates,
                                   trigger_sync = TRUE,
                                   verbose = TRUE) {
  
  if (verbose) {
    cli::cli_alert_info("Updating metadata for {length(updates)} bundle(s)...")
  }
  
  for (i in seq_along(updates)) {
    update <- updates[[i]]
    
    write_bundle_metadata(
      db_handle,
      session = update$session,
      bundle = update$bundle,
      metadata = update$metadata,
      merge = update$merge %||% TRUE,
      trigger_sync = FALSE,  # Don't sync individually
      verbose = FALSE
    )
    
    if (verbose && i %% 10 == 0) {
      cli::cli_alert_info("  {i}/{length(updates)} completed")
    }
  }
  
  if (verbose) {
    cli::cli_alert_success("Batch update complete")
  }
  
  # Single sync at the end
  if (trigger_sync) {
    auto_sync_check(db_handle)
  }
  
  invisible(length(updates))
}
