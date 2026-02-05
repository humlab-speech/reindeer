# ==============================================================================
# CORPUS CREATION UTILITIES
# ==============================================================================

#' Validate session or bundle name
#'
#' Checks if a name is valid for use as a session or bundle identifier.
#' Valid names must:
#' - Not be empty
#' - Not contain regex special characters (when used for creation)
#' - Not contain path separators
#' - Not start or end with whitespace
#'
#' @param name Character string to validate
#' @param type Either "session" or "bundle" for error messages
#' @param allow_regex Logical; if TRUE, regex patterns are allowed (for queries)
#' @return TRUE if valid, otherwise aborts with error
#' @keywords internal
validate_name <- function(name, type = "name", allow_regex = FALSE) {
  assertthat::assert_that(
    assertthat::is.string(name),
    msg = sprintf("%s must be a single character string", type)
  )
  
  assertthat::assert_that(
    nchar(name) > 0,
    msg = sprintf("%s cannot be empty", type)
  )
  
  # Check for regex special characters (unless allowed)
  if (!allow_regex) {
    regex_chars <- "[.*+?^${}()|\\[\\]]"
    if (grepl(regex_chars, name)) {
      cli::cli_abort(c(
        "{type} contains regex special characters",
        "x" = "Found pattern characters in: {.val {name}}",
        "i" = "Use literal names for creation: letters, numbers, underscore, hyphen"
      ))
    }
  }
  
  # Check for path separators
  if (grepl("[/\\\\]", name)) {
    cli::cli_abort(c(
      "{type} cannot contain path separators",
      "x" = "Invalid {type}: {.val {name}}"
    ))
  }
  
  # Check for leading/trailing whitespace
  if (grepl("^\\s|\\s$", name)) {
    cli::cli_abort(c(
      "{type} cannot start or end with whitespace",
      "x" = "Invalid {type}: {.val {name}}"
    ))
  }
  
  # Warn about potentially problematic characters
  if (grepl("\\s", name)) {
    cli::cli_alert_warning("{type} contains spaces: {.val {name}}. This may cause issues in some contexts.")
  }
  
  TRUE
}

#' Create a new Emu database structure
#'
#' Creates a new EMU-SDMS database with standard structure using emuR.
#' This is an internal helper for corpus() constructor with create=TRUE.
#'
#' @param path Path where database should be created (must end with _emuDB)
#' @param db_name Database name (extracted from path)
#' @param verbose Show progress messages
#' @return Path to created database
#' @keywords internal
create_new_emuDB <- function(path, db_name, verbose = FALSE) {
  # Ensure parent directory exists
  parent_dir <- dirname(path)
  if (!dir.exists(parent_dir)) {
    if (verbose) {
      cli::cli_alert_info("Creating directory: {.path {parent_dir}}")
    }
    dir.create(parent_dir, recursive = TRUE)
  }
  
  if (verbose) {
    cli::cli_h2("Creating new EMU database: {.field {db_name}}")
  }
  
  # Use emuR to create database structure
  tryCatch({
    emuR::create_emuDB(
      name = db_name,
      targetDir = parent_dir,
      verbose = verbose
    )
    
    if (verbose) {
      cli::cli_alert_success("Database structure created at {.path {path}}")
    }
    
    return(path)
    
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to create EMU database",
      "x" = as.character(e),
      "i" = "Check that emuR is installed and the path is writable"
    ))
  })
}

#' Create a new session and bundle in existing corpus
#'
#' Creates the directory structure and minimal annotation files for a new
#' session and bundle within an existing corpus.
#'
#' @param corpus_obj A corpus object
#' @param session_name Name for the new session (will be validated)
#' @param bundle_name Name for the new bundle (will be validated)
#' @param verbose Show progress messages
#' @return Invisibly returns TRUE on success
#' @keywords internal
create_session_and_bundle <- function(corpus_obj, session_name, bundle_name, verbose = TRUE) {
  # Validate names
  validate_name(session_name, "Session name", allow_regex = FALSE)
  validate_name(bundle_name, "Bundle name", allow_regex = FALSE)
  
  # Create session directory if it doesn't exist
  session_dir <- file.path(corpus_obj@basePath, paste0(session_name, "_ses"))
  
  if (!dir.exists(session_dir)) {
    if (verbose) {
      cli::cli_alert_info("Creating session: {.field {session_name}}")
    }
    dir.create(session_dir, recursive = TRUE)
    
    # Add session to cache
    con <- get_corpus_connection(corpus_obj)
    tryCatch({
      DBI::dbExecute(con, sprintf(
        "INSERT OR IGNORE INTO session (db_uuid, name) VALUES ('%s', '%s')",
        corpus_obj@.uuid, session_name
      ))
    }, finally = {
      DBI::dbDisconnect(con)
    })
  }
  
  # Create bundle directory
  bundle_dir <- file.path(session_dir, paste0(bundle_name, "_bndl"))
  
  if (dir.exists(bundle_dir)) {
    cli::cli_alert_warning("Bundle {.field {bundle_name}} already exists in session {.field {session_name}}")
    return(invisible(FALSE))
  }
  
  if (verbose) {
    cli::cli_alert_info("Creating bundle: {.field {bundle_name}}")
  }
  dir.create(bundle_dir, recursive = TRUE)
  
  # Create minimal annotation JSON
  annot <- list(
    name = bundle_name,
    annotates = "",  # Will be set when media is imported
    sampleRate = 16000,  # Default, will be updated from actual media
    levels = list()
  )
  
  annot_file <- file.path(bundle_dir, paste0(bundle_name, "_annot.json"))
  jsonlite::write_json(
    annot, 
    annot_file,
    auto_unbox = TRUE, 
    pretty = TRUE
  )
  
  # Add bundle to cache
  con <- get_corpus_connection(corpus_obj)
  tryCatch({
    DBI::dbExecute(con, sprintf(
      "INSERT OR IGNORE INTO bundle (db_uuid, session, name, annotates, sample_rate, md5_annot_json) 
       VALUES ('%s', '%s', '%s', '', 16000, '%s')",
      corpus_obj@.uuid, 
      session_name, 
      bundle_name,
      as.character(tools::md5sum(annot_file))
    ))
  }, finally = {
    DBI::dbDisconnect(con)
  })
  
  if (verbose) {
    cli::cli_alert_success("Created {.field {session_name}}/{.field {bundle_name}}")
  }
  
  invisible(TRUE)
}

#' Check if a session exists in corpus
#'
#' @param corpus_obj A corpus object
#' @param session_name Session name to check
#' @return Logical; TRUE if session exists
#' @keywords internal
session_exists <- function(corpus_obj, session_name) {
  session_dir <- file.path(corpus_obj@basePath, paste0(session_name, "_ses"))
  dir.exists(session_dir)
}

#' Check if a bundle exists in session
#'
#' @param corpus_obj A corpus object
#' @param session_name Session name
#' @param bundle_name Bundle name to check
#' @return Logical; TRUE if bundle exists
#' @keywords internal
bundle_exists <- function(corpus_obj, session_name, bundle_name) {
  bundle_dir <- file.path(
    corpus_obj@basePath, 
    paste0(session_name, "_ses"),
    paste0(bundle_name, "_bndl")
  )
  dir.exists(bundle_dir)
}
