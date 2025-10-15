# ==============================================================================
# OPTIMIZED METADATA MANAGEMENT FOR REINDEER
# ==============================================================================
#
# This module provides efficient metadata management with SQLite caching,
# elegant corpus summaries, and programmatic metadata manipulation.
#

## Constants
metadata.extension <- "meta_json"

# ==============================================================================
# SQLITE SCHEMA FOR METADATA CACHING
# ==============================================================================

#' Initialize metadata tables in SQLite cache
#'
#' Adds metadata-specific tables to the cache database
#' @param con SQLite connection
#' @keywords internal
initialize_metadata_schema <- function(con) {
  
  # metadata_fields table - tracks all known metadata fields and their types
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS metadata_fields (
      field_name TEXT PRIMARY KEY,
      field_type TEXT,  -- 'character', 'numeric', 'logical', 'date'
      first_seen TEXT,  -- timestamp
      last_modified TEXT
    )")
  
  # metadata_bundle table - bundle-level metadata
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS metadata_bundle (
      db_uuid VARCHAR(36),
      session TEXT,
      bundle TEXT,
      field_name TEXT,
      field_value TEXT,  -- stored as JSON-compatible text
      field_type TEXT,
      PRIMARY KEY (db_uuid, session, bundle, field_name),
      FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session, name) 
        ON DELETE CASCADE ON UPDATE CASCADE
    )")
  
  # metadata_session table - session-level defaults
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS metadata_session (
      db_uuid VARCHAR(36),
      session TEXT,
      field_name TEXT,
      field_value TEXT,
      field_type TEXT,
      PRIMARY KEY (db_uuid, session, field_name),
      FOREIGN KEY (db_uuid, session) REFERENCES session(db_uuid, name) 
        ON DELETE CASCADE ON UPDATE CASCADE
    )")
  
  # metadata_database table - database-level defaults
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS metadata_database (
      db_uuid VARCHAR(36),
      field_name TEXT,
      field_value TEXT,
      field_type TEXT,
      PRIMARY KEY (db_uuid, field_name),
      FOREIGN KEY (db_uuid) REFERENCES emu_db(uuid) 
        ON DELETE CASCADE ON UPDATE CASCADE
    )")
  
  # Indices for efficient queries
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_metadata_bundle_field 
    ON metadata_bundle(db_uuid, field_name)")
  
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_metadata_session_field 
    ON metadata_session(db_uuid, field_name)")
}

# ==============================================================================
# METADATA GATHERING FROM .meta_json FILES
# ==============================================================================

#' Gather all metadata from .meta_json files and update SQLite cache
#'
#' Efficiently scans database, session, and bundle .meta_json files
#' and populates the metadata tables in the cache
#' 
#' @param corpus_obj A corpus object
#' @param verbose Show progress messages
#' @export
gather_metadata <- function(corpus_obj, verbose = TRUE) {
  
  if (!"corpus" %in% class(corpus_obj)) {
    cli::cli_abort("Input must be a corpus object")
  }
  
  if (verbose) {
    cli::cli_h2("Gathering metadata from .meta_json files")
  }
  
  basePath <- corpus_obj@basePath
  db_uuid <- get_db_uuid(corpus_obj)
  con <- get_connection(corpus_obj)
  
  # Initialize schema if needed
  initialize_metadata_schema(con)
  
  # Clear existing metadata (we're rebuilding from ground truth)
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_bundle WHERE db_uuid = '%s'", db_uuid))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_session WHERE db_uuid = '%s'", db_uuid))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_database WHERE db_uuid = '%s'", db_uuid))
  
  # 1. Database-level metadata (from <dbname>.meta_json)
  db_name <- basename(basePath)
  db_name <- sub("_emuDB$", "", db_name)
  db_meta_file <- file.path(basePath, paste0(db_name, ".", metadata.extension))
  
  if (file.exists(db_meta_file)) {
    if (verbose) cli::cli_alert_info("Processing database-level defaults")
    db_meta <- jsonlite::read_json(db_meta_file, simplifyVector = TRUE)
    if (length(db_meta) > 0) {
      process_metadata_list(con, db_uuid, NULL, NULL, db_meta, "database")
    }
  }
  
  # 2. Session-level metadata
  sessions <- list_sessions_from_cache(con, db_uuid)
  if (nrow(sessions) > 0 && verbose) {
    cli::cli_alert_info("Processing {nrow(sessions)} session(s)")
  }
  
  for (i in seq_len(nrow(sessions))) {
    session_name <- sessions$name[i]
    session_meta_file <- file.path(basePath, paste0(session_name, "_ses"),
                                   paste0(session_name, ".", metadata.extension))
    
    if (file.exists(session_meta_file)) {
      meta_data <- jsonlite::read_json(session_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, NULL, meta_data, "session")
      }
    }
  }
  
  # 3. Bundle-level metadata
  bundles <- list_bundles_from_cache(con, db_uuid)
  if (nrow(bundles) > 0 && verbose) {
    cli::cli_progress_bar("Processing bundle metadata", total = nrow(bundles))
  }
  
  for (i in seq_len(nrow(bundles))) {
    session_name <- bundles$session[i]
    bundle_name <- bundles$name[i]
    
    bundle_meta_file <- file.path(
      basePath, 
      paste0(session_name, "_ses"),
      paste0(bundle_name, "_bndl"),
      paste0(bundle_name, ".", metadata.extension)
    )
    
    if (file.exists(bundle_meta_file)) {
      meta_data <- jsonlite::read_json(bundle_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, bundle_name, meta_data, "bundle")
      }
    }
    
    if (verbose) cli::cli_progress_update()
  }
  
  if (verbose && nrow(bundles) > 0) {
    cli::cli_progress_done()
    cli::cli_alert_success("Metadata gathering complete")
  }
  
  invisible(corpus_obj)
}

#' Process and insert metadata list into appropriate table
#' @keywords internal
process_metadata_list <- function(con, db_uuid, session, bundle, meta_list, level) {
  
  if (length(meta_list) == 0) return(invisible(NULL))
  
  DBI::dbWithTransaction(con, {
    for (field_name in names(meta_list)) {
      value <- meta_list[[field_name]]
      
      # Determine type and serialize value
      field_info <- serialize_metadata_value(value)
      
      # Register field if new
      register_metadata_field(con, field_name, field_info$type)
      
      # Insert into appropriate table
      if (level == "database") {
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_database (db_uuid, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s')",
          db_uuid, 
          DBI::dbQuoteString(con, field_name),
          DBI::dbQuoteString(con, field_info$value),
          field_info$type
        )
      } else if (level == "session") {
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_session (db_uuid, session, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s', '%s')",
          db_uuid, session,
          DBI::dbQuoteString(con, field_name),
          DBI::dbQuoteString(con, field_info$value),
          field_info$type
        )
      } else if (level == "bundle") {
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_bundle (db_uuid, session, bundle, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
          db_uuid, session, bundle,
          DBI::dbQuoteString(con, field_name),
          DBI::dbQuoteString(con, field_info$value),
          field_info$type
        )
      }
      
      DBI::dbExecute(con, sql)
    }
  })
}

#' Serialize a metadata value for storage
#' @keywords internal
serialize_metadata_value <- function(value) {
  if (is.null(value)) {
    return(list(value = "NULL", type = "NULL"))
  } else if (is.logical(value)) {
    return(list(value = as.character(value), type = "logical"))
  } else if (is.numeric(value)) {
    return(list(value = as.character(value), type = "numeric"))
  } else if (inherits(value, "Date")) {
    return(list(value = as.character(value), type = "date"))
  } else if (inherits(value, "POSIXt")) {
    return(list(value = format(value, "%Y-%m-%dT%H:%M:%S"), type = "datetime"))
  } else {
    return(list(value = as.character(value), type = "character"))
  }
}

#' Deserialize a metadata value from storage
#' @keywords internal
deserialize_metadata_value <- function(value_str, type_str) {
  if (is.na(value_str) || value_str == "NULL") {
    return(NA)
  }
  
  switch(type_str,
    "logical" = as.logical(value_str),
    "numeric" = as.numeric(value_str),
    "integer" = as.integer(value_str),
    "date" = as.Date(value_str),
    "datetime" = as.POSIXct(value_str),
    "character" = value_str,
    value_str  # default
  )
}

#' Register a metadata field in the fields tracking table
#' @keywords internal
register_metadata_field <- function(con, field_name, field_type) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Check if exists
  existing <- DBI::dbGetQuery(con, sprintf(
    "SELECT field_name FROM metadata_fields WHERE field_name = %s",
    DBI::dbQuoteString(con, field_name)
  ))
  
  if (nrow(existing) == 0) {
    # Insert new
    DBI::dbExecute(con, sprintf(
      "INSERT INTO metadata_fields (field_name, field_type, first_seen, last_modified) 
       VALUES (%s, '%s', '%s', '%s')",
      DBI::dbQuoteString(con, field_name), field_type, now, now
    ))
  } else {
    # Update timestamp
    DBI::dbExecute(con, sprintf(
      "UPDATE metadata_fields SET last_modified = '%s' WHERE field_name = %s",
      now, DBI::dbQuoteString(con, field_name)
    ))
  }
}

# ==============================================================================
# EFFICIENT METADATA RETRIEVAL
# ==============================================================================

#' Get complete metadata for all bundles with inheritance
#'
#' Retrieves metadata with proper precedence: bundle > session > database
#' @param corpus_obj A corpus object
#' @param session_pattern Optional regex pattern to filter sessions
#' @param bundle_pattern Optional regex pattern to filter bundles
#' @return A tibble with one row per bundle and columns for all metadata fields
#' @export
get_metadata <- function(corpus_obj, session_pattern = ".*", bundle_pattern = ".*") {
  
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  
  # Get all bundles
  bundles <- list_bundles_from_cache(con, db_uuid)
  
  # Filter by patterns
  if (session_pattern != ".*") {
    bundles <- bundles[grepl(session_pattern, bundles$session), ]
  }
  if (bundle_pattern != ".*") {
    bundles <- bundles[grepl(bundle_pattern, bundles$name), ]
  }
  
  if (nrow(bundles) == 0) {
    return(tibble::tibble())
  }
  
  # Get all metadata fields
  fields <- DBI::dbGetQuery(con, "SELECT field_name FROM metadata_fields ORDER BY field_name")
  
  # Start with bundle identifiers
  result <- bundles %>%
    dplyr::rename(bundle = name) %>%
    dplyr::select(session, bundle)
  
  # For each field, get values with proper precedence
  for (field_name in fields$field_name) {
    field_values <- get_metadata_field(con, db_uuid, field_name, bundles$session, bundles$name)
    result[[field_name]] <- field_values
  }
  
  result
}

#' Get values for a single metadata field across bundles
#' @keywords internal
get_metadata_field <- function(con, db_uuid, field_name, sessions, bundles) {
  
  values <- character(length(sessions))
  
  for (i in seq_along(sessions)) {
    session <- sessions[i]
    bundle <- bundles[i]
    
    # Try bundle level first
    bundle_val <- DBI::dbGetQuery(con, sprintf(
      "SELECT field_value, field_type FROM metadata_bundle 
       WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s' AND field_name = %s",
      db_uuid, session, bundle, DBI::dbQuoteString(con, field_name)
    ))
    
    if (nrow(bundle_val) > 0) {
      values[i] <- bundle_val$field_value[1]
      next
    }
    
    # Try session level
    session_val <- DBI::dbGetQuery(con, sprintf(
      "SELECT field_value, field_type FROM metadata_session 
       WHERE db_uuid = '%s' AND session = '%s' AND field_name = %s",
      db_uuid, session, DBI::dbQuoteString(con, field_name)
    ))
    
    if (nrow(session_val) > 0) {
      values[i] <- session_val$field_value[1]
      next
    }
    
    # Try database level
    db_val <- DBI::dbGetQuery(con, sprintf(
      "SELECT field_value, field_type FROM metadata_database 
       WHERE db_uuid = '%s' AND field_name = %s",
      db_uuid, DBI::dbQuoteString(con, field_name)
    ))
    
    if (nrow(db_val) > 0) {
      values[i] <- db_val$field_value[1]
    } else {
      values[i] <- NA_character_
    }
  }
  
  values
}

# ==============================================================================
# ENHANCED CORPUS SUMMARY
# ==============================================================================

#' Enhanced summary for corpus objects
#'
#' Provides comprehensive information similar to emuR database summary,
#' plus metadata diagnostics
#' 
#' @param object A corpus object
#' @param ... Additional arguments (ignored)
#' @export
summary.corpus <- function(object, ...) {
  
  con <- get_corpus_connection(object)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  db_uuid <- object@.uuid
  config <- object@config
  
  # Header
  cli::cli_rule(left = "Summary of emuDB")
  cat("\n")
  
  # Basic info
  cat(sprintf("%-25s %s\n", "Name:", object@dbName))
  cat(sprintf("%-25s %s\n", "UUID:", db_uuid))
  cat(sprintf("%-25s %s\n", "Directory:", object@basePath))
  
  # Counts from cache
  session_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM session WHERE db_uuid = '%s'", db_uuid
  ))$n
  
  bundle_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM bundle WHERE db_uuid = '%s'", db_uuid
  ))$n
  
  item_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM items WHERE db_uuid = '%s'", db_uuid
  ))$n
  
  label_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM labels WHERE db_uuid = '%s'", db_uuid
  ))$n
  
  link_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM links WHERE db_uuid = '%s'", db_uuid
  ))$n
  
  cat(sprintf("%-25s %d\n", "Session count:", session_count))
  cat(sprintf("%-25s %d\n", "Bundle count:", bundle_count))
  cat(sprintf("%-25s %d\n", "Annotation item count:", item_count))
  cat(sprintf("%-25s %d\n", "Label count:", label_count))
  cat(sprintf("%-25s %d\n", "Link count:", link_count))
  cat("\n")
  
  # Database configuration
  cli::cli_rule(left = "Database configuration")
  cat("\n")
  
  # SSFF track definitions
  if (!is.null(config$ssffTrackDefinitions) && length(config$ssffTrackDefinitions) > 0) {
    cat("SSFF track definitions:\n\n")
    
    ssff_df <- do.call(rbind, lapply(config$ssffTrackDefinitions, function(x) {
      data.frame(
        name = format(x$name %||% "", width = 12),
        columnName = format(x$columnName %||% "", width = 12),
        fileExtension = format(x$fileExtension %||% "", width = 14),
        fileFormat = format(x$fileFormat %||% "", width = 10),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }))
    
    print(ssff_df, row.names = FALSE, right = FALSE)
    cat("\n")
  }
  
  # Level definitions
  if (!is.null(config$levelDefinitions) && length(config$levelDefinitions) > 0) {
    cat("Level definitions:\n\n")
    
    level_df <- do.call(rbind, lapply(config$levelDefinitions, function(ld) {
      attr_names <- paste(sapply(ld$attributeDefinitions %||% list(), function(ad) 
        paste0(ad$name, ";")), collapse = " ")
      
      data.frame(
        name = format(ld$name, width = 13),
        type = format(ld$type, width = 8),
        nrOfAttrDefs = format(length(ld$attributeDefinitions %||% list()), width = 12),
        attrDefNames = attr_names,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }))
    
    print(level_df, row.names = FALSE, right = FALSE)
    cat("\n")
  }
  
  # Link definitions
  if (!is.null(config$linkDefinitions) && length(config$linkDefinitions) > 0) {
    cat("Link definitions:\n\n")
    
    link_df <- do.call(rbind, lapply(config$linkDefinitions, function(ld) {
      data.frame(
        type = format(ld$type, width = 13),
        superlevelName = format(ld$superlevelName, width = 15),
        sublevelName = format(ld$sublevelName, width = 13),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }))
    
    print(link_df, row.names = FALSE, right = FALSE)
    cat("\n")
  }
  
  # Metadata diagnostics
  tryCatch({
    metadata_summary <- get_metadata_diagnostics_internal(con, db_uuid)
    if (!is.null(metadata_summary) && nrow(metadata_summary) > 0) {
      cli::cli_rule(left = "Metadata summary")
      cat("\n")
      print(metadata_summary, row.names = FALSE, right = FALSE)
      cat("\n")
    }
  }, error = function(e) {
    # Silently skip if metadata tables don't exist yet
  })
  
  invisible(object)
}

#' Get metadata diagnostics
#' @keywords internal
get_metadata_diagnostics_internal <- function(con, db_uuid) {
  # Check if metadata tables exist
  tables_exist <- DBI::dbGetQuery(con, 
    "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE 'metadata_%'")
  
  if (nrow(tables_exist) == 0) {
    return(NULL)
  }
  
  # Get field statistics
  fields <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT 
        field_name,
        field_type,
        (SELECT COUNT(DISTINCT session || '/' || bundle) 
         FROM metadata_bundle mb 
         WHERE mb.field_name = mf.field_name AND mb.db_uuid = '%s') as bundle_count,
        (SELECT COUNT(DISTINCT session) 
         FROM metadata_session ms 
         WHERE ms.field_name = mf.field_name AND ms.db_uuid = '%s') as session_count,
        (SELECT COUNT(*) 
         FROM metadata_database md 
         WHERE md.field_name = mf.field_name AND md.db_uuid = '%s') as db_count
      FROM metadata_fields mf
      ORDER BY field_name", db_uuid, db_uuid, db_uuid))
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(fields) || nrow(fields) == 0) {
    return(NULL)
  }
  
  # Determine primary level for each field
  fields$level <- ifelse(fields$bundle_count > 0, "bundle",
                  ifelse(fields$session_count > 0, "session",
                  ifelse(fields$db_count > 0, "database", "none")))
  
  # Format for display
  result <- data.frame(
    Field = format(fields$field_name, width = 20),
    Type = format(fields$field_type, width = 10),
    Level = format(fields$level, width = 10),
    Bundles = format(fields$bundle_count, width = 8),
    Sessions = format(fields$session_count, width = 9),
    Database = format(fields$db_count, width = 9),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  result
}

# ==============================================================================
# PROGRAMMATIC METADATA MANIPULATION
# ==============================================================================

#' Set metadata for corpus, session, or bundle with validation
#'
#' mycorpus["Session1", "Bundle1"] <- list(Age = 25, Sex = "Male")
#' 
#' @param x A corpus object
#' @param i Session name (optional)
#' @param j Bundle name (optional)
#' @param value Named list of metadata values
#' @export
`[<-.corpus` <- function(x, i = NULL, j = NULL, value) {
  
  if (!is.list(value)) {
    cli::cli_abort("Value must be a named list")
  }
  
  if (length(names(value)) == 0 || any(names(value) == "")) {
    cli::cli_abort("All list elements must be named")
  }
  
  session <- if (!missing(i) && !is.null(i)) i else NULL
  bundle <- if (!missing(j) && !is.null(j)) j else NULL
  
  # Determine level
  if (is.null(session) && is.null(bundle)) {
    level <- "database"
  } else if (!is.null(session) && is.null(bundle)) {
    level <- "session"
  } else if (!is.null(session) && !is.null(bundle)) {
    level <- "bundle"
  } else {
    cli::cli_abort("Invalid combination: bundle requires session")
  }
  
  # Validate and set metadata
  set_metadata_validated(x, value, session, bundle, level)
  
  # Update .meta_json files (ground truth)
  write_metadata_to_json(x, value, session, bundle, level)
  
  # Update cache
  con <- get_connection(x)
  db_uuid <- get_db_uuid(x)
  process_metadata_list(con, db_uuid, session, bundle, value, level)
  DBI::dbDisconnect(con)
  
  invisible(x)
}

#' Validate and set metadata with user interaction for unknown fields
#' @keywords internal
set_metadata_validated <- function(corpus_obj, meta_list, session, bundle, level) {
  
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  
  # Get known fields
  known_fields <- DBI::dbGetQuery(con, "SELECT field_name, field_type FROM metadata_fields")
  
  for (field_name in names(meta_list)) {
    value <- meta_list[[field_name]]
    value_info <- serialize_metadata_value(value)
    
    # Check if field is known
    if (field_name %in% known_fields$field_name) {
      # Check type consistency
      existing_type <- known_fields$field_type[known_fields$field_name == field_name]
      
      if (existing_type != value_info$type) {
        # Try to convert
        converted <- tryCatch({
          deserialize_metadata_value(value_info$value, existing_type)
          TRUE
        }, error = function(e) FALSE)
        
        if (!converted) {
          cli::cli_abort(
            "Type mismatch for field {.field {field_name}}: expected {.val {existing_type}}, got {.val {value_info$type}}"
          )
        } else {
          cli::cli_alert_warning(
            "Converted {.field {field_name}} from {value_info$type} to {existing_type}"
          )
        }
      }
    } else {
      # New field - ask for confirmation
      response <- readline(sprintf(
        "Field '%s' is new. Add it as type '%s'? (y/n): ",
        field_name, value_info$type
      ))
      
      if (!tolower(response) %in% c("y", "yes")) {
        cli::cli_abort("Metadata update cancelled")
      }
    }
  }
}

#' Write metadata to .meta_json file (ground truth)
#' @keywords internal
write_metadata_to_json <- function(corpus_obj, meta_list, session, bundle, level) {
  
  basePath <- corpus_obj@basePath
  
  if (level == "database") {
    # Update <dbname>.meta_json
    db_name <- basename(basePath)
    db_name <- sub("_emuDB$", "", db_name)
    meta_file <- file.path(basePath, paste0(db_name, ".", metadata.extension))
    
    # Read existing or create new
    if (file.exists(meta_file)) {
      existing <- jsonlite::read_json(meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    updated <- utils::modifyList(existing, meta_list, keep.null = FALSE)
    jsonlite::write_json(updated, meta_file, auto_unbox = TRUE, pretty = TRUE)
    
  } else if (level == "session") {
    # Update session .meta_json
    meta_file <- file.path(basePath, paste0(session, "_ses"), paste0(session, ".", metadata.extension))
    
    # Read existing or create new
    if (file.exists(meta_file)) {
      existing <- jsonlite::read_json(meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    updated <- utils::modifyList(existing, meta_list, keep.null = FALSE)
    jsonlite::write_json(updated, meta_file, auto_unbox = TRUE, pretty = TRUE)
    
  } else if (level == "bundle") {
    # Update bundle .meta_json
    meta_file <- file.path(
      basePath, 
      paste0(session, "_ses"),
      paste0(bundle, "_bndl"),
      paste0(bundle, ".", metadata.extension)
    )
    
    # Read existing or create new
    if (file.exists(meta_file)) {
      existing <- jsonlite::read_json(meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    updated <- utils::modifyList(existing, meta_list, keep.null = FALSE)
    jsonlite::write_json(updated, meta_file, auto_unbox = TRUE, pretty = TRUE)
  }
}

# ==============================================================================
# EXCEL IMPORT/EXPORT
# ==============================================================================

#' Export metadata to Excel for convenient editing
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Path to Excel file to create
#' @param overwrite Whether to overwrite existing file
#' @param mandatory Vector of column names to ensure are present
#' @export
export_metadata <- function(corpus_obj, Excelfile, overwrite = FALSE, 
                           mandatory = c("Age", "Gender")) {
  
  if (!overwrite && file.exists(Excelfile)) {
    cli::cli_abort("File {.path {Excelfile}} exists. Use overwrite=TRUE to replace.")
  }
  
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  
  # Get complete metadata with inheritance
  bundle_metadata <- get_metadata(corpus_obj)
  
  # Ensure mandatory columns exist
  for (col in mandatory) {
    if (!col %in% names(bundle_metadata)) {
      bundle_metadata[[col]] <- NA
    }
  }
  
  # Get session-level metadata
  sessions <- list_sessions_from_cache(con, db_uuid)
  session_metadata <- tibble::tibble(session = sessions$name)
  
  fields <- DBI::dbGetQuery(con, "SELECT DISTINCT field_name FROM metadata_fields")
  
  for (field_name in fields$field_name) {
    session_vals <- character(nrow(sessions))
    for (i in seq_along(sessions$name)) {
      val <- DBI::dbGetQuery(con, sprintf(
        "SELECT field_value FROM metadata_session 
         WHERE db_uuid = '%s' AND session = '%s' AND field_name = %s",
        db_uuid, sessions$name[i], DBI::dbQuoteString(con, field_name)
      ))
      session_vals[i] <- if (nrow(val) > 0) val$field_value[1] else NA_character_
    }
    session_metadata[[field_name]] <- session_vals
  }
  
  # Ensure mandatory columns in session metadata
  for (col in mandatory) {
    if (!col %in% names(session_metadata)) {
      session_metadata[[col]] <- NA
    }
  }
  
  # Get database-level metadata
  db_metadata <- DBI::dbGetQuery(con, sprintf(
    "SELECT field_name, field_value FROM metadata_database WHERE db_uuid = '%s'",
    db_uuid
  ))
  
  if (nrow(db_metadata) > 0) {
    db_df <- as.data.frame(t(db_metadata$field_value))
    names(db_df) <- db_metadata$field_name
  } else {
    db_df <- data.frame()
  }
  
  # Create Excel workbook
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook(paste(corpus_obj@dbName, "metadata"))
    
    # Bundles sheet
    openxlsx::addWorksheet(wb, "bundles")
    openxlsx::writeDataTable(wb, "bundles", x = bundle_metadata, keepNA = FALSE, withFilter = FALSE)
    openxlsx::freezePane(wb, "bundles", firstActiveCol = 3)
    openxlsx::setColWidths(wb, "bundles", cols = 3:30, widths = 18)
    
    # Sessions sheet
    openxlsx::addWorksheet(wb, "sessions")
    openxlsx::writeDataTable(wb, "sessions", x = session_metadata, keepNA = FALSE, withFilter = FALSE)
    openxlsx::freezePane(wb, "sessions", firstActiveCol = 2)
    openxlsx::setColWidths(wb, "sessions", cols = 2:30, widths = 18)
    
    # Database sheet
    openxlsx::addWorksheet(wb, "database")
    if (nrow(db_df) > 0) {
      openxlsx::writeDataTable(wb, "database", x = db_df, keepNA = FALSE, withFilter = FALSE)
    } else {
      openxlsx::writeComment(wb, "database", col = 1, row = 1,
        openxlsx::createComment(
          "Set database-wide metadata by adding column headers and values",
          author = "Reindeer"
        ))
    }
    
    openxlsx::saveWorkbook(wb, file = Excelfile, overwrite = overwrite)
    cli::cli_alert_success("Metadata exported to {.path {Excelfile}}")
  } else {
    cli::cli_abort("Package {.pkg openxlsx} required for Excel export")
  }
  
  invisible(bundle_metadata)
}

#' Import metadata from Excel file
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Path to Excel file to import
#' @export
import_metadata <- function(corpus_obj, Excelfile) {
  
  if (!file.exists(Excelfile)) {
    cli::cli_abort("File {.path {Excelfile}} does not exist")
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg openxlsx} required for Excel import")
  }
  
  # Read all sheets
  bundle_meta <- openxlsx::read.xlsx(Excelfile, sheet = "bundles", detectDates = TRUE)
  session_meta <- openxlsx::read.xlsx(Excelfile, sheet = "sessions", detectDates = TRUE)
  
  tryCatch({
    db_meta <- openxlsx::read.xlsx(Excelfile, sheet = "database", detectDates = TRUE)
  }, error = function(e) {
    db_meta <<- data.frame()
  })
  
  # Process bundle metadata
  for (i in seq_len(nrow(bundle_meta))) {
    session <- bundle_meta$session[i]
    bundle <- bundle_meta$bundle[i]
    
    meta_list <- as.list(bundle_meta[i, !names(bundle_meta) %in% c("session", "bundle")])
    meta_list <- meta_list[!is.na(meta_list)]
    
    if (length(meta_list) > 0) {
      write_metadata_to_json(corpus_obj, meta_list, session, bundle, "bundle")
    }
  }
  
  # Process session metadata
  for (i in seq_len(nrow(session_meta))) {
    session <- session_meta$session[i]
    
    meta_list <- as.list(session_meta[i, !names(session_meta) %in% c("session")])
    meta_list <- meta_list[!is.na(meta_list)]
    
    if (length(meta_list) > 0) {
      write_metadata_to_json(corpus_obj, meta_list, session, NULL, "session")
    }
  }
  
  # Process database metadata
  if (nrow(db_meta) > 0) {
    meta_list <- as.list(db_meta[1, ])
    meta_list <- meta_list[!is.na(meta_list)]
    
    if (length(meta_list) > 0) {
      write_metadata_to_json(corpus_obj, meta_list, NULL, NULL, "database")
    }
  }
  
  # Rebuild cache
  cli::cli_alert_info("Rebuilding metadata cache...")
  gather_metadata(corpus_obj, verbose = FALSE)
  
  cli::cli_alert_success("Metadata imported from {.path {Excelfile}}")
  
  invisible(corpus_obj)
}

#' Add metadata programmatically
#'
#' @param corpus_obj A corpus object
#' @param metadataList Named list of metadata
#' @param session Optional session name
#' @param bundle Optional bundle name  
#' @param reset.before.add Clear existing metadata first
#' @export
add_metadata <- function(corpus_obj, metadataList, session = NULL, bundle = NULL, 
                        reset.before.add = FALSE) {
  
  # Determine level
  if (is.null(session) && is.null(bundle)) {
    level <- "database"
  } else if (!is.null(session) && is.null(bundle)) {
    level <- "session"
  } else if (!is.null(session) && !is.null(bundle)) {
    level <- "bundle"
  } else {
    cli::cli_abort("Bundle requires session")
  }
  
  if (reset.before.add) {
    # Clear existing metadata at this level
    clear_metadata(corpus_obj, session, bundle, level)
  }
  
  # Write to JSON files (ground truth)
  write_metadata_to_json(corpus_obj, metadataList, session, bundle, level)
  
  # Update cache
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  process_metadata_list(con, db_uuid, session, bundle, metadataList, level)
  DBI::dbDisconnect(con)
  
  invisible(corpus_obj)
}

#' Clear metadata at a specific level
#' @keywords internal
clear_metadata <- function(corpus_obj, session, bundle, level) {
  basePath <- corpus_obj@basePath
  
  if (level == "database") {
    # Delete <dbname>.meta_json
    db_name <- basename(basePath)
    db_name <- sub("_emuDB$", "", db_name)
    meta_file <- file.path(basePath, paste0(db_name, ".", metadata.extension))
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }
    
  } else if (level == "session") {
    meta_file <- file.path(basePath, paste0(session, "_ses"), paste0(session, ".", metadata.extension))
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }
    
  } else if (level == "bundle") {
    meta_file <- file.path(
      basePath, 
      paste0(session, "_ses"),
      paste0(bundle, "_bndl"),
      paste0(bundle, ".", metadata.extension)
    )
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }
  }
}

#' Add biographical metadata to segment list (biographize)
#'
#' @param segs_tbl Tibble from query
#' @param corpus_obj Corpus object
#' @param compute_digests Compute file checksums
#' @param algorithm Hash algorithm
#' @export
biographize <- function(segs_tbl, corpus_obj, compute_digests = FALSE, algorithm = "sha1") {
  
  if (!is.data.frame(segs_tbl) || !all(c("session", "bundle") %in% names(segs_tbl))) {
    cli::cli_abort("Input must be a data.frame with 'session' and 'bundle' columns")
  }
  
  if (compute_digests) {
    add_digests(corpus_obj, algorithm = algorithm)
  }
  
  # Get metadata
  metadata <- get_metadata(corpus_obj)
  
  # Join with segment list
  result <- segs_tbl %>%
    dplyr::left_join(metadata, by = c("session", "bundle"))
  
  result
}

#' Add file digests to metadata
#' @param corpus_obj Corpus object
#' @param sessionPattern Session pattern
#' @param bundlePattern Bundle pattern
#' @param algorithm Hash algorithm
#' @export
add_digests <- function(corpus_obj, sessionPattern = ".*", bundlePattern = ".*", 
                       algorithm = "sha1") {
  
  # This would require emuR functions to list files
  # Simplified version for now
  cli::cli_alert_info("Digest computation requires emuR integration")
  invisible(corpus_obj)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get database UUID from corpus object
#' @keywords internal
get_db_uuid <- function(corpus_obj) {
  config <- load_DBconfig(corpus_obj)
  config$UUID
}

#' Get connection from corpus object
#' @keywords internal
get_connection <- function(corpus_obj) {
  cache_path <- file.path(corpus_obj@basePath, paste0(corpus_obj@dbName, "_emuDBcache.sqlite"))
  
  if (!file.exists(cache_path)) {
    cli::cli_abort("Cache file not found. Run build_emuDB_cache() first.")
  }
  
  DBI::dbConnect(RSQLite::SQLite(), cache_path)
}

#' List sessions from cache
#' @keywords internal
list_sessions_from_cache <- function(con, db_uuid) {
  DBI::dbGetQuery(con, sprintf(
    "SELECT name FROM session WHERE db_uuid = '%s'", db_uuid
  ))
}

#' List bundles from cache
#' @keywords internal
list_bundles_from_cache <- function(con, db_uuid) {
  DBI::dbGetQuery(con, sprintf(
    "SELECT session, name FROM bundle WHERE db_uuid = '%s'", db_uuid
  ))
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
