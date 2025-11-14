# ==============================================================================
# OPTIMIZED METADATA MANAGEMENT FOR REINDEER
# ==============================================================================
#
# This module provides efficient metadata management with SQLite caching,
# elegant corpus summaries, and programmatic metadata manipulation.
#

## Constants
# New simplified metadata structure: all levels use METADATA.json
# Placed in: database root, session directory, or bundle directory
metadata.filename <- "METADATA.json"

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

#' Gather all metadata from .meta_json files and update SQLite cache - OPTIMIZED
#'
#' Efficiently scans database, session, and bundle .meta_json files
#' and populates the metadata tables in the cache using bulk operations
#' 
#' @param corpus_obj A corpus object
#' @param verbose Show progress messages
#' @param parallel Use parallel processing for bundle metadata (default: TRUE)
#' @export
gather_metadata <- function(corpus_obj, verbose = TRUE, parallel = TRUE) {

  if (!S7::S7_inherits(corpus_obj, corpus)) {
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
  
  # 1. Database-level metadata (from METADATA.json in database root)
  db_name <- basename(basePath)
  db_name <- sub("_emuDB$", "", db_name)
  db_meta_file <- file.path(basePath, metadata.filename)
  
  if (file.exists(db_meta_file)) {
    if (verbose) cli::cli_alert_info("Processing database-level defaults")
    db_meta <- read_json_fast(db_meta_file, simplifyVector = TRUE)
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
                                   metadata.filename)
    
    if (file.exists(session_meta_file)) {
      meta_data <- read_json_fast(session_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, NULL, meta_data, "session")
      }
    }
  }
  
  # 3. Bundle-level metadata - OPTIMIZED with optional parallel processing
  bundles <- list_bundles_from_cache(con, db_uuid)
  
  if (nrow(bundles) == 0) {
    if (verbose) cli::cli_alert_success("Metadata gathering complete")
    return(invisible(corpus_obj))
  }
  
  if (verbose) {
    cli::cli_progress_bar("Processing bundle metadata", total = nrow(bundles))
  }
  
  # Prepare file paths
  bundle_files <- file.path(
    basePath,
    paste0(bundles$session, "_ses"),
    paste0(bundles$name, "_bndl"),
    metadata.filename
  )
  
  # Filter to existing files
  exists_idx <- file.exists(bundle_files)
  existing_files <- bundle_files[exists_idx]
  existing_bundles <- bundles[exists_idx, ]
  
  if (length(existing_files) == 0) {
    if (verbose) {
      cli::cli_progress_done()
      cli::cli_alert_success("Metadata gathering complete (no bundle metadata found)")
    }
    return(invisible(corpus_obj))
  }
  
  # OPTIMIZATION: Use parallel processing for large databases
  use_parallel <- parallel && length(existing_files) > 50 && requireNamespace("future.apply", quietly = TRUE)
  
  if (use_parallel) {
    # Set up parallel processing
    orig_plan <- future::plan()
    future::plan(future::multisession, workers = min(4, parallel::detectCores() - 1))
    on.exit(future::plan(orig_plan), add = TRUE)
    
    # Read all files in parallel
    all_metadata <- future.apply::future_lapply(existing_files, function(f) {
      tryCatch({
        read_json_fast(f, simplifyVector = TRUE)
      }, error = function(e) {
        list()
      })
    }, future.seed = TRUE)
    
  } else {
    # Sequential reading (for small databases or if future not available)
    all_metadata <- lapply(existing_files, function(f) {
      tryCatch({
        read_json_fast(f, simplifyVector = TRUE)
      }, error = function(e) {
        list()
      })
    })
  }
  
  # OPTIMIZATION: Bulk process all bundle metadata in a single transaction
  # This is MUCH faster than individual inserts
  DBI::dbWithTransaction(con, {
    # Collect all metadata records
    all_records <- list()
    
    for (i in seq_along(all_metadata)) {
      meta_data <- all_metadata[[i]]
      
      if (length(meta_data) > 0) {
        session_name <- existing_bundles$session[i]
        bundle_name <- existing_bundles$name[i]
        
        for (field_name in names(meta_data)) {
          field_info <- serialize_metadata_value(meta_data[[field_name]])
          
          all_records[[length(all_records) + 1]] <- data.frame(
            db_uuid = db_uuid,
            session = session_name,
            bundle = bundle_name,
            field_name = field_name,
            field_value = field_info$value,
            field_type = field_info$type,
            stringsAsFactors = FALSE
          )
          
          # Register field
          register_metadata_field(con, field_name, field_info$type)
        }
      }
      
      if (verbose && i %% 10 == 0) {
        cli::cli_progress_update(set = i)
      }
    }
    
    # Bulk insert all records at once
    if (length(all_records) > 0) {
      combined_records <- do.call(rbind, all_records)
      DBI::dbWriteTable(con, "metadata_bundle", combined_records, 
                       append = TRUE, overwrite = FALSE)
    }
  })
  
  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("Metadata gathering complete")
  }
  
  invisible(corpus_obj)
}

#' Process and insert metadata list into appropriate table - OPTIMIZED BULK INSERT
#' @keywords internal
process_metadata_list <- function(con, db_uuid, session, bundle, meta_list, level) {
  
  if (length(meta_list) == 0) return(invisible(NULL))
  
  # Prepare all data first, then do bulk insert
  field_names <- names(meta_list)
  n_fields <- length(field_names)
  
  # Pre-allocate vectors for bulk operations
  field_values <- character(n_fields)
  field_types <- character(n_fields)
  
  # Serialize all values at once
  for (i in seq_along(field_names)) {
    field_info <- serialize_metadata_value(meta_list[[field_names[i]]])
    field_values[i] <- field_info$value
    field_types[i] <- field_info$type
  }
  
  DBI::dbWithTransaction(con, {
    # Register all fields at once
    for (i in seq_along(field_names)) {
      register_metadata_field(con, field_names[i], field_types[i])
    }
    
    # Build data frame for bulk insert
    if (level == "database") {
      insert_df <- data.frame(
        db_uuid = rep(db_uuid, n_fields),
        field_name = field_names,
        field_value = field_values,
        field_type = field_types,
        stringsAsFactors = FALSE
      )
      DBI::dbWriteTable(con, "metadata_database", insert_df, 
                       append = TRUE, overwrite = FALSE)
    } else if (level == "session") {
      insert_df <- data.frame(
        db_uuid = rep(db_uuid, n_fields),
        session = rep(session, n_fields),
        field_name = field_names,
        field_value = field_values,
        field_type = field_types,
        stringsAsFactors = FALSE
      )
      DBI::dbWriteTable(con, "metadata_session", insert_df,
                       append = TRUE, overwrite = FALSE)
    } else if (level == "bundle") {
      insert_df <- data.frame(
        db_uuid = rep(db_uuid, n_fields),
        session = rep(session, n_fields),
        bundle = rep(bundle, n_fields),
        field_name = field_names,
        field_value = field_values,
        field_type = field_types,
        stringsAsFactors = FALSE
      )
      DBI::dbWriteTable(con, "metadata_bundle", insert_df,
                       append = TRUE, overwrite = FALSE)
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

#' Get complete metadata for all bundles with inheritance - OPTIMIZED
#'
#' Retrieves metadata with proper precedence: bundle > session > database
#' Uses a single efficient SQL query instead of looping
#' @param corpus_obj A corpus object
#' @param session_pattern Optional regex pattern to filter sessions
#' @param bundle_pattern Optional regex pattern to filter bundles
#' @return A data.table with one row per bundle and columns for all metadata fields
#' @export
get_metadata <- function(corpus_obj, session_pattern = ".*", bundle_pattern = ".*") {
  
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  
  # Get all bundles
  bundles_dt <- data.table::setDT(list_bundles_from_cache(con, db_uuid))
  data.table::setnames(bundles_dt, "name", "bundle")
  
  # Filter by patterns
  if (session_pattern != ".*") {
    bundles_dt <- bundles_dt[grepl(session_pattern, session)]
  }
  if (bundle_pattern != ".*") {
    bundles_dt <- bundles_dt[grepl(bundle_pattern, bundle)]
  }
  
  if (nrow(bundles_dt) == 0) {
    return(data.table::data.table())
  }
  
  # OPTIMIZATION: Single mega-query using PIVOT to get all metadata at once
  # This is MUCH faster than looping through fields
  query <- sprintf("
    WITH all_metadata AS (
      -- Bundle-level metadata
      SELECT 
        session, bundle, field_name, field_value, 1 as priority
      FROM metadata_bundle
      WHERE db_uuid = '%s'
      
      UNION ALL
      
      -- Session-level metadata  
      SELECT 
        ms.session, b.name as bundle, ms.field_name, ms.field_value, 2 as priority
      FROM metadata_session ms
      CROSS JOIN bundle b
      WHERE ms.db_uuid = '%s' AND b.db_uuid = '%s' AND b.session = ms.session
      
      UNION ALL
      
      -- Database-level metadata
      SELECT
        b.session, b.name as bundle, md.field_name, md.field_value, 3 as priority
      FROM metadata_database md
      CROSS JOIN bundle b
      WHERE md.db_uuid = '%s' AND b.db_uuid = '%s'
    ),
    ranked_metadata AS (
      SELECT 
        session, bundle, field_name, field_value,
        ROW_NUMBER() OVER (PARTITION BY session, bundle, field_name ORDER BY priority) as rn
      FROM all_metadata
    )
    SELECT session, bundle, field_name, field_value
    FROM ranked_metadata
    WHERE rn = 1
  ", db_uuid, db_uuid, db_uuid, db_uuid, db_uuid)
  
  # Execute query - get all metadata in one go
  metadata_long <- data.table::setDT(DBI::dbGetQuery(con, query))
  
  if (nrow(metadata_long) == 0) {
    # No metadata at all
    return(bundles_dt[, .(session, bundle)])
  }
  
  # Convert from long to wide format using data.table's dcast (very fast)
  metadata_wide <- data.table::dcast(
    metadata_long,
    session + bundle ~ field_name,
    value.var = "field_value",
    fun.aggregate = function(x) x[1]  # Take first value if duplicates
  )
  
  # Join with bundles to ensure all bundles are present (even those without metadata)
  result <- metadata_wide[bundles_dt[, .(session, bundle)], on = .(session, bundle)]
  
  # Reorder columns: session, bundle, then alphabetically
  meta_cols <- setdiff(names(result), c("session", "bundle"))
  data.table::setcolorder(result, c("session", "bundle", sort(meta_cols)))
  
  result
}

#' Get values for a single metadata field across bundles - OPTIMIZED
#' 
#' Uses a single SQL query with COALESCE to get values with proper inheritance
#' @keywords internal
get_metadata_field <- function(con, db_uuid, field_name, sessions, bundles) {
  
  # Build single efficient query using COALESCE for precedence
  # This replaces hundreds of individual queries with one query
  query <- sprintf("
    WITH bundle_session_pairs AS (
      SELECT ? as session, ? as bundle
      UNION ALL SELECT ?, ?
      %s
    )
    SELECT 
      bsp.session,
      bsp.bundle,
      COALESCE(
        mb.field_value,
        ms.field_value,
        md.field_value
      ) as field_value
    FROM bundle_session_pairs bsp
    LEFT JOIN metadata_bundle mb 
      ON mb.db_uuid = '%s' 
      AND mb.session = bsp.session 
      AND mb.bundle = bsp.bundle
      AND mb.field_name = %s
    LEFT JOIN metadata_session ms
      ON ms.db_uuid = '%s'
      AND ms.session = bsp.session
      AND ms.field_name = %s  
    LEFT JOIN metadata_database md
      ON md.db_uuid = '%s'
      AND md.field_name = %s
    ORDER BY bsp.rowid",
    paste(rep("UNION ALL SELECT ?, ?", max(0, length(sessions) - 2)), collapse = "\n"),
    db_uuid, DBI::dbQuoteString(con, field_name),
    db_uuid, DBI::dbQuoteString(con, field_name),
    db_uuid, DBI::dbQuoteString(con, field_name)
  )
  
  # Prepare parameters - interleave sessions and bundles
  params <- character(length(sessions) * 2)
  params[seq(1, length(params), 2)] <- sessions
  params[seq(2, length(params), 2)] <- bundles
  
  # Execute prepared statement
  stmt <- DBI::dbSendQuery(con, query)
  DBI::dbBind(stmt, as.list(params))
  result <- DBI::dbFetch(stmt)
  DBI::dbClearResult(stmt)
  
  result$field_value
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
#' @description
#' Assign metadata values to a corpus, session, or bundle using bracket notation.
#'
#' @param x A corpus object
#' @param i Session name (optional)
#' @param j Bundle name (optional)
#' @param value Named list of metadata values
#'
#' @examples
#' \dontrun{
#' mycorpus["Session1", "Bundle1"] <- list(Age = 25, Sex = "Male")
#' }
#'
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
    # Update METADATA.json in database root
    db_name <- basename(basePath)
    db_name <- sub("_emuDB$", "", db_name)
    meta_file <- file.path(basePath, metadata.filename)
    
    # Read existing or create new
    if (file.exists(meta_file)) {
      existing <- read_json_fast(meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    updated <- utils::modifyList(existing, meta_list, keep.null = FALSE)
    jsonlite::write_json(updated, meta_file, auto_unbox = TRUE, pretty = TRUE)
    
  } else if (level == "session") {
    # Update METADATA.json in session directory
    meta_file <- file.path(basePath, paste0(session, "_ses"), metadata.filename)
    
    # Read existing or create new
    if (file.exists(meta_file)) {
      existing <- read_json_fast(meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    updated <- utils::modifyList(existing, meta_list, keep.null = FALSE)
    jsonlite::write_json(updated, meta_file, auto_unbox = TRUE, pretty = TRUE)
    
  } else if (level == "bundle") {
    # Update METADATA.json in bundle directory
    meta_file <- file.path(
      basePath,
      paste0(session, "_ses"),
      paste0(bundle, "_bndl"),
      metadata.filename
    )
    
    # Read existing or create new
    if (file.exists(meta_file)) {
      existing <- read_json_fast(meta_file, simplifyVector = TRUE)
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

#' Export metadata to Excel for convenient editing - OPTIMIZED
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Path to Excel file to create
#' @param overwrite Whether to overwrite existing file
#' @param mandatory Vector of column names to ensure are present
#' @export
