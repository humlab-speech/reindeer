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
#' @noRd
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

#' Rescan METADATA.json files into the metadata cache
#'
#' Deprecated alias for `load_metadata(corp, source = "files")`. Use the
#' new entry point in new code — both behave identically. Call this
#' (or `load_metadata()`) whenever you have edited `METADATA.json`
#' files outside of R and want the corpus to pick the changes up.
#'
#' @param corpus_obj A `corpus`.
#' @param verbose Print progress. Default `TRUE`.
#' @param parallel Scan bundles in parallel. Default `TRUE`.
#' @return The corpus, invisibly.
#' @examplesIf interactive()
#' corp <- corpus("path/to/ae_emuDB")
#' load_metadata(corp)              # preferred
#' gather_metadata(corp)            # equivalent
#' @seealso [load_metadata()], [set_metadata()], [get_metadata()]
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
  DBI::dbExecute(con, "DELETE FROM metadata_bundle WHERE db_uuid = ?", params = list(db_uuid))
  DBI::dbExecute(con, "DELETE FROM metadata_session WHERE db_uuid = ?", params = list(db_uuid))
  DBI::dbExecute(con, "DELETE FROM metadata_database WHERE db_uuid = ?", params = list(db_uuid))
  
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
#' @noRd
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
    
    # INSERT OR REPLACE so re-applying the same field at the same level
    # is idempotent (matches the user-facing semantics of add_metadata).
    if (level == "database") {
      sql <- "INSERT OR REPLACE INTO metadata_database
              (db_uuid, field_name, field_value, field_type)
              VALUES (?, ?, ?, ?)"
      params <- list(
        rep(db_uuid, n_fields), field_names, field_values, field_types
      )
    } else if (level == "session") {
      sql <- "INSERT OR REPLACE INTO metadata_session
              (db_uuid, session, field_name, field_value, field_type)
              VALUES (?, ?, ?, ?, ?)"
      params <- list(
        rep(db_uuid, n_fields), rep(session, n_fields),
        field_names, field_values, field_types
      )
    } else if (level == "bundle") {
      sql <- "INSERT OR REPLACE INTO metadata_bundle
              (db_uuid, session, bundle, field_name, field_value, field_type)
              VALUES (?, ?, ?, ?, ?, ?)"
      params <- list(
        rep(db_uuid, n_fields), rep(session, n_fields),
        rep(bundle, n_fields),
        field_names, field_values, field_types
      )
    } else {
      cli::cli_abort("Unknown metadata level: {.val {level}}")
    }
    DBI::dbExecute(con, sql, params = params)
  })
}

#' Serialize a metadata value for storage
#' @keywords internal
#' @noRd
serialize_metadata_value <- function(value) {
  if (is.null(value)) {
    return(list(value = "NULL", type = "NULL"))
  } else if (is.logical(value)) {
    return(list(value = as.character(value), type = "logical"))
  } else if (is.integer(value)) {
    return(list(value = as.character(value), type = "integer"))
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
#' @noRd
deserialize_metadata_value <- function(value_str, type_str) {
  if (is.na(value_str) || value_str == "NULL" || value_str == "NA") {
    return(NA)
  }

  tryCatch(
    switch(type_str,
      "logical" = as.logical(value_str),
      "numeric" = as.numeric(value_str),
      "integer" = as.integer(value_str),
      "date" = as.Date(value_str),
      "datetime" = as.POSIXct(value_str),
      "character" = value_str,
      value_str  # default
    ),
    error = function(e) value_str
  )
}

#' Register a metadata field in the fields tracking table
#' @keywords internal
#' @noRd
register_metadata_field <- function(con, field_name, field_type) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Check if exists
  existing <- DBI::dbGetQuery(
    con,
    "SELECT field_name FROM metadata_fields WHERE field_name = ?",
    params = list(field_name)
  )

  if (nrow(existing) == 0) {
    DBI::dbExecute(
      con,
      "INSERT INTO metadata_fields (field_name, field_type, first_seen, last_modified)
       VALUES (?, ?, ?, ?)",
      params = list(field_name, field_type, now, now)
    )
  } else {
    DBI::dbExecute(
      con,
      "UPDATE metadata_fields SET last_modified = ? WHERE field_name = ?",
      params = list(now, field_name)
    )
  }
}

# ==============================================================================
# EFFICIENT METADATA RETRIEVAL
# ==============================================================================

#' Read metadata with inheritance resolved
#'
#' Returns a tidy one-row-per-bundle tibble with the effective metadata
#' value at every level, picking bundle over session over database
#' defaults. This is what most analyses want — feed it to
#' [`dplyr::left_join`] against a `segment_list`, or use
#' [enrich(segs, corp)][enrich()] to do the join for you.
#'
#' @param corpus_obj A `corpus`.
#' @param session_pattern,bundle_pattern Optional regex filters.
#' @return A tibble: columns `session`, `bundle`, plus one column per
#'   metadata field.
#' @examplesIf interactive()
#' corp <- corpus("path/to/ae_emuDB")
#' get_metadata(corp)
#' get_metadata(corp, session_pattern = "Session1")
#' @seealso [set_metadata()], [load_metadata()], [enrich()]
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
    return(tibble::tibble(session = character(), bundle = character()))
  }
  
  # Inheritance resolved via LEFT JOIN + COALESCE (bundle > session > database).
  # Replaces an earlier UNION-of-CROSS-JOINs that produced
  # O(sessions x bundles x fields) intermediate rows before window dedup.
  #
  # Strategy: enumerate every (bundle, field_name) we *could* have a value for
  # by UNIONing the field-name dimension across the three metadata tables,
  # then left-join each level and COALESCE in precedence order.
  query <- "
    WITH field_universe AS (
      SELECT DISTINCT field_name FROM metadata_bundle   WHERE db_uuid = ?
      UNION
      SELECT DISTINCT field_name FROM metadata_session  WHERE db_uuid = ?
      UNION
      SELECT DISTINCT field_name FROM metadata_database WHERE db_uuid = ?
    ),
    cells AS (
      SELECT b.session, b.name AS bundle, f.field_name
      FROM bundle b
      CROSS JOIN field_universe f
      WHERE b.db_uuid = ?
    )
    SELECT
      c.session,
      c.bundle,
      c.field_name,
      COALESCE(mb.field_value, ms.field_value, md.field_value) AS field_value,
      COALESCE(mb.field_type,  ms.field_type,  md.field_type)  AS field_type
    FROM cells c
    LEFT JOIN metadata_bundle mb
      ON mb.db_uuid = ? AND mb.session = c.session
     AND mb.bundle  = c.bundle AND mb.field_name = c.field_name
    LEFT JOIN metadata_session ms
      ON ms.db_uuid = ? AND ms.session = c.session
     AND ms.field_name = c.field_name
    LEFT JOIN metadata_database md
      ON md.db_uuid = ? AND md.field_name = c.field_name
    WHERE COALESCE(mb.field_value, ms.field_value, md.field_value) IS NOT NULL
  "

  metadata_long <- data.table::setDT(DBI::dbGetQuery(
    con, query,
    params = list(db_uuid, db_uuid, db_uuid, db_uuid, db_uuid, db_uuid, db_uuid)
  ))
  
  if (nrow(metadata_long) == 0) {
    # No metadata at all
    return(tibble::as_tibble(bundles_dt[, .(session, bundle)]))
  }
  
  # Build a type lookup: field_name -> field_type (take first non-NA type per field)
  type_lookup <- metadata_long[
    !is.na(field_type) & field_type != "",
    .(field_type = field_type[1]),
    by = field_name
  ]
  
  # Convert from long to wide format using data.table's dcast (very fast)
  metadata_wide <- data.table::dcast(
    metadata_long,
    session + bundle ~ field_name,
    value.var = "field_value",
    fun.aggregate = function(x) x[1]  # Take first value if duplicates
  )
  
  # Apply type deserialization to restore proper R types
  for (i in seq_len(nrow(type_lookup))) {
    fname <- type_lookup$field_name[i]
    ftype <- type_lookup$field_type[i]
    if (fname %in% names(metadata_wide) && !is.null(ftype) && ftype != "character") {
      metadata_wide[[fname]] <- vapply(
        metadata_wide[[fname]],
        function(v) {
          if (is.na(v)) return(switch(ftype,
            "numeric" = NA_real_,
            "integer" = NA_integer_,
            "logical" = NA,
            NA_real_))
          deserialize_metadata_value(v, ftype)
        },
        switch(ftype,
          "numeric" = numeric(1),
          "integer" = integer(1),
          "logical" = logical(1),
          character(1))
      )
    }
  }
  
  # Join with bundles to ensure all bundles are present (even those without metadata)
  result <- metadata_wide[bundles_dt[, .(session, bundle)], on = .(session, bundle)]
  
  # Reorder columns: session, bundle, then alphabetically
  meta_cols <- setdiff(names(result), c("session", "bundle"))
  data.table::setcolorder(result, c("session", "bundle", sort(meta_cols)))

  tibble::as_tibble(result)
}

#' Get values for a single metadata field across bundles - OPTIMIZED
#' 
#' Uses a single SQL query with COALESCE to get values with proper inheritance
#' @keywords internal
#' @noRd
get_metadata_field <- function(con, db_uuid, field_name, sessions, bundles) {
  
  if (length(sessions) == 0) return(character(0))
  
  # Build single efficient query using COALESCE for precedence
  # Generate exactly N rows in the CTE
  quoted_field <- DBI::dbQuoteString(con, field_name)
  quoted_uuid <- DBI::dbQuoteString(con, db_uuid)
  
  pairs_sql <- paste(
    c("SELECT ? as session, ? as bundle",
      rep("UNION ALL SELECT ?, ?", max(0, length(sessions) - 1))),
    collapse = "\n      "
  )
  
  query <- sprintf("
    WITH bundle_session_pairs AS (
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
      ON mb.db_uuid = %s 
      AND mb.session = bsp.session 
      AND mb.bundle = bsp.bundle
      AND mb.field_name = %s
    LEFT JOIN metadata_session ms
      ON ms.db_uuid = %s
      AND ms.session = bsp.session
      AND ms.field_name = %s  
    LEFT JOIN metadata_database md
      ON md.db_uuid = %s
      AND md.field_name = %s",
    pairs_sql,
    quoted_uuid, quoted_field,
    quoted_uuid, quoted_field,
    quoted_uuid, quoted_field
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
# PROGRAMMATIC METADATA MANIPULATION
# ==============================================================================

#' Validate and set metadata with user interaction for unknown fields
#' @keywords internal
#' @noRd
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

# write_metadata_to_json was removed in v0.5.2 — add_metadata now routes
# through corpus_assign_metadata -> set_metadata_database/session/bundle,
# giving exactly one canonical write path for metadata.

# ==============================================================================
# EXCEL IMPORT/EXPORT
# ==============================================================================
# Implementation is in metadata_import_export.R
