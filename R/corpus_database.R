build_emuDB_cache <- function(database_dir,
                              parallel = TRUE,
                              workers = future::availableCores() - 1,
                              batch_size = 50,
                              verbose = TRUE) {

  # Validate database directory
  if (!dir.exists(database_dir)) {
    cli::cli_abort("Database directory {.path {database_dir}} does not exist")
  }

  # Extract database name from directory
  db_name <- stringr::str_replace(basename(database_dir), "_emuDB$", "")

  # Setup paths
  db_config_path <- file.path(database_dir, paste0(db_name, "_DBconfig.json"))
  cache_path <- file.path(database_dir, paste0(db_name, "_emuDBcache.sqlite"))

  if (!file.exists(db_config_path)) {
    cli::cli_abort("Database config file not found: {.path {db_config_path}}")
  }

  # Load database configuration
  db_config <- jsonlite::fromJSON(db_config_path, simplifyVector = FALSE)

  if (verbose) {
    cli::cli_h2("Building emuDB cache for {.field {db_name}}")
  }

  # Initialize SQLite connection
  if (file.exists(cache_path)) {
    if (verbose) cli::cli_alert_info("Using existing cache file")
    # For updates, we could add logic here to only update changed bundles
    # For now, we rebuild (safer for consistency)
  }
  
  if (verbose) {
    cli::cli_alert_info("Initializing cache database...")
  }
  
  # Remove old cache if it exists to ensure clean state
  if (file.exists(cache_path)) {
    unlink(cache_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Initialize database schema
  initialize_database_schema(con, db_config$UUID, db_name)

  # Discover sessions and bundles
  sessions_bundles <- discover_sessions_bundles(database_dir)

  if (nrow(sessions_bundles) == 0) {
    cli::cli_alert_warning("No bundles found in database")

    # Initialize metadata schema even if no bundles
    initialize_metadata_schema(con)

    # Close connection and return (corpus constructor will proceed)
    DBI::dbDisconnect(con)
    on.exit()  # Remove the on.exit handler

    return(invisible(NULL))
  }

  if (verbose) {
    cli::cli_alert_success("Found {.val {dplyr::n_distinct(sessions_bundles$session)}} sessions with {.val {nrow(sessions_bundles)}} bundles")
  }

  # Setup parallel processing if requested
  if (parallel && nrow(sessions_bundles) > 10) {
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)
  } else {
    parallel <- FALSE
  }

  # Process bundles
  results <- process_bundles_batch(
    con = con,
    sessions_bundles = sessions_bundles,
    database_dir = database_dir,
    db_config = db_config,
    batch_size = batch_size,
    parallel = parallel,
    verbose = verbose
  )

  # Report results
  if (verbose) {
    successful <- sum(results$success)
    failed <- sum(!results$success)

    cli::cli_alert_success("Successfully processed {.val {successful}} bundles")

    if (failed > 0) {
      cli::cli_alert_warning("Failed to process {.val {failed}} bundles")
      failed_bundles <- sessions_bundles[!results$success, ]
      for (i in seq_len(min(5, nrow(failed_bundles)))) {
        cli::cli_alert_danger("{failed_bundles$session[i]}/{failed_bundles$bundle[i]}: {results$error[!results$success][i]}")
      }
      if (failed > 5) {
        cli::cli_alert_info("... and {.val {failed - 5}} more")
      }
    }
  }
  
  # Initialize metadata schema
  if (verbose) {
    cli::cli_h2("Initializing metadata schema")
  }
  initialize_metadata_schema(con)

  # Close the connection we created (the corpus will create its own)
  DBI::dbDisconnect(con)
  on.exit() # Remove the on.exit handler


}

# ==============================================================================
# CACHE SQLITE FILE INITIALIZATION
# ==============================================================================

initialize_database_schema <- function(con, uuid, db_name) {
  # Create tables
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  # emu_db table
  DBI::dbExecute(con, "
    CREATE TABLE emu_db (
      uuid VARCHAR(36) NOT NULL,
      name TEXT,
      PRIMARY KEY (uuid)
    )"
  )

  # session table
  DBI::dbExecute(con, "
    CREATE TABLE session (
      db_uuid VARCHAR(36),
      name TEXT,
      PRIMARY KEY (db_uuid, name),
      FOREIGN KEY (db_uuid) REFERENCES emu_db(uuid) ON DELETE CASCADE ON UPDATE CASCADE
    )"
  )

  # bundle table
  DBI::dbExecute(con, "
    CREATE TABLE bundle (
      db_uuid VARCHAR(36),
      session TEXT,
      name TEXT,
      annotates TEXT,
      sample_rate FLOAT,
      md5_annot_json TEXT,
      PRIMARY KEY (db_uuid, session, name),
      FOREIGN KEY (db_uuid, session) REFERENCES session(db_uuid, name) ON DELETE CASCADE ON UPDATE CASCADE
    )"
  )

  # items table
  DBI::dbExecute(con, "
    CREATE TABLE items (
      db_uuid VARCHAR(36),
      session TEXT,
      bundle TEXT,
      item_id INTEGER,
      level TEXT,
      type TEXT,
      seq_idx INTEGER,
      sample_rate FLOAT,
      sample_point INTEGER,
      sample_start INTEGER,
      sample_dur INTEGER,
      PRIMARY KEY (db_uuid, session, bundle, item_id),
      FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session, name) ON DELETE CASCADE ON UPDATE CASCADE
    )"
  )

  # labels table
  DBI::dbExecute(con, "
    CREATE TABLE labels (
      db_uuid VARCHAR(36),
      session TEXT,
      bundle TEXT,
      item_id INTEGER,
      label_idx INTEGER,
      name TEXT,
      label TEXT,
      PRIMARY KEY (db_uuid, session, bundle, item_id, label_idx),
      FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session, name) ON DELETE CASCADE ON UPDATE CASCADE
    )"
  )

  # links table
  DBI::dbExecute(con, "
    CREATE TABLE links (
      db_uuid VARCHAR(36) NOT NULL,
      session TEXT,
      bundle TEXT,
      from_id INTEGER,
      to_id INTEGER,
      label TEXT,
      FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session, name) ON DELETE CASCADE ON UPDATE CASCADE
    )"
  )

  # Create indices
  DBI::dbExecute(con, "CREATE INDEX items_level_seq_idx ON items(db_uuid, session, bundle, level, seq_idx)")
  DBI::dbExecute(con, "CREATE INDEX links_both_ids_idx ON links(db_uuid, session, bundle, from_id, to_id)")
  DBI::dbExecute(con, "CREATE INDEX links_to_id_idx ON links(db_uuid, session, bundle, to_id)")
  DBI::dbExecute(con, "CREATE INDEX label_nameLabel_idx ON labels(db_uuid, bundle, session, item_id)")

  # Insert database record
  DBI::dbExecute(con, sprintf("INSERT INTO emu_db (uuid, name) VALUES ('%s', '%s')", uuid, db_name))
}

# ==============================================================================
# BUNDLE DISCOVERY
# ==============================================================================

discover_sessions_bundles <- function(database_dir) {
  session_dirs <- list.dirs(database_dir, recursive = FALSE, full.names = FALSE)
  session_dirs <- session_dirs[stringr::str_detect(session_dirs, "_ses$")]

  sessions_bundles <- purrr::map_dfr(session_dirs, function(ses_dir) {
    session_name <- stringr::str_replace(ses_dir, "_ses$", "")
    bundle_dirs <- list.dirs(file.path(database_dir, ses_dir),
                             recursive = FALSE, full.names = FALSE)
    bundle_dirs <- bundle_dirs[stringr::str_detect(bundle_dirs, "_bndl$")]
    bundle_names <- stringr::str_replace(bundle_dirs, "_bndl$", "")

    if (length(bundle_names) > 0) {
      tibble::tibble(
        session = session_name,
        bundle = bundle_names
      )
    } else {
      tibble::tibble()
    }
  })

  return(sessions_bundles)
}

# ==============================================================================
# BATCH PROCESSING
# ==============================================================================

process_bundles_batch <- function(con, sessions_bundles, database_dir,
                                  db_config, batch_size, parallel, verbose) {

  # Add sessions to database
  unique_sessions <- unique(sessions_bundles$session)
  session_insert <- sprintf(
    "INSERT OR IGNORE INTO session (db_uuid, name) VALUES ('%s', '%s')",
    db_config$UUID, unique_sessions
  )

  DBI::dbBegin(con)
  purrr::walk(session_insert, ~ DBI::dbExecute(con, .x))
  DBI::dbCommit(con)

  # Split into batches
  n_batches <- ceiling(nrow(sessions_bundles) / batch_size)
  sessions_bundles$batch <- rep(1:n_batches, each = batch_size, length.out = nrow(sessions_bundles))
  batches <- split(sessions_bundles, sessions_bundles$batch)

  # Process function for a single bundle
  process_bundle <- function(session_name, bundle_name, database_dir, db_config) {
    tryCatch({
      annot_path <- file.path(
        database_dir,
        paste0(session_name, "_ses"),
        paste0(bundle_name, "_bndl"),
        paste0(bundle_name, "_annot.json")
      )

      if (!file.exists(annot_path)) {
        return(list(success = FALSE, error = "Annotation file not found", data = NULL))
      }

      # Read and parse JSON
      annot_json <- jsonlite::fromJSON(annot_path, simplifyVector = FALSE)
      md5_hash <- as.character(tools::md5sum(annot_path))

      # Convert to data frames
      annot_dfs <- parse_annot_json(annot_json, db_config$UUID, session_name, bundle_name)

      return(list(
        success = TRUE,
        error = "",
        data = annot_dfs,
        md5 = md5_hash,
        sample_rate = annot_json$sampleRate,
        annotates = annot_json$annotates
      ))
    }, error = function(e) {
      return(list(success = FALSE, error = as.character(e), data = NULL))
    })
  }

  # Process batches with progress tracking
  results <- list()
  total_bundles <- nrow(sessions_bundles)
  
  if (verbose) {
    cli::cli_alert_info("Processing {total_bundles} bundles{?s} in {length(batches)} batch{?es}...")
    if (parallel) {
      cli::cli_alert_info("Using parallel processing with {workers} worker{?s}")
    }
    cli::cli_progress_bar(
      "Processing",
      total = total_bundles,
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      clear = FALSE
    )
  }

  for (i in seq_along(batches)) {
    batch <- batches[[i]]

    if (parallel) {
      batch_results <- furrr::future_pmap(
        list(batch$session, batch$bundle),
        process_bundle,
        database_dir = database_dir,
        db_config = db_config,
        .options = furrr::furrr_options(seed = TRUE)
      )
    } else {
      batch_results <- purrr::pmap(
        list(batch$session, batch$bundle),
        process_bundle,
        database_dir = database_dir,
        db_config = db_config
      )
    }

    # Insert successful results into database
    successful_results <- purrr::keep(batch_results, ~ .x$success)

    if (length(successful_results) > 0) {
      insert_batch_results(con, successful_results, db_config$UUID)
    }

    results <- c(results, batch_results)
    
    # Update progress bar
    if (verbose) {
      bundles_done <- sum(purrr::map_int(batches[1:i], nrow))
      cli::cli_progress_update(set = bundles_done)
    }
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Convert results to data frame
  results_df <- tibble::tibble(
    success = purrr::map_lgl(results, ~ .x$success),
    error = purrr::map_chr(results, ~ .x$error)
  )

  return(results_df)
}

# ==============================================================================
# ANNOTATION PARSING
# ==============================================================================

parse_annot_json <- function(annot_json, db_uuid, session_name, bundle_name) {

  # Initialize empty data frames
  items_list <- tibble::tibble()
  labels_list <- tibble::tibble()
  links_list <- tibble::tibble()

  # Parse levels if they exist
  if (!is.null(annot_json[["levels"]]) && is.list(annot_json[["levels"]])) {
    levels <- annot_json[["levels"]]

    for (level_idx in seq_along(levels)) {
      level <- levels[[level_idx]]

      # Ensure level is a list with expected fields
      if (!is.list(level) || is.null(level[["name"]]) || is.null(level[["type"]])) {
        next
      }

      level_name <- as.character(level[["name"]])
      level_type <- as.character(level[["type"]])

      # Process items if they exist
      if (!is.null(level[["items"]]) && is.list(level[["items"]])) {
        items <- level[["items"]]

        for (item_idx in seq_along(items)) {
          item <- items[[item_idx]]

          if (!is.list(item) || is.null(item[["id"]])) {
            next
          }

          # Create item row
          item_row <- tibble::tibble(
            db_uuid = db_uuid,
            session = session_name,
            bundle = bundle_name,
            item_id = as.integer(item[["id"]]),
            level = level_name,
            type = level_type,
            seq_idx = as.integer(item_idx),
            sample_rate = as.numeric(annot_json[["sampleRate"]]),
            sample_point = if (!is.null(item[["samplePoint"]])) as.integer(item[["samplePoint"]]) else NA_integer_,
            sample_start = if (!is.null(item[["sampleStart"]])) as.integer(item[["sampleStart"]]) else NA_integer_,
            sample_dur = if (!is.null(item[["sampleDur"]])) as.integer(item[["sampleDur"]]) else NA_integer_
          )

          items_list <- rbind(items_list, item_row)

          # Process labels for this item
          if (!is.null(item[["labels"]]) && is.list(item[["labels"]])) {
            item_labels <- item[["labels"]]

            for (label_idx in seq_along(item_labels)) {
              label <- item_labels[[label_idx]]

              if (!is.list(label) || is.null(label[["name"]])) {
                next
              }

              label_row <- tibble::tibble(
                db_uuid = db_uuid,
                session = session_name,
                bundle = bundle_name,
                item_id = as.integer(item[["id"]]),
                label_idx = as.integer(label_idx),
                name = as.character(label[["name"]]),
                label = if (!is.null(label[["value"]])) as.character(label[["value"]]) else ""
              )

              labels_list <- rbind(labels_list, label_row)
            }
          }
        }
      }
    }
  }

  # Parse links if they exist
  if (!is.null(annot_json[["links"]]) && is.list(annot_json[["links"]])) {
    links <- annot_json[["links"]]

    for (link_idx in seq_along(links)) {
      link <- links[[link_idx]]

      if (!is.list(link) || is.null(link[["fromID"]]) || is.null(link[["toID"]])) {
        next
      }

      link_row <- tibble::tibble(
        db_uuid = db_uuid,
        session = session_name,
        bundle = bundle_name,
        from_id = as.integer(link[["fromID"]]),
        to_id = as.integer(link[["toID"]]),
        label = if (!is.null(link[["label"]])) as.character(link[["label"]]) else NA_character_
      )

      links_list <- rbind(links_list, link_row)
    }
  }

  return(list(
    items = items_list,
    labels = labels_list,
    links = links_list
  ))
}

# ==============================================================================
# DATABASE INSERTION
# ==============================================================================

insert_batch_results <- function(con, results, db_uuid) {
  DBI::dbBegin(con)

  tryCatch({
    # Prepare bundle data
    bundle_data <- purrr::map_dfr(results, function(r) {
      tibble::tibble(
        db_uuid = db_uuid,
        session = r$data$items$session[1],
        name = r$data$items$bundle[1],
        annotates = r$annotates,
        sample_rate = r$sample_rate,
        md5_annot_json = r$md5
      )
    })

    # Combine all items, labels, and links
    all_items <- purrr::map_dfr(results, ~ .x$data$items)
    all_labels <- purrr::map_dfr(results, ~ .x$data$labels)
    all_links <- purrr::map_dfr(results, ~ .x$data$links)

    # Insert bundles
    if (nrow(bundle_data) > 0) {
      DBI::dbAppendTable(con, "bundle", bundle_data)
    }

    # Insert items
    if (nrow(all_items) > 0) {
      DBI::dbAppendTable(con, "items", all_items)
    }

    # Insert labels
    if (nrow(all_labels) > 0) {
      DBI::dbAppendTable(con, "labels", all_labels)
    }

    # Insert links
    if (nrow(all_links) > 0) {
      DBI::dbAppendTable(con, "links", all_links)
    }

    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop(e)
  })
}#' Gather all metadata from .meta_json files (internal, called during construction)
#' @keywords internal
gather_metadata_internal <- function(corpus_obj, verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("Scanning .meta_json files...")
  }
  
  basePath <- corpus_obj@basePath
  db_uuid <- corpus_obj@.uuid
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Clear existing metadata
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_bundle WHERE db_uuid = '%s'", db_uuid))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_session WHERE db_uuid = '%s'", db_uuid))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_database WHERE db_uuid = '%s'", db_uuid))
  
  # 1. Database-level metadata
  db_meta_file <- file.path(basePath, paste0(corpus_obj@dbName, ".meta_json"))
  
  if (file.exists(db_meta_file)) {
    if (verbose) cli::cli_alert_info("Loading database defaults")
    db_meta <- jsonlite::read_json(db_meta_file, simplifyVector = TRUE)
    if (length(db_meta) > 0) {
      process_metadata_list(con, db_uuid, NULL, NULL, db_meta, "database")
    }
  }
  
  # 2. Session-level metadata
  sessions <- list_sessions_from_cache(con, db_uuid)
  
  if (verbose && nrow(sessions) > 0) {
    cli::cli_progress_bar(
      "Loading session metadata",
      total = nrow(sessions),
      format = "{cli::pb_spin} Session {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent}"
    )
  }
  
  for (i in seq_len(nrow(sessions))) {
    session_name <- sessions$name[i]
    session_meta_file <- file.path(basePath, paste0(session_name, "_ses"),
                                   paste0(session_name, ".meta_json"))
    
    if (file.exists(session_meta_file)) {
      meta_data <- jsonlite::read_json(session_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, NULL, meta_data, "session")
      }
    }
    
    if (verbose && nrow(sessions) > 0) {
      cli::cli_progress_update()
    }
  }
  
  if (verbose && nrow(sessions) > 0) {
    cli::cli_progress_done()
  }
  
  # 3. Bundle-level metadata
  bundles <- list_bundles_from_cache(con, db_uuid)
  
  if (verbose && nrow(bundles) > 0) {
    cli::cli_progress_bar(
      "Loading bundle metadata",
      total = nrow(bundles),
      format = "{cli::pb_spin} Bundle {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent}"
    )
  }
  
  for (i in seq_len(nrow(bundles))) {
    session_name <- bundles$session[i]
    bundle_name <- bundles$name[i]
    
    bundle_meta_file <- file.path(
      basePath, 
      paste0(session_name, "_ses"),
      paste0(bundle_name, "_bndl"),
      paste0(bundle_name, ".meta_json")
    )
    
    if (file.exists(bundle_meta_file)) {
      meta_data <- jsonlite::read_json(bundle_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, bundle_name, meta_data, "bundle")
      }
    }
    
    if (verbose && nrow(bundles) > 0) {
      cli::cli_progress_update()
    }
  }
  
  if (verbose && nrow(bundles) > 0) {
    cli::cli_progress_done()
  }
  
  if (verbose) {
    cli::cli_alert_success("Metadata loaded")
  }
}

# ==============================================================================
# METADATA HELPER FUNCTIONS (from reindeeR_metadata_optimized.R)
# ==============================================================================

#' Process and insert metadata list
#' @keywords internal
process_metadata_list <- function(con, db_uuid, session, bundle, meta_list, level) {
  if (length(meta_list) == 0) return(invisible(NULL))
  
  DBI::dbWithTransaction(con, {
    for (field_name in names(meta_list)) {
      value <- meta_list[[field_name]]
      
      # Serialize value
      field_info <- serialize_metadata_value(value)
      
      # Register field
      register_metadata_field(con, field_name, field_info$type)
      
      # Insert into appropriate table
      if (level == "database") {
        # Remove quotes from field_name in SQL
        field_name_clean <- gsub("'", "''", field_name)
        value_clean <- gsub("'", "''", field_info$value)
        
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_database (db_uuid, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s')",
          db_uuid, field_name_clean, value_clean, field_info$type
        )
      } else if (level == "session") {
        field_name_clean <- gsub("'", "''", field_name)
        value_clean <- gsub("'", "''", field_info$value)
        session_clean <- gsub("'", "''", session)
        
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_session (db_uuid, session, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s', '%s')",
          db_uuid, session_clean, field_name_clean, value_clean, field_info$type
        )
      } else if (level == "bundle") {
        field_name_clean <- gsub("'", "''", field_name)
        value_clean <- gsub("'", "''", field_info$value)
        session_clean <- gsub("'", "''", session)
        bundle_clean <- gsub("'", "''", bundle)
        
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_bundle (db_uuid, session, bundle, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
          db_uuid, session_clean, bundle_clean, field_name_clean, value_clean, field_info$type
        )
      }
      
      DBI::dbExecute(con, sql)
    }
  })
}

#' Serialize metadata value
#' @keywords internal
serialize_metadata_value <- function(value) {
  if (is.null(value)) {
    return(list(value = "NULL", type = "NULL"))
  } else if (is.logical(value)) {
    return(list(value = as.character(value), type = "logical"))
  } else if (is.numeric(value) && !is.integer(value)) {
    return(list(value = as.character(value), type = "numeric"))
  } else if (is.integer(value)) {
    return(list(value = as.character(value), type = "integer"))
  } else if (inherits(value, "Date")) {
    return(list(value = as.character(value), type = "date"))
  } else if (inherits(value, "POSIXt")) {
    return(list(value = format(value, "%Y-%m-%dT%H:%M:%S"), type = "datetime"))
  } else {
    return(list(value = as.character(value), type = "character"))
  }
}

#' Deserialize metadata value
#' @keywords internal
deserialize_metadata_value <- function(value_str, type_str) {
  if (is.na(value_str) || value_str == "NULL" || value_str == "NA") {
    return(NA)
  }
  
  tryCatch({
    switch(type_str,
      "logical" = as.logical(value_str),
      "numeric" = as.numeric(value_str),
      "integer" = as.integer(value_str),
      "date" = as.Date(value_str),
      "datetime" = as.POSIXct(value_str),
      "character" = value_str,
      value_str  # default
    )
  }, error = function(e) {
    value_str
  })
}

#' Register metadata field
#' @keywords internal
register_metadata_field <- function(con, field_name, field_type) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  field_name_clean <- gsub("'", "''", field_name)
  
  # Check if exists
  existing <- DBI::dbGetQuery(con, sprintf(
    "SELECT field_name FROM metadata_fields WHERE field_name = '%s'",
    field_name_clean
  ))
  
  if (nrow(existing) == 0) {
    # Insert new
    DBI::dbExecute(con, sprintf(
      "INSERT INTO metadata_fields (field_name, field_type, first_seen, last_modified) 
       VALUES ('%s', '%s', '%s', '%s')",
      field_name_clean, field_type, now, now
    ))
  } else {
    # Update timestamp
    DBI::dbExecute(con, sprintf(
      "UPDATE metadata_fields SET last_modified = '%s' WHERE field_name = '%s'",
      now, field_name_clean
    ))
  }
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
