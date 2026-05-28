#' Build or rebuild the SQLite cache for an emuDB database
#'
#' Parses all annotation JSON files and populates the SQLite cache
#' with items, labels, links, and bundle metadata. Uses parallel
#' processing by default for large databases.
#'
#' @param database_dir Path to the `_emuDB` directory
#' @param parallel Whether to use parallel processing (default TRUE)
#' @param workers Number of parallel workers
#' @param batch_size Number of bundles per batch
#' @param verbose Whether to show progress messages
#' @return Invisible NULL, called for side effects
#' @keywords internal
#' @noRd
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
  db_name <- sub("_emuDB$", "", basename(database_dir))

  # Setup paths
  db_config_path <- file.path(database_dir, paste0(db_name, "_DBconfig.json"))
  cache_path <- file.path(database_dir, paste0(db_name, database.cache.suffix))

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
    cli::cli_alert_success("Found {.val {length(unique(sessions_bundles$session))}} sessions with {.val {nrow(sessions_bundles)}} bundles")
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
  # Covers the simple-query access pattern: WHERE i.level=? AND l.name=? AND l.label=?
  # (paired with items_level_seq_idx). Without this, label lookups full-scan the labels table.
  DBI::dbExecute(con, "CREATE INDEX labels_name_label_idx ON labels(db_uuid, name, label)")

  # Insert database record
  DBI::dbExecute(con, "INSERT INTO emu_db (uuid, name) VALUES (?, ?)", params = list(uuid, db_name))
}

# ==============================================================================
# BUNDLE DISCOVERY
# ==============================================================================

discover_sessions_bundles <- function(database_dir) {
  session_dirs <- list.dirs(database_dir, recursive = FALSE, full.names = FALSE)
  session_dirs <- session_dirs[grepl("_ses$", session_dirs)]

  sessions_bundles <- do.call(rbind, lapply(session_dirs, function(ses_dir) {
    session_name <- sub("_ses$", "", ses_dir)
    bundle_dirs <- list.dirs(file.path(database_dir, ses_dir),
                             recursive = FALSE, full.names = FALSE)
    bundle_dirs <- bundle_dirs[grepl("_bndl$", bundle_dirs)]
    bundle_names <- sub("_bndl$", "", bundle_dirs)

    if (length(bundle_names) > 0) {
      tibble::tibble(
        session = session_name,
        bundle = bundle_names
      )
    } else {
      NULL
    }
  }))

  if (is.null(sessions_bundles)) {
    sessions_bundles <- tibble::tibble(session = character(), bundle = character())
  }

  return(sessions_bundles)
}

# ==============================================================================
# BATCH PROCESSING
# ==============================================================================

process_bundles_batch <- function(con, sessions_bundles, database_dir,
                                  db_config, batch_size, parallel, verbose) {

  # Add sessions to database
  unique_sessions <- unique(sessions_bundles$session)

  DBI::dbBegin(con)
  for (sess in unique_sessions) {
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO session (db_uuid, name) VALUES (?, ?)",
      params = list(db_config$UUID, sess))
  }
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
      batch_results <- Map(
        process_bundle,
        batch$session, batch$bundle,
        MoreArgs = list(database_dir = database_dir, db_config = db_config)
      )
    }

    # Insert successful results into database
    successful_results <- Filter(function(x) x$success, batch_results)

    if (length(successful_results) > 0) {
      insert_batch_results(con, successful_results, db_config$UUID)
    }

    results <- c(results, batch_results)
    
    # Update progress bar
    if (verbose) {
      bundles_done <- sum(vapply(batches[1:i], nrow, integer(1)))
      cli::cli_progress_update(set = bundles_done)
    }
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Convert results to data frame
  results_df <- tibble::tibble(
    success = vapply(results, function(x) x$success, logical(1)),
    error = vapply(results, function(x) x$error, character(1))
  )

  return(results_df)
}

# ==============================================================================
# ANNOTATION PARSING
# ==============================================================================

parse_annot_json <- function(annot_json, db_uuid, session_name, bundle_name) {

  # List collectors (avoid rbind-in-loop quadratic growth)
  items_collector <- vector("list", 0L)
  labels_collector <- vector("list", 0L)
  links_collector <- vector("list", 0L)

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

          items_collector[[length(items_collector) + 1L]] <- tibble::tibble(
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

          # Process labels for this item
          if (!is.null(item[["labels"]]) && is.list(item[["labels"]])) {
            item_labels <- item[["labels"]]

            for (label_idx in seq_along(item_labels)) {
              label <- item_labels[[label_idx]]

              if (!is.list(label) || is.null(label[["name"]])) {
                next
              }

              labels_collector[[length(labels_collector) + 1L]] <- tibble::tibble(
                db_uuid = db_uuid,
                session = session_name,
                bundle = bundle_name,
                item_id = as.integer(item[["id"]]),
                label_idx = as.integer(label_idx),
                name = as.character(label[["name"]]),
                label = if (!is.null(label[["value"]])) as.character(label[["value"]]) else ""
              )
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

      links_collector[[length(links_collector) + 1L]] <- tibble::tibble(
        db_uuid = db_uuid,
        session = session_name,
        bundle = bundle_name,
        from_id = as.integer(link[["fromID"]]),
        to_id = as.integer(link[["toID"]]),
        label = if (!is.null(link[["label"]])) as.character(link[["label"]]) else NA_character_
      )
    }
  }

  return(list(
    items = if (length(items_collector) > 0) do.call(rbind, items_collector) else tibble::tibble(),
    labels = if (length(labels_collector) > 0) do.call(rbind, labels_collector) else tibble::tibble(),
    links = if (length(links_collector) > 0) do.call(rbind, links_collector) else tibble::tibble()
  ))
}

# ==============================================================================
# DATABASE INSERTION
# ==============================================================================

insert_batch_results <- function(con, results, db_uuid) {
  DBI::dbBegin(con)

  tryCatch({
    # Prepare bundle data
    bundle_data <- do.call(rbind, lapply(results, function(r) {
      tibble::tibble(
        db_uuid = db_uuid,
        session = r$data$items$session[1],
        name = r$data$items$bundle[1],
        annotates = r$annotates,
        sample_rate = r$sample_rate,
        md5_annot_json = r$md5
      )
    }))

    # Combine all items, labels, and links
    all_items <- do.call(rbind, lapply(results, function(x) x$data$items))
    all_labels <- do.call(rbind, lapply(results, function(x) x$data$labels))
    all_links <- do.call(rbind, lapply(results, function(x) x$data$links))

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
    cli::cli_abort("Cache build failed: {conditionMessage(e)}", parent = e)
  })
}#' Gather all metadata from .meta_json files (internal, called during construction)
#' @keywords internal
#' @noRd
gather_metadata_internal <- function(corpus_obj, verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("Scanning .meta_json files...")
  }
  
  basePath <- corpus_obj@basePath
  db_uuid <- corpus_obj@.uuid
  con <- get_corpus_connection(corpus_obj)
  
  # Clear existing metadata
  DBI::dbExecute(con, "DELETE FROM metadata_bundle WHERE db_uuid = ?", params = list(db_uuid))
  DBI::dbExecute(con, "DELETE FROM metadata_session WHERE db_uuid = ?", params = list(db_uuid))
  DBI::dbExecute(con, "DELETE FROM metadata_database WHERE db_uuid = ?", params = list(db_uuid))
  
  # 1. Database-level metadata
  db_meta_file <- file.path(basePath, metadata.filename)
  
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
                                   metadata.filename)
    
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
      metadata.filename
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


# Metadata helpers (process_metadata_list, serialize_metadata_value,
# deserialize_metadata_value, register_metadata_field) are defined in metadata_core.R
