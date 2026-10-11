initialize_database_schema <- function(con, uuid, db_name, indices = TRUE) {
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

  if (indices) .create_cache_indices(con)

  # Insert database record
  DBI::dbExecute(con, "INSERT INTO emu_db (uuid, name) VALUES (?, ?)", params = list(uuid, db_name))
}

.create_cache_indices <- function(con) {
  DBI::dbExecute(con, "CREATE INDEX items_level_seq_idx ON items(db_uuid, session, bundle, level, seq_idx)")
  DBI::dbExecute(con, "CREATE INDEX links_both_ids_idx ON links(db_uuid, session, bundle, from_id, to_id)")
  DBI::dbExecute(con, "CREATE INDEX links_to_id_idx ON links(db_uuid, session, bundle, to_id)")
  DBI::dbExecute(con, "CREATE INDEX label_nameLabel_idx ON labels(db_uuid, bundle, session, item_id)")
  # Covers the simple-query access pattern: WHERE i.level=? AND l.name=? AND l.label=?
  # (paired with items_level_seq_idx). Without this, label lookups full-scan the labels table.
  DBI::dbExecute(con, "CREATE INDEX labels_name_label_idx ON labels(db_uuid, name, label)")
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
