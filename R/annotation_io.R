# ==============================================================================
# NATIVE ANNOTATION I/O
# ==============================================================================
#
# Internal functions for reading/writing bundle annotations and bundle lists
# directly from/to the emuDBcache.sqlite and JSON files. These replace the
# emuR annotation I/O functions used in serve().
#

#' Read a bundle list file
#'
#' Reads a bundle list JSON file from the database basePath.
#' Bundle lists are stored as \code{<basePath>/<name>_bundleList.json}.
#'
#' @param basePath Database basePath
#' @param name Bundle list name (e.g., "default")
#' @return tibble with columns: session, name, comment, finishedEditing
#' @keywords internal
.read_bundle_list <- function(basePath, name) {
  bl_path <- file.path(basePath, paste0(name, "_bundleList.json"))

  empty <- tibble::tibble(
    session = character(),
    name = character(),
    comment = character(),
    finishedEditing = logical()
  )

  if (!file.exists(bl_path)) {
    return(empty)
  }

  result <- jsonlite::read_json(bl_path, simplifyVector = TRUE)

  if (is.null(result) || length(result) == 0) {
    return(empty)
  }

  tibble::as_tibble(result)
}

#' Write a bundle list file
#'
#' Writes bundle list data to a JSON file in the database basePath.
#'
#' @param basePath Database basePath
#' @param name Bundle list name
#' @param data tibble/data.frame with bundle list data
#' @return Invisible path to written file
#' @keywords internal
.write_bundle_list <- function(basePath, name, data) {
  bl_path <- file.path(basePath, paste0(name, "_bundleList.json"))
  jsonlite::write_json(data, bl_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(bl_path)
}

#' Load bundle annotation data frames from SQLite
#'
#' Retrieves items, labels, and links for a specific bundle from the
#' emuDBcache.sqlite database.
#'
#' @param con DBI connection to emuDBcache.sqlite
#' @param session Session name
#' @param bundle Bundle name
#' @return List with items, labels, links data frames
#' @keywords internal
.load_bundle_annot <- function(con, session, bundle) {
  items <- DBI::dbGetQuery(con,
    "SELECT * FROM items WHERE session = ? AND bundle = ?",
    params = list(session, bundle))

  labels <- DBI::dbGetQuery(con,
    "SELECT * FROM labels WHERE session = ? AND bundle = ?",
    params = list(session, bundle))

  links <- DBI::dbGetQuery(con,
    "SELECT * FROM links WHERE session = ? AND bundle = ?",
    params = list(session, bundle))

  list(items = items, labels = labels, links = links)
}

#' Store bundle annotation data frames to SQLite
#'
#' Writes items, labels, and links data frames into the emuDBcache.sqlite
#' database. Appends to existing tables; caller should remove old data first
#' via \code{.remove_bundle_from_db()}.
#'
#' @param con DBI connection
#' @param annotDFs List with items, labels, links data frames
#' @param session Session name
#' @param bundle Bundle name
#' @keywords internal
.store_bundle_annot <- function(con, annotDFs, session, bundle) {
  if (nrow(annotDFs$items) > 0) {
    DBI::dbWriteTable(con, "items", annotDFs$items, append = TRUE)
  }

  if (nrow(annotDFs$labels) > 0) {
    DBI::dbWriteTable(con, "labels", annotDFs$labels, append = TRUE)
  }

  if (!is.null(annotDFs$links) && nrow(annotDFs$links) > 0) {
    DBI::dbWriteTable(con, "links", annotDFs$links, append = TRUE)
  }
}

#' Parse annotation JSON into data frames
#'
#' Converts the JSON string from an \code{_annot.json} file into structured
#' data frames of items, labels, and links suitable for SQLite storage.
#'
#' The returned data frames have \code{db_uuid}, \code{session}, and
#' \code{bundle} columns set to empty strings; the caller must fill these
#' before inserting into the database.
#'
#' @param json_string Character JSON string from _annot.json
#' @return List with: annotates, sampleRate, items (df), labels (df), links (df)
#' @keywords internal
.parse_annot_json <- function(json_string) {
  annot <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)

  sample_rate <- annot$sampleRate
  annotates <- annot$annotates

  items_list <- list()
  labels_list <- list()
  links_list <- list()

  for (level in annot$levels) {
    level_name <- level$name
    level_type <- level$type

    if (!is.null(level$items)) {
      for (item in level$items) {
        item_row <- data.frame(
          db_uuid = "",
          session = "",
          bundle = "",
          item_id = item$id,
          level = level_name,
          type = level_type,
          seq_idx = if (!is.null(item$seqIdx)) item$seqIdx else NA_integer_,
          sample_rate = sample_rate,
          sample_point = if (!is.null(item$samplePoint)) item$samplePoint else NA_integer_,
          sample_start = if (!is.null(item$sampleStart)) item$sampleStart else NA_integer_,
          sample_dur = if (!is.null(item$sampleDur)) item$sampleDur else NA_integer_,
          stringsAsFactors = FALSE
        )
        items_list[[length(items_list) + 1L]] <- item_row

        if (!is.null(item$labels)) {
          for (label in item$labels) {
            label_row <- data.frame(
              db_uuid = "",
              session = "",
              bundle = "",
              item_id = item$id,
              label_idx = if (!is.null(label$labelIdx)) label$labelIdx else 0L,
              name = label$name,
              label = if (!is.null(label$value)) label$value else "",
              stringsAsFactors = FALSE
            )
            labels_list[[length(labels_list) + 1L]] <- label_row
          }
        }
      }
    }
  }

  if (!is.null(annot$links)) {
    for (link in annot$links) {
      link_row <- data.frame(
        db_uuid = "",
        session = "",
        bundle = "",
        from_id = link$fromID,
        to_id = link$toID,
        label = if (!is.null(link$label)) link$label else NA_character_,
        stringsAsFactors = FALSE
      )
      links_list[[length(links_list) + 1L]] <- link_row
    }
  }

  items_df <- if (length(items_list) > 0) {
    do.call(rbind, items_list)
  } else {
    data.frame(
      db_uuid = character(), session = character(), bundle = character(),
      item_id = integer(), level = character(), type = character(),
      seq_idx = integer(), sample_rate = numeric(), sample_point = integer(),
      sample_start = integer(), sample_dur = integer(),
      stringsAsFactors = FALSE
    )
  }

  labels_df <- if (length(labels_list) > 0) {
    do.call(rbind, labels_list)
  } else {
    data.frame(
      db_uuid = character(), session = character(), bundle = character(),
      item_id = integer(), label_idx = integer(), name = character(),
      label = character(),
      stringsAsFactors = FALSE
    )
  }

  links_df <- if (length(links_list) > 0) {
    do.call(rbind, links_list)
  } else {
    data.frame(
      db_uuid = character(), session = character(), bundle = character(),
      from_id = integer(), to_id = integer(), label = character(),
      stringsAsFactors = FALSE
    )
  }

  list(
    annotates = annotates,
    sampleRate = sample_rate,
    items = items_df,
    labels = labels_df,
    links = links_df
  )
}

#' Remove bundle data from SQLite
#'
#' Deletes all items, labels, links, and the bundle record for a specific
#' session/bundle combination from the emuDBcache.sqlite database.
#'
#' @param con DBI connection
#' @param session Session name
#' @param bundle Bundle name
#' @keywords internal
.remove_bundle_from_db <- function(con, session, bundle) {
  DBI::dbExecute(con,
    "DELETE FROM items WHERE session = ? AND bundle = ?",
    params = list(session, bundle))
  DBI::dbExecute(con,
    "DELETE FROM labels WHERE session = ? AND bundle = ?",
    params = list(session, bundle))
  DBI::dbExecute(con,
    "DELETE FROM links WHERE session = ? AND bundle = ?",
    params = list(session, bundle))
  DBI::dbExecute(con,
    "DELETE FROM bundle WHERE session = ? AND name = ?",
    params = list(session, bundle))
}

#' Add bundle record to SQLite
#'
#' Inserts a new bundle record into the bundle table of the emuDBcache.sqlite.
#'
#' @param con DBI connection
#' @param db_uuid Database UUID
#' @param session Session name
#' @param name Bundle name
#' @param annotates What the bundle annotates (e.g. wav file)
#' @param sample_rate Sample rate
#' @param md5 MD5 hash of annotation JSON
#' @keywords internal
.add_bundle_to_db <- function(con, db_uuid, session, name, annotates, sample_rate, md5) {
  DBI::dbExecute(con,
    "INSERT INTO bundle (db_uuid, session, name, annotates, sample_rate, md5_annot_json)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(db_uuid, session, name, annotates, sample_rate, md5))
}

#' Add session record to SQLite
#'
#' Inserts a session record into the session table. Uses INSERT OR IGNORE
#' so existing sessions are not duplicated.
#'
#' @param con DBI connection
#' @param db_uuid Database UUID
#' @param name Session name
#' @keywords internal
.add_session_to_db <- function(con, db_uuid, name) {
  DBI::dbExecute(con,
    "INSERT OR IGNORE INTO session (db_uuid, name) VALUES (?, ?)",
    params = list(db_uuid, name))
}
