#' Export metadata to Excel file
#'
#' Creates a multi-sheet Excel workbook with bundle, session, and database
#' metadata. Requires the \pkg{openxlsx} package.
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Output path for the Excel file
#' @param overwrite Overwrite existing file (default: FALSE)
#' @param mandatory Character vector of metadata columns to always include,
#'   even if empty (default: \code{c("Age", "Gender")})
#' @return A data.frame of bundle metadata, invisibly
#'
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' export_metadata(corp, "corpus_metadata.xlsx")
#'
#' @export
export_metadata <- function(corpus_obj, Excelfile, overwrite = FALSE, 
                           mandatory = c("Age", "Gender")) {
  
  if (!overwrite && file.exists(Excelfile)) {
    cli::cli_abort("File {.path {Excelfile}} exists. Use overwrite=TRUE to replace.")
  }
  
  con <- get_connection(corpus_obj)
  db_uuid <- get_db_uuid(corpus_obj)
  
  # OPTIMIZATION: Use the optimized get_metadata which does everything in one query
  bundle_metadata <- get_metadata(corpus_obj)
  
  # Convert to data.frame for openxlsx
  bundle_metadata <- as.data.frame(bundle_metadata)
  
  # Ensure mandatory columns exist
  for (col in mandatory) {
    if (!col %in% names(bundle_metadata)) {
      bundle_metadata[[col]] <- NA
    }
  }
  
  # OPTIMIZATION: Get session metadata with a single query
  session_meta_long <- data.table::setDT(DBI::dbGetQuery(con, "
    SELECT 
      s.name as session,
      ms.field_name,
      ms.field_value
    FROM session s
    LEFT JOIN metadata_session ms 
      ON ms.db_uuid = s.db_uuid AND ms.session = s.name
    WHERE s.db_uuid = ?
  ", params = list(db_uuid)))
  
  if (nrow(session_meta_long) > 0 && !all(is.na(session_meta_long$field_name))) {
    # Convert to wide format
    session_metadata <- data.table::dcast(
      session_meta_long[!is.na(field_name)],
      session ~ field_name,
      value.var = "field_value",
      fun.aggregate = function(x) x[1]
    )
    session_metadata <- as.data.frame(session_metadata)
  } else {
    # Just session names
    sessions <- list_sessions_from_cache(con, db_uuid)
    session_metadata <- data.frame(session = sessions$name)
  }
  
  # Ensure mandatory columns in session metadata
  for (col in mandatory) {
    if (!col %in% names(session_metadata)) {
      session_metadata[[col]] <- NA
    }
  }
  
  # Get database-level metadata
  db_metadata <- DBI::dbGetQuery(con,
    "SELECT field_name, field_value FROM metadata_database WHERE db_uuid = ?",
    params = list(db_uuid)
  )
  
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
#' Reads bundle, session, and database metadata from an Excel workbook
#' (as created by \code{\link{export_metadata}}) and writes it back to
#' the corpus JSON files and SQLite cache.
#'
#' @param corpus_obj A corpus object
#' @param Excelfile Path to Excel file to import
#' @return The corpus object, invisibly
#'
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' import_metadata(corp, "corpus_metadata.xlsx")
#'
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
      add_metadata(corpus_obj, meta_list, session = session, bundle = bundle)
    }
  }

  # Process session metadata
  for (i in seq_len(nrow(session_meta))) {
    session <- session_meta$session[i]

    meta_list <- as.list(session_meta[i, !names(session_meta) %in% c("session")])
    meta_list <- meta_list[!is.na(meta_list)]

    if (length(meta_list) > 0) {
      add_metadata(corpus_obj, meta_list, session = session)
    }
  }

  # Process database metadata
  if (nrow(db_meta) > 0) {
    meta_list <- as.list(db_meta[1, ])
    meta_list <- meta_list[!is.na(meta_list)]

    if (length(meta_list) > 0) {
      add_metadata(corpus_obj, meta_list)
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
#' Sets metadata at the database, session, or bundle level. Writes to
#' METADATA.json (ground truth) and updates the SQLite cache. Level is
#' determined by which of \code{session}/\code{bundle} are provided.
#'
#' @param corpus_obj A corpus object
#' @param metadataList Named list of metadata key-value pairs
#' @param session Optional session name (required for session/bundle level)
#' @param bundle Optional bundle name (requires session)
#' @param reset.before.add Clear existing metadata at this level first (default: FALSE)
#' @return The corpus object, invisibly
#'
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' add_metadata(corp, list(Project = "MyStudy"))
#' add_metadata(corp, list(Age = 25, Gender = "Female"), session = "S1")
#'
#' @export
add_metadata <- function(corpus_obj, metadataList, session = NULL, bundle = NULL,
                        reset.before.add = FALSE) {

  if (!is.null(bundle) && is.null(session)) {
    cli::cli_abort("Bundle requires session")
  }

  level <- if (is.null(session)) "database"
           else if (is.null(bundle)) "session"
           else "bundle"

  if (reset.before.add) {
    clear_metadata(corpus_obj, session, bundle, level)
  }

  # Route through the bracket-assignment dispatcher so there is exactly one
  # write path for metadata. Literal names are anchored as regex so they
  # do not accidentally match other sessions/bundles.
  to_pattern <- function(s) {
    if (is.null(s)) return(NULL)
    paste0("^", gsub("([\\.\\*\\+\\?\\^\\$\\{\\}\\(\\)\\|\\[\\]\\\\])",
                     "\\\\\\1", s, perl = TRUE), "$")
  }

  corpus_assign_metadata(
    corpus_obj,
    session_pattern = to_pattern(session),
    bundle_pattern = to_pattern(bundle),
    metadata_list = metadataList
  )

  invisible(corpus_obj)
}

#' Clear metadata at a specific level
#' @keywords internal
clear_metadata <- function(corpus_obj, session, bundle, level) {
  basePath <- corpus_obj@basePath
  
  if (level == "database") {
    # Delete METADATA.json from database root
    db_name <- basename(basePath)
    db_name <- sub("_emuDB$", "", db_name)
    meta_file <- file.path(basePath, metadata.filename)
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }

  } else if (level == "session") {
    # Delete METADATA.json from session directory
    meta_file <- file.path(basePath, paste0(session, "_ses"), metadata.filename)
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }

  } else if (level == "bundle") {
    # Delete METADATA.json from bundle directory
    meta_file <- file.path(
      basePath,
      paste0(session, "_ses"),
      paste0(bundle, "_bndl"),
      metadata.filename
    )
    if (file.exists(meta_file)) {
      unlink(meta_file)
    }
  }
}

#' Enrich query results with metadata
#'
#' Joins metadata (Age, Gender, etc.) onto a segment list or data.frame
#' containing session and bundle columns.
#'
#' @param segs_tbl A segment_list or data.frame with session and bundle columns
#' @param corpus_obj A corpus object
#' @param compute_digests Compute file checksums before joining (default: FALSE)
#' @param algorithm Hash algorithm for digests (default: "sha1")
#' @return An \code{extended_segment_list} with metadata columns appended
#'
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' segs <- ask_for(corp, "Phonetic == t")
#' enriched <- biographize(segs, corp)
#'
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
  result <- merge(segs_tbl, metadata, by = c("session", "bundle"), all.x = TRUE)

  # Preserve S7 class after join
  if (S7::S7_inherits(segs_tbl, extended_segment_list)) {
    result <- extended_segment_list(data = as.data.frame(result))
  } else if (S7::S7_inherits(segs_tbl, segment_list)) {
    result <- extended_segment_list(data = as.data.frame(result))
  }

  if (S7::S7_inherits(segs_tbl, segment_list)) {
    result <- .record_step(result, segs_tbl, "biographize", sys.call(-1L))
  }

  result
}

#' Add file digests to metadata
#'
#' Computes checksums for signal files and stores them as metadata.
#'
#' @param corpus_obj A corpus object
#' @param sessionPattern Regex pattern to filter sessions (default: ".*")
#' @param bundlePattern Regex pattern to filter bundles (default: ".*")
#' @param algorithm Hash algorithm (default: "sha1")
#' @return The corpus object, invisibly
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

#' Get connection from corpus object (alias for get_corpus_connection)
#' @keywords internal
get_connection <- function(corpus_obj) {
  get_corpus_connection(corpus_obj)
}

#' List sessions from cache
#' @keywords internal
list_sessions_from_cache <- function(con, db_uuid) {
  DBI::dbGetQuery(con,
    "SELECT name FROM session WHERE db_uuid = ?",
    params = list(db_uuid)
  )
}

#' List bundles from cache
#' @keywords internal
list_bundles_from_cache <- function(con, db_uuid) {
  DBI::dbGetQuery(con,
    "SELECT session, name FROM bundle WHERE db_uuid = ?",
    params = list(db_uuid)
  )
}
