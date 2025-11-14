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
  query_session <- sprintf("
    SELECT 
      s.name as session,
      ms.field_name,
      ms.field_value
    FROM session s
    LEFT JOIN metadata_session ms 
      ON ms.db_uuid = s.db_uuid AND ms.session = s.name
    WHERE s.db_uuid = '%s'
  ", db_uuid)
  
  session_meta_long <- data.table::setDT(DBI::dbGetQuery(con, query_session))
  
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
