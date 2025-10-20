# ==============================================================================
# DRAFT ANNOTATION CACHE SYSTEM
# ==============================================================================
#
# Infrastructure for caching computationally intensive draft annotation generation
# with resume capability and parameter tracking.
#
# Key features:
# - Date-based cache file naming
# - Automatic resume from interrupted processes
# - Parameter storage with results
# - Fallback to previous day's cache
# - Force overwrite option

# ==============================================================================
# CACHE FILE NAMING AND LOCATION
# ==============================================================================

#' Get cache directory for draft annotations
#'
#' Returns the directory where draft annotation caches are stored,
#' using the same location as enrich/quantify caches.
#'
#' @param corpus_obj A corpus object
#' @return Path to draft cache directory
#' @keywords internal
get_draft_cache_dir <- function(corpus_obj) {
  base_path <- corpus_obj@basePath
  cache_dir <- file.path(base_path, ".draft_cache")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_dir
}

#' Generate cache file name for draft function
#'
#' Creates a standardized cache file name based on draft function name and date.
#' Format: <function_name>_<YYYYMMDD>.sqlite
#' Where function_name has "draft_" prefix removed.
#'
#' @param draft_function_name Name of the draft function (with or without "draft_" prefix)
#' @param date Date for cache file (defaults to today)
#' @return Cache file name
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_draft_cache_filename("draft_momel_intsint")
#' # Returns: "momel_intsint_20251020.sqlite"
#'
#' get_draft_cache_filename("momel_intsint", as.Date("2025-10-19"))
#' # Returns: "momel_intsint_20251019.sqlite"
#' }
get_draft_cache_filename <- function(draft_function_name, date = Sys.Date()) {
  # Remove "draft_" prefix if present
  clean_name <- sub("^draft_", "", draft_function_name)

  # Format date as YYYYMMDD
  date_str <- format(date, "%Y%m%d")

  sprintf("%s_%s.sqlite", clean_name, date_str)
}

#' Get full path to draft cache file
#'
#' @param corpus_obj A corpus object
#' @param draft_function_name Name of the draft function
#' @param date Date for cache file (defaults to today)
#' @return Full path to cache file
#' @keywords internal
get_draft_cache_path <- function(corpus_obj, draft_function_name, date = Sys.Date()) {
  cache_dir <- get_draft_cache_dir(corpus_obj)
  filename <- get_draft_cache_filename(draft_function_name, date)
  file.path(cache_dir, filename)
}

#' Find existing draft cache files
#'
#' Searches for cache files for a given draft function, ordered by date (newest first).
#' Includes today's cache and previous days' caches.
#'
#' @param corpus_obj A corpus object
#' @param draft_function_name Name of the draft function
#' @return Character vector of cache file paths, sorted by date (newest first)
#' @keywords internal
find_draft_cache_files <- function(corpus_obj, draft_function_name) {
  cache_dir <- get_draft_cache_dir(corpus_obj)
  clean_name <- sub("^draft_", "", draft_function_name)

  # Pattern: <function_name>_YYYYMMDD.sqlite
  pattern <- sprintf("%s_[0-9]{8}\\.sqlite$", clean_name)

  cache_files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)

  if (length(cache_files) == 0) {
    return(character(0))
  }

  # Extract dates and sort by date (newest first)
  extract_date <- function(path) {
    basename(path) %>%
      sub(pattern = sprintf("%s_", clean_name), replacement = "") %>%
      sub(pattern = "\\.sqlite$", replacement = "") %>%
      as.Date(format = "%Y%m%d")
  }

  dates <- sapply(cache_files, extract_date)
  cache_files[order(dates, decreasing = TRUE)]
}

# ==============================================================================
# CACHE DATABASE SCHEMA
# ==============================================================================

#' Initialize draft cache database
#'
#' Creates SQLite database for storing draft annotations with metadata.
#'
#' @param cache_path Path to cache file
#' @param draft_function_name Name of the draft function
#' @return Connection to cache database
#' @keywords internal
initialize_draft_cache <- function(cache_path, draft_function_name) {

  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)

  # Metadata table - stores information about the draft generation run
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS draft_metadata (
      id INTEGER PRIMARY KEY,
      draft_function TEXT NOT NULL,
      created_at TEXT NOT NULL,
      last_updated TEXT NOT NULL,
      corpus_path TEXT NOT NULL,
      corpus_uuid TEXT NOT NULL,
      n_bundles_total INTEGER,
      n_bundles_completed INTEGER DEFAULT 0,
      parameters_json TEXT,  -- JSON of parameters passed to draft function
      completed LOGICAL DEFAULT 0
    )")

  # Drafts table - stores draft annotations for each bundle
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS draft_annotations (
      draft_id INTEGER PRIMARY KEY AUTOINCREMENT,
      session TEXT NOT NULL,
      bundle TEXT NOT NULL,
      level_name TEXT NOT NULL,
      level_type TEXT NOT NULL,  -- SEGMENT, EVENT, or ITEM
      attribute_name TEXT NOT NULL,
      annotations_blob BLOB NOT NULL,  -- Serialized data.frame of annotations
      parameters_json TEXT,  -- Parameters used for this bundle
      created_at TEXT NOT NULL,
      error_occurred LOGICAL DEFAULT 0,
      error_message TEXT,
      UNIQUE (session, bundle, level_name, attribute_name)
    )")

  # Create indices
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_draft_bundle
    ON draft_annotations(session, bundle)")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_draft_level
    ON draft_annotations(level_name)")

  con
}

# ==============================================================================
# CACHE OPERATIONS
# ==============================================================================

#' Get or create draft cache for today
#'
#' Returns connection to today's cache file, creating it if necessary.
#' If cache exists, checks for compatibility and optionally resumes.
#'
#' @param corpus_obj A corpus object
#' @param draft_function_name Name of the draft function
#' @param parameters Named list of parameters passed to draft function
#' @param force_overwrite If TRUE, ignore existing cache and start fresh
#' @param verbose Show progress messages
#' @return List with: con (connection), is_new (logical), n_completed (integer)
#' @keywords internal
get_draft_cache <- function(corpus_obj, draft_function_name, parameters,
                             force_overwrite = FALSE, verbose = TRUE) {

  cache_path <- get_draft_cache_path(corpus_obj, draft_function_name, Sys.Date())
  cache_exists <- file.exists(cache_path)

  if (force_overwrite && cache_exists) {
    if (verbose) {
      cli::cli_alert_info("Force overwrite: removing existing cache")
    }
    unlink(cache_path)
    cache_exists <- FALSE
  }

  # Initialize or connect to cache
  con <- initialize_draft_cache(cache_path, draft_function_name)

  is_new <- !cache_exists
  n_completed <- 0

  if (!is_new) {
    # Check existing metadata
    metadata <- DBI::dbGetQuery(con, "SELECT * FROM draft_metadata LIMIT 1")

    if (nrow(metadata) > 0) {
      n_completed <- metadata$n_bundles_completed[1]

      if (verbose) {
        cli::cli_alert_info(
          "Found existing cache with {n_completed} completed bundle{?s}"
        )
      }

      # Update last_updated timestamp
      DBI::dbExecute(con, "
        UPDATE draft_metadata
        SET last_updated = ?
        WHERE id = 1",
        params = list(as.character(Sys.time()))
      )
    } else {
      # Cache file exists but no metadata - treat as new
      is_new <- TRUE
    }
  }

  if (is_new) {
    # Insert initial metadata
    DBI::dbExecute(con, "
      INSERT INTO draft_metadata
        (draft_function, created_at, last_updated, corpus_path, corpus_uuid,
         parameters_json, n_bundles_total)
      VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(
        draft_function_name,
        as.character(Sys.time()),
        as.character(Sys.time()),
        corpus_obj@basePath,
        corpus_obj@.uuid,
        jsonlite::toJSON(parameters, auto_unbox = TRUE),
        NA_integer_  # Will be updated when we know total
      )
    )
  }

  list(
    con = con,
    path = cache_path,
    is_new = is_new,
    n_completed = n_completed
  )
}

#' Check if bundle is already in cache
#'
#' @param con Database connection
#' @param session Session name
#' @param bundle Bundle name
#' @param level_name Level name
#' @param attribute_name Attribute name
#' @return Logical; TRUE if bundle is cached
#' @keywords internal
is_bundle_cached <- function(con, session, bundle, level_name, attribute_name) {
  result <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) as count
    FROM draft_annotations
    WHERE session = ? AND bundle = ? AND level_name = ? AND attribute_name = ?
      AND error_occurred = 0",
    params = list(session, bundle, level_name, attribute_name)
  )

  result$count[1] > 0
}

#' Store draft annotations in cache
#'
#' @param con Database connection
#' @param session Session name
#' @param bundle Bundle name
#' @param level_name Level name
#' @param level_type Level type (SEGMENT, EVENT, or ITEM)
#' @param attribute_name Attribute name
#' @param annotations Data frame of annotations
#' @param parameters Parameters used for this bundle
#' @param error_occurred Did an error occur?
#' @param error_message Error message if error occurred
#' @keywords internal
store_draft_annotations <- function(con, session, bundle, level_name, level_type,
                                    attribute_name, annotations, parameters,
                                    error_occurred = FALSE, error_message = NULL) {

  # Serialize annotations
  annotations_blob <- if (!error_occurred) {
    qs::qserialize(annotations)
  } else {
    NULL
  }

  # Insert or replace
  DBI::dbExecute(con, "
    INSERT OR REPLACE INTO draft_annotations
      (session, bundle, level_name, level_type, attribute_name,
       annotations_blob, parameters_json, created_at,
       error_occurred, error_message)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      session, bundle, level_name, level_type, attribute_name,
      list(annotations_blob),
      jsonlite::toJSON(parameters, auto_unbox = TRUE),
      as.character(Sys.time()),
      as.integer(error_occurred),
      error_message
    )
  )

  # Update completed count
  DBI::dbExecute(con, "
    UPDATE draft_metadata
    SET n_bundles_completed = (
      SELECT COUNT(*) FROM draft_annotations WHERE error_occurred = 0
    ),
    last_updated = ?
    WHERE id = 1",
    params = list(as.character(Sys.time()))
  )
}

#' Retrieve draft annotations from cache
#'
#' @param con Database connection
#' @param session Session name (optional)
#' @param bundle Bundle name (optional)
#' @param level_name Level name (optional)
#' @return Data frame with cached annotations and metadata
#' @keywords internal
retrieve_draft_annotations <- function(con, session = NULL, bundle = NULL, level_name = NULL) {

  query <- "
    SELECT session, bundle, level_name, level_type, attribute_name,
           annotations_blob, parameters_json, created_at,
           error_occurred, error_message
    FROM draft_annotations
    WHERE 1=1"

  params <- list()

  if (!is.null(session)) {
    query <- paste0(query, " AND session = ?")
    params <- c(params, list(session))
  }

  if (!is.null(bundle)) {
    query <- paste0(query, " AND bundle = ?")
    params <- c(params, list(bundle))
  }

  if (!is.null(level_name)) {
    query <- paste0(query, " AND level_name = ?")
    params <- c(params, list(level_name))
  }

  result <- DBI::dbGetQuery(con, query, params = params)

  if (nrow(result) == 0) {
    return(data.frame())
  }

  # Deserialize annotations
  result$annotations <- lapply(result$annotations_blob, function(blob) {
    if (is.null(blob)) return(NULL)
    qs::qdeserialize(blob[[1]])
  })

  result$annotations_blob <- NULL

  result
}

#' Mark draft generation as completed
#'
#' @param con Database connection
#' @keywords internal
mark_draft_completed <- function(con) {
  DBI::dbExecute(con, "
    UPDATE draft_metadata
    SET completed = 1, last_updated = ?
    WHERE id = 1",
    params = list(as.character(Sys.time()))
  )
}

#' Get draft cache summary
#'
#' @param corpus_obj A corpus object
#' @param draft_function_name Name of draft function
#' @param date Date of cache (defaults to today)
#' @return List with cache statistics
#' @export
draft_cache_summary <- function(corpus_obj, draft_function_name, date = Sys.Date()) {

  cache_path <- get_draft_cache_path(corpus_obj, draft_function_name, date)

  if (!file.exists(cache_path)) {
    return(list(
      exists = FALSE,
      message = sprintf("No cache found for %s on %s", draft_function_name, date)
    ))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  metadata <- DBI::dbGetQuery(con, "SELECT * FROM draft_metadata")

  if (nrow(metadata) == 0) {
    return(list(exists = TRUE, has_data = FALSE))
  }

  annotations_summary <- DBI::dbGetQuery(con, "
    SELECT
      level_name,
      COUNT(*) as n_bundles,
      SUM(CASE WHEN error_occurred = 0 THEN 1 ELSE 0 END) as n_successful,
      SUM(CASE WHEN error_occurred = 1 THEN 1 ELSE 0 END) as n_errors
    FROM draft_annotations
    GROUP BY level_name
  ")

  list(
    exists = TRUE,
    has_data = TRUE,
    cache_path = cache_path,
    draft_function = metadata$draft_function[1],
    created_at = metadata$created_at[1],
    last_updated = metadata$last_updated[1],
    n_bundles_total = metadata$n_bundles_total[1],
    n_bundles_completed = metadata$n_bundles_completed[1],
    completed = as.logical(metadata$completed[1]),
    parameters = jsonlite::fromJSON(metadata$parameters_json[1]),
    annotations_by_level = annotations_summary
  )
}

#' List all draft caches for a corpus
#'
#' @param corpus_obj A corpus object
#' @return Data frame with cache information
#' @export
list_draft_caches <- function(corpus_obj) {

  cache_dir <- get_draft_cache_dir(corpus_obj)
  cache_files <- list.files(cache_dir, pattern = "\\.sqlite$", full.names = TRUE)

  if (length(cache_files) == 0) {
    cli::cli_alert_info("No draft caches found")
    return(data.frame())
  }

  cache_info <- lapply(cache_files, function(cache_path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    metadata <- DBI::dbGetQuery(con, "SELECT * FROM draft_metadata LIMIT 1")

    if (nrow(metadata) == 0) {
      return(NULL)
    }

    data.frame(
      cache_file = basename(cache_path),
      draft_function = metadata$draft_function[1],
      created_at = metadata$created_at[1],
      last_updated = metadata$last_updated[1],
      n_completed = metadata$n_bundles_completed[1],
      n_total = metadata$n_bundles_total[1],
      completed = as.logical(metadata$completed[1]),
      path = cache_path,
      stringsAsFactors = FALSE
    )
  })

  cache_info <- Filter(Negate(is.null), cache_info)

  if (length(cache_info) == 0) {
    return(data.frame())
  }

  do.call(rbind, cache_info)
}
