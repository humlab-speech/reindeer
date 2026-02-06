# ==============================================================================
# NATIVE SESSION/BUNDLE LISTING
# ==============================================================================
#
# Replaces emuR::list_sessions, emuR::list_bundles, emuR::list_perspectives,
# and emuR::list_ssffTrackDefinitions with native implementations that query
# _emuDBcache.sqlite directly or scan the filesystem as fallback.
#
# Works with both corpus S7 objects and emuDBhandle-like lists.

#' List sessions in a database
#'
#' Queries the SQLite cache for session names. Falls back to filesystem scan
#' if the cache file does not exist.
#'
#' @param obj A corpus object or emuDBhandle-like list with basePath and dbName
#' @return A tibble with column \code{name}
#' @keywords internal
.list_sessions <- function(obj) {
  bp <- .extract_basePath_dbName(obj)
  basePath <- bp$basePath
  dbName <- bp$dbName

  # Try SQLite cache first

  cache_path <- file.path(basePath, paste0(dbName, database.cache.suffix))
  if (file.exists(cache_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    result <- DBI::dbGetQuery(con, "SELECT name FROM session ORDER BY name")
    return(tibble::as_tibble(result))
  }

  # Fallback: scan filesystem for *_ses directories
  ses_dirs <- list.dirs(basePath, full.names = FALSE, recursive = FALSE)
  ses_dirs <- ses_dirs[grepl(paste0(session.suffix, "$"), ses_dirs)]
  names <- sub(paste0(session.suffix, "$"), "", ses_dirs)
  tibble::tibble(name = sort(names))
}

#' List bundles in a database
#'
#' Queries the SQLite cache for bundle names, optionally filtered by session.
#' Falls back to filesystem scan if the cache file does not exist.
#'
#' @param obj A corpus object or emuDBhandle-like list
#' @param session Optional session name to filter by
#' @return A tibble with columns \code{session} and \code{name}
#' @keywords internal
.list_bundles <- function(obj, session = NULL) {
  bp <- .extract_basePath_dbName(obj)
  basePath <- bp$basePath
  dbName <- bp$dbName

  # Try SQLite cache first
  cache_path <- file.path(basePath, paste0(dbName, database.cache.suffix))
  if (file.exists(cache_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (!is.null(session)) {
      result <- DBI::dbGetQuery(
        con,
        "SELECT session, name FROM bundle WHERE session = ? ORDER BY session, name",
        params = list(session)
      )
    } else {
      result <- DBI::dbGetQuery(
        con,
        "SELECT session, name FROM bundle ORDER BY session, name"
      )
    }
    return(tibble::as_tibble(result))
  }

  # Fallback: scan filesystem
  sessions <- if (!is.null(session)) {
    session
  } else {
    ses_dirs <- list.dirs(basePath, full.names = FALSE, recursive = FALSE)
    sub(paste0(session.suffix, "$"), "", ses_dirs[grepl(paste0(session.suffix, "$"), ses_dirs)])
  }

  result <- data.frame(session = character(), name = character(), stringsAsFactors = FALSE)
  for (sess in sessions) {
    ses_dir <- file.path(basePath, paste0(sess, session.suffix))
    if (!dir.exists(ses_dir)) next

    bndl_dirs <- list.dirs(ses_dir, full.names = FALSE, recursive = FALSE)
    bndl_dirs <- bndl_dirs[grepl(paste0(bundle.dir.suffix, "$"), bndl_dirs)]
    if (length(bndl_dirs) > 0) {
      result <- rbind(result, data.frame(
        session = sess,
        name = sub(paste0(bundle.dir.suffix, "$"), "", bndl_dirs),
        stringsAsFactors = FALSE
      ))
    }
  }
  tibble::as_tibble(result)
}

#' List perspectives from DBconfig
#'
#' Reads the database configuration and extracts perspective names.
#'
#' @param obj A corpus object or emuDBhandle-like list
#' @return A tibble with column \code{name}
#' @keywords internal
.list_perspectives <- function(obj) {
  config <- load_DBconfig(obj)
  perspectives <- config$EMUwebAppConfig$perspectives
  if (is.null(perspectives) || length(perspectives) == 0) {
    return(tibble::tibble(name = character()))
  }
  names <- vapply(perspectives, function(p) p$name %||% "", character(1))
  tibble::tibble(name = names)
}

#' List SSFF track definitions from DBconfig
#'
#' Reads the database configuration and extracts SSFF track definitions.
#'
#' @param obj A corpus object or emuDBhandle-like list
#' @return A tibble with columns \code{name}, \code{columnName}, \code{fileExtension}
#' @keywords internal
.list_ssffTrackDefinitions <- function(obj) {
  config <- load_DBconfig(obj)
  tracks <- config$ssffTrackDefinitions
  if (is.null(tracks) || length(tracks) == 0) {
    return(tibble::tibble(
      name = character(),
      columnName = character(),
      fileExtension = character()
    ))
  }
  tibble::tibble(
    name = vapply(tracks, function(t) t$name %||% "", character(1)),
    columnName = vapply(tracks, function(t) t$columnName %||% "", character(1)),
    fileExtension = vapply(tracks, function(t) t$fileExtension %||% "", character(1))
  )
}

#' Get SSFF track names in use from a DBconfig
#'
#' Extracts the names of all defined SSFF tracks from a pre-loaded
#' database configuration list.
#'
#' @param config A DBconfig list (as returned by \code{load_DBconfig})
#' @return Character vector of track names
#' @keywords internal
.get_ssff_tracks_in_use <- function(config) {
  if (is.null(config$ssffTrackDefinitions)) return(character(0))
  vapply(config$ssffTrackDefinitions, function(t) t$name %||% "", character(1))
}

#' Validate that an object is a corpus or emuDBhandle
#'
#' Checks whether the input is a corpus S7 object, an emuDBhandle, or a list
#' with the required \code{basePath} and \code{dbName} fields.
#'
#' @param obj Object to check
#' @return Invisible \code{TRUE} on success; aborts otherwise
#' @keywords internal
.check_db_handle <- function(obj) {
  if (S7::S7_inherits(obj, corpus)) return(invisible(TRUE))
  if (is.list(obj) && inherits(obj, "emuDBhandle")) return(invisible(TRUE))
  if (is.list(obj) && !is.null(obj$basePath) && !is.null(obj$dbName)) return(invisible(TRUE))
  cli::cli_abort("Expected a corpus object or emuDBhandle, got {.cls {class(obj)}}")
}

# ==============================================================================
# INTERNAL HELPER
# ==============================================================================

#' Extract basePath and dbName from a corpus or emuDBhandle
#'
#' @param obj A corpus object or emuDBhandle-like list
#' @return A list with \code{basePath} and \code{dbName}
#' @keywords internal
.extract_basePath_dbName <- function(obj) {
  if (S7::S7_inherits(obj, corpus)) {
    return(list(basePath = obj@basePath, dbName = obj@dbName))
  }
  if (is.list(obj) && !is.null(obj$basePath)) {
    return(list(basePath = obj$basePath, dbName = obj$dbName))
  }
  cli::cli_abort("obj must be a corpus or emuDBhandle")
}
