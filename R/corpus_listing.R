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
#' @noRd
.list_sessions <- function(obj) {
  bp <- .extract_basePath_dbName(obj)
  basePath <- bp$basePath
  dbName <- bp$dbName

  # Use cached connection for corpus S7 objects
  if (S7::S7_inherits(obj, corpus)) {
    con <- get_or_create_connection(obj)
    result <- DBI::dbGetQuery(con, "SELECT name FROM session ORDER BY name")
    return(tibble::as_tibble(result))
  }

  # Try SQLite cache for other object types
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
#' @noRd
.list_bundles <- function(obj, session = NULL) {
  bp <- .extract_basePath_dbName(obj)
  basePath <- bp$basePath
  dbName <- bp$dbName

  # Use cached connection for corpus S7 objects
  if (S7::S7_inherits(obj, corpus)) {
    con <- get_or_create_connection(obj)
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

  # Try SQLite cache for other object types
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

  result_collector <- vector("list", length(sessions))
  for (i in seq_along(sessions)) {
    sess <- sessions[i]
    ses_dir <- file.path(basePath, paste0(sess, session.suffix))
    if (!dir.exists(ses_dir)) next

    bndl_dirs <- list.dirs(ses_dir, full.names = FALSE, recursive = FALSE)
    bndl_dirs <- bndl_dirs[grepl(paste0(bundle.dir.suffix, "$"), bndl_dirs)]
    if (length(bndl_dirs) > 0) {
      result_collector[[i]] <- data.frame(
        session = sess,
        name = sub(paste0(bundle.dir.suffix, "$"), "", bndl_dirs),
        stringsAsFactors = FALSE
      )
    }
  }
  tibble::as_tibble(do.call(rbind, result_collector))
}

#' List perspectives from DBconfig
#'
#' Reads the database configuration and extracts perspective names.
#'
#' @param obj A corpus object or emuDBhandle-like list
#' @return A tibble with column \code{name}
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
.check_db_handle <- function(obj) {
  if (S7::S7_inherits(obj, corpus)) return(invisible(TRUE))
  if (is.list(obj) && inherits(obj, "emuDBhandle")) return(invisible(TRUE))
  if (is.list(obj) && !is.null(obj$basePath) && !is.null(obj$dbName)) return(invisible(TRUE))
  cli::cli_abort("Expected a corpus object or emuDBhandle, got {.cls {class(obj)}}")
}

# ==============================================================================
# SIGNAL FILE DISCOVERY
# ==============================================================================

#' List signal (media) files in a corpus
#'
#' Queries the SQLite cache for all bundles and constructs the file paths
#' to their media files. Only files that exist on disk are returned.
#' This is a fast, emuR-independent replacement for \code{emuR::list_files()}.
#'
#' @param corpus_obj A \code{\link{corpus}} S7 object
#' @param extension Optional media file extension override. Defaults to the
#'   value in the database configuration (\code{mediafileExtension}), falling
#'   back to \code{"wav"}.
#' @return A \code{data.table} with columns:
#'   \describe{
#'     \item{session}{Session name (character)}
#'     \item{bundle}{Bundle name (character)}
#'     \item{name}{Signal filename including extension, e.g. \code{"msajc003.wav"} (character)}
#'     \item{extension}{File extension, e.g. \code{"wav"} (character)}
#'     \item{full_path}{Absolute path to the signal file (character)}
#'   }
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/ae_emuDB")
#' sig <- peek_signals(corp)
#' sig$full_path[1]
#' }
#' @export
peek_signals <- function(corpus_obj, extension = NULL) {
  if (!S7::S7_inherits(corpus_obj, corpus)) {
    cli::cli_abort("{.arg corpus_obj} must be a {.cls corpus} object")
  }

  ext <- extension %||% corpus_obj@config$mediafileExtension %||% "wav"
  basePath <- corpus_obj@basePath

  con <- get_or_create_connection(corpus_obj)
  bundles <- DBI::dbGetQuery(
    con,
    "SELECT session, name FROM bundle ORDER BY session, name"
  )

  if (nrow(bundles) == 0L) {
    return(data.table::data.table(
      session   = character(0L),
      bundle    = character(0L),
      name      = character(0L),
      extension = character(0L),
      full_path = character(0L)
    ))
  }

  dt <- data.table::as.data.table(bundles)
  data.table::setnames(dt, "name", "bundle")

  dt[, full_path := file.path(
    basePath,
    paste0(session, "_ses"),
    paste0(bundle, "_bndl"),
    paste0(bundle, ".", ext)
  )]
  dt[, name := paste0(bundle, ".", ext)]
  dt[, extension := ext]

  # Keep only files that actually exist on disk
  dt <- dt[file.exists(full_path)]

  # Return in canonical column order
  dt[, .(session, bundle, name, extension, full_path)]
}

# ==============================================================================
# INTERNAL HELPER
# ==============================================================================

#' Extract basePath and dbName from a corpus or emuDBhandle
#'
#' @param obj A corpus object or emuDBhandle-like list
#' @return A list with \code{basePath} and \code{dbName}
#' @keywords internal
#' @noRd
.extract_basePath_dbName <- function(obj) {
  if (S7::S7_inherits(obj, corpus)) {
    return(list(basePath = obj@basePath, dbName = obj@dbName))
  }
  if (is.list(obj) && !is.null(obj$basePath)) {
    return(list(basePath = obj$basePath, dbName = obj$dbName))
  }
  cli::cli_abort("obj must be a corpus or emuDBhandle")
}
