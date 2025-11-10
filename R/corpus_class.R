# ==============================================================================
# CORPUS S7 CLASS DEFINITION
# ==============================================================================

#' Corpus Class - Represents an EmuR database with persistent connection and metadata management
#'
#' An S7 class representing a speech corpus that provides efficient access to
#' annotations, metadata, and signal data stored in an Emu-SDMS database.
#'
#' @param path Either a file path ending in '_emuDB' or an emuDBhandle object
#' @param verbose Show progress messages during construction
#'
#' @returns A corpus object with access to database contents
#'
#' @section Properties:
#' \describe{
#'   \item{dbName}{The database name (without _emuDB suffix)}
#'   \item{basePath}{Full path to the database directory}
#'   \item{config}{Database configuration (DBconfig) loaded from JSON}
#'   \item{.uuid}{Database UUID for identification}
#'   \item{.connection}{Cached SQLite database connection}
#'   \item{.connection_valid}{Whether the cached connection is valid}
#' }
#'
#' @section Usage:
#' \describe{
#'   \item{`corpus(path)`}{Create corpus from path or emuDBhandle}
#'   \item{`corp["Session","Bundle"]`}{Get bundle metadata}
#'   \item{`corp["Session",]`}{Get all bundles in session}
#'   \item{`corp[,"Bundle"]`}{Get bundle across sessions (if unique)}
#'   \item{`corp["Sess.*","Bund.*"]`}{Use regex patterns}
#'   \item{`corp["Session","Bundle"] <- list(Age=25)`}{Set metadata}
#'   \item{`corp["Session","Bundle"] <- "path/to/audio.mp3"`}{Import media}
#'   \item{`summary(corp)`}{Display comprehensive database summary}
#' }
#'
#' @export
corpus <- S7::new_class(
  "corpus",
  properties = list(
    dbName = S7::class_character,
    basePath = S7::class_character,
    config = S7::class_any,
    .uuid = S7::class_character,
    .connection = S7::class_any,
    .connection_valid = S7::class_logical
  ),
  constructor = function(path, verbose = FALSE) {
    # Input validation with assertthat
    assertthat::assert_that(
      !is.null(path),
      length(path) > 0,
      msg = "path cannot be NULL or empty"
    )
    assertthat::assert_that(
      assertthat::is.flag(verbose),
      msg = "verbose must be TRUE or FALSE"
    )

    if (is.character(path)) {
      assertthat::assert_that(
        assertthat::is.string(path),
        msg = "path must be a single character string"
      )
      assertthat::assert_that(
        dir.exists(path),
        msg = sprintf("Database path '%s' does not exist", path)
      )
      assertthat::assert_that(
        stringr::str_ends(basename(path), "_emuDB"),
        msg = "Database directory should end with '_emuDB'"
      )
      basePath <- path
      dbName <- stringr::str_remove(basename(basePath), "_emuDB$")

      # Build/update cache with progress display
      build_emuDB_cache(basePath, verbose = verbose)

      # Gather metadata into cache
      if (verbose) {
        cli::cli_h2("Gathering metadata")
      }

    } else if ("emuDBhandle" %in% class(path)) {
      handle <- path
      dbName <- handle$dbName
      basePath <- handle$basePath

      # Ensure cache exists
      cache_file <- file.path(basePath, paste0(dbName, "_emuDBcache.sqlite"))
      if (!file.exists(cache_file)) {
        build_emuDB_cache(basePath, verbose = verbose)
      }
    } else {
      cli::cli_abort("Invalid input: expected path or emuDBhandle")
    }

    # Load configuration
    config <- load_DBconfig(basePath)

    # Create corpus object
    corpus_obj <- S7::new_object(
      S7::S7_object(),
      dbName = dbName,
      basePath = basePath,
      config = config,
      .uuid = config$UUID,
      .connection = NULL,
      .connection_valid = FALSE
    )

    # Check for auto-sync on load
    sync_config <- load_sync_config_from_path(basePath)
    if (!is.null(sync_config) && sync_config$enabled && verbose) {
      cli::cli_alert_info("Auto-sync is enabled for this database")
    }

    # Gather metadata after object creation
    con <- get_corpus_connection(corpus_obj)
    initialize_metadata_schema(con)
    DBI::dbDisconnect(con)

    # Gather from .meta_json files (ground truth)
    gather_metadata_internal(corpus_obj, verbose = verbose)

    corpus_obj
  },
  validator = function(self) {
    if (!dir.exists(self@basePath)) {
      "Database path must exist"
    } else if (is.null(self@dbName) || nchar(self@dbName) == 0) {
      "Database name must be specified"
    } else if (is.null(self@.uuid) || nchar(self@.uuid) == 0) {
      "Database UUID must be specified"
    }
  }
)

# ==============================================================================
# BUNDLE_LIST S7 CLASS - Result of corpus subsetting
# ==============================================================================

#' Bundle List Class - Tibble with session/bundle information and metadata
#'
#' An S7 class that extends tibble to represent a list of bundles with
#' their associated metadata following inheritance rules.
#'
#' @export
bundle_list <- S7::new_class(
  "bundle_list",
  parent = S7::new_S3_class("tbl_df"),
  properties = list(
    .data = S7::class_any
  ),
  constructor = function(.data = tibble::tibble()) {
    # Ensure required columns exist
    if (!all(c("session", "bundle") %in% names(.data))) {
      .data <- tibble::tibble(session = character(), bundle = character())
    }

    # Convert to tibble if needed
    if (!inherits(.data, "tbl_df")) {
      .data <- tibble::as_tibble(.data)
    }

    S7::new_object(
      S7::S7_object(),
      .data = .data
    )
  },
  validator = function(self) {
    required_cols <- c("session", "bundle")
    if (!all(required_cols %in% names(self@.data))) {
      sprintf("bundle_list must contain columns: %s",
              paste(required_cols, collapse = ", "))
    }
  }
)
