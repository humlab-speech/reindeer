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
#' @param create Logical; if TRUE and path doesn't exist, create a new database
#' @param sync_eaf Logical; enable auto-sync of EAF files on annotation changes
#' @param sync_cmdi Logical; enable auto-sync of CMDI metadata file on changes
#' @param cache_dir Character; path to quantify cache dir (default: \code{basePath/.quantify_cache})
#' @param quick Logical; if TRUE, skip cache/metadata rebuild when existing cache is present
#'
#' @returns A corpus object with access to database contents
#'
#' @section Properties:
#' \describe{
#'   \item{dbName}{The database name (without _emuDB suffix)}
#'   \item{basePath}{Full path to the database directory}
#'   \item{config}{Database configuration (DBconfig) loaded from JSON}
#'   \item{.uuid}{Database UUID for identification}
#'   \item{.connection}{Environment holding cached SQLite connection (reference semantics)}
#'   \item{.cache_dir}{Path to the quantify/enrich cache directory}
#'   \item{.sync}{Sync configuration list, or NULL if sync not configured}
#' }
#'
#' @section Usage:
#' \describe{
#'   \item{`corpus(path)`}{Create corpus from path or emuDBhandle}
#'   \item{`corpus(path, sync_eaf=TRUE)`}{Enable EAF auto-sync}
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
    .cache_dir = S7::class_character,
    .sync = S7::class_any
  ),
  constructor = function(path, verbose = FALSE, create = FALSE,
                         sync_eaf = FALSE, sync_cmdi = FALSE,
                         cache_dir = NULL, quick = FALSE) {
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
    assertthat::assert_that(
      assertthat::is.flag(create),
      msg = "create must be TRUE or FALSE"
    )

    if (is.character(path)) {
      assertthat::assert_that(
        assertthat::is.string(path),
        msg = "path must be a single character string"
      )
      
      # Auto-append _emuDB if not present
      if (!endsWith(path, "_emuDB")) {
        path <- paste0(path, "_emuDB")
        if (verbose) {
          cli::cli_alert_info("Auto-appending suffix: {.path {path}}")
        }
      }
      
      # Check if path exists
      if (!dir.exists(path)) {
        if (create) {
          # Create new database
          dbName <- sub("_emuDB$", "", basename(path))
          basePath <- create_new_emuDB(path, dbName, verbose)
        } else {
          # Provide helpful error message
          cli::cli_abort(c(
            "Database path {.path {path}} does not exist",
            "i" = "To create a new corpus, use: {.code corpus('{path}', create = TRUE)}",
            "i" = "Or create with emuR first: {.code emuR::create_emuDB(name='{sub('_emuDB$', '', basename(path))}', targetDir='{dirname(path)}')}"
          ))
        }
      } else {
        # Path exists - validate it's a proper _emuDB
        assertthat::assert_that(
          endsWith(basename(path), "_emuDB"),
          msg = "Database directory should end with '_emuDB'"
        )
        basePath <- path
        dbName <- sub("_emuDB$", "", basename(basePath))
      }

      # Build/update cache (skip if quick mode and cache already exists)
      cache_file <- file.path(basePath, paste0(dbName, "_emuDBcache.sqlite"))
      if (quick && file.exists(cache_file)) {
        if (verbose) cli::cli_alert_info("Quick mode: reusing existing cache")
      } else {
        build_emuDB_cache(basePath, verbose = verbose)
      }

      # Gather metadata into cache
      if (verbose) {
        cli::cli_h2("Gathering metadata")
      }

    } else if ("emuDBhandle" %in% class(path)) {
      handle <- path
      dbName <- handle$dbName
      basePath <- handle$basePath

      # Ensure cache exists (in quick mode, skip rebuild if cache present)
      cache_file <- file.path(basePath, paste0(dbName, "_emuDBcache.sqlite"))
      if (!file.exists(cache_file)) {
        build_emuDB_cache(basePath, verbose = verbose)
      } else if (!quick) {
        build_emuDB_cache(basePath, verbose = verbose)
      }
    } else {
      cli::cli_abort("Invalid input: expected path or emuDBhandle")
    }

    # Load configuration
    config <- load_DBconfig(basePath)

    # Resolve cache directory
    resolved_cache_dir <- cache_dir %||% file.path(basePath, ".quantify_cache")

    # Handle sync configuration
    if (sync_eaf || sync_cmdi) {
      existing_sync <- load_sync_config_from_path(basePath)
      sync_config <- .init_sync_config(basePath, sync_eaf, sync_cmdi,
                                        existing = existing_sync)
    } else {
      sync_config <- load_sync_config_from_path(basePath)
    }

    if (!is.null(sync_config) && sync_config$enabled && verbose) {
      cli::cli_alert_info("Auto-sync is enabled for this database")
    }

    # Create corpus object
    corpus_obj <- S7::new_object(
      S7::S7_object(),
      dbName = dbName,
      basePath = basePath,
      config = config,
      .uuid = config$UUID,
      .connection = new.env(parent = emptyenv()),
      .cache_dir = resolved_cache_dir,
      .sync = sync_config
    )

    # Add "corpus" as FIRST class for S3 method dispatch priority
    # This allows [<-.corpus to work, taking precedence over S7's subsettability check
    class(corpus_obj) <- c("corpus", class(corpus_obj))

    # Gather metadata after object creation
    con <- get_or_create_connection(corpus_obj)
    initialize_metadata_schema(con)

    # Gather from .meta_json files (ground truth) — skip in quick mode if metadata exists
    if (quick) {
      has_metadata <- tryCatch({
        n <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM metadata_bundle")$n
        n > 0
      }, error = function(e) FALSE)
      if (!has_metadata) {
        gather_metadata_internal(corpus_obj, verbose = verbose)
      } else if (verbose) {
        cli::cli_alert_info("Quick mode: reusing cached metadata")
      }
    } else {
      gather_metadata_internal(corpus_obj, verbose = verbose)
    }

    # Auto-regenerate FAIR artifacts when the dirty bit was flipped by an
    # earlier metadata write. Gated by an option so power users can disable
    # it; default off in 0.8.x to preserve the previous opt-in behaviour.
    if (isTRUE(getOption("reindeer.auto_cmdi", FALSE)) &&
        .is_metadata_dirty(corpus_obj)) {
      tryCatch(
        describe_corpus(corpus_obj, verbose = FALSE),
        error = function(e) {
          if (verbose) {
            cli::cli_alert_warning(
              "Auto-CMDI regeneration failed: {conditionMessage(e)}"
            )
          }
        }
      )
    }

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
#' @param .data A data.frame with at least \code{session} and \code{bundle}
#'   character columns. Additional columns represent metadata fields.
#' @return A \code{bundle_list} object (inherits from data.frame)
#'
#' @keywords internal
bundle_list <- S7::new_class(
  "bundle_list",
  parent = S7::class_data.frame,
  constructor = function(.data = data.frame(session = character(), bundle = character(),
                                            stringsAsFactors = FALSE)) {
    # Ensure required columns exist
    if (!all(c("session", "bundle") %in% names(.data))) {
      .data <- data.frame(session = character(), bundle = character(),
                          stringsAsFactors = FALSE)
    }

    # Coerce to plain data.frame (S7 class_data.frame parent requires it)
    if (!is.data.frame(.data)) {
      .data <- as.data.frame(.data, stringsAsFactors = FALSE)
    } else if (inherits(.data, "data.table")) {
      .data <- as.data.frame(.data, stringsAsFactors = FALSE)
    }

    S7::new_object(
      .parent = .data
    )
  },
  validator = function(self) {
    required_cols <- c("session", "bundle")
    if (!all(required_cols %in% names(self))) {
      sprintf("bundle_list must contain columns: %s",
              paste(required_cols, collapse = ", "))
    }
  }
)
