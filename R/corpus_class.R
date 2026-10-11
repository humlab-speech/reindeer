# ==============================================================================
# CORPUS S7 CLASS DEFINITION
# ==============================================================================

#' Open or create a speech corpus
#'
#' `corpus()` is the entry point to every reindeer workflow. Point it at
#' an EMU-SDMS database (a directory whose name ends in `_emuDB`) and you
#' get back a `corpus` object you can [query()], [quantify()], [annotate()],
#' or [describe_corpus()].
#'
#' @param path Path to the corpus directory (the `_emuDB` suffix is
#'   appended automatically if missing), or an existing `emuDBhandle`.
#' @param create If `TRUE` and the path does not exist, create an empty
#'   corpus there. Default `FALSE`.
#' @param sync_eaf,sync_cmdi Deprecated and ignored; auto-sync has been
#'   removed.
#' @param verbose Print progress while loading. Default `FALSE`.
#' @param quick Deprecated and ignored. The cache is synced incrementally on
#'   every open (only bundles whose annotation file changed are re-read), and
#'   metadata files are re-scanned when their mtime changed.
#' @param cache_dir Override the quantify/enrich cache directory.
#'   Default `<basePath>/.quantify_cache`.
#' @return A `corpus` object. The S7 class exposes the following
#'   properties you can read from user code (use `@`):
#'   * `@dbName` - corpus name (the basename minus `_emuDB`).
#'   * `@basePath` - path to the `_emuDB` directory.
#'   * `@config` - parsed `_DBconfig.json` (level + link definitions).
#'   * Internal: `@.uuid`, `@.connection`, `@.cache_dir` -
#'     used by the cache machinery; treat as read-only.
#' @section Bracket access:
#' Once you have a corpus, you can read and write metadata by name:
#' \preformatted{
#' corp["Session1", "Bundle1"]                # read metadata
#' corp["Session1", ]                         # all bundles in session
#' corp["Sess.*", "Bund.*"]                   # regex
#' corp["Session1", "Bundle1"] <- list(Age=25)# write metadata
#' corp["Session1", "Bundle1"] <- "audio.mp3" # import media
#' }
#' For programmatic metadata see [set_metadata()] / [get_metadata()].
#' @seealso [demo_corpus()], [query()], [quantify()], [load_metadata()],
#'   [annotate()]
#' @examplesIf requireNamespace("superassp", quietly = TRUE)
#' corp <- demo_corpus()
#' vowels <- query(corp, "Phonetic =~ [aeiou]", lazy = FALSE)
#' formants <- quantify(vowels, superassp::trk_formant_forest)
#' @export
corpus <- S7::new_class(
  "corpus",
  properties = list(
    dbName = S7::class_character,
    basePath = S7::class_character,
    config = S7::class_any,
    .uuid = S7::class_character,
    .connection = S7::class_any,
    .cache_dir = S7::class_character
  ),
  constructor = function(path, verbose = FALSE, create = FALSE,
                         sync_eaf = NULL, sync_cmdi = NULL,
                         cache_dir = NULL, quick = NULL) {
    if (!is.null(quick)) {
      cli::cli_warn(
        "{.arg quick} is ignored: the cache is always synced incrementally on open.",
        class = "reindeer_deprecated")
    }
    # Input validation 
    .assert(
      !is.null(path),
      length(path) > 0,
      msg = "path cannot be NULL or empty"
    )
    .assert(
      .is_flag(verbose),
      msg = "verbose must be TRUE or FALSE"
    )
    .assert(
      .is_flag(create),
      msg = "create must be TRUE or FALSE"
    )

    if (is.character(path)) {
      .assert(
        .is_string(path),
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
        .assert(
          endsWith(basename(path), "_emuDB"),
          msg = "Database directory should end with '_emuDB'"
        )
        basePath <- path
        dbName <- sub("_emuDB$", "", basename(basePath))
      }

      # Create or incrementally update the cache: only bundles whose file
      # changed since the last open are re-read.
      sync_emuDB_cache(basePath, verbose = verbose)

      # Gather metadata into cache
      if (verbose) {
        cli::cli_h2("Gathering metadata")
      }

    } else if ("emuDBhandle" %in% class(path)) {
      handle <- path
      dbName <- handle$dbName
      basePath <- handle$basePath

      sync_emuDB_cache(basePath, verbose = verbose)
    } else {
      cli::cli_abort("Invalid input: expected path or emuDBhandle")
    }

    # Load configuration
    config <- load_DBconfig(basePath)

    # Resolve cache directory
    resolved_cache_dir <- cache_dir %or_empty% file.path(basePath, ".quantify_cache")

    if (!is.null(sync_eaf) || !is.null(sync_cmdi)) {
      cli::cli_warn(
        "{.arg sync_eaf}/{.arg sync_cmdi} are ignored: auto-sync has been removed and had no effect.",
        class = "reindeer_deprecated"
      )
    }

    # Create corpus object
    corpus_obj <- S7::new_object(
      S7::S7_object(),
      dbName = dbName,
      basePath = basePath,
      config = config,
      .uuid = config$UUID,
      .connection = new.env(parent = emptyenv()),
      .cache_dir = resolved_cache_dir
    )

    # Add "corpus" as FIRST class for S3 method dispatch priority
    # This allows [<-.corpus to work, taking precedence over S7's subsettability check
    class(corpus_obj) <- c("corpus", class(corpus_obj))

    # Registered before any work that can fail (cache open, metadata parse):
    # nothing else closes the SQLite handle, and an abort below would
    # otherwise leak it until the process exits.
    .register_connection_finalizer(corpus_obj)

    # Gather metadata after object creation
    con <- get_or_create_connection(corpus_obj)
    initialize_metadata_schema(con)

    # Sync metadata from METADATA.json / legacy .meta_json files. The mtime
    # gate inside gather_metadata() makes this a no-op when nothing changed,
    # and picks up edits made outside R since the corpus was last opened.
    gather_metadata(corpus_obj, verbose = verbose)

    # FAIR artifacts are regenerated after write verbs only (add_metadata() and
    # friends), never on open: opening a corpus does not touch the disk.

    .register_corpus(corpus_obj)
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
#' @noRd
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
