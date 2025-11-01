
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
    # Validate input
    if (length(path) == 0 || is.null(path)) {
      cli::cli_abort("Path cannot be NULL or empty")
    }
    
    if (is.character(path)) {
      if (!dir.exists(path)) {
        cli::cli_abort("Database path {.path {path}} does not exist")
      }
      if (!stringr::str_ends(basename(path), "_emuDB")) {
        cli::cli_abort("Database directory should end with '_emuDB'")
      }
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

# ==============================================================================
# CORPUS SUBSETTING OPERATORS - corpus[i, j]
# ==============================================================================

#' Subset corpus to get bundle list with metadata
#'
#' @description 
#' Subset corpus to get bundle list with metadata
#' 
#' @param x corpus object
#' @param i session pattern (regex or literal)
#' @param j bundle pattern (regex or literal)
#' @return bundle_list object (tibble with session, bundle, and metadata columns)
#' @export
#' 

S7::method(`[`, corpus) <- function(x, i, j, ..., drop = FALSE) {
  # Handle various indexing patterns
  session_pattern <- if (!missing(i) && !is.null(i)) i else ".*"
  bundle_pattern <- if (!missing(j) && !is.null(j)) j else ".*"
  
  # If only one index provided and not named, it could be bundle name
  if (!missing(i) && missing(j)) {
    # Check if i looks like a session (contains "ses" or matches existing session)
    con <- get_corpus_connection(x)
    sessions <- list_sessions_from_cache(con, x@.uuid)
    DBI::dbDisconnect(con)
    
    # If i matches any session name, treat as session pattern
    if (any(grepl(i, sessions$name, ignore.case = TRUE))) {
      session_pattern <- i
      bundle_pattern <- ".*"
    } else {
      # Otherwise treat as bundle pattern across all sessions
      session_pattern <- ".*"
      bundle_pattern <- i
    }
  }
  
  # Get metadata for matching bundles
  metadata_df <- get_metadata_for_patterns(x, session_pattern, bundle_pattern)
  
  # Convert to bundle_list
  if (nrow(metadata_df) == 0) {
    cli::cli_alert_warning("No bundles match pattern session={.val {session_pattern}}, bundle={.val {bundle_pattern}}")
  }
  
  bundle_list(.data = metadata_df)
}

#' Assign values to corpus bundles - metadata or media import
#'
#' @param x corpus object
#' @param i session pattern
#' @param j bundle pattern
#' @param value Either a named list (metadata) or character vector (media files)
#' @export
S7::method(`[<-`, corpus) <- function(x, i, j, ..., value) {
  # Determine what type of assignment this is
  if (is.list(value) && !is.null(names(value))) {
    # Metadata assignment
    corpus_assign_metadata(x, i, j, value)
  } else if (is.character(value)) {
    # Media file import
    corpus_import_media(x, i, j, value)
  } else {
    cli::cli_abort("Value must be a named list (metadata) or character vector (media files)")
  }
  
  invisible(x)
}

# ==============================================================================
# PRINT, SUMMARY, AND GLIMPSE METHODS
# ==============================================================================

#' Print method for corpus with tidyverse-style formatting
S7::method(print, corpus) <- function(x, ...) {
  cli::cli_h1("{.cls corpus}: {.field {x@dbName}}")
  
  # Database identification
  cli::cli_div(theme = list(rule = list(`margin-top` = 0)))
  cli::cli_text("")
  cli::cli_dl(c(
    "UUID" = x@.uuid,
    "Path" = cli::style_hyperlink(x@basePath, paste0("file://", x@basePath))
  ))
  cli::cli_text("")
  
  # Get summary statistics
  tryCatch({
    con <- get_corpus_connection(x)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # Count basic entities
    stats <- list(
      sessions = DBI::dbGetQuery(con, sprintf(
        "SELECT COUNT(*) as n FROM session WHERE db_uuid = '%s'", x@.uuid
      ))$n,
      bundles = DBI::dbGetQuery(con, sprintf(
        "SELECT COUNT(*) as n FROM bundle WHERE db_uuid = '%s'", x@.uuid
      ))$n,
      items = DBI::dbGetQuery(con, sprintf(
        "SELECT COUNT(*) as n FROM items WHERE db_uuid = '%s'", x@.uuid
      ))$n,
      labels = DBI::dbGetQuery(con, sprintf(
        "SELECT COUNT(*) as n FROM labels WHERE db_uuid = '%s'", x@.uuid
      ))$n
    )
    
    # Levels available
    levels_query <- sprintf(
      "SELECT DISTINCT level FROM items WHERE db_uuid = '%s' ORDER BY level", 
      x@.uuid
    )
    levels <- DBI::dbGetQuery(con, levels_query)$level
    
    # Metadata fields - check if table exists first
    n_metadata_fields <- tryCatch({
      fields_query <- "SELECT COUNT(DISTINCT field_name) as n FROM metadata_fields"
      DBI::dbGetQuery(con, fields_query)$n
    }, error = function(e) {
      0  # Table doesn't exist yet
    })
    
    # Content summary
    cli::cli_text("{.strong Content:}")
    cli::cli_ul(c(
      "{cli::col_blue(stats$sessions)} session{?s}",
      "{cli::col_blue(stats$bundles)} bundle{?s}",
      "{cli::col_blue(stats$items)} annotation item{?s}",
      "{cli::col_blue(stats$labels)} label{?s}"
    ))
    
    cli::cli_text("")
    cli::cli_text("{.strong Annotation levels:} {.val {paste(levels, collapse = ', ')}}")
    
    if (n_metadata_fields > 0) {
      cli::cli_text("{.strong Metadata fields:} {cli::col_blue(n_metadata_fields)}")
    }
    
    cli::cli_text("")
    cli::cli_text("{.emph Use {.fn summary} for detailed information}")
    cli::cli_text("{.emph Use {.code corpus[session, bundle]} to access bundles}")
    
  }, error = function(e) {
    cli::cli_alert_warning("Could not load database statistics: {e$message}")
  })
  
  invisible(x)
}

#' Summary method for corpus - comprehensive database overview
S7::method(summary, corpus) <- function(object, ...) {
  con <- get_corpus_connection(object)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  cli::cli_rule("Summary of emuDB")
  cli::cli_text("")
  
  # Basic info
  session_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM session WHERE db_uuid = '%s'", object@.uuid
  ))$n
  
  bundle_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM bundle WHERE db_uuid = '%s'", object@.uuid
  ))$n
  
  item_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM items WHERE db_uuid = '%s'", object@.uuid
  ))$n
  
  label_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM labels WHERE db_uuid = '%s'", object@.uuid
  ))$n
  
  link_count <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM links WHERE db_uuid = '%s'", object@.uuid
  ))$n
  
  cli::cli_text("Name:            {.field {object@dbName}}")
  cli::cli_text("UUID:            {.val {object@.uuid}}")
  cli::cli_text("Directory:       {.path {object@basePath}}")
  cli::cli_text("Session count:   {.val {session_count}}")
  cli::cli_text("Bundle count:    {.val {bundle_count}}")
  cli::cli_text("Annotation item count:  {.val {item_count}}")
  cli::cli_text("Label count:     {.val {label_count}}")
  cli::cli_text("Link count:      {.val {link_count}}")
  cli::cli_text("")
  
  # Database configuration
  cli::cli_rule("Database configuration")
  cli::cli_text("")
  
  # SSFF track definitions
  if (!is.null(object@config$ssffTrackDefinitions) && 
      length(object@config$ssffTrackDefinitions) > 0) {
    cli::cli_h3("SSFF track definitions")
    cli::cli_text("")
    
    track_df <- purrr::map_dfr(object@config$ssffTrackDefinitions, function(track) {
      tibble::tibble(
        name = track$name,
        columnName = track$columnName,
        fileExtension = track$fileExtension,
        fileFormat = track$format %||% "ssff"
      )
    })
    
    print(track_df)
    cli::cli_text("")
  }
  
  # Level definitions
  if (!is.null(object@config$levelDefinitions) && 
      length(object@config$levelDefinitions) > 0) {
    cli::cli_h3("Level definitions")
    cli::cli_text("")
    
    level_df <- purrr::map_dfr(object@config$levelDefinitions, function(level) {
      attr_defs <- if (!is.null(level$attributeDefinitions)) {
        paste(purrr::map_chr(level$attributeDefinitions, ~.x$name), collapse = "; ")
      } else {
        ""
      }
      
      attr_defs <- paste0(attr_defs, ";")
      
      tibble::tibble(
        name = level$name,
        type = level$type,
        nrOfAttrDefs = length(level$attributeDefinitions %||% list()),
        attrDefNames = attr_defs
      )
    })
    
    print(level_df)
    cli::cli_text("")
  }
  
  # Link definitions
  if (!is.null(object@config$linkDefinitions) && 
      length(object@config$linkDefinitions) > 0) {
    cli::cli_h3("Link definitions")
    cli::cli_text("")
    
    link_df <- purrr::map_dfr(object@config$linkDefinitions, function(link) {
      tibble::tibble(
        type = link$type,
        superlevelName = link$superlevelName,
        sublevelName = link$sublevelName
      )
    })
    
    print(link_df)
    cli::cli_text("")
  }
  
  # Metadata summary - check if table exists
  fields <- tryCatch({
    fields_query <- "SELECT DISTINCT field_name FROM metadata_fields ORDER BY field_name"
    DBI::dbGetQuery(con, fields_query)
  }, error = function(e) {
    # Table doesn't exist
    data.frame(field_name = character(0))
  })
  
  if (nrow(fields) > 0) {
    cli::cli_h3("Metadata fields defined")
    cli::cli_text("")
    cli::cli_text(paste(fields$field_name, collapse = ", "))
    cli::cli_text("")
  }
  
  invisible(object)
}

#' Glimpse at an object
#' 
#' Provides a quick overview of object structure. For corpus objects, shows levels,
#' sample labels, and sessions. For segment_list objects, shows column-wise preview.
#' 
#' @param x An object to glimpse at  
#' @param ... Additional arguments passed to methods
#' @return Invisibly returns the object
#' @export
glimpse <- function(x, ...) {
  # Handle S7 classes explicitly since S3 dispatch doesn't work with namespace-qualified class names
  if (inherits(x, "S7_object")) {
    class_name <- class(x)[1]
    if (class_name == "reindeer::corpus") {
      return(glimpse_corpus_impl(x, ...))
    } else if (class_name == "reindeer::segment_list") {
      return(glimpse_segment_list_impl(x, ...))
    } else if (class_name == "reindeer::extended_segment_list") {
      return(glimpse_extended_segment_list_impl(x, ...))
    } else if (class_name == "reindeer::lazy_segment_list") {
      return(glimpse_lazy_segment_list_impl(x, ...))
    }
  }
  # Fall back to S3 dispatch for other classes (e.g. tibbles)
  UseMethod("glimpse")
}

# Implementation function for corpus
glimpse_corpus_impl <- function(x, ...) {
  cli::cli_h2("Corpus: {.field {x@dbName}}")
  
  con <- get_corpus_connection(x)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Get sample data for levels
  levels_query <- sprintf(
    "SELECT DISTINCT level FROM items WHERE db_uuid = '%s' LIMIT 10", 
    x@.uuid
  )
  levels <- DBI::dbGetQuery(con, levels_query)$level
  
  # Show level structure
  cli::cli_text("{.strong Levels ({length(levels)}):} {.val {paste(levels, collapse = ', ')}}")
  
  # Sample labels per level
  for (lvl in head(levels, 3)) {
    labels_query <- sprintf(
      "SELECT DISTINCT l.label 
       FROM labels l 
       INNER JOIN items i ON l.db_uuid = i.db_uuid AND l.session = i.session 
         AND l.bundle = i.bundle AND l.item_id = i.item_id
       WHERE i.db_uuid = '%s' AND i.level = '%s' 
       LIMIT 5",
      x@.uuid, lvl
    )
    labels <- DBI::dbGetQuery(con, labels_query)$label
    cli::cli_text("  {.field {lvl}}: {.val {paste(head(labels, 5), collapse = ', ')}}")
  }
  
  # Sample sessions
  sessions_query <- sprintf(
    "SELECT name FROM session WHERE db_uuid = '%s' LIMIT 5", 
    x@.uuid
  )
  sessions <- DBI::dbGetQuery(con, sessions_query)$name
  cli::cli_text("")
  cli::cli_text("{.strong Sessions (sample):} {.val {paste(sessions, collapse = ', ')}}")
  
  invisible(x)
}

#' Print method for bundle_list with tidyverse-style formatting
S7::method(print, bundle_list) <- function(x, ..., n = NULL) {
  if (is.null(n)) {
    n <- getOption("pillar.print_max", 10)
  }
  
  cli::cli_rule(
    left = cli::style_bold("bundle_list"),
    right = "{cli::col_silver('{nrow(x@.data)} bundle{?s}')}"
  )
  
  if (nrow(x@.data) == 0) {
    cli::cli_alert_warning("Empty bundle list")
    return(invisible(x))
  }
  
  # Quick stats
  n_sessions <- dplyr::n_distinct(x@.data$session)
  metadata_cols <- setdiff(names(x@.data), c("session", "bundle"))
  
  cli::cli_text("")
  cli::cli_text(
    "{cli::col_blue(n_sessions)} session{?s}, ",
    "{cli::col_blue(length(metadata_cols))} metadata field{?s}"
  )
  
  if (length(metadata_cols) > 0) {
    cli::cli_text("Fields: {.val {paste(head(metadata_cols, 5), collapse = ', ')}}")
  }
  
  cli::cli_rule()
  cli::cli_text("")
  
  # Use tibble for nice display
  print(tibble::as_tibble(x@.data), n = n, ...)
  
  invisible(x)
}

# ==============================================================================
# INTERACTIVE TESTING
# ==============================================================================

if (FALSE) {
  # Test corpus construction and usage
  library(reindeer)
  
  # Create test database
  reindeer:::create_ae_db(verbose = FALSE) -> ae
  
  # Construct corpus
  VISP <- corpus(ae$basePath, verbose = TRUE)
  
  # Print corpus
  print(VISP)
  
  # Summary
  summary(VISP)
  
  # Test subsetting
  VISP["0000", "msajc003"]  # Specific bundle
  VISP["0000", ]            # All bundles in session
  VISP[, "msajc003"]        # Bundle across sessions (if unique)
  VISP[".*", "msajc.*"]     # Regex patterns
  
  # Test metadata assignment
  VISP["0000", "msajc003"] <- list(Age = 25, Gender = "Male")
  VISP["0000", ] <- list(Age = 30, Gender = "Female")
  VISP <- corpus(ae$basePath)  # Reload
  VISP["0000", "msajc003"]  # Check metadata
  
  # Test media import (if media files available)
  # VISP["0000", "msajc003"] <- "path/to/audio.wav"
  # VISP["0000", "msajc003"] <- c("path/to/audio.wav", "egg", NULL, "flow")
}

#' Get or restore database handle for corpus
#' @param corpus_obj A corpus object
#' @param verbose Whether to show loading messages
#' @return An emuDBhandle with valid connection
get_handle <- function(corpus_obj, verbose = FALSE) {
  # Check if we have a valid cached connection
  if (!is.null(corpus_obj@.connection) && corpus_obj@.connection_valid) {
    tryCatch({
      # Test connection validity
      if (DBI::dbIsValid(corpus_obj@.connection$connection)) {
        return(corpus_obj@.connection)
      }
    }, error = function(e) {
      # Connection is invalid, need to reload
    })
  }

  # Load fresh handle
  if (verbose) {
    cli::cli_alert_info("Restoring database connection for {.field {corpus_obj@dbName}}")
  }

  handle <- emuR::load_emuDB(corpus_obj@basePath, verbose = verbose)

  # Cache the connection in the corpus object
  corpus_obj@.connection <- handle
  corpus_obj@.connection_valid <- TRUE

  return(handle)
}

# ==============================================================================
# HELPER FUNCTIONS FOR CORPUS OPERATIONS
# ==============================================================================

#' Get SQLite connection for corpus cache
#' @keywords internal
get_corpus_connection <- function(corpus_obj) {
  cache_path <- file.path(corpus_obj@basePath, paste0(corpus_obj@dbName, "_emuDBcache.sqlite"))
  
  if (!file.exists(cache_path)) {
    cli::cli_abort("Cache file not found at {.path {cache_path}}. Run corpus(path) to rebuild.")
  }
  
  DBI::dbConnect(RSQLite::SQLite(), cache_path)
}

#' Get metadata for bundles matching patterns
#' @keywords internal
get_metadata_for_patterns <- function(corpus_obj, session_pattern, bundle_pattern) {
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Get all bundles
  bundles <- list_bundles_from_cache(con, corpus_obj@.uuid)
  
  if (nrow(bundles) == 0) {
    return(tibble::tibble(session = character(), bundle = character()))
  }
  
  # Filter by patterns (support both literal and regex)
  session_matches <- grepl(session_pattern, bundles$session)
  bundle_matches <- grepl(bundle_pattern, bundles$name)
  
  filtered_bundles <- bundles[session_matches & bundle_matches, ]
  
  if (nrow(filtered_bundles) == 0) {
    return(tibble::tibble(session = character(), bundle = character()))
  }
  
  # Start with session and bundle columns
  result <- tibble::tibble(
    session = filtered_bundles$session,
    bundle = filtered_bundles$name
  )
  
  # Get all metadata fields
  fields_query <- "SELECT DISTINCT field_name FROM metadata_fields ORDER BY field_name"
  fields <- DBI::dbGetQuery(con, fields_query)
  
  if (nrow(fields) > 0) {
    # For each field, get values with proper precedence
    for (field_name in fields$field_name) {
      field_values <- get_metadata_field_values(
        con, corpus_obj@.uuid, field_name, 
        result$session, result$bundle
      )
      result[[field_name]] <- field_values
    }
  }
  
  result
}

#' Get values for a metadata field with inheritance
#' @keywords internal
get_metadata_field_values <- function(con, db_uuid, field_name, sessions, bundles) {
  values <- character(length(sessions))
  types <- character(length(sessions))
  
  for (i in seq_along(sessions)) {
    session <- sessions[i]
    bundle <- bundles[i]
    
    # Try bundle level first
    bundle_val <- DBI::dbGetQuery(con, sprintf(
      "SELECT field_value, field_type FROM metadata_bundle 
       WHERE db_uuid = '%s' AND session = '%s' AND bundle = '%s' AND field_name = %s",
      db_uuid, session, bundle, DBI::dbQuoteString(con, field_name)
    ))
    
    if (nrow(bundle_val) > 0) {
      values[i] <- bundle_val$field_value[1]
      types[i] <- bundle_val$field_type[1]
      next
    }
    
    # Try session level
    session_val <- DBI::dbGetQuery(con, sprintf(
      "SELECT field_value, field_type FROM metadata_session 
       WHERE db_uuid = '%s' AND session = '%s' AND field_name = %s",
      db_uuid, session, DBI::dbQuoteString(con, field_name)
    ))
    
    if (nrow(session_val) > 0) {
      values[i] <- session_val$field_value[1]
      types[i] <- session_val$field_type[1]
      next
    }
    
    # Try database level
    db_val <- DBI::dbGetQuery(con, sprintf(
      "SELECT field_value, field_type FROM metadata_database 
       WHERE db_uuid = '%s' AND field_name = %s",
      db_uuid, DBI::dbQuoteString(con, field_name)
    ))
    
    if (nrow(db_val) > 0) {
      values[i] <- db_val$field_value[1]
      types[i] <- db_val$field_type[1]
    } else {
      values[i] <- NA_character_
      types[i] <- "character"
    }
  }
  
  # Deserialize values based on type
  result <- vector("list", length(values))
  for (i in seq_along(values)) {
    result[[i]] <- deserialize_metadata_value(values[i], types[i])
  }
  
  # Try to simplify to vector if possible
  tryCatch({
    unlist(result)
  }, error = function(e) {
    result
  })
}

#' Assign metadata to corpus bundles/sessions
#' @keywords internal
corpus_assign_metadata <- function(corpus_obj, session_pattern, bundle_pattern, metadata_list) {
  if (!is.list(metadata_list) || is.null(names(metadata_list))) {
    cli::cli_abort("Metadata must be a named list")
  }
  
  # Determine level based on patterns
  is_session_all <- is.null(session_pattern) || session_pattern == ".*"
  is_bundle_all <- is.null(bundle_pattern) || bundle_pattern == ".*"
  
  if (is_session_all && is_bundle_all) {
    # Database level
    set_metadata_database(corpus_obj, metadata_list)
  } else if (!is_session_all && is_bundle_all) {
    # Session level
    set_metadata_session(corpus_obj, session_pattern, metadata_list)
  } else if (!is_session_all && !is_bundle_all) {
    # Bundle level
    set_metadata_bundle(corpus_obj, session_pattern, bundle_pattern, metadata_list)
  } else {
    cli::cli_abort("Invalid pattern combination: bundle requires session")
  }
}

#' Set database-level metadata
#' @keywords internal
set_metadata_database <- function(corpus_obj, metadata_list) {
  # Write to <dbname>.meta_json
  db_meta_file <- file.path(corpus_obj@basePath, 
                            paste0(corpus_obj@dbName, ".meta_json"))
  
  # Read existing
  if (file.exists(db_meta_file)) {
    existing <- jsonlite::read_json(db_meta_file, simplifyVector = TRUE)
  } else {
    existing <- list()
  }
  
  # Merge
  updated <- utils::modifyList(existing, metadata_list, keep.null = FALSE)
  
  # Write
  jsonlite::write_json(updated, db_meta_file, auto_unbox = TRUE, pretty = TRUE)
  
  # Update cache
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  process_metadata_list(con, corpus_obj@.uuid, NULL, NULL, metadata_list, "database")
  
  cli::cli_alert_success("Database metadata updated")
}

#' Set session-level metadata
#' @keywords internal
set_metadata_session <- function(corpus_obj, session_pattern, metadata_list) {
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Find matching sessions
  sessions <- list_sessions_from_cache(con, corpus_obj@.uuid)
  matches <- grepl(session_pattern, sessions$name)
  matching_sessions <- sessions$name[matches]
  
  if (length(matching_sessions) == 0) {
    cli::cli_abort("No sessions match pattern {.val {session_pattern}}")
  }
  
  if (length(matching_sessions) > 1 && !grepl("[.*+?^${}()|\\[\\]]", session_pattern)) {
    # Looks like literal match but got multiple - warn user
    cli::cli_alert_warning("Pattern {.val {session_pattern}} matches {length(matching_sessions)} sessions")
  }
  
  # Update each matching session
  for (session_name in matching_sessions) {
    session_meta_file <- file.path(
      corpus_obj@basePath,
      paste0(session_name, "_ses"),
      paste0(session_name, ".meta_json")
    )
    
    # Read existing
    if (file.exists(session_meta_file)) {
      existing <- jsonlite::read_json(session_meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    # Merge
    updated <- utils::modifyList(existing, metadata_list, keep.null = FALSE)
    
    # Ensure directory exists
    session_dir <- dirname(session_meta_file)
    if (!dir.exists(session_dir)) {
      dir.create(session_dir, recursive = TRUE)
    }
    
    # Write
    jsonlite::write_json(updated, session_meta_file, auto_unbox = TRUE, pretty = TRUE)
    
    # Update cache
    process_metadata_list(con, corpus_obj@.uuid, session_name, NULL, metadata_list, "session")
  }
  
  cli::cli_alert_success("Metadata updated for {length(matching_sessions)} session{?s}")
}

#' Set bundle-level metadata
#' @keywords internal
set_metadata_bundle <- function(corpus_obj, session_pattern, bundle_pattern, metadata_list) {
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Find matching bundles
  bundles <- list_bundles_from_cache(con, corpus_obj@.uuid)
  session_matches <- grepl(session_pattern, bundles$session)
  bundle_matches <- grepl(bundle_pattern, bundles$name)
  
  matching_bundles <- bundles[session_matches & bundle_matches, ]
  
  if (nrow(matching_bundles) == 0) {
    cli::cli_abort("No bundles match session={.val {session_pattern}}, bundle={.val {bundle_pattern}}")
  }
  
  # Update each matching bundle
  for (i in seq_len(nrow(matching_bundles))) {
    session_name <- matching_bundles$session[i]
    bundle_name <- matching_bundles$name[i]
    
    bundle_meta_file <- file.path(
      corpus_obj@basePath,
      paste0(session_name, "_ses"),
      paste0(bundle_name, "_bndl"),
      paste0(bundle_name, ".meta_json")
    )
    
    # Read existing
    if (file.exists(bundle_meta_file)) {
      existing <- jsonlite::read_json(bundle_meta_file, simplifyVector = TRUE)
    } else {
      existing <- list()
    }
    
    # Merge
    updated <- utils::modifyList(existing, metadata_list, keep.null = FALSE)
    
    # Ensure directory exists
    bundle_dir <- dirname(bundle_meta_file)
    if (!dir.exists(bundle_dir)) {
      dir.create(bundle_dir, recursive = TRUE)
    }
    
    # Write
    jsonlite::write_json(updated, bundle_meta_file, auto_unbox = TRUE, pretty = TRUE)
    
    # Update cache
    process_metadata_list(con, corpus_obj@.uuid, session_name, bundle_name, metadata_list, "bundle")
  }
  
  cli::cli_alert_success("Metadata updated for {nrow(matching_bundles)} bundle{?s}")
}

#' Import media files to corpus bundles
#' @keywords internal
corpus_import_media <- function(corpus_obj, session_pattern, bundle_pattern, media_spec) {
  # media_spec can be:
  # - Single file path: imports to mediafileExtension
  # - Vector with channel assignments: c("egg", "path.mp3", NULL, "flow")
  # - Vector with file first: c("path.mp3", "egg", NULL, "flow")
  
  if (grepl("[.*+?^${}()|\\[\\]]", session_pattern) || 
      grepl("[.*+?^${}()|\\[\\]]", bundle_pattern)) {
    cli::cli_abort("Regex patterns not allowed for media import. Specify exact session and bundle names.")
  }
  
  # Find the unique bundle
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  bundles <- list_bundles_from_cache(con, corpus_obj@.uuid)
  session_matches <- bundles$session == session_pattern
  bundle_matches <- bundles$name == bundle_pattern
  
  matching_bundles <- bundles[session_matches & bundle_matches, ]
  
  if (nrow(matching_bundles) == 0) {
    cli::cli_abort("Bundle {.val {bundle_pattern}} not found in session {.val {session_pattern}}")
  }
  
  if (nrow(matching_bundles) > 1) {
    cli::cli_abort("Multiple bundles found. Be more specific.")
  }
  
  # Parse media specification
  file_path <- NULL
  channel_map <- list()
  
  if (length(media_spec) == 1) {
    # Simple case: just a file path
    file_path <- media_spec
    channel_map[[corpus_obj@config$mediafileExtension %||% "wav"]] <- 1
  } else {
    # Complex case: channel mappings
    # Find the file path (should be the only element that exists as a file)
    for (elem in media_spec) {
      if (!is.null(elem) && file.exists(elem)) {
        file_path <- elem
        break
      }
    }
    
    if (is.null(file_path)) {
      cli::cli_abort("No valid file path found in media specification")
    }
    
    # Build channel map
    channel_num <- 1
    for (elem in media_spec) {
      if (!is.null(elem)) {
        if (elem == file_path) {
          # This is the file path - assign to mediafileExtension
          channel_map[[corpus_obj@config$mediafileExtension %||% "wav"]] <- channel_num
        } else {
          # This is a track extension
          channel_map[[elem]] <- channel_num
        }
      }
      channel_num <- channel_num + 1
    }
  }
  
  # Now import the media file
  import_media_to_bundle(
    corpus_obj = corpus_obj,
    session_name = session_pattern,
    bundle_name = bundle_pattern,
    file_path = file_path,
    channel_map = channel_map
  )
}

#' Import media file to a specific bundle
#' @keywords internal
import_media_to_bundle <- function(corpus_obj, session_name, bundle_name, 
                                  file_path, channel_map) {
  
  if (!file.exists(file_path)) {
    cli::cli_abort("Media file not found: {.path {file_path}}")
  }
  
  if (!requireNamespace("av", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg av} required for media import")
  }
  
  # Get target extension from config or default to wav
  target_ext <- corpus_obj@config$mediafileExtension %||% "wav"
  
  # Create bundle directory if it doesn't exist
  bundle_dir <- file.path(
    corpus_obj@basePath,
    paste0(session_name, "_ses"),
    paste0(bundle_name, "_bndl")
  )
  
  if (!dir.exists(bundle_dir)) {
    cli::cli_abort(
      "Bundle directory does not exist: {.path {bundle_dir}}. Create bundle first."
    )
  }
  
  # Get audio info
  audio_info <- av::av_media_info(file_path)
  n_channels <- audio_info$audio$channels
  sample_rate <- audio_info$audio$sample_rate
  
  if (is.null(n_channels) || n_channels == 0) {
    cli::cli_abort("No audio channels found in {.path {file_path}}")
  }
  
  # Validate channel numbers
  requested_channels <- unique(unlist(channel_map))
  if (any(requested_channels > n_channels)) {
    cli::cli_abort(
      "File has {n_channels} channel{?s}, but channel {max(requested_channels)} was requested"
    )
  }
  
  # Convert and extract channels
  for (ext in names(channel_map)) {
    channel_num <- channel_map[[ext]]
    
    # Determine output file
    output_file <- file.path(bundle_dir, paste0(bundle_name, ".", ext))
    
    # For single channel files, can convert directly
    if (n_channels == 1 && channel_num == 1) {
      # Direct conversion
      av::av_audio_convert(
        audio = file_path,
        output = output_file,
        format = target_ext,
        sample_rate = sample_rate,
        channels = 1
      )
    } else {
      # For multi-channel, need to extract specific channel
      # Use av to convert to temporary wav, then extract channel with av filters
      temp_wav <- tempfile(fileext = ".wav")
      on.exit(unlink(temp_wav), add = TRUE)
      
      # Convert to wav with all channels
      av::av_audio_convert(
        audio = file_path,
        output = temp_wav,
        format = "wav",
        sample_rate = sample_rate
      )
      
      # Extract specific channel using av audio filter
      # The format is: channelmap=map=FL filter extracts left (first) channel
      # For channel N, use: pan=mono|c0=c{N-1}
      channel_filter <- sprintf("pan=mono|c0=c%d", channel_num - 1)
      
      av::av_audio_convert(
        audio = temp_wav,
        output = output_file,
        format = target_ext,
        sample_rate = sample_rate,
        channels = 1,
        audio_filters = channel_filter
      )
    }
    
    cli::cli_alert_success("Wrote channel {channel_num} to {.file {basename(output_file)}}")
  }
  
  # Update bundle annotation if needed (sample rate, annotates field)
  annot_file <- file.path(bundle_dir, paste0(bundle_name, "_annot.json"))
  if (file.exists(annot_file)) {
    annot <- jsonlite::read_json(annot_file, simplifyVector = FALSE)
    annot$sampleRate <- sample_rate
    annot$annotates <- paste0(bundle_name, ".", target_ext)
    jsonlite::write_json(annot, annot_file, auto_unbox = TRUE, pretty = TRUE)
  }
  
  cli::cli_alert_success("Media imported to {.val {session_name}/{bundle_name}}")
}




#' Corpus cache file update
#'
#' Builds an Emu-DBMS compliant cache file for a corpus.
#'
#' @param database_dir Path to the Emu-DBMS corpus base directory
#' @param parallel Use parallel processing (default: TRUE)
#' @param workers Number of parallel workers (default: all but one available cores are used)
#' @param batch_size Number of bundles to process in each batch for DB insertion
#' @param verbose Show progress information
#' @return A corpus object
build_emuDB_cache <- function(database_dir,
                              parallel = TRUE,
                              workers = future::availableCores() - 1,
                              batch_size = 50,
                              verbose = TRUE) {

  # Validate database directory
  if (!dir.exists(database_dir)) {
    cli::cli_abort("Database directory {.path {database_dir}} does not exist")
  }

  # Extract database name from directory
  db_name <- stringr::str_replace(basename(database_dir), "_emuDB$", "")

  # Setup paths
  db_config_path <- file.path(database_dir, paste0(db_name, "_DBconfig.json"))
  cache_path <- file.path(database_dir, paste0(db_name, "_emuDBcache.sqlite"))

  if (!file.exists(db_config_path)) {
    cli::cli_abort("Database config file not found: {.path {db_config_path}}")
  }

  # Load database configuration
  db_config <- jsonlite::fromJSON(db_config_path, simplifyVector = FALSE)

  if (verbose) {
    cli::cli_h2("Building emuDB cache for {.field {db_name}}")
  }

  # Initialize SQLite connection
  if (file.exists(cache_path)) {
    if (verbose) cli::cli_alert_info("Using existing cache file")
    # For updates, we could add logic here to only update changed bundles
    # For now, we rebuild (safer for consistency)
  }
  
  if (verbose) {
    cli::cli_alert_info("Initializing cache database...")
  }
  
  # Remove old cache if it exists to ensure clean state
  if (file.exists(cache_path)) {
    unlink(cache_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Initialize database schema
  initialize_database_schema(con, db_config$UUID, db_name)

  # Discover sessions and bundles
  sessions_bundles <- discover_sessions_bundles(database_dir)

  if (nrow(sessions_bundles) == 0) {
    cli::cli_alert_warning("No bundles found in database")

    
    return(corpus(database_dir, verbose = FALSE))
  }

  if (verbose) {
    cli::cli_alert_success("Found {.val {dplyr::n_distinct(sessions_bundles$session)}} sessions with {.val {nrow(sessions_bundles)}} bundles")
  }

  # Setup parallel processing if requested
  if (parallel && nrow(sessions_bundles) > 10) {
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)
  } else {
    parallel <- FALSE
  }

  # Process bundles
  results <- process_bundles_batch(
    con = con,
    sessions_bundles = sessions_bundles,
    database_dir = database_dir,
    db_config = db_config,
    batch_size = batch_size,
    parallel = parallel,
    verbose = verbose
  )

  # Report results
  if (verbose) {
    successful <- sum(results$success)
    failed <- sum(!results$success)

    cli::cli_alert_success("Successfully processed {.val {successful}} bundles")

    if (failed > 0) {
      cli::cli_alert_warning("Failed to process {.val {failed}} bundles")
      failed_bundles <- sessions_bundles[!results$success, ]
      for (i in seq_len(min(5, nrow(failed_bundles)))) {
        cli::cli_alert_danger("{failed_bundles$session[i]}/{failed_bundles$bundle[i]}: {results$error[!results$success][i]}")
      }
      if (failed > 5) {
        cli::cli_alert_info("... and {.val {failed - 5}} more")
      }
    }
  }
  
  # Initialize metadata schema
  if (verbose) {
    cli::cli_h2("Initializing metadata schema")
  }
  initialize_metadata_schema(con)

  # Close the connection we created (the corpus will create its own)
  DBI::dbDisconnect(con)
  on.exit() # Remove the on.exit handler


}

# ==============================================================================
# CACHE SQLITE FILE INITIALIZATION
# ==============================================================================

initialize_database_schema <- function(con, uuid, db_name) {
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

  # Create indices
  DBI::dbExecute(con, "CREATE INDEX items_level_seq_idx ON items(db_uuid, session, bundle, level, seq_idx)")
  DBI::dbExecute(con, "CREATE INDEX links_both_ids_idx ON links(db_uuid, session, bundle, from_id, to_id)")
  DBI::dbExecute(con, "CREATE INDEX links_to_id_idx ON links(db_uuid, session, bundle, to_id)")
  DBI::dbExecute(con, "CREATE INDEX label_nameLabel_idx ON labels(db_uuid, bundle, session, item_id)")

  # Insert database record
  DBI::dbExecute(con, sprintf("INSERT INTO emu_db (uuid, name) VALUES ('%s', '%s')", uuid, db_name))
}

# ==============================================================================
# BUNDLE DISCOVERY
# ==============================================================================

discover_sessions_bundles <- function(database_dir) {
  session_dirs <- list.dirs(database_dir, recursive = FALSE, full.names = FALSE)
  session_dirs <- session_dirs[stringr::str_detect(session_dirs, "_ses$")]

  sessions_bundles <- purrr::map_dfr(session_dirs, function(ses_dir) {
    session_name <- stringr::str_replace(ses_dir, "_ses$", "")
    bundle_dirs <- list.dirs(file.path(database_dir, ses_dir),
                             recursive = FALSE, full.names = FALSE)
    bundle_dirs <- bundle_dirs[stringr::str_detect(bundle_dirs, "_bndl$")]
    bundle_names <- stringr::str_replace(bundle_dirs, "_bndl$", "")

    if (length(bundle_names) > 0) {
      tibble::tibble(
        session = session_name,
        bundle = bundle_names
      )
    } else {
      tibble::tibble()
    }
  })

  return(sessions_bundles)
}

# ==============================================================================
# BATCH PROCESSING
# ==============================================================================

process_bundles_batch <- function(con, sessions_bundles, database_dir,
                                  db_config, batch_size, parallel, verbose) {

  # Add sessions to database
  unique_sessions <- unique(sessions_bundles$session)
  session_insert <- sprintf(
    "INSERT OR IGNORE INTO session (db_uuid, name) VALUES ('%s', '%s')",
    db_config$UUID, unique_sessions
  )

  DBI::dbBegin(con)
  purrr::walk(session_insert, ~ DBI::dbExecute(con, .x))
  DBI::dbCommit(con)

  # Split into batches
  n_batches <- ceiling(nrow(sessions_bundles) / batch_size)
  sessions_bundles$batch <- rep(1:n_batches, each = batch_size, length.out = nrow(sessions_bundles))
  batches <- split(sessions_bundles, sessions_bundles$batch)

  # Process function for a single bundle
  process_bundle <- function(session_name, bundle_name, database_dir, db_config) {
    tryCatch({
      annot_path <- file.path(
        database_dir,
        paste0(session_name, "_ses"),
        paste0(bundle_name, "_bndl"),
        paste0(bundle_name, "_annot.json")
      )

      if (!file.exists(annot_path)) {
        return(list(success = FALSE, error = "Annotation file not found", data = NULL))
      }

      # Read and parse JSON
      annot_json <- jsonlite::fromJSON(annot_path, simplifyVector = FALSE)
      md5_hash <- as.character(tools::md5sum(annot_path))

      # Convert to data frames
      annot_dfs <- parse_annot_json(annot_json, db_config$UUID, session_name, bundle_name)

      return(list(
        success = TRUE,
        error = "",
        data = annot_dfs,
        md5 = md5_hash,
        sample_rate = annot_json$sampleRate,
        annotates = annot_json$annotates
      ))
    }, error = function(e) {
      return(list(success = FALSE, error = as.character(e), data = NULL))
    })
  }

  # Process batches with progress tracking
  results <- list()
  total_bundles <- nrow(sessions_bundles)
  
  if (verbose) {
    cli::cli_alert_info("Processing {total_bundles} bundles{?s} in {length(batches)} batch{?es}...")
    if (parallel) {
      cli::cli_alert_info("Using parallel processing with {workers} worker{?s}")
    }
    cli::cli_progress_bar(
      "Processing",
      total = total_bundles,
      format = "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      clear = FALSE
    )
  }

  for (i in seq_along(batches)) {
    batch <- batches[[i]]

    if (parallel) {
      batch_results <- furrr::future_pmap(
        list(batch$session, batch$bundle),
        process_bundle,
        database_dir = database_dir,
        db_config = db_config,
        .options = furrr::furrr_options(seed = TRUE)
      )
    } else {
      batch_results <- purrr::pmap(
        list(batch$session, batch$bundle),
        process_bundle,
        database_dir = database_dir,
        db_config = db_config
      )
    }

    # Insert successful results into database
    successful_results <- purrr::keep(batch_results, ~ .x$success)

    if (length(successful_results) > 0) {
      insert_batch_results(con, successful_results, db_config$UUID)
    }

    results <- c(results, batch_results)
    
    # Update progress bar
    if (verbose) {
      bundles_done <- sum(purrr::map_int(batches[1:i], nrow))
      cli::cli_progress_update(set = bundles_done)
    }
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Convert results to data frame
  results_df <- tibble::tibble(
    success = purrr::map_lgl(results, ~ .x$success),
    error = purrr::map_chr(results, ~ .x$error)
  )

  return(results_df)
}

# ==============================================================================
# ANNOTATION PARSING
# ==============================================================================

parse_annot_json <- function(annot_json, db_uuid, session_name, bundle_name) {

  # Initialize empty data frames
  items_list <- tibble::tibble()
  labels_list <- tibble::tibble()
  links_list <- tibble::tibble()

  # Parse levels if they exist
  if (!is.null(annot_json[["levels"]]) && is.list(annot_json[["levels"]])) {
    levels <- annot_json[["levels"]]

    for (level_idx in seq_along(levels)) {
      level <- levels[[level_idx]]

      # Ensure level is a list with expected fields
      if (!is.list(level) || is.null(level[["name"]]) || is.null(level[["type"]])) {
        next
      }

      level_name <- as.character(level[["name"]])
      level_type <- as.character(level[["type"]])

      # Process items if they exist
      if (!is.null(level[["items"]]) && is.list(level[["items"]])) {
        items <- level[["items"]]

        for (item_idx in seq_along(items)) {
          item <- items[[item_idx]]

          if (!is.list(item) || is.null(item[["id"]])) {
            next
          }

          # Create item row
          item_row <- tibble::tibble(
            db_uuid = db_uuid,
            session = session_name,
            bundle = bundle_name,
            item_id = as.integer(item[["id"]]),
            level = level_name,
            type = level_type,
            seq_idx = as.integer(item_idx),
            sample_rate = as.numeric(annot_json[["sampleRate"]]),
            sample_point = if (!is.null(item[["samplePoint"]])) as.integer(item[["samplePoint"]]) else NA_integer_,
            sample_start = if (!is.null(item[["sampleStart"]])) as.integer(item[["sampleStart"]]) else NA_integer_,
            sample_dur = if (!is.null(item[["sampleDur"]])) as.integer(item[["sampleDur"]]) else NA_integer_
          )

          items_list <- rbind(items_list, item_row)

          # Process labels for this item
          if (!is.null(item[["labels"]]) && is.list(item[["labels"]])) {
            item_labels <- item[["labels"]]

            for (label_idx in seq_along(item_labels)) {
              label <- item_labels[[label_idx]]

              if (!is.list(label) || is.null(label[["name"]])) {
                next
              }

              label_row <- tibble::tibble(
                db_uuid = db_uuid,
                session = session_name,
                bundle = bundle_name,
                item_id = as.integer(item[["id"]]),
                label_idx = as.integer(label_idx),
                name = as.character(label[["name"]]),
                label = if (!is.null(label[["value"]])) as.character(label[["value"]]) else ""
              )

              labels_list <- rbind(labels_list, label_row)
            }
          }
        }
      }
    }
  }

  # Parse links if they exist
  if (!is.null(annot_json[["links"]]) && is.list(annot_json[["links"]])) {
    links <- annot_json[["links"]]

    for (link_idx in seq_along(links)) {
      link <- links[[link_idx]]

      if (!is.list(link) || is.null(link[["fromID"]]) || is.null(link[["toID"]])) {
        next
      }

      link_row <- tibble::tibble(
        db_uuid = db_uuid,
        session = session_name,
        bundle = bundle_name,
        from_id = as.integer(link[["fromID"]]),
        to_id = as.integer(link[["toID"]]),
        label = if (!is.null(link[["label"]])) as.character(link[["label"]]) else NA_character_
      )

      links_list <- rbind(links_list, link_row)
    }
  }

  return(list(
    items = items_list,
    labels = labels_list,
    links = links_list
  ))
}

# ==============================================================================
# DATABASE INSERTION
# ==============================================================================

insert_batch_results <- function(con, results, db_uuid) {
  DBI::dbBegin(con)

  tryCatch({
    # Prepare bundle data
    bundle_data <- purrr::map_dfr(results, function(r) {
      tibble::tibble(
        db_uuid = db_uuid,
        session = r$data$items$session[1],
        name = r$data$items$bundle[1],
        annotates = r$annotates,
        sample_rate = r$sample_rate,
        md5_annot_json = r$md5
      )
    })

    # Combine all items, labels, and links
    all_items <- purrr::map_dfr(results, ~ .x$data$items)
    all_labels <- purrr::map_dfr(results, ~ .x$data$labels)
    all_links <- purrr::map_dfr(results, ~ .x$data$links)

    # Insert bundles
    if (nrow(bundle_data) > 0) {
      DBI::dbAppendTable(con, "bundle", bundle_data)
    }

    # Insert items
    if (nrow(all_items) > 0) {
      DBI::dbAppendTable(con, "items", all_items)
    }

    # Insert labels
    if (nrow(all_labels) > 0) {
      DBI::dbAppendTable(con, "labels", all_labels)
    }

    # Insert links
    if (nrow(all_links) > 0) {
      DBI::dbAppendTable(con, "links", all_links)
    }

    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop(e)
  })
}#' Gather all metadata from .meta_json files (internal, called during construction)
#' @keywords internal
gather_metadata_internal <- function(corpus_obj, verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("Scanning .meta_json files...")
  }
  
  basePath <- corpus_obj@basePath
  db_uuid <- corpus_obj@.uuid
  con <- get_corpus_connection(corpus_obj)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Clear existing metadata
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_bundle WHERE db_uuid = '%s'", db_uuid))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_session WHERE db_uuid = '%s'", db_uuid))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_database WHERE db_uuid = '%s'", db_uuid))
  
  # 1. Database-level metadata
  db_meta_file <- file.path(basePath, paste0(corpus_obj@dbName, ".meta_json"))
  
  if (file.exists(db_meta_file)) {
    if (verbose) cli::cli_alert_info("Loading database defaults")
    db_meta <- jsonlite::read_json(db_meta_file, simplifyVector = TRUE)
    if (length(db_meta) > 0) {
      process_metadata_list(con, db_uuid, NULL, NULL, db_meta, "database")
    }
  }
  
  # 2. Session-level metadata
  sessions <- list_sessions_from_cache(con, db_uuid)
  
  if (verbose && nrow(sessions) > 0) {
    cli::cli_progress_bar(
      "Loading session metadata",
      total = nrow(sessions),
      format = "{cli::pb_spin} Session {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent}"
    )
  }
  
  for (i in seq_len(nrow(sessions))) {
    session_name <- sessions$name[i]
    session_meta_file <- file.path(basePath, paste0(session_name, "_ses"),
                                   paste0(session_name, ".meta_json"))
    
    if (file.exists(session_meta_file)) {
      meta_data <- jsonlite::read_json(session_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, NULL, meta_data, "session")
      }
    }
    
    if (verbose && nrow(sessions) > 0) {
      cli::cli_progress_update()
    }
  }
  
  if (verbose && nrow(sessions) > 0) {
    cli::cli_progress_done()
  }
  
  # 3. Bundle-level metadata
  bundles <- list_bundles_from_cache(con, db_uuid)
  
  if (verbose && nrow(bundles) > 0) {
    cli::cli_progress_bar(
      "Loading bundle metadata",
      total = nrow(bundles),
      format = "{cli::pb_spin} Bundle {cli::pb_current}/{cli::pb_total} | {cli::pb_bar} {cli::pb_percent}"
    )
  }
  
  for (i in seq_len(nrow(bundles))) {
    session_name <- bundles$session[i]
    bundle_name <- bundles$name[i]
    
    bundle_meta_file <- file.path(
      basePath, 
      paste0(session_name, "_ses"),
      paste0(bundle_name, "_bndl"),
      paste0(bundle_name, ".meta_json")
    )
    
    if (file.exists(bundle_meta_file)) {
      meta_data <- jsonlite::read_json(bundle_meta_file, simplifyVector = TRUE)
      if (length(meta_data) > 0) {
        process_metadata_list(con, db_uuid, session_name, bundle_name, meta_data, "bundle")
      }
    }
    
    if (verbose && nrow(bundles) > 0) {
      cli::cli_progress_update()
    }
  }
  
  if (verbose && nrow(bundles) > 0) {
    cli::cli_progress_done()
  }
  
  if (verbose) {
    cli::cli_alert_success("Metadata loaded")
  }
}

# ==============================================================================
# METADATA HELPER FUNCTIONS (from reindeeR_metadata_optimized.R)
# ==============================================================================

#' Process and insert metadata list
#' @keywords internal
process_metadata_list <- function(con, db_uuid, session, bundle, meta_list, level) {
  if (length(meta_list) == 0) return(invisible(NULL))
  
  DBI::dbWithTransaction(con, {
    for (field_name in names(meta_list)) {
      value <- meta_list[[field_name]]
      
      # Serialize value
      field_info <- serialize_metadata_value(value)
      
      # Register field
      register_metadata_field(con, field_name, field_info$type)
      
      # Insert into appropriate table
      if (level == "database") {
        # Remove quotes from field_name in SQL
        field_name_clean <- gsub("'", "''", field_name)
        value_clean <- gsub("'", "''", field_info$value)
        
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_database (db_uuid, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s')",
          db_uuid, field_name_clean, value_clean, field_info$type
        )
      } else if (level == "session") {
        field_name_clean <- gsub("'", "''", field_name)
        value_clean <- gsub("'", "''", field_info$value)
        session_clean <- gsub("'", "''", session)
        
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_session (db_uuid, session, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s', '%s')",
          db_uuid, session_clean, field_name_clean, value_clean, field_info$type
        )
      } else if (level == "bundle") {
        field_name_clean <- gsub("'", "''", field_name)
        value_clean <- gsub("'", "''", field_info$value)
        session_clean <- gsub("'", "''", session)
        bundle_clean <- gsub("'", "''", bundle)
        
        sql <- sprintf(
          "INSERT OR REPLACE INTO metadata_bundle (db_uuid, session, bundle, field_name, field_value, field_type) 
           VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
          db_uuid, session_clean, bundle_clean, field_name_clean, value_clean, field_info$type
        )
      }
      
      DBI::dbExecute(con, sql)
    }
  })
}

#' Serialize metadata value
#' @keywords internal
serialize_metadata_value <- function(value) {
  if (is.null(value)) {
    return(list(value = "NULL", type = "NULL"))
  } else if (is.logical(value)) {
    return(list(value = as.character(value), type = "logical"))
  } else if (is.numeric(value) && !is.integer(value)) {
    return(list(value = as.character(value), type = "numeric"))
  } else if (is.integer(value)) {
    return(list(value = as.character(value), type = "integer"))
  } else if (inherits(value, "Date")) {
    return(list(value = as.character(value), type = "date"))
  } else if (inherits(value, "POSIXt")) {
    return(list(value = format(value, "%Y-%m-%dT%H:%M:%S"), type = "datetime"))
  } else {
    return(list(value = as.character(value), type = "character"))
  }
}

#' Deserialize metadata value
#' @keywords internal
deserialize_metadata_value <- function(value_str, type_str) {
  if (is.na(value_str) || value_str == "NULL" || value_str == "NA") {
    return(NA)
  }
  
  tryCatch({
    switch(type_str,
      "logical" = as.logical(value_str),
      "numeric" = as.numeric(value_str),
      "integer" = as.integer(value_str),
      "date" = as.Date(value_str),
      "datetime" = as.POSIXct(value_str),
      "character" = value_str,
      value_str  # default
    )
  }, error = function(e) {
    value_str
  })
}

#' Register metadata field
#' @keywords internal
register_metadata_field <- function(con, field_name, field_type) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  field_name_clean <- gsub("'", "''", field_name)
  
  # Check if exists
  existing <- DBI::dbGetQuery(con, sprintf(
    "SELECT field_name FROM metadata_fields WHERE field_name = '%s'",
    field_name_clean
  ))
  
  if (nrow(existing) == 0) {
    # Insert new
    DBI::dbExecute(con, sprintf(
      "INSERT INTO metadata_fields (field_name, field_type, first_seen, last_modified) 
       VALUES ('%s', '%s', '%s', '%s')",
      field_name_clean, field_type, now, now
    ))
  } else {
    # Update timestamp
    DBI::dbExecute(con, sprintf(
      "UPDATE metadata_fields SET last_modified = '%s' WHERE field_name = '%s'",
      now, field_name_clean
    ))
  }
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
