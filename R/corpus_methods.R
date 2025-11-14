#' @importFrom S7 method method<-
NULL

# ==============================================================================
# CORPUS SUBSETTING OPERATORS - corpus[i, j]
# ==============================================================================

#' Subset corpus to get bundle list with metadata
#'
#' @description 
#' Subset corpus to get bundle list with metadata
#' 
#' Extract bundles from corpus using bracket notation
#' @param x corpus object
#' @param i session pattern (regex or literal)
#' @param j bundle pattern (regex or literal)
#' @param ... Additional arguments (unused)
#' @param drop Logical; unused (for S3 compatibility)
#' @return bundle_list object (tibble with session, bundle, and metadata columns)
#' @name [.corpus
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
#' @param ... Additional arguments (unused)
#' @param value Either a named list (metadata) or character vector (media files)
#' @name [<-.corpus
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

# Print method for corpus - to be registered with S7
#' @keywords internal
.print_corpus <- function(x, ...) {
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

# Summary method for corpus - to be registered with S7
#' @keywords internal
.summary_corpus <- function(object, ...) {
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
#' @param x bundle_list object
#' @param ... Additional arguments (unused)
#' @param n Number of rows to show
#' @name print.bundle_list
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
