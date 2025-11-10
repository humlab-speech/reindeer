ask_for <- function(emuDB, query, ...) {
  # Handle corpus objects
  if (S7::S7_inherits(emuDB, reindeer::corpus)) {
    base_path <- emuDB@basePath
    db_name <- emuDB@dbName
    
    # Try both naming conventions
    db_path1 <- file.path(base_path, paste0(db_name, "_emuDB.sqlite"))
    db_path2 <- file.path(base_path, paste0(db_name, "_emuDBcache.sqlite"))
    
    if (file.exists(db_path1)) {
      db_path <- db_path1
    } else if (file.exists(db_path2)) {
      db_path <- db_path2
    } else {
      stop("SQLite database not found at: ", db_path1, " or ", db_path2)
    }
    
    database_dir <- base_path
    
  } else if (is.character(emuDB)) {
    # emuDB is a path to the database directory
    # Extract the database name from the path (remove _emuDB suffix if present)
    base_name <- basename(emuDB)
    db_name <- sub("_emuDB$", "", base_name)
    
    # Try both naming conventions
    db_path1 <- file.path(emuDB, paste0(db_name, "_emuDB.sqlite"))
    db_path2 <- file.path(emuDB, paste0(db_name, "_emuDBcache.sqlite"))
    
    if (file.exists(db_path1)) {
      db_path <- db_path1
    } else if (file.exists(db_path2)) {
      db_path <- db_path2
    } else {
      stop("SQLite database not found at: ", db_path1, " or ", db_path2)
    }
    
    database_dir <- emuDB
    
  } else {
    base_path <- attr(emuDB, "basePath")
    db_name <- attr(emuDB, "dbName")
    
    if (is.null(base_path) || is.null(db_name)) {
      stop("Invalid emuDB object: missing basePath or dbName attributes")
    }
    
    db_path1 <- file.path(base_path, paste0(db_name, "_emuDB.sqlite"))
    db_path2 <- file.path(base_path, paste0(db_name, "_emuDBcache.sqlite"))
    
    if (file.exists(db_path1)) {
      db_path <- db_path1
    } else if (file.exists(db_path2)) {
      db_path <- db_path2
    } else {
      stop("SQLite database not found at: ", db_path1, " or ", db_path2)
    }
    
    database_dir <- base_path
  }
  
  if (!file.exists(db_path)) {
    stop("SQLite database not found at: ", db_path)
  }
  
  # Check for lazy parameter (default FALSE until lazy evaluation is fully implemented)
  dots <- list(...)
  lazy <- if ("lazy" %in% names(dots)) dots$lazy else FALSE
  
  if (lazy) {
    # Return lazy segment list without executing query
    # Build base SQL query but don't execute
    parsed <- parse_eql_query(query)
    base_sql <- build_base_sql(db_path, parsed, dots)
    
    # Get db_uuid from database
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn))
    db_uuid_result <- DBI::dbGetQuery(conn, "SELECT uuid FROM emu_db LIMIT 1")
    db_uuid <- if (nrow(db_uuid_result) > 0) db_uuid_result$uuid[1] else ""
    
    # Create corpus object if we have one
    corpus_obj <- if (S7::S7_inherits(emuDB, reindeer::corpus)) emuDB else NULL
    
    return(lazy_segment_list(
      corpus = corpus_obj,
      query_parts = list(
        base = base_sql,
        transforms = list()
      ),
      db_path = db_path,
      db_uuid = db_uuid,
      materialized = FALSE,
      cache = NULL
    ))
  } else {
    # Execute immediately (old behavior)
    result <- execute_query(db_path, query, ...)
    
    # Convert to segment_list if result is a data.frame
    if (is.data.frame(result) && !inherits(result, "segment_list")) {
      # Extract db_uuid and db_path for segment_list
      db_uuid <- if ("db_uuid" %in% names(result)) unique(result$db_uuid)[1] else ""
      result <- segment_list(result, db_uuid = db_uuid, db_path = database_dir)
    }
    
    return(result)
  }
}

# Note: query() is an alias for ask_for() defined in reindeeR_emuR_re-export.R

#' Build Base SQL Query Without Execution
#'
#' Extracts the SQL query that would be executed, for use in lazy evaluation.
#' 
#' @param db_path Path to SQLite database
#' @param parsed Parsed EQL query
#' @param opts Additional options
#' @return SQL query string
#' @keywords internal
build_base_sql <- function(db_path, parsed, opts = list()) {
  result_level <- opts$result_level %||% NULL
  
  # Build SQL based on query type
  sql <- switch(parsed$type,
    "simple" = build_simple_query_sql(db_path, parsed),
    "sequence" = build_sequence_query_sql(db_path, parsed, result_level),
    "dominance" = build_dominance_query_sql(db_path, parsed, result_level),
    "function" = build_function_query_sql(db_path, parsed),
    "conjunction" = build_conjunction_query_sql(db_path, parsed, result_level),
    "disjunction" = build_disjunction_query_sql(db_path, parsed, result_level),
    stop("Unknown query type: ", parsed$type)
  )
  
  return(sql)
}

#' Build SQL for Simple Query
#' @keywords internal
build_simple_query_sql <- function(db_path, parsed) {
  level <- parsed$level
  operator <- parsed$operator
  pattern <- parsed$pattern
  attribute <- parsed$attribute %||% level
  
  # Base SQL for simple query
  base_sql <- paste0(
    "SELECT i.*, l.label as labels ",
    "FROM items i ",
    "INNER JOIN labels l ON ",
    "  i.db_uuid = l.db_uuid AND ",
    "  i.session = l.session AND ",
    "  i.bundle = l.bundle AND ",
    "  i.item_id = l.item_id ",
    "WHERE i.level = '", level, "' ",
    "  AND l.name = '", attribute, "' "
  )
  
  # Add filter based on operator
  if (operator == "==") {
    base_sql <- paste0(base_sql, "AND l.label = '", pattern, "'")
  } else if (operator == "!=") {
    base_sql <- paste0(base_sql, "AND l.label != '", pattern, "'")
  } else if (operator == "=~") {
    # SQLite LIKE pattern
    like_pattern <- gsub("\\.", "%", pattern)
    like_pattern <- gsub("\\*", "%", like_pattern)
    base_sql <- paste0(base_sql, "AND l.label LIKE '", like_pattern, "'")
  } else if (operator == "!~") {
    like_pattern <- gsub("\\.", "%", pattern)
    like_pattern <- gsub("\\*", "%", like_pattern)
    base_sql <- paste0(base_sql, "AND l.label NOT LIKE '", like_pattern, "'")
  }
  
  return(base_sql)
}

# Placeholder functions for other query types (to be implemented)
build_sequence_query_sql <- function(db_path, parsed, result_level = NULL) {
  # For now, fall back to execution
  # TODO: Implement lazy SQL building for sequences
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn))
  result <- execute_sequence_query_corrected(db_path, parsed, result_level)
  # Extract the SQL that was used (if available)
  return(NULL)  # Will cause execution
}

build_dominance_query_sql <- function(db_path, parsed, result_level = NULL) {
  # TODO: Implement
  return(NULL)
}

build_function_query_sql <- function(db_path, parsed) {
  # TODO: Implement
  return(NULL)
}

build_conjunction_query_sql <- function(db_path, parsed, result_level = NULL) {
  # TODO: Implement
  return(NULL)
}

build_disjunction_query_sql <- function(db_path, parsed, result_level = NULL) {
  # TODO: Implement
  return(NULL)
}

# Main execution dispatcher
execute_query <- function(db_path, query_string, result_level = NULL) {
  tryCatch({
    parsed <- parse_eql_query(query_string)
    
    result <- switch(parsed$type,
      "simple" = execute_simple_query_corrected(db_path, parsed),
      "sequence" = execute_sequence_query_corrected(db_path, parsed, result_level),
      "dominance" = execute_dominance_query_corrected(db_path, parsed, result_level),
      "function" = execute_function_query_corrected(db_path, parsed),
      "conjunction" = execute_conjunction_query(db_path, parsed, result_level),
      "disjunction" = execute_disjunction_query(db_path, parsed, result_level),
      stop("Unknown query type: ", parsed$type)
    )
    
    return(format_as_emuRsegs(result))
    
  }, error = function(e) {
    cat("Query execution failed:", e$message, "\n")
    return(create_empty_emuRsegs())
  })
}

# Query parser
