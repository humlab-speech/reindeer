#' Query an EMU database using EQL (EMU Query Language)
#'
#' Executes a query against the SQLite cache of an EMU database and returns
#' matching annotation segments. Supports simple, sequence, dominance,
#' and function queries.
#'
#' @param emuDB A \code{corpus} object, path to an emuDB directory, or emuDBhandle
#' @param query Character string with an EQL query (e.g. \code{"Phonetic == t"})
#' @param ... Additional arguments: \code{lazy = TRUE} returns a
#'   \code{lazy_segment_list} for deferred execution
#' @return A \code{\link{segment_list}} object (or \code{lazy_segment_list} if
#'   \code{lazy = TRUE})
#'
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/db_emuDB")
#' segs <- ask_for(corp, "Phonetic == t")
#' segs <- ask_for(corp, "[Phonetic == t -> Phonetic == s]")
#' segs <- ask_for(corp, "Phonetic == t", lazy = TRUE)
#' }
#' @export
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
      cli::cli_abort("SQLite database not found at: {.path {db_path1}} or {.path {db_path2}}")
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
      cli::cli_abort("SQLite database not found at: {.path {db_path1}} or {.path {db_path2}}")
    }
    
    database_dir <- emuDB
    
  } else {
    base_path <- attr(emuDB, "basePath")
    db_name <- attr(emuDB, "dbName")
    
    if (is.null(base_path) || is.null(db_name)) {
      cli::cli_abort("Invalid emuDB object: missing basePath or dbName attributes")
    }
    
    db_path1 <- file.path(base_path, paste0(db_name, "_emuDB.sqlite"))
    db_path2 <- file.path(base_path, paste0(db_name, "_emuDBcache.sqlite"))
    
    if (file.exists(db_path1)) {
      db_path <- db_path1
    } else if (file.exists(db_path2)) {
      db_path <- db_path2
    } else {
      cli::cli_abort("SQLite database not found at: {.path {db_path1}} or {.path {db_path2}}")
    }
    
    database_dir <- base_path
  }
  
  if (!file.exists(db_path)) {
    cli::cli_abort("SQLite database not found at: {.path {db_path}}")
  }
  
  # Check for lazy parameter (default FALSE until lazy evaluation is fully implemented)
  dots <- list(...)
  lazy <- if ("lazy" %in% names(dots)) dots$lazy else FALSE
  
  if (lazy) {
    # Return lazy segment list without executing query
    # Build base SQL query but don't execute — returns list(sql, params)
    parsed <- parse_eql_query(query)
    base_query <- build_base_sql(db_path, parsed, dots)
    
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
        base = base_query,  # list(sql, params)
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
    if (is.data.frame(result) && !S7::S7_inherits(result, segment_list)) {
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
#' Returns a parameterized query as list(sql, params).
#' 
#' @param db_path Path to SQLite database
#' @param parsed Parsed EQL query
#' @param opts Additional options
#' @return list(sql = "...", params = list(...))
#' @keywords internal
build_base_sql <- function(db_path, parsed, opts = list()) {
  result_level <- opts$result_level %||% NULL
  
  # Build SQL based on query type — all builders return list(sql, params)
  result <- switch(parsed$type,
    "simple" = build_simple_query_sql(db_path, parsed),
    "sequence" = build_sequence_query_sql(db_path, parsed, result_level),
    "dominance" = build_dominance_query_sql(db_path, parsed, result_level),
    "function" = build_function_query_sql(db_path, parsed),
    "conjunction" = build_conjunction_query_sql(db_path, parsed, result_level),
    "disjunction" = build_disjunction_query_sql(db_path, parsed, result_level),
    cli::cli_abort("Unknown query type: {.val {parsed$type}}")
  )
  
  return(result)
}

#' Build SQL for Simple Query
#'
#' Returns a parameterized query as list(sql, params) to prevent SQL injection.
#' @keywords internal
build_simple_query_sql <- function(db_path, parsed) {
  level <- parsed$level
  operator <- parsed$operator
  pattern <- parsed$pattern %||% parsed$value
  attribute <- parsed$attribute %||% level

  base_sql <- paste0(
    "SELECT i.*, l.label as labels ",
    "FROM items i ",
    "INNER JOIN labels l ON ",
    "  i.db_uuid = l.db_uuid AND ",
    "  i.session = l.session AND ",
    "  i.bundle = l.bundle AND ",
    "  i.item_id = l.item_id ",
    "WHERE i.level = ? ",
    "  AND l.name = ? "
  )
  params <- list(level, attribute)

  # Add filter based on operator
  if (operator == "==") {
    base_sql <- paste0(base_sql, "AND l.label = ?")
    params <- c(params, list(pattern))
  } else if (operator == "!=") {
    base_sql <- paste0(base_sql, "AND l.label != ?")
    params <- c(params, list(pattern))
  } else if (operator == "=~") {
    like_pattern <- gsub("\\.", "%", pattern)
    like_pattern <- gsub("\\*", "%", like_pattern)
    base_sql <- paste0(base_sql, "AND l.label LIKE ?")
    params <- c(params, list(like_pattern))
  } else if (operator == "!~") {
    like_pattern <- gsub("\\.", "%", pattern)
    like_pattern <- gsub("\\*", "%", like_pattern)
    base_sql <- paste0(base_sql, "AND l.label NOT LIKE ?")
    params <- c(params, list(like_pattern))
  }

  return(list(sql = base_sql, params = params))
}

# Placeholder functions for query types not yet supporting lazy SQL generation
build_sequence_query_sql <- function(db_path, parsed, result_level = NULL) {
  # TODO: Implement lazy SQL building for sequences
  return(NULL)
}

build_dominance_query_sql <- function(db_path, parsed, result_level = NULL) {
  # TODO: Implement lazy SQL building for dominance
  return(NULL)
}

build_function_query_sql <- function(db_path, parsed) {
  # TODO: Implement lazy SQL building for function queries
  return(NULL)
}

build_conjunction_query_sql <- function(db_path, parsed, result_level = NULL) {
  left_result <- build_base_sql(db_path, parsed$left, list(result_level = result_level))
  right_result <- build_base_sql(db_path, parsed$right, list(result_level = result_level))
  if (is.null(left_result) || is.null(right_result)) return(NULL)
  list(
    sql = paste0("(", left_result$sql, ") INTERSECT (", right_result$sql, ")"),
    params = c(left_result$params, right_result$params)
  )
}

build_disjunction_query_sql <- function(db_path, parsed, result_level = NULL) {
  left_result <- build_base_sql(db_path, parsed$left, list(result_level = result_level))
  right_result <- build_base_sql(db_path, parsed$right, list(result_level = result_level))
  if (is.null(left_result) || is.null(right_result)) return(NULL)
  list(
    sql = paste0("(", left_result$sql, ") UNION (", right_result$sql, ")"),
    params = c(left_result$params, right_result$params)
  )
}

# Main execution dispatcher
execute_query <- function(db_path, query_string, result_level = NULL) {
  # Open a single connection and thread it through all sub-executors
  con <- .open_query_connection(db_path)
  on.exit(DBI::dbDisconnect(con))

  tryCatch({
    parsed <- parse_eql_query(query_string)
    
    result <- switch(parsed$type,
      "simple" = execute_simple_query_corrected(db_path, parsed, con = con),
      "sequence" = execute_sequence_query_corrected(db_path, parsed, result_level, con = con),
      "dominance" = execute_dominance_query_corrected(db_path, parsed, result_level, con = con),
      "function" = execute_function_query_corrected(db_path, parsed, con = con),
      "conjunction" = execute_conjunction_query(db_path, parsed, result_level, con = con),
      "disjunction" = execute_disjunction_query(db_path, parsed, result_level, con = con),
      cli::cli_abort("Unknown query type: {.val {parsed$type}}")
    )
    
    # Deduce times for ITEM-type levels before formatting
    result <- deduce_item_times(result, db_path)
    return(format_as_emuRsegs(result))
    
  }, error = function(e) {
    cli::cli_abort(c(
      "Query execution failed",
      "x" = conditionMessage(e),
      "i" = "Query: {.code {query_string}}"
    ), parent = e)
  })
}

# Query parser
