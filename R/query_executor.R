#' Query an EMU database using EQL (EMU Query Language)
#'
#' Executes a query against the SQLite cache of an EMU database and returns
#' matching annotation segments. Supports simple, sequence, dominance,
#' and function queries.
#'
#' @param emuDB A \code{corpus} object, path to an emuDB directory, or emuDBhandle
#' @param eql Character string with an EQL query (e.g. \code{"Phonetic == t"})
#' @param ... Additional arguments: \code{lazy = FALSE} forces eager
#'   materialisation. Defaults to \code{lazy = TRUE} as of v0.7.0, returning
#'   a \code{lazy_segment_list} that auto-collects on data access.
#' @return A \code{lazy_segment_list} (default) or a \code{\link{segment_list}}
#'   when \code{lazy = FALSE}. Both behave identically for data access via
#'   \code{$}, \code{[}, \code{nrow()}, \code{head()}, etc.
#'
#' @examples
#' \dontrun{
#' corp <- corpus("path/to/db_emuDB")
#' segs <- query(corp, "Phonetic == t")
#' segs <- query(corp, "[Phonetic == t -> Phonetic == s]")
#' segs <- query(corp, "Phonetic == t", lazy = TRUE)
#' }
#' @export
query <- function(emuDB, eql, ...) {
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
      .query_abort("SQLite database not found at: {.path {db_path1}} or {.path {db_path2}}")
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
      .query_abort("SQLite database not found at: {.path {db_path1}} or {.path {db_path2}}")
    }
    
    database_dir <- emuDB
    
  } else {
    base_path <- attr(emuDB, "basePath")
    db_name <- attr(emuDB, "dbName")
    
    if (is.null(base_path) || is.null(db_name)) {
      .query_abort("Invalid emuDB object: missing basePath or dbName attributes")
    }
    
    db_path1 <- file.path(base_path, paste0(db_name, "_emuDB.sqlite"))
    db_path2 <- file.path(base_path, paste0(db_name, "_emuDBcache.sqlite"))
    
    if (file.exists(db_path1)) {
      db_path <- db_path1
    } else if (file.exists(db_path2)) {
      db_path <- db_path2
    } else {
      .query_abort("SQLite database not found at: {.path {db_path1}} or {.path {db_path2}}")
    }
    
    database_dir <- base_path
  }
  
  if (!file.exists(db_path)) {
    .query_abort("SQLite database not found at: {.path {db_path}}")
  }
  
  # Lazy is the default as of v0.7.0; auto-collect S3 methods preserve
  # data-access behaviour for callers that expect a materialised segment_list.
  dots <- list(...)
  lazy <- if ("lazy" %in% names(dots)) dots$lazy else TRUE
  
  if (lazy) {
    # Return lazy segment list without executing query
    # Build base SQL query but don't execute — returns list(sql, params)
    parsed <- parse_eql_query(eql)
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
    # Execute immediately (old behavior). Strip `lazy` from dots since
    # execute_query() doesn't accept it.
    eager_dots <- dots[setdiff(names(dots), "lazy")]
    result <- do.call(execute_query, c(list(db_path, eql), eager_dots))

    # Convert to segment_list if result is a data.frame
    if (is.data.frame(result) && !S7::S7_inherits(result, segment_list)) {
      # Extract db_uuid and db_path for segment_list
      db_uuid <- if ("db_uuid" %in% names(result)) unique(result$db_uuid)[1] else ""
      result <- segment_list(result, db_uuid = db_uuid, db_path = database_dir)
    }

    if (S7::S7_inherits(result, segment_list)) {
      result <- .seed_provenance(result, "query", sys.call())
    }
    return(result)
  }
}

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
    .query_abort("Unknown query type: {.val {parsed$type}}")
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

  # Attribute -> level resolution: queries like `Text == "always"` reference
  # the *attribute* "Text" which lives on level "Word" (or wherever defined).
  # The eager execute_simple_query_corrected path resolves this via
  # .resolve_level_attribute; mirror it here so lazy parity holds.
  con <- .open_query_connection(db_path)
  on.exit(DBI::dbDisconnect(con))
  resolved <- .resolve_level_attribute(con, level, attribute)
  level <- resolved$level
  attribute <- resolved$attribute

  # Emit columns that downstream lazy transforms (scout/retreat/ascend/
  # descend) expect: start/end_item_id and start/end_item_seq_idx aliases.
  # For simple queries each row's start and end refer to the same item.
  base_sql <- paste0(
    "SELECT i.*, l.label as labels, ",
    "  i.item_id AS start_item_id, i.item_id AS end_item_id, ",
    "  i.seq_idx AS start_item_seq_idx, i.seq_idx AS end_item_seq_idx ",
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

  # Add filter based on operator. =~ / !~ use SQLite REGEXP so behaviour
  # matches the eager execute_simple_query_corrected path. Label alternatives
  # ("m|n|p") map to IN (?, ?, ?) -- same as the eager path. The parser puts
  # the alt list in parsed$alternatives (first element duplicated in
  # parsed$value), so check that first.
  alts <- parsed$alternatives
  if (operator %in% c("==", "!=")) {
    if (!is.null(alts) && length(alts) > 1) {
      placeholders <- paste(rep("?", length(alts)), collapse = ",")
      op_sql <- if (operator == "==") "IN" else "NOT IN"
      base_sql <- paste0(base_sql, "AND l.label ", op_sql, " (", placeholders, ")")
      params <- c(params, as.list(alts))
    } else {
      op_sql <- if (operator == "==") "=" else "!="
      base_sql <- paste0(base_sql, "AND l.label ", op_sql, " ?")
      params <- c(params, list(pattern))
    }
  } else if (operator == "=~") {
    base_sql <- paste0(base_sql, "AND l.label REGEXP ?")
    params <- c(params, list(pattern))
  } else if (operator == "!~") {
    base_sql <- paste0(base_sql, "AND NOT (l.label REGEXP ?)")
    params <- c(params, list(pattern))
  }

  return(list(sql = base_sql, params = params))
}

# Lazy SQL builders for sequence / dominance / function queries.
# Each delegates to the build_*_query_sql_impl helper in R/query_parser.R
# (which is also called by the eager execute_*_query_corrected wrapper),
# giving lazy and eager exactly one source of truth for the SQL.
#
# The build helpers need a DB connection at build time for attribute
# resolution and (for sequence/dominance) for materialising any non-simple
# sub-queries. We open and close one here; the SQL string + params are
# returned, and the lazy collect() opens a fresh connection to execute.
#
# Returning NULL means the query is statically empty (e.g. a sub-query
# resolved to zero rows). The lazy path turns that into an empty-shape
# SELECT so collect() yields a zero-row segment_list rather than aborting.

build_sequence_query_sql <- function(db_path, parsed, result_level = NULL) {
  con <- .open_query_connection(db_path)
  on.exit(DBI::dbDisconnect(con))
  q <- build_sequence_query_sql_impl(db_path, parsed, result_level, con = con)
  if (is.null(q)) return(.empty_query_sql())
  q
}

build_dominance_query_sql <- function(db_path, parsed, result_level = NULL) {
  con <- .open_query_connection(db_path)
  on.exit(DBI::dbDisconnect(con))
  q <- build_dominance_query_sql_impl(db_path, parsed, result_level, con = con)
  if (is.null(q)) return(.empty_query_sql())
  q
}

build_function_query_sql <- function(db_path, parsed) {
  con <- .open_query_connection(db_path)
  on.exit(DBI::dbDisconnect(con))

  func_name <- parsed$func_name
  level1 <- .resolve_level_attribute(con, parsed$level1, parsed$level1)$level
  level2 <- .resolve_level_attribute(con, parsed$level2, parsed$level2)$level
  operator <- parsed$operator
  value <- as.numeric(parsed$value)
  position <- parsed$position

  if (func_name %in% c("Start", "End", "Medial")) {
    build_position_function_sql(func_name, level1, level2, operator, value, position)
  } else if (func_name == "Num") {
    build_count_function_sql(level1, level2, operator, value)
  } else {
    .query_abort("Unknown function: {.val {func_name}}")
  }
}

build_conjunction_query_sql <- function(db_path, parsed, result_level = NULL) {
  left_result <- build_base_sql(db_path, parsed$left, list(result_level = result_level))
  right_result <- build_base_sql(db_path, parsed$right, list(result_level = result_level))
  if (is.null(left_result) || is.null(right_result)) return(NULL)
  # SQLite does not accept parenthesised SELECTs around a compound operator;
  # use the bare form.
  list(
    sql = paste0(left_result$sql, " INTERSECT ", right_result$sql),
    params = c(left_result$params, right_result$params)
  )
}

build_disjunction_query_sql <- function(db_path, parsed, result_level = NULL) {
  left_result <- build_base_sql(db_path, parsed$left, list(result_level = result_level))
  right_result <- build_base_sql(db_path, parsed$right, list(result_level = result_level))
  if (is.null(left_result) || is.null(right_result)) return(NULL)
  list(
    sql = paste0(left_result$sql, " UNION ", right_result$sql),
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
      .query_abort("Unknown query type: {.val {parsed$type}}")
    )
    
    # Deduce times for ITEM-type levels before formatting
    result <- deduce_item_times(result, db_path)
    return(format_as_emuRsegs(result))
    
  }, error = function(e) {
    .query_abort(c(
      "Query execution failed",
      "x" = conditionMessage(e),
      "i" = "Query: {.code {query_string}}"
    ), parent = e)
  })
}

# Query parser
