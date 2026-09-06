#' Query a corpus with EMU Query Language (EQL)
#'
#' Returns the annotation segments that match an EQL expression. Syntax
#' is the same as `emuR::query()` so existing queries work unchanged;
#' see the EMU-SDMS manual for the full grammar. Results come back as a
#' tibble-like `segment_list` ready for [enrich()] / [quantify()] / dplyr.
#'
#' Since v0.7 the query is **deferred by default**: a [lazy_segment_list]
#' is returned and the main result SQL fires when you access the rows
#' (`nrow()`, `head()`, `$`, dplyr verbs, [collect()] etc.). Deferral is
#' partial: plan-time work still resolves level/attribute names and
#' materialises compound sub-queries (sequences, dominance), so the query
#' string is fully built at call time. Pass `lazy = FALSE` to force
#' immediate execution.
#'
#' @param emuDB A `corpus` (preferred), a path to an `_emuDB` directory,
#'   or an existing `emuDBhandle`.
#' @param eql An EQL expression. Examples:
#'   `"Phonetic == t"`, `"Phonetic =~ [aeiou]"`,
#'   `"[Phonetic == t -> Phonetic =~ [aeiou]]"`,
#'   `"[Syllable #== S ^ Phonetic =~ [aeiou]]"`.
#' @param ... Passed to the executor. The most useful one is
#'   `lazy = FALSE` for eager execution.
#' @return A [lazy_segment_list] (default) or a [segment_list]
#'   (`lazy = FALSE`). Both have one row per matched annotation with the
#'   following columns:
#'   * `session`, `bundle` — corpus location.
#'   * `start`, `end` — segment times in ms (start == end for events).
#'   * `label` — the annotation label that matched.
#'   * `level`, `attribute`, `type` — annotation level and the matched
#'     attribute name, plus the level type (`SEGMENT` / `EVENT` /
#'     `ITEM`).
#'   * `start_item_id`, `end_item_id`, `start_item_seq_idx`,
#'     `end_item_seq_idx` — internal item references used by
#'     [scout()] / [ascend_to()] / [descend_to()].
#'   * `db_uuid` — for joining against the cache.
#'   Provenance is recorded on the result; see [provenance()].
#' @section Supported EQL features:
#'   reindeer's query engine targets parity with `emuR::query()` and
#'   supports:
#'   * Equality / inequality: `Level == "a"`, `Level != "a"`,
#'     with label alternatives via `|` (e.g. `"m|n|p"`).
#'   * Regex: `Level =~ "[aeiou]"`, `Level !~ "[aeiou]"`.
#'   * Sequences: `[A -> B]` (immediate adjacency at the same level).
#'   * Dominance: `[A ^ B]` (A dominates B in the hierarchy);
#'     `[Syllable #== S ^ Phonetic =~ "[aeiou]"]` projects the
#'     dominating level.
#'   * Conjunction `&` and disjunction `|` of sub-queries inside
#'     brackets.
#'   * Position functions: `Start()`, `End()`, `Medial()`, `Num()`.
#'   * Scope filters on `Session` / `Bundle`.
#'   * Attribute-as-level resolution: querying a defined attribute
#'     name (e.g. `Text == "always"`) resolves to its host level.
#' @section Common pitfalls:
#'   * Use `==` (double equals) for equality — `=` triggers a parse error.
#'   * Regex patterns go with `=~`/`!~`, exact strings with `==`/`!=`.
#'   * Wrap sequences and dominances in `[ ]`: `[A -> B]`, `[A ^ B]`.
#' @family query
#' @seealso [scout()], [ascend_to()], [descend_to()], [collect()],
#'   [provenance()]
#' @examplesIf interactive()
#' corp <- demo_corpus()
#' query(corp, "Phonetic == n")                          # lazy by default
#' query(corp, "[Phonetic == t -> Phonetic =~ [aeiou]]") # sequence
#' query(corp, "Phonetic =~ [aeiou]", lazy = FALSE)      # eager
#' @export
query <- function(emuDB, eql, ...) {
  # Handle corpus objects
  if (S7::S7_inherits(emuDB, reindeer::corpus)) {
    base_path <- emuDB@basePath
    db_name <- emuDB@dbName
    
    # Try both naming conventions
    db_path1 <- file.path(base_path, paste0(db_name, "_emuDB.sqlite"))
    db_path2 <- file.path(base_path, paste0(db_name, database.cache.suffix))
    
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
    db_path2 <- file.path(emuDB, paste0(db_name, database.cache.suffix))
    
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
    db_path2 <- file.path(base_path, paste0(db_name, database.cache.suffix))
    
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

    # One connection threaded through all sub-builders avoids per-branch
    # connection churn on nested EQL (e.g. `[A & [B -> C]]` previously spawned
    # 5+ connections at SQL-build time).
    conn <- .open_query_connection(db_path)
    on.exit(DBI::dbDisconnect(conn))

    base_query <- build_base_sql(db_path, parsed, dots, con = conn)

    # Get db_uuid from database (reuses the same connection)
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
#' @noRd
build_base_sql <- function(db_path, parsed, opts = list(), con = NULL) {
  result_level <- opts$result_level %||% NULL

  # Build SQL based on query type — all builders return list(sql, params).
  # `con` is threaded through so nested builders share a single connection;
  # NULL falls back to per-builder open/close for back-compat.
  result <- switch(parsed$type,
    "simple" = build_simple_query_sql(db_path, parsed, con = con),
    "scope_filter" = build_scope_filter_sql(db_path, parsed),
    "sequence" = build_sequence_query_sql(db_path, parsed, result_level, con = con),
    "dominance" = build_dominance_query_sql(db_path, parsed, result_level, con = con),
    "function" = build_function_query_sql(db_path, parsed, con = con),
    "conjunction" = build_conjunction_query_sql(db_path, parsed, result_level, con = con),
    "disjunction" = build_disjunction_query_sql(db_path, parsed, result_level, con = con),
    .query_abort("Unknown query type: {.val {parsed$type}}")
  )

  return(result)
}

#' Build SQL for a scope filter (`Session == X` / `Bundle == Y`)
#'
#' Returns ALL items whose owning session (or bundle) matches the predicate.
#' When used standalone, gives every annotation in that session/bundle.
#' When used in a conjunction, the INTERSECT semantics narrow the other
#' side of the conjunction to the requested scope.
#'
#' @keywords internal
#' @noRd
build_scope_filter_sql <- function(db_path, parsed) {
  column <- if (identical(parsed$kind, "session")) "i.session" else "i.bundle"
  operator <- parsed$operator
  pattern <- parsed$value
  alts <- parsed$alternatives

  base_sql <- paste0(
    "SELECT i.*, l.label as labels, l.name as attribute, ",
    "  i.item_id AS start_item_id, i.item_id AS end_item_id, ",
    "  i.seq_idx AS start_item_seq_idx, i.seq_idx AS end_item_seq_idx ",
    "FROM items i ",
    "INNER JOIN labels l ON ",
    "  i.db_uuid = l.db_uuid AND ",
    "  i.session = l.session AND ",
    "  i.bundle = l.bundle AND ",
    "  i.item_id = l.item_id ",
    "WHERE 1 = 1 "
  )
  params <- list()

  if (operator %in% c("==", "!=")) {
    if (!is.null(alts) && length(alts) > 1) {
      placeholders <- paste(rep("?", length(alts)), collapse = ",")
      op_sql <- if (operator == "==") "IN" else "NOT IN"
      base_sql <- paste0(base_sql, "AND ", column, " ", op_sql,
                         " (", placeholders, ")")
      params <- c(params, as.list(alts))
    } else {
      op_sql <- if (operator == "==") "=" else "!="
      base_sql <- paste0(base_sql, "AND ", column, " ", op_sql, " ?")
      params <- c(params, list(pattern))
    }
  } else if (operator == "=~") {
    base_sql <- paste0(base_sql, "AND ", column, " REGEXP ?")
    params <- c(params, list(pattern))
  } else if (operator == "!~") {
    base_sql <- paste0(base_sql, "AND NOT (", column, " REGEXP ?)")
    params <- c(params, list(pattern))
  }

  list(sql = base_sql, params = params)
}

#' Build SQL for Simple Query
#'
#' Returns a parameterized query as list(sql, params) to prevent SQL injection.
#' @keywords internal
#' @noRd
build_simple_query_sql <- function(db_path, parsed, con = NULL) {
  level <- parsed$level
  operator <- parsed$operator
  pattern <- parsed$pattern %||% parsed$value
  attribute <- parsed$attribute %||% level

  # Attribute -> level resolution: queries like `Text == "always"` reference
  # the *attribute* "Text" which lives on level "Word" (or wherever defined).
  # The eager execute_simple_query_corrected path resolves this via
  # .resolve_level_attribute; mirror it here so lazy parity holds.
  own_con <- is.null(con)
  if (own_con) {
    con <- .open_query_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
  }
  resolved <- .resolve_level_attribute(con, level, attribute)
  level <- resolved$level
  attribute <- resolved$attribute

  # Emit columns that downstream lazy transforms (scout/retreat/ascend/
  # descend) expect: start/end_item_id and start/end_item_seq_idx aliases.
  # For simple queries each row's start and end refer to the same item.
  # Project l.name AS attribute so format_as_emuRsegs() picks up the
  # attribute name from the predicate (e.g. "Text") rather than falling
  # back to the host level ("Word").
  base_sql <- paste0(
    "SELECT i.*, l.label as labels, l.name as attribute, ",
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

build_sequence_query_sql <- function(db_path, parsed, result_level = NULL, con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- .open_query_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
  }
  q <- build_sequence_query_sql_impl(db_path, parsed, result_level, con = con)
  if (is.null(q)) return(.empty_query_sql())
  # Strip trailing ORDER BY so the SQL can nest inside a compound operator.
  q$sql <- sub("\\s*ORDER BY[^()]*$", "", q$sql)
  q
}

build_dominance_query_sql <- function(db_path, parsed, result_level = NULL, con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- .open_query_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
  }
  q <- build_dominance_query_sql_impl(db_path, parsed, result_level, con = con)
  if (is.null(q)) return(.empty_query_sql())
  q$sql <- sub("\\s*ORDER BY[^()]*$", "", q$sql)
  q
}

build_function_query_sql <- function(db_path, parsed, con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- .open_query_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
  }

  func_name <- parsed$func_name
  level1 <- .resolve_level_attribute(con, parsed$level1, parsed$level1)$level
  level2 <- .resolve_level_attribute(con, parsed$level2, parsed$level2)$level
  operator <- parsed$operator
  value <- as.numeric(parsed$value)
  position <- parsed$position

  q <- if (func_name %in% c("Start", "End", "Medial")) {
    build_position_function_sql(func_name, level1, level2, operator, value, position)
  } else if (func_name == "Num") {
    build_count_function_sql(level1, level2, operator, value)
  } else {
    .query_abort("Unknown function: {.val {func_name}}")
  }

  # ORDER BY is invalid inside an INTERSECT / UNION compound on SQLite, so
  # strip a trailing ORDER BY here — the lazy path applies its own ordering
  # after collect(), and conjunction/disjunction can safely embed the result.
  q$sql <- sub("\\s*ORDER BY[^()]*$", "", q$sql)
  q
}

#' Eager scope-filter executor — reuses the lazy SQL builder
#' @keywords internal
#' @noRd
execute_scope_filter_eager <- function(db_path, parsed, con = NULL) {
  q <- build_scope_filter_sql(db_path, parsed)
  if (is.null(con)) {
    con <- .open_query_connection(db_path)
    on.exit(DBI::dbDisconnect(con))
  }
  df <- if (length(q$params) == 0L) {
    DBI::dbGetQuery(con, q$sql)
  } else {
    DBI::dbGetQuery(con, q$sql, params = q$params)
  }
  if ("labels" %in% names(df) && !"label" %in% names(df)) {
    names(df)[names(df) == "labels"] <- "label"
  }
  df
}

build_conjunction_query_sql <- function(db_path, parsed, result_level = NULL, con = NULL) {
  left_result <- build_base_sql(db_path, parsed$left, list(result_level = result_level), con = con)
  right_result <- build_base_sql(db_path, parsed$right, list(result_level = result_level), con = con)
  if (is.null(left_result) || is.null(right_result)) return(NULL)
  # Tuple-EXISTS instead of INTERSECT: the left side carries the row shape
  # of a segment_list, the right side just supplies an item-identity filter.
  # This is robust to column-count mismatches between disparate builders
  # (function vs simple) and to two attribute predicates on the same items
  # (e.g. `[Text == always & Accent == S]`).
  list(
    sql = paste0(
      "SELECT base.* FROM (", left_result$sql, ") base ",
      "WHERE EXISTS (SELECT 1 FROM (", right_result$sql, ") sub ",
      "WHERE sub.db_uuid = base.db_uuid AND sub.session = base.session ",
      "AND sub.bundle = base.bundle AND sub.item_id = base.item_id)"
    ),
    params = c(left_result$params, right_result$params)
  )
}

build_disjunction_query_sql <- function(db_path, parsed, result_level = NULL, con = NULL) {
  left_result <- build_base_sql(db_path, parsed$left, list(result_level = result_level), con = con)
  right_result <- build_base_sql(db_path, parsed$right, list(result_level = result_level), con = con)
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
      "scope_filter" = execute_scope_filter_eager(db_path, parsed, con = con),
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
