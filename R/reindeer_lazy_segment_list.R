#' Lazy Segment List S7 Class
#'
#' Implements lazy evaluation for EMU query operations. Query chains are built
#' up without executing SQL until `collect()` is called or the data is needed.
#'
#' @section Benefits:
#' - Query optimization: Multiple operations can be combined into single SQL query
#' - Reduced memory: Only materialize when needed
#' - Better performance: SQL database handles complex joins efficiently
#'
#' @examples
#' \dontrun{
#' # Build query chain without execution
#' lazy_segs <- ask_for(corpus, "Phonetic == t") %>%
#'   scout(1) %>%
#'   ascend_to("Word")
#' 
#' # Force execution
#' segs <- collect(lazy_segs)
#' 
#' # Or implicitly via print/summary
#' print(lazy_segs)  # Shows preview
#' }

library(S7)

#' @export
lazy_segment_list <- S7::new_class(
  "lazy_segment_list",
  properties = list(
    corpus = class_any,  # reindeer::corpus object or NULL
    query_parts = class_list,  # List of query components
    db_path = class_character,
    db_uuid = class_character,
    materialized = class_logical,
    cache = class_any  # NULL or data.table when materialized
  ),
  constructor = function(corpus = NULL, query_parts = list(), 
                        db_path = "", db_uuid = "", 
                        materialized = FALSE, cache = NULL) {
    S7::new_object(
      S7::S7_object(),
      corpus = corpus,
      query_parts = query_parts,
      db_path = db_path,
      db_uuid = db_uuid,
      materialized = materialized,
      cache = cache
    )
  }
)

#' Collect/Materialize a Lazy Segment List
#'
#' Forces execution of the query chain and returns a regular segment_list.
#'
#' @param lazy_sl A lazy_segment_list object
#' @param verbose Logical, whether to print SQL query
#' @return A segment_list object
#' @export
collect <- function(lazy_sl, verbose = FALSE) {
  UseMethod("collect")
}

#' @export
collect.default <- function(lazy_sl, verbose = FALSE) {
  # If already a regular segment_list, return as-is
  if (inherits(lazy_sl, "segment_list") && !inherits(lazy_sl, "lazy_segment_list")) {
    return(lazy_sl)
  }
  stop("collect() requires a lazy_segment_list or segment_list object")
}

#' @export
collect.lazy_segment_list <- function(lazy_sl, verbose = FALSE) {
  # If already materialized, return cache
  if (lazy_sl@materialized && !is.null(lazy_sl@cache)) {
    return(lazy_sl@cache)
  }
  
  # Build SQL query from query_parts
  sql_query <- build_sql_from_parts(lazy_sl@query_parts, verbose = verbose)
  
  if (verbose) {
    cli::cli_alert_info("Executing SQL query:")
    cli::cli_code(sql_query)
  }
  
  # Execute query
  conn <- DBI::dbConnect(RSQLite::SQLite(), lazy_sl@db_path)
  on.exit(DBI::dbDisconnect(conn))
  
  result_dt <- data.table::setDT(
    DBI::dbGetQuery(conn, sql_query)
  )
  
  # Set keys for fast operations
  if (nrow(result_dt) > 0) {
    data.table::setkey(result_dt, session, bundle, start_item_id)
  }
  
  # Convert to segment_list
  seg_list <- segment_list(
    data = as.data.frame(result_dt),
    db_uuid = lazy_sl@db_uuid,
    db_path = lazy_sl@db_path
  )
  
  # Cache result
  lazy_sl@cache <- seg_list
  lazy_sl@materialized <- TRUE
  
  return(seg_list)
}

#' Build SQL Query from Query Parts
#'
#' @param query_parts List of query components
#' @param verbose Logical
#' @return SQL query string
#' @keywords internal
build_sql_from_parts <- function(query_parts, verbose = FALSE) {
  # Start with base query
  if (is.null(query_parts$base)) {
    stop("No base query found in query_parts")
  }
  
  base_sql <- query_parts$base
  
  # If no transforms, return base
  if (is.null(query_parts$transforms) || length(query_parts$transforms) == 0) {
    return(base_sql)
  }
  
  # Apply transforms sequentially
  for (transform in query_parts$transforms) {
    base_sql <- apply_transform(base_sql, transform, verbose = verbose)
  }
  
  return(base_sql)
}

#' Apply a Transform to SQL Query
#'
#' @param sql Current SQL query
#' @param transform List with type and parameters
#' @param verbose Logical
#' @return Modified SQL query
#' @keywords internal
apply_transform <- function(sql, transform, verbose = FALSE) {
  type <- transform$type
  
  if (verbose) {
    cli::cli_alert("Applying transform: {type}")
  }
  
  switch(type,
    scout = apply_scout_transform(sql, transform$n),
    retreat = apply_retreat_transform(sql, transform$n),
    ascend = apply_ascend_transform(sql, transform$level),
    descend = apply_descend_transform(sql, transform$level),
    stop("Unknown transform type: ", type)
  )
}

#' Apply Scout (Forward Sequence) Transform
#'
#' @keywords internal
apply_scout_transform <- function(sql, n = 1) {
  # Wrap current query as subquery and add sequence logic
  # This builds: SELECT * FROM items WHERE seq_idx = (subquery.seq_idx + n)
  paste0(
    "WITH base AS (", sql, ") ",
    "SELECT i.* FROM items i ",
    "INNER JOIN base b ON ",
    "  i.db_uuid = b.db_uuid AND ",
    "  i.session = b.session AND ",
    "  i.bundle = b.bundle AND ",
    "  i.level = b.level AND ",
    "  i.seq_idx = b.end_item_seq_idx + ", n
  )
}

#' Apply Retreat (Backward Sequence) Transform
#'
#' @keywords internal
apply_retreat_transform <- function(sql, n = 1) {
  paste0(
    "WITH base AS (", sql, ") ",
    "SELECT i.* FROM items i ",
    "INNER JOIN base b ON ",
    "  i.db_uuid = b.db_uuid AND ",
    "  i.session = b.session AND ",
    "  i.bundle = b.bundle AND ",
    "  i.level = b.level AND ",
    "  i.seq_idx = b.start_item_seq_idx - ", n
  )
}

#' Apply Ascend (Dominance Upward) Transform
#'
#' @keywords internal
apply_ascend_transform <- function(sql, level) {
  paste0(
    "WITH base AS (", sql, ") ",
    "SELECT DISTINCT i.* FROM items i ",
    "INNER JOIN links l ON ",
    "  l.db_uuid = i.db_uuid AND ",
    "  l.session = i.session AND ",
    "  l.bundle = i.bundle AND ",
    "  l.to_id = i.item_id ",
    "INNER JOIN base b ON ",
    "  l.db_uuid = b.db_uuid AND ",
    "  l.session = b.session AND ",
    "  l.bundle = b.bundle AND ",
    "  l.from_id = b.start_item_id ",
    "WHERE i.level = '", level, "'"
  )
}

#' Apply Descend (Dominance Downward) Transform
#'
#' @keywords internal
apply_descend_transform <- function(sql, level) {
  paste0(
    "WITH base AS (", sql, ") ",
    "SELECT DISTINCT i.* FROM items i ",
    "INNER JOIN links l ON ",
    "  l.db_uuid = i.db_uuid AND ",
    "  l.session = i.session AND ",
    "  l.bundle = i.bundle AND ",
    "  l.from_id = i.item_id ",
    "INNER JOIN base b ON ",
    "  l.db_uuid = b.db_uuid AND ",
    "  l.session = b.session AND ",
    "  l.bundle = b.bundle AND ",
    "  l.to_id = b.start_item_id ",
    "WHERE i.level = '", level, "'"
  )
}

#' Print Method for Lazy Segment List
#'
#' Shows query structure without materializing (unless already cached)
#'
#' @param x A lazy_segment_list object
#' @param ... Additional arguments
#' @export
print.lazy_segment_list <- function(x, ...) {
  cli::cli_h1("Lazy Segment List")
  
  if (x@materialized && !is.null(x@cache)) {
    cli::cli_alert_success("Materialized (cached)")
    cli::cli_text("Showing cached result:")
    print(x@cache)
  } else {
    cli::cli_alert_info("Not yet materialized")
    
    # Show query structure
    cli::cli_h2("Query Structure")
    cli::cli_text("Base: {.code {substr(x@query_parts$base, 1, 60)}}...")
    
    if (!is.null(x@query_parts$transforms) && length(x@query_parts$transforms) > 0) {
      cli::cli_h3("Transforms")
      for (i in seq_along(x@query_parts$transforms)) {
        t <- x@query_parts$transforms[[i]]
        cli::cli_li("{i}. {t$type} {.emph {paste(names(t)[-1], t[-1], sep='=', collapse=', ')}}")
      }
    }
    
    cli::cli_text("")
    cli::cli_alert("Call {.fn collect} to execute query and get results")
    
    # Show preview if possible (execute with LIMIT)
    tryCatch({
      preview_sql <- paste0(build_sql_from_parts(x@query_parts), " LIMIT 5")
      conn <- DBI::dbConnect(RSQLite::SQLite(), x@db_path)
      on.exit(DBI::dbDisconnect(conn))
      preview_dt <- data.table::setDT(DBI::dbGetQuery(conn, preview_sql))
      
      if (nrow(preview_dt) > 0) {
        cli::cli_h3("Preview (first 5 rows)")
        print(preview_dt)
      }
    }, error = function(e) {
      cli::cli_alert_warning("Could not generate preview: {e$message}")
    })
  }
  
  invisible(x)
}

#' Summary Method for Lazy Segment List
#'
#' @param object A lazy_segment_list object
#' @param ... Additional arguments
#' @export
summary.lazy_segment_list <- function(object, ...) {
  if (object@materialized && !is.null(object@cache)) {
    cli::cli_alert_success("Materialized lazy segment list")
    summary(object@cache)
  } else {
    cli::cli_alert_info("Lazy segment list (not materialized)")
    cli::cli_text("Database: {.path {object@db_path}}")
    cli::cli_text("UUID: {.val {object@db_uuid}}")
    
    # Count transforms
    n_transforms <- length(object@query_parts$transforms %||% list())
    cli::cli_text("Pending transforms: {.val {n_transforms}}")
    
    # Try to get count without materializing full result
    tryCatch({
      count_sql <- paste0(
        "SELECT COUNT(*) as n FROM (",
        build_sql_from_parts(object@query_parts),
        ")"
      )
      conn <- DBI::dbConnect(RSQLite::SQLite(), object@db_path)
      on.exit(DBI::dbDisconnect(conn))
      n <- DBI::dbGetQuery(conn, count_sql)$n
      cli::cli_text("Estimated rows: {.val {n}}")
    }, error = function(e) {
      cli::cli_alert_warning("Could not estimate row count")
    })
  }
  
  invisible(object)
}

#' Convert Lazy Segment List to Data Frame
#'
#' Forces materialization
#'
#' @param x A lazy_segment_list object
#' @param ... Additional arguments
#' @export
as.data.frame.lazy_segment_list <- function(x, ...) {
  seg_list <- collect(x)
  as.data.frame(seg_list)
}

#' Check if Object is Lazy
#'
#' @param x An object
#' @return Logical
#' @export
is_lazy <- function(x) {
  inherits(x, "lazy_segment_list") && !x@materialized
}

#' Check if Object Needs Materialization
#'
#' @param x An object
#' @return Logical
#' @export
needs_collect <- function(x) {
  is_lazy(x)
}
