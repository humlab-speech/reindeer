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
#' lazy_segs <- query(corpus, "Phonetic == t") |>
#'   scout(1) |>
#'   ascend_to("Word")
#' 
#' # Force execution
#' segs <- collect(lazy_segs)
#' 
#' # Or inspect via print/summary (runs a COUNT(*) + LIMIT 3 preview)
#' print(lazy_segs)
#' }
#'
#' @param corpus A `reindeer::corpus` object, or `NULL`.
#' @param query_parts List of query components (base query plus transforms).
#' @param db_path Path to the corpus database directory.
#' @param db_uuid Database UUID (single string).
#' @param materialized Logical; whether the query chain has been executed.
#' @param cache The cached materialized `segment_list`, or `NULL`.
#' @name lazy_segment_list
#' @export
lazy_segment_list <- S7::new_class(
  "lazy_segment_list",
  properties = list(
    corpus = S7::class_any,  # reindeer::corpus object or NULL
    query_parts = S7::class_list,  # List of query components
    db_path = S7::class_character,
    db_uuid = S7::class_character,
    .state = S7::class_any  # Environment with $materialized and $cache (reference semantics)
  ),
  validator = function(self) {
    if (!is.null(self@corpus) && !S7::S7_inherits(self@corpus, corpus)) {
      return("corpus must be NULL or a reindeer corpus object")
    }
    if (!is.list(self@query_parts)) {
      return("query_parts must be a list")
    }
    if (!is.environment(self@.state)) {
      return(".state must be an environment")
    }
    if (!is.character(self@db_uuid) || length(self@db_uuid) != 1) {
      return("db_uuid must be a single character string")
    }
    NULL
  },
  constructor = function(corpus = NULL, query_parts = list(),
                        db_path = "", db_uuid = "",
                        materialized = FALSE, cache = NULL) {
    state <- new.env(parent = emptyenv())
    state$materialized <- materialized
    state$cache <- cache
    S7::new_object(
      S7::S7_object(),
      corpus = corpus,
      query_parts = query_parts,
      db_path = db_path,
      db_uuid = db_uuid,
      .state = state
    )
  }
)

#' Declaration order of a plan's steps
#'
#' `query_parts` keeps SQL steps (`transforms`) and R-side steps
#' (`post_transforms`) in separate lists; `order` records how they interleave.
#' Plans built without it (older objects, tests) run SQL steps first.
#' @noRd
.step_order <- function(query_parts) {
  if (!is.null(query_parts$order)) return(query_parts$order)
  c(rep("sql", length(query_parts$transforms)),
    rep("post", length(query_parts$post_transforms)))
}

#' Append a step to a lazy plan, returning a new plan with its own state
#'
#' Plans are values: deriving one must never share the materialization cache
#' of the plan it came from.
#' @param x A `lazy_segment_list`.
#' @param kind `"sql"` (a `transforms` entry) or `"post"` (`post_transforms`).
#' @param spec The step specification (a list with `type`).
#' @noRd
.fork_lazy <- function(x, kind, spec) {
  qp <- x@query_parts
  qp$order <- c(.step_order(qp), kind)
  field <- if (kind == "sql") "transforms" else "post_transforms"
  qp[[field]] <- c(qp[[field]], list(spec))
  lazy_segment_list(corpus = x@corpus, query_parts = qp,
                    db_path = x@db_path, db_uuid = x@db_uuid)
}

#' Collect/Materialize a Lazy Segment List
#'
#' `collect()` is [dplyr::collect()], re-exported. On a `lazy_segment_list` it
#' executes the query plan and returns a regular `segment_list`; on a
#' `segment_list` or tibble it returns the object unchanged.
#'
#' @param x A `lazy_segment_list`, `segment_list` or data frame.
#' @param ... Unused, for the generic.
#' @param verbose Logical, whether to print the SQL query.
#' @return A `segment_list` object.
#' @importFrom dplyr collect
#' @export
dplyr::collect

# Registered on dplyr::collect for the S7 class in .onLoad (S3 dispatch on
# the namespaced class name "reindeer::lazy_segment_list").
.collect_lazy_segment_list <- function(x, ..., verbose = FALSE) {
  collect_lazy_impl(x, verbose = verbose)
}

#' @keywords internal
collect_lazy_impl <- function(lazy_sl, verbose = FALSE) {
  # If already materialized, return cache
  if (lazy_sl@.state$materialized && !is.null(lazy_sl@.state$cache)) {
    return(lazy_sl@.state$cache)
  }
  
  # Steps run in declaration order. The leading run of SQL steps is fused into
  # the one query; later steps run on the materialized segment_list.
  parts <- lazy_sl@query_parts
  order <- .step_order(parts)
  n_lead <- if (length(order) == 0) 0L else {
    nz <- which(order != "sql")
    if (length(nz) == 0) length(order) else nz[1] - 1L
  }
  sql_steps <- parts$transforms
  post_steps <- parts$post_transforms
  lead_parts <- parts
  lead_parts$transforms <- sql_steps[seq_len(n_lead)]
  query <- build_sql_from_parts(lead_parts, verbose = verbose)
  
  if (verbose) {
    cli::cli_alert_info("Executing SQL query:")
    cli::cli_code(query$sql)
  }
  
  # Execute query with parameters. Use .open_query_connection so REGEXP is
  # registered (=~ / !~ in dominance / sequence sub-queries depends on it).
  conn <- .open_query_connection(lazy_sl@db_path)
  on.exit(DBI::dbDisconnect(conn))
  
  # DBI rejects params = list() when the SQL has no ? placeholders, so pass
  # NULL (= no params) when the builder produced an empty params list.
  raw_df <- if (length(query$params) == 0) {
    DBI::dbGetQuery(conn, query$sql)
  } else {
    DBI::dbGetQuery(conn, query$sql, params = query$params)
  }
  
  if (nrow(raw_df) == 0) {
    result_df <- format_as_emuRsegs(raw_df)
  } else {
    # The lazy SQL returns raw items columns (item_id, seq_idx, sample_start,
    # sample_dur, sample_point, labels). Rename 'labels' -> 'label' to match
    # what format_as_emuRsegs expects, then convert to emuR segment format.
    if ("labels" %in% names(raw_df) && !"label" %in% names(raw_df)) {
      names(raw_df)[names(raw_df) == "labels"] <- "label"
    }
    # Fill in start/end for ITEM-type levels (those without explicit times)
    # by recursively walking their dominance children. Mirrors the eager
    # execute_query() path at R/query_executor.R.
    raw_df <- deduce_item_times(raw_df, lazy_sl@db_path)
    result_df <- format_as_emuRsegs(raw_df)
  }
  
  # Convert to segment_list. lazy_sl@db_path is the .sqlite file; the
  # segment_list constructor expects the corpus directory, so use its
  # parent dir.
  seg_list <- segment_list(
    data = as.data.frame(result_df),
    db_uuid = lazy_sl@db_uuid,
    db_path = dirname(lazy_sl@db_path)
  )

  # Seed provenance as if the user had run the eager path: the verb is
  # "query" so downstream tests and inspectors see a consistent first step
  # regardless of whether the segment_list arrived eager or via collect().
  seg_list <- .seed_provenance(seg_list, "query", sys.call(-1L))

  # Replay the remaining steps in declaration order on the materialized list.
  i_sql <- n_lead
  i_post <- 0L
  for (kind in order[seq_along(order) > n_lead]) {
    if (kind == "sql") {
      i_sql <- i_sql + 1L
      seg_list <- .apply_sql_step_eager(seg_list, sql_steps[[i_sql]])
    } else {
      i_post <- i_post + 1L
      tr <- post_steps[[i_post]]
      seg_list <- switch(tr$type,
        "quantify"    = do.call(quantify, c(list(seg_list, tr$dsp_function), tr$args)),
        "biographize" = do.call(biographize, c(list(seg_list, tr$corpus_obj), tr$args)),
        cli::cli_abort("Unknown post-materialization transform type: {.val {tr$type}}")
      )
    }
  }

  # Cache result (uses environment reference semantics -- mutates in place)
  lazy_sl@.state$cache <- seg_list
  lazy_sl@.state$materialized <- TRUE

  return(seg_list)
}

#' Run one SQL-step spec through the eager verbs
#' @noRd
.apply_sql_step_eager <- function(seg, tr) {
  switch(tr$type,
    scout = scout(seg, tr$n, count_from = tr$count_from, capture = tr$capture,
                  ignore_bundle_boundaries = tr$ignore_bundle_boundaries),
    ascend = ascend_to(seg, tr$level),
    descend = descend_to(seg, tr$level),
    cli::cli_abort("Unknown transform type: {.val {tr$type}}")
  )
}

#' Build Parameterized SQL Query from Query Parts
#'
#' @param query_parts List of query components (base is list(sql, params))
#' @param verbose Logical
#' @return list(sql = "...", params = list(...))
#' @keywords internal
#' @noRd
build_sql_from_parts <- function(query_parts, verbose = FALSE) {
  # Start with base query -- list(sql, params)
  if (is.null(query_parts$base)) {
    cli::cli_abort("No base query found in query_parts")
  }
  
  current <- query_parts$base  # list(sql, params)
  
  # If no transforms, return base
  if (is.null(query_parts$transforms) || length(query_parts$transforms) == 0) {
    return(current)
  }
  
  # Apply transforms sequentially -- each returns list(sql, params)
  for (transform in query_parts$transforms) {
    current <- apply_transform(current, transform, verbose = verbose)
  }
  
  return(current)
}

#' Apply a Transform to Parameterized SQL Query
#'
#' @param query list(sql, params) -- current query
#' @param transform List with type and parameters
#' @param verbose Logical
#' @return list(sql, params) -- modified query
#' @keywords internal
#' @noRd
apply_transform <- function(query, transform, verbose = FALSE) {
  type <- transform$type
  
  if (verbose) {
    cli::cli_alert("Applying transform: {type}")
  }
  
  switch(type,
    scout = apply_scout_transform(query, transform$n),
    ascend = apply_ascend_transform(query, transform$level),
    descend = apply_descend_transform(query, transform$level),
    cli::cli_abort("Unknown transform type: {.val {type}}")
  )
}

#' Apply Scout (Forward Sequence) Transform
#'
#' @param query list(sql, params) -- current query
#' @param n integer -- number of steps forward
#' @return list(sql, params) -- modified query
#' @keywords internal
#' @noRd
apply_scout_transform <- function(query, n = 1) {
  n <- as.integer(n)
  # Project the segment_list-shaped columns the eager path emits so
  # collect()'s deduce_item_times + format_as_emuRsegs see what they
  # expect: items.* plus labels/attribute and start/end_item_id +
  # start/end_item_seq_idx aliases.
  sql <- paste0(
    "WITH base AS (", query$sql, ") ",
    "SELECT i.*, ",
    "  l.label AS labels, l.name AS attribute, ",
    "  i.item_id AS start_item_id, i.item_id AS end_item_id, ",
    "  i.seq_idx AS start_item_seq_idx, i.seq_idx AS end_item_seq_idx ",
    "FROM items i ",
    "INNER JOIN base b ON ",
    "  i.db_uuid = b.db_uuid AND ",
    "  i.session = b.session AND ",
    "  i.bundle = b.bundle AND ",
    "  i.level = b.level AND ",
    "  i.seq_idx = b.end_item_seq_idx + ? ",
    "LEFT JOIN labels l ON ",
    "  l.db_uuid = i.db_uuid AND ",
    "  l.session = i.session AND ",
    "  l.bundle = i.bundle AND ",
    "  l.item_id = i.item_id AND ",
    "  l.name = b.attribute")
  list(sql = sql, params = c(query$params, list(n)))
}


#' Apply Ascend (Dominance Upward) Transform
#'
#' @param query list(sql, params) -- current query
#' @param level character -- target level to ascend to
#' @return list(sql, params) -- modified query
#' @keywords internal
#' @noRd
apply_ascend_transform <- function(query, level) {
  # Project segment_list-shape columns; default attribute on the target
  # level is the level name itself, so label lookup keys on l.name = i.level.
  sql <- paste0(
    "WITH base AS (", query$sql, ") ",
    "SELECT DISTINCT i.*, ",
    "  lb.label AS labels, lb.name AS attribute, ",
    "  i.item_id AS start_item_id, i.item_id AS end_item_id, ",
    "  i.seq_idx AS start_item_seq_idx, i.seq_idx AS end_item_seq_idx ",
    "FROM items i ",
    "INNER JOIN links lk ON ",
    "  lk.db_uuid = i.db_uuid AND ",
    "  lk.session = i.session AND ",
    "  lk.bundle = i.bundle AND ",
    "  lk.from_id = i.item_id ",
    "INNER JOIN base b ON ",
    "  lk.db_uuid = b.db_uuid AND ",
    "  lk.session = b.session AND ",
    "  lk.bundle = b.bundle AND ",
    "  lk.to_id = b.start_item_id ",
    "LEFT JOIN labels lb ON ",
    "  lb.db_uuid = i.db_uuid AND ",
    "  lb.session = i.session AND ",
    "  lb.bundle = i.bundle AND ",
    "  lb.item_id = i.item_id AND ",
    "  lb.name = i.level ",
    "WHERE i.level = ?")
  list(sql = sql, params = c(query$params, list(level)))
}

#' Apply Descend (Dominance Downward) Transform
#'
#' @param query list(sql, params) -- current query
#' @param level character -- target level to descend to
#' @return list(sql, params) -- modified query
#' @keywords internal
#' @noRd
apply_descend_transform <- function(query, level) {
  sql <- paste0(
    "WITH base AS (", query$sql, ") ",
    "SELECT DISTINCT i.*, ",
    "  lb.label AS labels, lb.name AS attribute, ",
    "  i.item_id AS start_item_id, i.item_id AS end_item_id, ",
    "  i.seq_idx AS start_item_seq_idx, i.seq_idx AS end_item_seq_idx ",
    "FROM items i ",
    "INNER JOIN links lk ON ",
    "  lk.db_uuid = i.db_uuid AND ",
    "  lk.session = i.session AND ",
    "  lk.bundle = i.bundle AND ",
    "  lk.to_id = i.item_id ",
    "INNER JOIN base b ON ",
    "  lk.db_uuid = b.db_uuid AND ",
    "  lk.session = b.session AND ",
    "  lk.bundle = b.bundle AND ",
    "  lk.from_id = b.start_item_id ",
    "LEFT JOIN labels lb ON ",
    "  lb.db_uuid = i.db_uuid AND ",
    "  lb.session = i.session AND ",
    "  lb.bundle = i.bundle AND ",
    "  lb.item_id = i.item_id AND ",
    "  lb.name = i.level ",
    "WHERE i.level = ?")
  list(sql = sql, params = c(query$params, list(level)))
}

# ==============================================================================
# PRINT, SUMMARY, AND GLIMPSE METHODS FOR LAZY_SEGMENT_LIST
# ==============================================================================

#' Print method for lazy_segment_list
#' @param x lazy_segment_list object
#' @param ... Additional arguments (unused)
#' @param preview Show a preview of the queued pipeline without materialising
#'   it. Default `TRUE`; set to `FALSE` to print only the plan summary.
#' @name print.lazy_segment_list
.print_lazy_segment_list <- function(x, ..., preview = TRUE) {
  cli::cli_rule(
    left = cli::style_bold("lazy_segment_list"),
    right = if (x@.state$materialized) "{cli::col_green('\u2713 materialized')}" else "{cli::col_silver('\u29d7 lazy')}"
  )

  cli::cli_text("")

  if (x@.state$materialized && !is.null(x@.state$cache)) {
    cli::cli_alert_success("Query executed (cached)")
    cli::cli_text("")
    print(x@.state$cache, ...)
    return(invisible(x))
  }

  cli::cli_alert_info("Query not yet executed")

  # Query structure
  cli::cli_text("")
  cli::cli_text("{.strong Query plan:}")

  if (!is.null(x@query_parts$base)) {
    base_preview <- substr(x@query_parts$base$sql, 1, 60)
    cli::cli_text("  Base: {.code {base_preview}}...")
  }

  if (!is.null(x@query_parts$transforms) && length(x@query_parts$transforms) > 0) {
    cli::cli_text("")
    cli::cli_text("  {.strong Transforms:}")
    for (i in seq_along(x@query_parts$transforms)) {
      t <- x@query_parts$transforms[[i]]
      t_desc <- paste(names(t[-1]), t[-1], sep = "=", collapse = ", ")
      cli::cli_text("    {i}. {.fn {t$type}} ({t_desc})")
    }
  }

  if (!isTRUE(preview)) {
    cli::cli_text("")
    cli::cli_text("{.emph Call {.fn collect} to execute and get results; pass {.code preview = TRUE} for an estimated count.}")
    return(invisible(x))
  }

  # Try to estimate size and show preview using a single connection.
  # Cache the row count on the object's mutable .state so repeated REPL
  # prints don't re-run the (potentially expensive) COUNT(*).
  tryCatch({
    query <- build_sql_from_parts(x@query_parts)
    conn <- DBI::dbConnect(RSQLite::SQLite(), x@db_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    cached_n <- x@.state$preview_count
    n <- if (!is.null(cached_n)) {
      cached_n
    } else {
      count_sql <- paste0("SELECT COUNT(*) as n FROM (", query$sql, ")")
      n_val <- DBI::dbGetQuery(conn, count_sql, params = query$params)$n
      x@.state$preview_count <- n_val
      n_val
    }
    cli::cli_text("")
    cli::cli_text("  Estimated result: {cli::col_blue(n)} row{?s}")

    cli::cli_text("")
    cli::cli_text("{.emph Call {.fn collect} to execute and get results}")

    cli::cli_text("")
    cli::cli_text("{.strong Preview (first 3 rows):}")
    preview_sql <- paste0(query$sql, " LIMIT 3")
    preview_tbl <- tibble::as_tibble(DBI::dbGetQuery(conn, preview_sql, params = query$params))

    if (nrow(preview_tbl) > 0) {
      print(preview_tbl)
    } else {
      cli::cli_alert_warning("Query would return 0 rows")
    }
  }, error = function(e) {
    cli::cli_text("")
    cli::cli_text("{.emph Call {.fn collect} to execute and get results}")
    cli::cli_text("")
    cli::cli_alert_warning("Could not generate preview")
  })

  invisible(x)
}

#' Summary method for lazy_segment_list
#' @param object lazy_segment_list object
#' @param ... Additional arguments (unused)
#' @name summary.lazy_segment_list
.summary_lazy_segment_list <- function(object, ...) {
  if (object@.state$materialized && !is.null(object@.state$cache)) {
    cli::cli_h1("Lazy Segment List (Materialized)")
    summary(object@.state$cache, ...)
  } else {
    cli::cli_h1("Lazy Segment List (Not Materialized)")
    
    cli::cli_dl(c(
      "Database" = object@db_path,
      "UUID" = object@db_uuid,
      "Status" = "Lazy (call collect() to execute)"
    ))
    
    cli::cli_h2("Query Plan")
    
    n_transforms <- length(object@query_parts$transforms %or_empty% list())
    cli::cli_text("Transforms: {.val {n_transforms}}")
    
    if (n_transforms > 0) {
      for (i in seq_along(object@query_parts$transforms)) {
        t <- object@query_parts$transforms[[i]]
        cli::cli_text("  {i}. {.fn {t$type}}")
      }
    }
    
    # Try to estimate result size
    tryCatch({
      query <- build_sql_from_parts(object@query_parts)
      count_sql <- paste0("SELECT COUNT(*) as n FROM (", query$sql, ")")
      conn <- DBI::dbConnect(RSQLite::SQLite(), object@db_path)
      on.exit(DBI::dbDisconnect(conn))
      n <- DBI::dbGetQuery(conn, count_sql, params = query$params)$n
      cli::cli_text("")
      cli::cli_text("Estimated rows: {.val {n}}")
    }, error = function(e) {
      cli::cli_alert_warning("Could not estimate row count")
    })
  }
  
  invisible(object)
}

# Implementation function for lazy_segment_list
glimpse_lazy_segment_list_impl <- function(x, ...) {
  if (x@.state$materialized && !is.null(x@.state$cache)) {
    glimpse(x@.state$cache, ...)
  } else {
    cli::cli_h2("lazy_segment_list {cli::col_silver('[not materialized]')}")
    cli::cli_text("Query: {substr(x@query_parts$base$sql, 1, 80)}...")
    cli::cli_text("Transforms: {length(x@query_parts$transforms %or_empty% list())}")
    cli::cli_text("")
    cli::cli_text("{.emph Call {.fn collect} to execute}")
  }
  
  invisible(x)
}

# Convert lazy_segment_list to data.frame (forces materialization).
# S7 namespaces the class as "reindeer::lazy_segment_list", so the S3
# method as.data.frame.lazy_segment_list never dispatches. We register an
# S7 method on as.data.frame instead.
.as_data_frame_lazy_segment_list <- function(x, ...) {
  seg_list <- collect(x)
  as.data.frame(seg_list)
}

#' Check if Object is Lazy
#'
#' @param x An object
#' @return Logical
#' @keywords internal
#' @noRd
is_lazy <- function(x) {
  S7::S7_inherits(x, lazy_segment_list) && !x@.state$materialized
}

#' Check if Object Needs Materialization
#'
#' @param x An object
#' @return Logical
#' @keywords internal
#' @noRd
needs_collect <- function(x) {
  is_lazy(x)
}

# ---------------------------------------------------------------------------
# Auto-collect S3 methods (registered in .onLoad for the namespaced class
# "reindeer::lazy_segment_list"). Any path that asks for actual data
# (subset, $, [[, dim/nrow, head/tail, as_tibble) materialises the lazy
# pipeline once and delegates to the resulting segment_list. The cache
# lives on @.state, so repeat accesses are cheap.
# ---------------------------------------------------------------------------

.lazy_dim <- function(x) dim(collect(x))
.lazy_length <- function(x) length(collect(x))
.lazy_names <- function(x) names(collect(x))
.lazy_bracket <- function(x, ...) collect(x)[...]
.lazy_double_bracket <- function(x, ...) collect(x)[[...]]
.lazy_dollar <- function(x, name) collect(x)[[name]]
.lazy_head <- function(x, n = 6L, ...) utils::head(collect(x), n = n, ...)
.lazy_tail <- function(x, n = 6L, ...) utils::tail(collect(x), n = n, ...)
.lazy_as_tibble <- function(x, ...) tibble::as_tibble(collect(x), ...)
.lazy_as_data_frame <- function(x, ...) as.data.frame(collect(x), ...)

# dplyr verbs on lazy_segment_list: collect, then delegate. The collected
# segment_list inherits tbl_df so dplyr's default methods apply; the
# dplyr_reconstruct hook in segment_list_dplyr.R preserves class + props.
# dplyr verbs collect-and-delegate: they materialize so the segment_list's own
# dplyr methods record provenance / loss immediately (Aim 5). Deferring them was
# tried and reverted -- it moved loss accounting to collect() time, which broke
# the "filter records a dropped step now" contract; the lazy win was marginal.
.lazy_dplyr_filter    <- function(.data, ...)        dplyr::filter(collect(.data), ...)
.lazy_dplyr_mutate    <- function(.data, ...)        dplyr::mutate(collect(.data), ...)
.lazy_dplyr_select    <- function(.data, ...)        dplyr::select(collect(.data), ...)
.lazy_dplyr_arrange   <- function(.data, ...)        dplyr::arrange(collect(.data), ...)
.lazy_dplyr_slice     <- function(.data, ...)        dplyr::slice(collect(.data), ...)
.lazy_dplyr_rename    <- function(.data, ...)        dplyr::rename(collect(.data), ...)
.lazy_dplyr_distinct  <- function(.data, ..., .keep_all = FALSE) dplyr::distinct(collect(.data), ..., .keep_all = .keep_all)
.lazy_dplyr_transmute <- function(.data, ...)        dplyr::transmute(collect(.data), ...)
.lazy_dplyr_group_by  <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) dplyr::group_by(collect(.data), ..., .add = .add, .drop = .drop)
.lazy_dplyr_ungroup   <- function(x, ...)            dplyr::ungroup(collect(x), ...)
.lazy_dplyr_summarise <- function(.data, ..., .groups = NULL) dplyr::summarise(collect(.data), ..., .groups = .groups)
.lazy_dplyr_count     <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) dplyr::count(collect(x), ..., wt = !!rlang::enquo(wt), sort = sort, name = name)
.lazy_dplyr_tally     <- function(x, wt = NULL, sort = FALSE, name = NULL) dplyr::tally(collect(x), wt = !!rlang::enquo(wt), sort = sort, name = name)
.lazy_dplyr_left_join  <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) dplyr::left_join(collect(x), y, by = by, copy = copy, suffix = suffix, ..., keep = keep)
.lazy_dplyr_right_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) dplyr::right_join(collect(x), y, by = by, copy = copy, suffix = suffix, ..., keep = keep)
.lazy_dplyr_inner_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) dplyr::inner_join(collect(x), y, by = by, copy = copy, suffix = suffix, ..., keep = keep)
.lazy_dplyr_full_join  <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) dplyr::full_join(collect(x), y, by = by, copy = copy, suffix = suffix, ..., keep = keep)
.lazy_dplyr_anti_join  <- function(x, y, by = NULL, copy = FALSE, ...) dplyr::anti_join(collect(x), y, by = by, copy = copy, ...)
.lazy_dplyr_semi_join  <- function(x, y, by = NULL, copy = FALSE, ...) dplyr::semi_join(collect(x), y, by = by, copy = copy, ...)
