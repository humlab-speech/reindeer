#' Optimized Sequence Operations with data.table
#'
#' This module provides highly optimized implementations of sequential
#' query operations (scout, retreat) using data.table and supporting
#' lazy evaluation through SQL query building.
#'
#' @section Performance:
#' - Uses data.table for fast joins and filtering
#' - Supports lazy evaluation via lazy_segment_list
#' - 2-5x faster than emuR::requery_seq for large datasets
#'
#' @author reindeer package
#' @include segment_list_classes.R reindeer_lazy_segment_list.R
NULL

#' Move forward or backward along an annotation level
#'
#' Given a `segment_list`, returns the item(s) `steps_forward` positions
#' away on the same level - typically the previous or next phone, word,
#' or syllable. Use negative steps to go backward, or `capture > 1` to
#' grab a run of consecutive items.
#'
#' @param .segments A `segment_list` or `lazy_segment_list`.
#' @param steps_forward Integer offset. `1` = next item, `-1` = previous,
#'   `2` = item after next.
#' @param count_from `"START"` (default) or `"END"` - which edge of the
#'   current segment to count from.
#' @param capture Number of consecutive items to return per input
#'   segment. Default `1`.
#' @param ignore_bundle_boundaries If `TRUE`, walk past the end of a
#'   bundle into the next one. Default `FALSE` (recommended).
#' @param calculate_times,times_from Recompute start/end times after the
#'   move; advanced, defaults are usually right.
#' @param .from Optional `corpus` (only needed if the segment list lost
#'   its `db_path`).
#' @param .quiet Suppress informational messages.
#' @param collect Materialise the result (default `TRUE`). With
#'   `FALSE` the operation is deferred into the lazy plan - note that
#'   the lazy SQL path for `scout` / `ascend_to` / `descend_to` does
#'   not yet preserve every derived column (labels, attribute,
#'   start_item_id, ...), so eager evaluation is the safe default
#'   until full SQL parity lands.
#' @usage
#' scout(.segments, steps_forward, count_from = "START", capture = 1,
#'   ignore_bundle_boundaries = FALSE, calculate_times = TRUE,
#'   times_from = NULL, .from = NULL, .quiet = TRUE, collect = TRUE)
#' @return A `segment_list`, or `lazy_segment_list` when `collect = FALSE`.
#' @examplesIf interactive()
#' corp <- corpus("path/to/ae_emuDB")
#' stops <- query(corp, "Phonetic =~ [ptk]")
#' next_phone <- scout(stops, steps_forward = 1)     # following phoneme
#' prev_two   <- scout(stops, steps_forward = -1, capture = 2)
#' @seealso [ascend_to()], [descend_to()]
#' @export
scout <- S7::new_generic("scout", ".segments", fun = function(.segments,
                                                                steps_forward,
                                                                count_from = "START",
                                                                capture = 1,
                                                                ignore_bundle_boundaries = FALSE,
                                                                calculate_times = TRUE,
                                                                times_from = NULL,
                                                                .from = NULL,
                                                                .quiet = TRUE,
                                                                collect = TRUE) {
  S7::S7_dispatch()
})

#' Scout method for segment_list (eager data.table path)
#' @rdname scout
#' @usage NULL
#' @name scout.segment_list
S7::method(scout, segment_list) <- function(.segments,
                                              steps_forward,
                                              count_from = "START",
                                              capture = 1,
                                              ignore_bundle_boundaries = FALSE,
                                              calculate_times = TRUE,
                                              times_from = NULL,
                                              .from = NULL,
                                              .quiet = TRUE,
                                              collect = TRUE) {
  if (missing(steps_forward)) {
    cli::cli_abort("{.arg steps_forward} is required")
  }
  scout_dt(.segments,
           steps_forward = steps_forward,
           count_from = count_from,
           capture = capture,
           ignore_bundle_boundaries = ignore_bundle_boundaries,
           calculate_times = calculate_times,
           times_from = times_from,
           .from = .from,
           .quiet = .quiet)
}

#' Scout method for lazy_segment_list
#'
#' When `collect = TRUE` (default) the lazy plan is materialised first and
#' the segment_list method runs; when `FALSE` a `scout` transform is
#' appended to the lazy plan for SQL-side evaluation at the next
#' `collect()`. The SQL form does not yet preserve every derived column,
#' so eager remains the safe default.
#'
#' @rdname scout
#' @usage NULL
#' @name scout.lazy_segment_list
S7::method(scout, lazy_segment_list) <- function(.segments,
                                                   steps_forward,
                                                   count_from = "START",
                                                   capture = 1,
                                                   ignore_bundle_boundaries = FALSE,
                                                   calculate_times = TRUE,
                                                   times_from = NULL,
                                                   .from = NULL,
                                                   .quiet = TRUE,
                                                   collect = TRUE) {
  if (missing(steps_forward)) {
    cli::cli_abort("{.arg steps_forward} is required")
  }
  if (collect) {
    return(scout(collect(.segments),
                 steps_forward = steps_forward,
                 count_from = count_from,
                 capture = capture,
                 ignore_bundle_boundaries = ignore_bundle_boundaries,
                 calculate_times = calculate_times,
                 times_from = times_from,
                 .from = .from,
                 .quiet = .quiet,
                 collect = TRUE))
  }
  transform <- list(
    type = "scout",
    n = steps_forward,
    count_from = count_from,
    capture = capture,
    ignore_bundle_boundaries = ignore_bundle_boundaries
  )
  .fork_lazy(.segments, "sql", transform)
}

#' Scout Implementation Using data.table
#'
#' Internal optimized implementation using data.table operations.
#'
#' @keywords internal
#' @noRd
#' SQL scope for a navigation read
#'
#' Navigation verbs only ever look at the bundles they were handed, but their
#' queries used to scan every item, link and label in the database. Scoping the
#' read keeps the cost proportional to the query rather than to the corpus.
#'
#' @param dt A data.table with `session` and `bundle` columns.
#' @return List with `sep`, `in_list` (the placeholder list), `sql` (the
#'   composite-key expression for unqualified columns) and `params` (the key
#'   list, appended after the caller's own parameters).
#' @noRd
.scope_to_segments <- function(dt) {
  sep <- "\u001f"
  keys <- unique(paste(dt$session, dt$bundle, sep = sep))
  in_list <- paste(rep("?", length(keys)), collapse = ", ")
  list(
    sep = sep,
    in_list = in_list,
    sql = sprintf("(session || '%s' || bundle) IN (%s)", sep, in_list),
    params = as.list(keys)
  )
}

scout_dt <- function(.segments, 
                     steps_forward,
                     count_from = "START",
                     capture = 1,
                     ignore_bundle_boundaries = FALSE,
                     calculate_times = TRUE,
                     times_from = NULL,
                     .from = NULL,
                     .quiet = TRUE) {
  
  # Convert to data.table
  if (S7::S7_inherits(.segments, reindeer::segment_list)) {
    dt <- data.table::as.data.table(S7::S7_data(.segments))
    db_path <- S7::prop(.segments, "db_path")
    db_uuid <- S7::prop(.segments, "db_uuid")
  } else {
    dt <- data.table::as.data.table(.segments)
    db_path <- if (!is.null(.from)) .from@basePath else ""
    db_uuid <- if (!is.null(.from)) .from@config$UUID else unique(dt$db_uuid)[1]
  }
  
  # Determine reference sequence index
  if (count_from == "END") {
    dt[, ref_seq_idx := end_item_seq_idx]
  } else {
    dt[, ref_seq_idx := start_item_seq_idx]
  }
  
  # Get corpus/database connection for querying all items
  if (!is.null(.from) && S7::S7_inherits(.from, reindeer::corpus)) {
    corp <- .from
  } else if (nzchar(db_path) && dir.exists(db_path)) {
    corp <- get_corpus_cached(.segments, .from)
  } else {
    cli::cli_abort("Cannot determine corpus. Provide via {.arg .from}")
  }
  
  # Get database connection - reuse cached connection for corpus objects
  conn <- get_or_create_connection(corp)

  # Query all items on the same levels
  levels <- unique(dt$level)
  level_placeholders <- paste(rep("?", length(levels)), collapse = ", ")
  
  scope <- .scope_to_segments(dt)
  items_query <- sprintf(
    "SELECT * FROM items WHERE db_uuid = ? AND level IN (%s) AND %s",
    level_placeholders, scope$sql
  )

  all_items_dt <- data.table::setDT(DBI::dbGetQuery(
    conn, items_query,
    params = c(list(db_uuid), as.list(levels), scope$params)
  ))

  all_items_dt <- .nav_fill_item_times(all_items_dt, corp)

  # Labels of the default attribute (the level itself): one per item.
  labels_query <- sprintf("SELECT db_uuid, session, bundle, item_id, label
     FROM labels
     WHERE db_uuid = ? AND name IN (%s) AND %s",
     level_placeholders, scope$sql)
  labels_dt <- data.table::setDT(DBI::dbGetQuery(
    conn, labels_query, params = c(list(db_uuid), as.list(levels), scope$params)))
  labels_dt <- unique(labels_dt, by = c("db_uuid", "session", "bundle", "item_id"))
  data.table::setkey(labels_dt, db_uuid, session, bundle, item_id)

  # Position of every item along its level. With ignore_bundle_boundaries the
  # bundles of a session are laid end to end (alphabetical), so stepping past
  # the last item of one bundle lands on the first of the next.
  if (ignore_bundle_boundaries) {
    data.table::setorder(all_items_dt, db_uuid, session, level, bundle, seq_idx)
    n_per_bundle <- all_items_dt[, .(n = max(seq_idx)), by = .(db_uuid, session, level, bundle)]
    n_per_bundle[, offset := cumsum(data.table::shift(n, fill = 0L)), by = .(db_uuid, session, level)]
    all_items_dt <- n_per_bundle[all_items_dt, on = .(db_uuid, session, level, bundle)]
    all_items_dt[, pos := seq_idx + offset]
    seg_off <- n_per_bundle[, .(db_uuid, session, level, bundle, seg_offset = offset)]
    dt <- seg_off[dt, on = .(db_uuid, session, level, bundle)]
    dt[, ref_pos := ref_seq_idx + seg_offset]
    match_keys <- c("db_uuid", "session", "level", "pos")
  } else {
    all_items_dt[, pos := seq_idx]
    dt[, ref_pos := ref_seq_idx]
    match_keys <- c("db_uuid", "session", "bundle", "level", "pos")
  }
  data.table::setkeyv(all_items_dt, match_keys)

  # One candidate position per (segment, captured step), matched with a single
  # equi-join: no per-segment loop.
  dt[, row_id := .I]
  cand <- dt[rep(seq_len(.N), each = capture)]
  cand[, step := rep(seq_len(capture) - 1L, times = nrow(dt))]
  cand[, pos := ref_pos + steps_forward + step]
  m <- all_items_dt[cand[, c("row_id", "step", match_keys), with = FALSE],
                    on = match_keys, nomatch = NULL]
  m <- labels_dt[m, on = .(db_uuid, session, bundle, item_id)]
  .nav_times(m)
  data.table::setorder(m, row_id, step)

  seg_attr <- dt[, .(row_id, seg_attribute = attribute)]
  result_dt <- m[, .(
    labels = paste(label, collapse = " "),
    start = start[1L], end = end[.N],
    db_uuid = db_uuid[1L], session = session[1L], bundle = bundle[1L],
    start_item_id = item_id[1L], end_item_id = item_id[.N],
    level = level[1L],
    start_item_seq_idx = seq_idx[1L], end_item_seq_idx = seq_idx[.N],
    type = type[1L],
    sample_start = sample_start[1L], sample_end = sample_end[.N],
    sample_rate = sample_rate[1L]
  ), by = row_id]
  result_dt <- seg_attr[result_dt, on = "row_id"]
  data.table::setnames(result_dt, "seg_attribute", "attribute")
  data.table::setorder(result_dt, row_id)
  result_dt[, row_id := NULL]
  data.table::setcolorder(result_dt, c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate"))

  # Handle time calculation if needed
  if (calculate_times && !is.null(times_from)) {
    # Calculate times from reference level
    # This would require additional logic to traverse hierarchy
    cli::cli_warn("times_from parameter not yet fully implemented in optimized version")
  }
  
  # Convert to segment_list
  result <- segment_list(
    data = as.data.frame(if (ncol(result_dt) == 0L) dt[0L] else result_dt),
    db_uuid = db_uuid,
    db_path = db_path
  )

  result <- .record_step(result, .segments, "scout", sys.call(-1L))

  return(result)
}


#' Move up the annotation hierarchy
#'
#' Returns the parent item(s) at `level` for every segment in the input.
#' Follows the dominance links recorded in the corpus, so e.g. ascending
#' from a phone to `"Word"` gives the containing word, and from a
#' phone to `"Syllable"` gives the containing syllable.
#'
#' @param .segments A `segment_list` or `lazy_segment_list`.
#' @param level Name of the target level.
#' @param .from Optional `corpus` (only needed if the segment list lost
#'   its `db_path`).
#' @param .quiet Suppress informational messages.
#' @param collect Materialise (default `TRUE`); pass `FALSE` to defer
#'   into the lazy plan (see note in [scout()] about partial SQL
#'   coverage).
#' @usage ascend_to(.segments, level, .from = NULL, .quiet = TRUE, collect = TRUE)
#' @return A `segment_list` (or `lazy_segment_list` when `collect = FALSE`).
#' @examplesIf interactive()
#' corp <- corpus("path/to/ae_emuDB")
#' vowels <- query(corp, "Phonetic =~ [aeiou]")
#' words  <- ascend_to(vowels, "Word")
#' @seealso [descend_to()], [scout()]
#' @export
ascend_to <- S7::new_generic("ascend_to", ".segments", fun = function(.segments,
                                                                        level,
                                                                        .from = NULL,
                                                                        .quiet = TRUE,
                                                                        collect = TRUE) {
  S7::S7_dispatch()
})

#' Ascend method for segment_list
#' @rdname ascend_to
#' @usage NULL
#' @name ascend_to.segment_list
S7::method(ascend_to, segment_list) <- function(.segments, level,
                                                  .from = NULL,
                                                  .quiet = TRUE,
                                                  collect = TRUE) {
  if (missing(level)) {
    cli::cli_abort("{.arg level} is required")
  }
  ascend_dt(.segments, level = level, .from = .from, .quiet = .quiet)
}

#' Ascend method for lazy_segment_list
#' @rdname ascend_to
#' @usage NULL
#' @name ascend_to.lazy_segment_list
S7::method(ascend_to, lazy_segment_list) <- function(.segments, level,
                                                       .from = NULL,
                                                       .quiet = TRUE,
                                                       collect = TRUE) {
  if (missing(level)) {
    cli::cli_abort("{.arg level} is required")
  }
  if (collect) {
    return(ascend_to(collect(.segments), level = level,
                     .from = .from, .quiet = .quiet, collect = TRUE))
  }
  transform <- list(type = "ascend", level = level)
  .fork_lazy(.segments, "sql", transform)
}

#' Fill sample_end and ms times for navigation results
#'
#' SEGMENTs (and ITEMs whose span was deduced from time-bearing descendants)
#' use emuR's half-sample convention; EVENTs sit at their sample point and have
#' end 0. Items with no time information keep NA.
#' @noRd
.nav_times <- function(dt) {
  dt[, sample_end := data.table::fifelse(
    !is.na(sample_start) & !is.na(sample_dur), sample_start + sample_dur, NA_integer_)]
  dt[, `:=`(
    start = data.table::fifelse(
      type == "EVENT", sample_point / sample_rate * 1000,
      data.table::fifelse(!is.na(sample_start), (sample_start - 0.5) / sample_rate * 1000, NA_real_)),
    end = data.table::fifelse(
      type == "EVENT", 0,
      data.table::fifelse(!is.na(sample_end), (sample_end + 0.5) / sample_rate * 1000, NA_real_))
  )]
  invisible(dt)
}

#' Give ITEM rows the span of their time-bearing descendants
#' @noRd
.nav_fill_item_times <- function(items_dt, corp) {
  if (nrow(items_dt) == 0L || !any(items_dt$type == "ITEM" & is.na(items_dt$sample_start))) {
    return(items_dt)
  }
  sqlite <- file.path(corp@basePath, paste0(corp@dbName, database.cache.suffix))
  data.table::as.data.table(deduce_item_times(as.data.frame(items_dt), sqlite))
}


#' Transitive parent->child links between two levels
#'
#' Same shape as the `links` table (`from_id` = ancestor, `to_id` = descendant),
#' but spanning every level between `ancestor_level` and `descendant_level`
#' (e.g. Syllable -> Phoneme -> Phonetic), restricted to the bundles in `dt`.
#' Built from the DBconfig hierarchy by the same join chain the EQL position
#' and count functions use.
#' @noRd
.hier_links <- function(conn, db_uuid, ancestor_level, descendant_level, hier, dt) {
  from <- .hier_from_clause(ancestor_level, descendant_level, hier)
  pairs <- data.table::setDT(DBI::dbGetQuery(conn, paste0(
    "SELECT DISTINCT p.db_uuid AS db_uuid, p.session AS session, p.bundle AS bundle,
            p.item_id AS from_id, c.item_id AS to_id ", from,
    " WHERE p.db_uuid = ? AND p.level = ? AND c.level = ?"),
    params = list(db_uuid, ancestor_level, descendant_level)))
  keep <- unique(dt[, .(session, bundle)])
  pairs[keep, on = .(session, bundle), nomatch = NULL]
}

#' Ascend Implementation Using data.table
#'
#' @keywords internal
#' @noRd
ascend_dt <- function(.segments, level, .from = NULL, .quiet = TRUE) {
  
  # Convert to data.table
  if (S7::S7_inherits(.segments, reindeer::segment_list)) {
    dt <- data.table::as.data.table(S7::S7_data(.segments))
    db_path <- S7::prop(.segments, "db_path")
    db_uuid <- S7::prop(.segments, "db_uuid")
  } else {
    dt <- data.table::as.data.table(.segments)
    db_path <- if (!is.null(.from)) .from@basePath else ""
    db_uuid <- if (!is.null(.from)) .from@config$UUID else unique(dt$db_uuid)[1]
  }
  
  # Get corpus
  if (!is.null(.from) && S7::S7_inherits(.from, reindeer::corpus)) {
    corp <- .from
  } else if (nzchar(db_path) && dir.exists(db_path)) {
    corp <- get_corpus_cached(.segments, .from)
  } else {
    cli::cli_abort("Cannot determine corpus")
  }
  
  # Get database connection - reuse cached connection for corpus objects
  conn <- get_or_create_connection(corp)

  # Links from the target level down to each input level, however many
  # hierarchy levels lie between them.
  scope <- .scope_to_segments(dt)
  hier <- get_hierarchy_info(corp@basePath)
  links_dt <- data.table::rbindlist(lapply(
    unique(dt$level), function(lv) .hier_links(conn, db_uuid, level, lv, hier, dt)))

  # Query target level items
  items_query <- sprintf(
    "SELECT * FROM items WHERE db_uuid = ? AND level = ? AND %s", scope$sql)

  target_items_dt <- data.table::setDT(DBI::dbGetQuery(
    conn, items_query, params = c(list(db_uuid, level), scope$params)
  ))

  target_items_dt <- .nav_fill_item_times(target_items_dt, corp)

  # Query labels for target level
  # The default attribute of a level is the level itself: one label per item.
  labels_query <- "SELECT db_uuid, session, bundle, item_id, label
     FROM labels
     WHERE db_uuid = ? AND name = ?"

  labels_dt <- data.table::setDT(DBI::dbGetQuery(conn, labels_query, params = list(db_uuid, level)))

  # Set keys
  data.table::setkey(links_dt, db_uuid, session, bundle, to_id)
  data.table::setkey(target_items_dt, db_uuid, session, bundle, item_id)
  data.table::setkey(labels_dt, db_uuid, session, bundle, item_id)

  # For each segment, find parent at target level
  dt[, seg_idx := .I]

  # Join segments with links (segments are "to" side, parents are "from" side)
  seg_links <- links_dt[dt,
                        on = .(db_uuid, session, bundle, to_id = start_item_id),
                        nomatch = NULL]

  # Join with target level items
  result_dt <- target_items_dt[seg_links,
                               on = .(db_uuid, session, bundle, item_id = from_id),
                               nomatch = NULL]

  # Join with labels
  result_dt <- labels_dt[result_dt,
                         on = .(db_uuid, session, bundle, item_id),
                         nomatch = NULL]

  data.table::setnames(result_dt, "label", "labels", skip_absent = TRUE)

  # Calculate times if available
  .nav_times(result_dt)
  result_dt[, `:=`(
    start_item_id = item_id,
    end_item_id = item_id,
    start_item_seq_idx = seq_idx,
    end_item_seq_idx = seq_idx,
    attribute = level  # Default attribute is level name
  )]

  # Select required columns
  result_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate"
  )

  result_dt <- result_dt[, ..result_cols]

  # Convert to segment_list
  result <- segment_list(
    data = as.data.frame(if (ncol(result_dt) == 0L) dt[0L] else result_dt),
    db_uuid = db_uuid,
    db_path = db_path
  )

  result <- .record_step(result, .segments, "ascend_to", sys.call(-1L))

  return(result)
}

#' Move down the annotation hierarchy
#'
#' Returns the child item(s) at `level` for every segment in the input.
#' Inverse of [ascend_to()]: descending from a word to `"Phonetic"`
#' gives every phone in that word.
#'
#' @param .segments A `segment_list` or `lazy_segment_list`.
#' @param level Name of the target level.
#' @param .from Optional `corpus`.
#' @param .quiet Suppress messages.
#' @param collect Materialise (default `TRUE`); pass `FALSE` to defer
#'   into the lazy plan (see note in [scout()] about partial SQL
#'   coverage).
#' @usage descend_to(.segments, level, .from = NULL, .quiet = TRUE, collect = TRUE)
#' @return A `segment_list` (or `lazy_segment_list` when `collect = FALSE`).
#' @examplesIf interactive()
#' words  <- query(corp, "Word =~ .*")
#' phones <- descend_to(words, "Phonetic")
#' @seealso [ascend_to()], [scout()]
#' @export
descend_to <- S7::new_generic("descend_to", ".segments", fun = function(.segments,
                                                                          level,
                                                                          .from = NULL,
                                                                          .quiet = TRUE,
                                                                          collect = TRUE) {
  S7::S7_dispatch()
})

#' Descend method for segment_list
#' @rdname descend_to
#' @usage NULL
#' @name descend_to.segment_list
S7::method(descend_to, segment_list) <- function(.segments, level,
                                                   .from = NULL,
                                                   .quiet = TRUE,
                                                   collect = TRUE) {
  if (missing(level)) {
    cli::cli_abort("{.arg level} is required")
  }
  descend_dt(.segments, level = level, .from = .from, .quiet = .quiet)
}

#' Descend method for lazy_segment_list
#' @rdname descend_to
#' @usage NULL
#' @name descend_to.lazy_segment_list
S7::method(descend_to, lazy_segment_list) <- function(.segments, level,
                                                        .from = NULL,
                                                        .quiet = TRUE,
                                                        collect = TRUE) {
  if (missing(level)) {
    cli::cli_abort("{.arg level} is required")
  }
  if (collect) {
    return(descend_to(collect(.segments), level = level,
                      .from = .from, .quiet = .quiet, collect = TRUE))
  }
  transform <- list(type = "descend", level = level)
  .fork_lazy(.segments, "sql", transform)
}

#' Descend Implementation Using data.table
#'
#' @keywords internal
#' @noRd
descend_dt <- function(.segments, level, .from = NULL, .quiet = TRUE) {
  
  # Convert to data.table
  if (S7::S7_inherits(.segments, reindeer::segment_list)) {
    dt <- data.table::as.data.table(S7::S7_data(.segments))
    db_path <- S7::prop(.segments, "db_path")
    db_uuid <- S7::prop(.segments, "db_uuid")
  } else {
    dt <- data.table::as.data.table(.segments)
    db_path <- if (!is.null(.from)) .from@basePath else ""
    db_uuid <- if (!is.null(.from)) .from@config$UUID else unique(dt$db_uuid)[1]
  }
  
  # Get corpus
  if (!is.null(.from) && S7::S7_inherits(.from, reindeer::corpus)) {
    corp <- .from
  } else if (nzchar(db_path) && dir.exists(db_path)) {
    corp <- get_corpus_cached(.segments, .from)
  } else {
    cli::cli_abort("Cannot determine corpus")
  }
  
  # Get database connection - reuse cached connection for corpus objects
  conn <- get_or_create_connection(corp)

  # Links from each input level down to the target level, across every
  # intermediate level.
  scope <- .scope_to_segments(dt)
  hier <- get_hierarchy_info(corp@basePath)
  links_dt <- data.table::rbindlist(lapply(
    unique(dt$level), function(lv) .hier_links(conn, db_uuid, lv, level, hier, dt)))

  # Query target level items
  items_query <- sprintf(
    "SELECT * FROM items WHERE db_uuid = ? AND level = ? AND %s", scope$sql)

  target_items_dt <- data.table::setDT(DBI::dbGetQuery(
    conn, items_query, params = c(list(db_uuid, level), scope$params)
  ))

  target_items_dt <- .nav_fill_item_times(target_items_dt, corp)

  # Query labels
  labels_query <- sprintf("SELECT db_uuid, session, bundle, item_id, label
     FROM labels
     WHERE db_uuid = ? AND name = ? AND %s", scope$sql)

  labels_dt <- data.table::setDT(DBI::dbGetQuery(
    conn, labels_query, params = c(list(db_uuid, level), scope$params)))

  # Set keys
  data.table::setkey(links_dt, db_uuid, session, bundle, from_id)
  data.table::setkey(target_items_dt, db_uuid, session, bundle, item_id)
  data.table::setkey(labels_dt, db_uuid, session, bundle, item_id)
  
  dt[, seg_idx := .I]

  # Join segments with links (segments are "from" side, children are "to" side)
  seg_links <- links_dt[dt,
                        on = .(db_uuid, session, bundle, from_id = start_item_id),
                        nomatch = NULL]
  
  # Join with target level items
  result_dt <- target_items_dt[seg_links,
                               on = .(db_uuid, session, bundle, item_id = to_id),
                               nomatch = NULL]
  
  # Join with labels
  result_dt <- labels_dt[result_dt,
                         on = .(db_uuid, session, bundle, item_id),
                         nomatch = NULL]
  
  data.table::setnames(result_dt, "label", "labels", skip_absent = TRUE)
  
  # Calculate times
  .nav_times(result_dt)
  result_dt[, `:=`(
    start_item_id = item_id,
    end_item_id = item_id,
    start_item_seq_idx = seq_idx,
    end_item_seq_idx = seq_idx,
    attribute = level
  )]
  
  # emuR semantics: ONE row per input segment, spanning its first to its last
  # child (labels joined with "->").
  result_dt <- unique(result_dt, by = c("seg_idx", "item_id"))
  data.table::setorder(result_dt, seg_idx, seq_idx)
  result_dt <- result_dt[, .(
    labels = paste(labels, collapse = "->"),
    start = start[1L], end = end[.N],
    db_uuid = db_uuid[1L], session = session[1L], bundle = bundle[1L],
    start_item_id = start_item_id[1L], end_item_id = end_item_id[.N],
    level = level[1L], attribute = attribute[1L],
    start_item_seq_idx = start_item_seq_idx[1L], end_item_seq_idx = end_item_seq_idx[.N],
    type = type[1L],
    sample_start = sample_start[1L], sample_end = sample_end[.N],
    sample_rate = sample_rate[1L]
  ), by = seg_idx]

  # Select required columns
  result_cols <- c(
    "labels", "start", "end", "db_uuid", "session", "bundle",
    "start_item_id", "end_item_id", "level", "attribute",
    "start_item_seq_idx", "end_item_seq_idx", "type",
    "sample_start", "sample_end", "sample_rate"
  )
  
  result_dt <- result_dt[, ..result_cols]

  # Convert to segment_list
  result <- segment_list(
    data = as.data.frame(if (ncol(result_dt) == 0L) dt[0L] else result_dt),
    db_uuid = db_uuid,
    db_path = db_path
  )

  result <- .record_step(result, .segments, "descend_to", sys.call(-1L))

  return(result)
}
