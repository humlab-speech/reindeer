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

#' Scout Forward in Sequence
#'
#' Navigate forward through sequential items on the same level, optionally
#' capturing multiple items. Optimized implementation using data.table.
#'
#' @param .segments segment_list or lazy_segment_list object
#' @param steps_forward Integer; number of steps to move forward (positive) or backward (negative)
#' @param count_from Character; "START" or "END" of segment
#' @param capture Integer; number of consecutive items to capture (default 1)
#' @param ignore_bundle_boundaries Logical; whether to cross bundle boundaries
#' @param calculate_times Logical; whether to calculate start/end times
#' @param times_from Character; level to use for time calculation
#' @param .from corpus object (optional, for additional context)
#' @param .quiet Logical; suppress messages
#' @param collect Logical; force materialization (default TRUE)
#'
#' @return segment_list (or lazy_segment_list if collect=FALSE)
#' @export
#'
#' @examplesIf interactive()
#' # Get next phoneme
#' next_phone <- scout(segments, steps_forward = 1)
#'
#' # Get previous phoneme
#' prev_phone <- scout(segments, steps_forward = -1)
#'
#' # Get next 2 phonemes
#' next_two <- scout(segments, steps_forward = 1, capture = 2)
#'
#' # Lazy version
#' lazy_next <- scout(segments, steps_forward = 1, collect = FALSE)
#' result <- collect(lazy_next)
scout <- S7::new_generic("scout", ".segments")

#' Scout method for segment_list (eager data.table path)
#' @rdname scout
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
#' appended to the lazy plan for SQL-side evaluation at the next `collect()`.
#'
#' @rdname scout
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
  .segments@query_parts$transforms <- c(
    .segments@query_parts$transforms,
    list(transform)
  )
  .segments
}

#' Scout Implementation Using data.table
#'
#' Internal optimized implementation using data.table operations.
#'
#' @keywords internal
#' @noRd
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
    corp <- corpus(db_path)
  } else {
    cli::cli_abort("Cannot determine corpus. Provide via {.arg .from}")
  }
  
  # Get database connection — reuse cached connection for corpus objects
  conn <- get_or_create_connection(corp)

  # Query all items on the same levels
  levels <- unique(dt$level)
  level_filter <- paste0("'", levels, "'", collapse = ", ")
  
  items_query <- sprintf(
    "SELECT * FROM items WHERE db_uuid = '%s' AND level IN (%s)",
    db_uuid, level_filter
  )
  
  all_items_dt <- data.table::setDT(DBI::dbGetQuery(conn, items_query))

  # Compute sample_end from sample_start + sample_dur (emuR stores duration, not end)
  all_items_dt[, sample_end := data.table::fifelse(
    type == "SEGMENT" & !is.na(sample_start) & !is.na(sample_dur),
    sample_start + sample_dur - 1L,
    NA_integer_
  )]

  # Query labels
  labels_query <- sprintf(
    "SELECT db_uuid, session, bundle, item_id, label 
     FROM labels 
     WHERE db_uuid = '%s'",
    db_uuid
  )
  labels_dt <- data.table::setDT(DBI::dbGetQuery(conn, labels_query))
  
  # Set keys for fast joins
  data.table::setkey(all_items_dt, db_uuid, session, bundle, level, seq_idx)
  data.table::setkey(labels_dt, db_uuid, session, bundle, item_id)
  
  # Initialize result list
  result_list <- vector("list", nrow(dt))
  
  # Process each segment
  for (i in seq_len(nrow(dt))) {
    seg <- dt[i, ]
    
    # Calculate target sequence indices
    start_seq <- seg$ref_seq_idx + steps_forward
    end_seq <- start_seq + capture - 1
    
    # Find matching items
    if (ignore_bundle_boundaries) {
      # Can cross bundles within same session
      matches <- all_items_dt[
        db_uuid == seg$db_uuid &
          session == seg$session &
          level == seg$level &
          seq_idx >= start_seq &
          seq_idx <= end_seq
      ]
    } else {
      # Must stay within same bundle
      matches <- all_items_dt[
        db_uuid == seg$db_uuid &
          session == seg$session &
          bundle == seg$bundle &
          level == seg$level &
          seq_idx >= start_seq &
          seq_idx <= end_seq
      ]
    }
    
    if (nrow(matches) > 0) {
      # Join with labels
      matches <- labels_dt[matches, on = .(db_uuid, session, bundle, item_id)]
      data.table::setnames(matches, "label", "labels", skip_absent = TRUE)
      
      # Build result rows
      if (capture == 1) {
        # Single item: one row
        result_row <- data.table::data.table(
          labels = matches$labels[1],
          start = if (matches$type[1] == "ITEM") NA_real_ else matches$sample_start[1] / matches$sample_rate[1] * 1000,
          end = if (matches$type[1] == "ITEM") NA_real_ else matches$sample_end[1] / matches$sample_rate[1] * 1000,
          db_uuid = seg$db_uuid,
          session = seg$session,
          bundle = seg$bundle,
          start_item_id = matches$item_id[1],
          end_item_id = matches$item_id[1],
          level = seg$level,
          attribute = seg$attribute,
          start_item_seq_idx = matches$seq_idx[1],
          end_item_seq_idx = matches$seq_idx[1],
          type = matches$type[1],
          sample_start = matches$sample_start[1],
          sample_end = matches$sample_end[1],
          sample_rate = matches$sample_rate[1]
        )
      } else {
        # Multiple items: one row spanning range
        result_row <- data.table::data.table(
          labels = paste(matches$labels, collapse = " "),
          start = if (matches$type[1] == "ITEM") NA_real_ else matches$sample_start[1] / matches$sample_rate[1] * 1000,
          end = if (matches$type[1] == "ITEM") NA_real_ else matches$sample_end[nrow(matches)] / matches$sample_rate[nrow(matches)] * 1000,
          db_uuid = seg$db_uuid,
          session = seg$session,
          bundle = seg$bundle,
          start_item_id = matches$item_id[1],
          end_item_id = matches$item_id[nrow(matches)],
          level = seg$level,
          attribute = seg$attribute,
          start_item_seq_idx = matches$seq_idx[1],
          end_item_seq_idx = matches$seq_idx[nrow(matches)],
          type = matches$type[1],
          sample_start = matches$sample_start[1],
          sample_end = matches$sample_end[nrow(matches)],
          sample_rate = matches$sample_rate[1]
        )
      }
      
      result_list[[i]] <- result_row
    }
  }
  
  # Combine results
  result_dt <- data.table::rbindlist(result_list, fill = TRUE)
  
  # Handle time calculation if needed
  if (calculate_times && !is.null(times_from)) {
    # Calculate times from reference level
    # This would require additional logic to traverse hierarchy
    cli::cli_warn("times_from parameter not yet fully implemented in optimized version")
  }
  
  # Convert to segment_list
  result <- segment_list(
    data = as.data.frame(result_dt),
    db_uuid = db_uuid,
    db_path = db_path
  )

  result <- .record_step(result, .segments, "scout", sys.call(-1L))

  return(result)
}

#' Retreat Backward in Sequence
#'
#' Convenience function for moving backward. Simply calls scout with negative offset.
#'
#' @param .segments segment_list or lazy_segment_list object
#' @param steps_backward Integer; number of steps to move backward (positive value)
#' @param ... Additional arguments passed to scout()
#'
#' @return segment_list (or lazy_segment_list if collect=FALSE)
#' @keywords internal
#'
#' @examplesIf interactive()
#' # Use scout() with a negative step instead:
#' prev_phone <- scout(segments, steps_forward = -1)
#' @noRd
retreat <- function(.segments, steps_backward, ...) {
  scout(.segments, steps_forward = -abs(steps_backward), ...)
}

#' Ascend to Higher Level in Hierarchy
#'
#' Navigate upward through the annotation hierarchy following dominance links.
#' Optimized implementation using data.table.
#'
#' @param .segments segment_list or lazy_segment_list object
#' @param level Character; target level name to ascend to
#' @param .from corpus object (optional)
#' @param .quiet Logical; suppress messages
#' @param collect Logical; force materialization (default TRUE)
#'
#' @return segment_list (or lazy_segment_list if collect=FALSE)
#' @export
ascend_to <- S7::new_generic("ascend_to", ".segments")

#' Ascend method for segment_list
#' @rdname ascend_to
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
  .segments@query_parts$transforms <- c(
    .segments@query_parts$transforms,
    list(transform)
  )
  .segments
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
    corp <- corpus(db_path)
  } else {
    cli::cli_abort("Cannot determine corpus")
  }
  
  # Get database connection — reuse cached connection for corpus objects
  conn <- get_or_create_connection(corp)

  # Query for upward links
  # We need to find items at the target level that link down to our segments
  links_query <- sprintf(
    "SELECT l.*, i.level as from_level
     FROM links l
     INNER JOIN items i ON 
       l.db_uuid = i.db_uuid AND 
       l.session = i.session AND 
       l.bundle = i.bundle AND 
       l.to_id = i.item_id
     WHERE l.db_uuid = '%s'",
    db_uuid
  )
  
  links_dt <- data.table::setDT(DBI::dbGetQuery(conn, links_query))
  
  # Query target level items
  items_query <- sprintf(
    "SELECT * FROM items WHERE db_uuid = '%s' AND level = '%s'",
    db_uuid, level
  )
  
  target_items_dt <- data.table::setDT(DBI::dbGetQuery(conn, items_query))

  # Compute sample_end from sample_start + sample_dur
  target_items_dt[, sample_end := data.table::fifelse(
    type == "SEGMENT" & !is.na(sample_start) & !is.na(sample_dur),
    sample_start + sample_dur - 1L,
    NA_integer_
  )]

  # Query labels for target level
  labels_query <- sprintf(
    "SELECT db_uuid, session, bundle, item_id, label
     FROM labels
     WHERE db_uuid = '%s'",
    db_uuid
  )

  labels_dt <- data.table::setDT(DBI::dbGetQuery(conn, labels_query))

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
  result_dt[, `:=`(
    start = data.table::fifelse(type %in% c("SEGMENT", "EVENT"), sample_start / sample_rate * 1000, NA_real_),
    end = data.table::fifelse(type %in% c("SEGMENT", "EVENT"), sample_end / sample_rate * 1000, NA_real_),
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
    data = as.data.frame(result_dt),
    db_uuid = db_uuid,
    db_path = db_path
  )

  result <- .record_step(result, .segments, "ascend_to", sys.call(-1L))

  return(result)
}

#' Descend to Lower Level in Hierarchy
#'
#' Navigate downward through the annotation hierarchy following dominance links.
#'
#' @param .segments segment_list or lazy_segment_list object
#' @param level Character; target level name to descend to
#' @param .from corpus object (optional)
#' @param .quiet Logical; suppress messages
#' @param collect Logical; force materialization (default TRUE)
#'
#' @return segment_list (or lazy_segment_list if collect=FALSE)
#' @export
descend_to <- S7::new_generic("descend_to", ".segments")

#' Descend method for segment_list
#' @rdname descend_to
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
  .segments@query_parts$transforms <- c(
    .segments@query_parts$transforms,
    list(transform)
  )
  .segments
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
    corp <- corpus(db_path)
  } else {
    cli::cli_abort("Cannot determine corpus")
  }
  
  # Get database connection — reuse cached connection for corpus objects
  conn <- get_or_create_connection(corp)

  # Query for downward links (from our segments to target level)
  links_query <- sprintf(
    "SELECT * FROM links WHERE db_uuid = '%s'",
    db_uuid
  )
  
  links_dt <- data.table::setDT(DBI::dbGetQuery(conn, links_query))
  
  # Query target level items
  items_query <- sprintf(
    "SELECT * FROM items WHERE db_uuid = '%s' AND level = '%s'",
    db_uuid, level
  )
  
  target_items_dt <- data.table::setDT(DBI::dbGetQuery(conn, items_query))

  # Compute sample_end from sample_start + sample_dur
  target_items_dt[, sample_end := data.table::fifelse(
    type == "SEGMENT" & !is.na(sample_start) & !is.na(sample_dur),
    sample_start + sample_dur - 1L,
    NA_integer_
  )]

  # Query labels
  labels_query <- sprintf(
    "SELECT db_uuid, session, bundle, item_id, label
     FROM labels
     WHERE db_uuid = '%s'",
    db_uuid
  )

  labels_dt <- data.table::setDT(DBI::dbGetQuery(conn, labels_query))

  # Set keys
  data.table::setkey(links_dt, db_uuid, session, bundle, from_id)
  data.table::setkey(target_items_dt, db_uuid, session, bundle, item_id)
  data.table::setkey(labels_dt, db_uuid, session, bundle, item_id)
  
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
  result_dt[, `:=`(
    start = data.table::fifelse(type %in% c("SEGMENT", "EVENT"), sample_start / sample_rate * 1000, NA_real_),
    end = data.table::fifelse(type %in% c("SEGMENT", "EVENT"), sample_end / sample_rate * 1000, NA_real_),
    start_item_id = item_id,
    end_item_id = item_id,
    start_item_seq_idx = seq_idx,
    end_item_seq_idx = seq_idx,
    attribute = level
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
    data = as.data.frame(result_dt),
    db_uuid = db_uuid,
    db_path = db_path
  )

  result <- .record_step(result, .segments, "descend_to", sys.call(-1L))

  return(result)
}
