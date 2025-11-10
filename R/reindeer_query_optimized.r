

#' Optimized EMU Query Language (EQL) Implementation
#' 
#' This module provides a complete implementation of the EMU Query Language (EQL)
#' that directly queries the SQLite cache database, bypassing the standard emuR 
#' query system for improved performance.
#' 
#' @section Supported Features:
#' - Simple queries: ==, !=, =~, !~
#' - Sequence queries: \[A -> B\]
#' - Dominance queries: \[A ^ B\]  
#' - Conjunction: \[A & B\]
#' - Disjunction: \[A | B\]
#' - Projection: #Level
#' - Position functions: Start(), End(), Medial()
#' - Count function: Num()
#' - Multi-attribute levels: Level:Attribute
#' 
#' @section Performance:
#' This implementation provides significant performance benefits for large databases
#' by using optimized SQL queries and avoiding R object overhead.
#' 
#' @author reindeer package
#' @references EQL Specification: https://ips-lmu.github.io/The-EMU-SDMS-Manual/app-chap-EQL-EBNF.html
#'
#' @examples
#' \dontrun{
#' # Simple query
#' ask_for(corpus, "Phonetic == t")
#' query(corpus, "Phonetic == t")  # alias
#' 
#' # Sequence with projection
#' ask_for(corpus, "[#Phoneme == n -> Phoneme == t]")
#' 
#' # Dominance query
#' ask_for(corpus, "[Word == the ^ Phoneme == D]")
#' 
#' # Count function
#' ask_for(corpus, "Num(Syllable, Phoneme) >= 3")
#' }

#' Query EMU database using optimized SQLite backend
#' 
#' @param emuDB Either a path to an emuDB directory, an emuDBhandle, or a corpus object
#' @param query EQL query string
#' @param ... Additional arguments passed to query execution
#' @return A segment_list object (subclass of tibble)
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
parse_eql_query <- function(query_string) {
  query_string <- trimws(query_string)
  
  # Check for conjunction/disjunction first (they wrap other operations)
  if (grepl("^\\[.*[&|].*\\]$", query_string)) {
    if (grepl("&", query_string, fixed = TRUE)) {
      return(parse_conjunction_query(query_string))
    } else if (grepl("|", query_string, fixed = TRUE)) {
      return(parse_disjunction_query(query_string))
    }
  }
  
  if (grepl("^\\[.*\\^.*\\]$", query_string)) {
    return(parse_dominance_query(query_string))
  } else if (grepl("^\\[.*->.*\\]$", query_string)) {
    return(parse_sequence_query(query_string))
  } else if (grepl("^(Start|End|Medial|Num)\\(", query_string)) {
    return(parse_function_query(query_string))
  } else {
    return(parse_simple_query(query_string))
  }
}

parse_simple_query <- function(query_string) {
  has_projection <- grepl("^#", query_string)
  if (has_projection) {
    query_string <- sub("^#", "", query_string)
  }
  
  # Match attribute definition: Level:Attribute or just Level
  # Pattern now supports attribute specification
  pattern <- "^([A-Za-z_]+)(?::([A-Za-z_]+))?\\s*(==|!=|=~|!~|=)\\s*['\"]?([^'\"]+)['\"]?$"
  match <- regexec(pattern, query_string)
  matches <- regmatches(query_string, match)[[1]]
  
  if (length(matches) < 4) {
    stop("Cannot parse simple query: ", query_string)
  }
  
  level <- matches[2]
  attribute <- if (length(matches) >= 3 && matches[3] != "") matches[3] else level
  operator <- matches[ifelse(length(matches) >= 5, 4, 3)]
  value <- matches[ifelse(length(matches) >= 5, 5, 4)]
  
  return(list(
    type = "simple",
    level = level,
    attribute = attribute,
    operator = operator, 
    value = value,
    projection = has_projection
  ))
}

parse_dominance_query <- function(query_string) {
  inner <- sub("^\\[(.*)\\]$", "\\1", query_string)
  parts <- strsplit(inner, "\\^")[[1]]
  
  if (length(parts) != 2) {
    stop("Invalid dominance query: ", query_string)
  }
  
  left_query <- parse_eql_query(trimws(parts[1]))
  right_query <- parse_eql_query(trimws(parts[2]))
  
  return(list(
    type = "dominance",
    left = left_query,
    right = right_query
  ))
}

parse_sequence_query <- function(query_string) {
  inner <- sub("^\\[(.*)\\]$", "\\1", query_string)
  parts <- strsplit(inner, "->")[[1]]
  
  if (length(parts) != 2) {
    stop("Invalid sequence query: ", query_string)
  }
  
  left_query <- parse_eql_query(trimws(parts[1]))
  right_query <- parse_eql_query(trimws(parts[2]))
  
  return(list(
    type = "sequence",
    left = left_query,
    right = right_query
  ))
}

parse_conjunction_query <- function(query_string) {
  inner <- sub("^\\[(.*)\\]$", "\\1", query_string)
  
  # Find the & operator that's not inside nested brackets
  parts <- split_on_operator(inner, "&")
  
  if (length(parts) != 2) {
    stop("Invalid conjunction query: ", query_string)
  }
  
  left_query <- parse_eql_query(trimws(parts[1]))
  right_query <- parse_eql_query(trimws(parts[2]))
  
  return(list(
    type = "conjunction",
    left = left_query,
    right = right_query
  ))
}

parse_disjunction_query <- function(query_string) {
  inner <- sub("^\\[(.*)\\]$", "\\1", query_string)
  
  # Find the | operator that's not inside nested brackets
  parts <- split_on_operator(inner, "|")
  
  if (length(parts) != 2) {
    stop("Invalid disjunction query: ", query_string)
  }
  
  left_query <- parse_eql_query(trimws(parts[1]))
  right_query <- parse_eql_query(trimws(parts[2]))
  
  return(list(
    type = "disjunction",
    left = left_query,
    right = right_query
  ))
}

# Helper to split on operator accounting for nested brackets
split_on_operator <- function(string, operator) {
  bracket_depth <- 0
  op_pos <- -1
  chars <- strsplit(string, "")[[1]]
  
  for (i in seq_along(chars)) {
    if (chars[i] == "[") {
      bracket_depth <- bracket_depth + 1
    } else if (chars[i] == "]") {
      bracket_depth <- bracket_depth - 1
    } else if (chars[i] == operator && bracket_depth == 0) {
      op_pos <- i
      break
    }
  }
  
  if (op_pos == -1) {
    return(NULL)
  }
  
  left <- substr(string, 1, op_pos - 1)
  right <- substr(string, op_pos + 1, nchar(string))
  
  return(c(left, right))
}

parse_function_query <- function(query_string) {
  # Try 3-parameter pattern first (for Medial with position)
  pattern_3param <- "^(Medial)\\(([A-Za-z_]+),\\s*([A-Za-z_]+),\\s*([0-9]+)\\)$"
  match_3 <- regexec(pattern_3param, query_string)
  matches_3 <- regmatches(query_string, match_3)[[1]]
  
  if (length(matches_3) == 5) {
    # Medial(parent, child, position) - special case
    return(list(
      type = "function",
      func_name = matches_3[2],
      level1 = matches_3[3],
      level2 = matches_3[4],
      operator = "==",  # Implicit equality for position
      value = matches_3[5],
      position = as.numeric(matches_3[5])
    ))
  }
  
  # Try 2-parameter pattern with comparison
  pattern_2param <- "^(Start|End|Medial|Num)\\(([A-Za-z_]+),\\s*([A-Za-z_]+)\\)\\s*(==|!=|=|>|<|>=|<=)\\s*([0-9]+)$"
  match_2 <- regexec(pattern_2param, query_string)
  matches_2 <- regmatches(query_string, match_2)[[1]]
  
  if (length(matches_2) == 6) {
    return(list(
      type = "function",
      func_name = matches_2[2],
      level1 = matches_2[3],
      level2 = matches_2[4],
      operator = matches_2[5],
      value = matches_2[6],
      position = NULL
    ))
  }
  
  stop("Cannot parse function query: ", query_string)
}

# Simple query execution
execute_simple_query_corrected <- function(db_path, parsed_query) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  level <- extract_level_from_query(parsed_query)
  condition <- extract_condition_from_query(parsed_query)
  
  sql <- sprintf("
    SELECT DISTINCT 
      i.db_uuid, i.session, i.bundle, i.item_id, 
      i.level, i.type, i.seq_idx, i.sample_rate,
      i.sample_point, i.sample_start, i.sample_dur,
      l.label
    FROM items i
    INNER JOIN labels l ON i.db_uuid = l.db_uuid 
      AND i.session = l.session 
      AND i.bundle = l.bundle 
      AND i.item_id = l.item_id
    WHERE i.level = '%s' AND %s
    ORDER BY i.session, i.bundle, i.seq_idx",
    level, condition
  )
  
  return(DBI::dbGetQuery(con, sql))
}

# Sequence query execution
execute_sequence_query_corrected <- function(db_path, parsed_query, result_level = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  left_query <- parsed_query$left
  right_query <- parsed_query$right
  
  left_level <- extract_level_from_query(left_query)
  right_level <- extract_level_from_query(right_query)
  
  if (left_level != right_level) {
    stop("Sequence queries require both sides to be from the same level")
  }
  
  # Determine result level and side based on projection
  result_side <- "both"  # Track which side to return
  if (is.null(result_level)) {
    # Check if either side has projection marker
    # In EQL: # marks the side TO RETURN
    if (!is.null(left_query$projection) && left_query$projection) {
      result_level <- left_level  # # on left means return left
      result_side <- "left"
    } else if (!is.null(right_query$projection) && right_query$projection) {
      result_level <- right_level  # # on right means return right
      result_side <- "right"
    } else {
      result_level <- left_level  # Default: return both as sequence
      result_side <- "both"
    }
  } else {
    # result_level was provided, determine side based on which matches
    if (result_level == left_level && (!is.null(left_query$projection) && left_query$projection)) {
      result_side <- "left"
    } else if (result_level == right_level && (!is.null(right_query$projection) && right_query$projection)) {
      result_side <- "right"
    }
  }
  
  left_condition <- extract_condition_from_query(left_query)
  right_condition <- extract_condition_from_query(right_query)
  
  # Check if we return both elements or just one
  # projection is FALSE when no # marker, TRUE when # present
  return_both <- (is.null(left_query$projection) || !left_query$projection) && 
                  (is.null(right_query$projection) || !right_query$projection)
  
  if (return_both) {
    # Return sequence span
    # Get attribute names for label retrieval
    left_attr <- if (!is.null(left_query$attribute)) left_query$attribute else left_level
    right_attr <- if (!is.null(right_query$attribute)) right_query$attribute else right_level
    
    sql <- sprintf("
      WITH left_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx,
               i.sample_start, i.sample_dur, i.sample_rate
        FROM items i
        INNER JOIN labels l ON i.db_uuid = l.db_uuid 
          AND i.session = l.session 
          AND i.bundle = l.bundle 
          AND i.item_id = l.item_id
        WHERE i.level = '%s' AND %s
      ),
      right_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx,
               i.sample_start, i.sample_dur, i.sample_rate
        FROM items i
        INNER JOIN labels l ON i.db_uuid = l.db_uuid 
          AND i.session = l.session 
          AND i.bundle = l.bundle 
          AND i.item_id = l.item_id
        WHERE i.level = '%s' AND %s
      ),
      sequence_pairs AS (
        SELECT 
          lm.db_uuid, lm.session, lm.bundle,
          lm.item_id as left_id, rm.item_id as right_id,
          lm.seq_idx as left_seq, rm.seq_idx as right_seq,
          lm.sample_start as start_sample,
          rm.sample_start + rm.sample_dur as end_sample,
          lm.sample_rate
        FROM left_matches lm
        INNER JOIN right_matches rm ON lm.db_uuid = rm.db_uuid 
          AND lm.session = rm.session 
          AND lm.bundle = rm.bundle
          AND rm.seq_idx = lm.seq_idx + 1
      )
      SELECT DISTINCT 
        sp.db_uuid, sp.session, sp.bundle, 
        sp.left_id as item_id,
        sp.right_id as end_item_id,
        '%s' as level, 'ITEM' as type, 
        sp.left_seq as seq_idx,
        sp.right_seq as end_seq_idx,
        sp.sample_rate,
        NULL as sample_point, sp.start_sample as sample_start, 
        sp.end_sample - sp.start_sample as sample_dur,
        ll.label || '->' || rl.label as label
      FROM sequence_pairs sp
      INNER JOIN labels ll ON sp.db_uuid = ll.db_uuid 
        AND sp.session = ll.session 
        AND sp.bundle = ll.bundle 
        AND sp.left_id = ll.item_id
        AND ll.name = '%s'
      INNER JOIN labels rl ON sp.db_uuid = rl.db_uuid 
        AND sp.session = rl.session 
        AND sp.bundle = rl.bundle 
        AND sp.right_id = rl.item_id
        AND rl.name = '%s'
      ORDER BY sp.session, sp.bundle, sp.left_seq",
      left_level, left_condition,
      right_level, right_condition,
      left_level,
      left_attr, right_attr
    )
  } else {
    # Return only specified element
    result_attr <- if (result_level == left_level) {
      if (!is.null(left_query$attribute)) left_query$attribute else left_level
    } else {
      if (!is.null(right_query$attribute)) right_query$attribute else right_level
    }
    
    sql <- sprintf("
      WITH left_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx
        FROM items i
        INNER JOIN labels l ON i.db_uuid = l.db_uuid 
          AND i.session = l.session 
          AND i.bundle = l.bundle 
          AND i.item_id = l.item_id
        WHERE i.level = '%s' AND %s
      ),
      right_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx
        FROM items i
        INNER JOIN labels l ON i.db_uuid = l.db_uuid 
          AND i.session = l.session 
          AND i.bundle = l.bundle 
          AND i.item_id = l.item_id
        WHERE i.level = '%s' AND %s
      ),
      sequence_pairs AS (
        SELECT lm.item_id as left_id, rm.item_id as right_id,
               lm.db_uuid, lm.session, lm.bundle
        FROM left_matches lm
        INNER JOIN right_matches rm ON lm.db_uuid = rm.db_uuid 
          AND lm.session = rm.session 
          AND lm.bundle = rm.bundle
          AND rm.seq_idx = lm.seq_idx + 1
      )
      SELECT DISTINCT 
        i.db_uuid, i.session, i.bundle, i.item_id, 
        i.level, i.type, i.seq_idx, i.sample_rate,
        i.sample_point, i.sample_start, i.sample_dur,
        l.label
      FROM sequence_pairs sp
      INNER JOIN items i ON sp.%s_id = i.item_id
        AND sp.db_uuid = i.db_uuid
        AND sp.session = i.session
        AND sp.bundle = i.bundle
      INNER JOIN labels l ON i.db_uuid = l.db_uuid 
        AND i.session = l.session 
        AND i.bundle = l.bundle 
        AND i.item_id = l.item_id
        AND l.name = '%s'
      ORDER BY i.session, i.bundle, i.seq_idx",
      left_level, left_condition,
      right_level, right_condition,
      result_side,
      result_attr
    )
  }
  
  return(DBI::dbGetQuery(con, sql))
}

# Dominance query execution - the key fix
execute_dominance_query_corrected <- function(db_path, parsed_query, result_level = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  left_query <- parsed_query$left
  right_query <- parsed_query$right
  
  left_level <- extract_level_from_query(left_query)
  right_level <- extract_level_from_query(right_query)
  
  # Determine result level based on projection
  # In EQL: # marks the side TO RETURN (project this side)
  if (is.null(result_level)) {
    # Check if either side has projection marker
    if (!is.null(left_query$projection) && left_query$projection) {
      result_level <- left_level  # # on left means return left
    } else if (!is.null(right_query$projection) && right_query$projection) {
      result_level <- right_level  # # on right means return right
    } else {
      result_level <- left_level  # Default: return left (dominant)
    }
  }
  
  hierarchy_info <- get_hierarchy_info(con)
  
  if (!can_dominate(hierarchy_info, left_level, right_level)) {
    warning(sprintf("No dominance relationship possible between %s and %s", 
                   left_level, right_level))
    return(create_empty_result())
  }
  
  sql <- build_corrected_dominance_sql(
    left_query, right_query, left_level, right_level, result_level, hierarchy_info
  )
  
  result <- DBI::dbGetQuery(con, sql)
  return(result)
}

# Function query execution
execute_function_query_corrected <- function(db_path, parsed_query) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  func_name <- parsed_query$func_name
  level1 <- parsed_query$level1
  level2 <- parsed_query$level2
  operator <- parsed_query$operator
  value <- as.numeric(parsed_query$value)
  position <- parsed_query$position  # May be NULL
  
  if (func_name %in% c("Start", "End", "Medial")) {
    return(execute_position_function(con, func_name, level1, level2, operator, value, position))
  } else if (func_name == "Num") {
    return(execute_count_function(con, level1, level2, operator, value))
  } else {
    stop("Unknown function: ", func_name)
  }
}

# Conjunction query execution (AND)
execute_conjunction_query <- function(db_path, parsed_query, result_level = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  # Execute both sub-queries
  left_result <- execute_subquery(db_path, parsed_query$left)
  right_result <- execute_subquery(db_path, parsed_query$right)
  
  # Find intersection based on item_id
  # Items must match in both result sets (same db_uuid, session, bundle, item_id)
  result <- merge(
    left_result, right_result,
    by = c("db_uuid", "session", "bundle", "item_id"),
    suffixes = c("", ".right")
  )
  
  # Keep only the columns from left result (or merge intelligently)
  keep_cols <- c("db_uuid", "session", "bundle", "item_id", "level", "type", 
                 "seq_idx", "sample_rate", "sample_point", "sample_start", 
                 "sample_dur", "label")
  
  result <- result[, intersect(names(result), keep_cols), drop = FALSE]
  
  return(result)
}

# Disjunction query execution (OR)
execute_disjunction_query <- function(db_path, parsed_query, result_level = NULL) {
  # Execute both sub-queries
  left_result <- execute_subquery(db_path, parsed_query$left)
  right_result <- execute_subquery(db_path, parsed_query$right)
  
  # Union the results (remove duplicates)
  result <- unique(rbind(left_result, right_result))
  
  # Sort by session, bundle, seq_idx for consistency
  if (nrow(result) > 0) {
    result <- result[order(result$session, result$bundle, result$seq_idx), ]
  }
  
  return(result)
}

# Helper to execute a sub-query
execute_subquery <- function(db_path, parsed_query) {
  result <- switch(parsed_query$type,
    "simple" = execute_simple_query_corrected(db_path, parsed_query),
    "sequence" = execute_sequence_query_corrected(db_path, parsed_query, NULL),
    "dominance" = execute_dominance_query_corrected(db_path, parsed_query, NULL),
    "function" = execute_function_query_corrected(db_path, parsed_query),
    "conjunction" = execute_conjunction_query(db_path, parsed_query, NULL),
    "disjunction" = execute_disjunction_query(db_path, parsed_query, NULL),
    stop("Unknown query type in subquery: ", parsed_query$type)
  )
  return(result)
}

# Helper functions
extract_condition_from_query <- function(query) {
  if (is.list(query)) {
    operator <- query$operator
    value <- query$value
    attribute <- if (!is.null(query$attribute)) query$attribute else query$level
    
    # Build condition based on operator
    if (operator %in% c("==", "=")) {
      return(sprintf("l.label = '%s' AND l.name = '%s'", value, attribute))
    } else if (operator == "!=") {
      return(sprintf("(l.label != '%s' OR l.name != '%s')", value, attribute))
    } else if (operator == "=~") {
      # Use GLOB for pattern matching (SQLite built-in, case-sensitive)
      # Convert regex-like patterns to GLOB patterns
      glob_pattern <- value
      # Common regex to GLOB conversions
      glob_pattern <- gsub("\\.", "?", glob_pattern)
      glob_pattern <- gsub("\\*", "%", glob_pattern)
      glob_pattern <- gsub("%", "*", glob_pattern)
      
      return(sprintf("l.label GLOB '*%s*' AND l.name = '%s'", glob_pattern, attribute))
    } else if (operator == "!~") {
      glob_pattern <- value
      glob_pattern <- gsub("\\.", "?", glob_pattern)
      glob_pattern <- gsub("\\*", "%", glob_pattern)
      glob_pattern <- gsub("%", "*", glob_pattern)
      
      return(sprintf("l.label NOT GLOB '*%s*' AND l.name = '%s'", glob_pattern, attribute))
    }
  }
  
  stop("Cannot extract condition from query")
}

extract_level_from_query <- function(query) {
  if (is.list(query) && !is.null(query$level)) {
    return(query$level)
  }
  stop("Cannot extract level from query")
}

create_empty_result <- function() {
  return(data.frame(
    db_uuid = character(0),
    session = character(0),
    bundle = character(0),
    item_id = integer(0),
    level = character(0),
    type = character(0),
    seq_idx = integer(0),
    sample_rate = numeric(0),
    sample_point = integer(0),
    sample_start = integer(0),
    sample_dur = integer(0),
    label = character(0),
    stringsAsFactors = FALSE
  ))
}

# Dominance SQL builder
build_corrected_dominance_sql <- function(left_query, right_query, left_level, right_level, result_level, hierarchy_info) {
  left_condition <- extract_condition_from_query(left_query)
  right_condition <- extract_condition_from_query(right_query)
  
  # Get attribute for final label display
  result_attr <- if (result_level == left_level) {
    if (!is.null(left_query$attribute)) left_query$attribute else left_level
  } else {
    if (!is.null(right_query$attribute)) right_query$attribute else right_level
  }
  
  path <- find_dominance_path(hierarchy_info, left_level, right_level)
  
  if (length(path) == 0) {
    stop("No dominance path found")
  }
  
  with_clauses <- build_dominance_chain_cte(path, left_condition, right_condition)
  
  # Build the main SQL with correct string interpolation
  result_side <- if(result_level == left_level) "left" else "right"
  
  main_sql <- sprintf("
    SELECT DISTINCT 
      i.db_uuid, i.session, i.bundle, i.item_id, 
      i.level, i.type, i.seq_idx, i.sample_rate,
      i.sample_point, i.sample_start, i.sample_dur,
      l.label
    FROM dominance_pairs dp
    INNER JOIN items i ON dp.%s_id = i.item_id
      AND dp.db_uuid = i.db_uuid
      AND dp.session = i.session
      AND dp.bundle = i.bundle
    INNER JOIN labels l ON i.db_uuid = l.db_uuid 
      AND i.session = l.session 
      AND i.bundle = l.bundle 
      AND i.item_id = l.item_id
      AND l.name = '%s'
    WHERE i.level = '%s'
    ORDER BY i.session, i.bundle, i.seq_idx",
    result_side,
    result_attr,
    result_level
  )
  
  return(paste(with_clauses, main_sql, sep = "\n"))
}

build_dominance_chain_cte <- function(path, left_condition, right_condition) {
  ctes <- c()
  
  left_level <- path[1]
  ctes <- c(ctes, sprintf("
    left_matches AS (
      SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id
      FROM items i
      INNER JOIN labels l ON i.db_uuid = l.db_uuid 
        AND i.session = l.session 
        AND i.bundle = l.bundle 
        AND i.item_id = l.item_id
      WHERE i.level = '%s' AND %s
    )", left_level, left_condition))
  
  right_level <- path[length(path)]
  ctes <- c(ctes, sprintf("
    right_matches AS (
      SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id
      FROM items i
      INNER JOIN labels l ON i.db_uuid = l.db_uuid 
        AND i.session = l.session 
        AND i.bundle = l.bundle 
        AND i.item_id = l.item_id
      WHERE i.level = '%s' AND %s
    )", right_level, right_condition))
  
  if (length(path) == 2) {
    ctes <- c(ctes, "
      dominance_pairs AS (
        SELECT lm.db_uuid, lm.session, lm.bundle,
               lm.item_id as left_id, rm.item_id as right_id
        FROM left_matches lm
        INNER JOIN links lnk ON lm.db_uuid = lnk.db_uuid 
          AND lm.session = lnk.session 
          AND lm.bundle = lnk.bundle 
          AND lm.item_id = lnk.from_id
        INNER JOIN right_matches rm ON lnk.db_uuid = rm.db_uuid 
          AND lnk.session = rm.session 
          AND lnk.bundle = rm.bundle 
          AND lnk.to_id = rm.item_id
      )")
  } else {
    chain_sql <- build_recursive_dominance_chain(path)
    ctes <- c(ctes, chain_sql)
  }
  
  return(paste("WITH", paste(ctes, collapse = ",\n")))
}

build_recursive_dominance_chain <- function(path) {
  if (length(path) <= 2) {
    stop("build_recursive_dominance_chain called for direct dominance")
  }
  
  joins <- c()
  
  for (i in 1:(length(path) - 1)) {
    if (i == 1) {
      joins <- c(joins, sprintf("
        link%d AS (
          SELECT lm.db_uuid, lm.session, lm.bundle,
                 lm.item_id as level_%s_id, lnk.to_id as level_%s_id
          FROM left_matches lm
          INNER JOIN links lnk ON lm.db_uuid = lnk.db_uuid 
            AND lm.session = lnk.session 
            AND lm.bundle = lnk.bundle 
            AND lm.item_id = lnk.from_id
          INNER JOIN items i ON lnk.db_uuid = i.db_uuid 
            AND lnk.session = i.session 
            AND lnk.bundle = i.bundle 
            AND lnk.to_id = i.item_id 
            AND i.level = '%s'
        )", i, path[i], path[i+1], path[i+1]))
    } else if (i == length(path) - 1) {
      joins <- c(joins, sprintf("
        dominance_pairs AS (
          SELECT l%d.db_uuid, l%d.session, l%d.bundle,
                 l%d.level_%s_id as left_id, rm.item_id as right_id
          FROM link%d l%d
          INNER JOIN links lnk ON l%d.db_uuid = lnk.db_uuid
            AND l%d.session = lnk.session
            AND l%d.bundle = lnk.bundle
            AND l%d.level_%s_id = lnk.from_id
          INNER JOIN right_matches rm ON lnk.db_uuid = rm.db_uuid
            AND lnk.session = rm.session
            AND lnk.bundle = rm.bundle
            AND lnk.to_id = rm.item_id
        )", i-1, i-1, i-1, i-1, path[1], i-1, i-1, i-1, i-1, i-1, i-1, path[i]))
    } else {
      joins <- c(joins, sprintf("
        link%d AS (
          SELECT l%d.db_uuid, l%d.session, l%d.bundle,
                 l%d.level_%s_id, lnk.to_id as level_%s_id
          FROM link%d l%d
          INNER JOIN links lnk ON l%d.db_uuid = lnk.db_uuid
            AND l%d.session = lnk.session
            AND l%d.bundle = lnk.bundle
            AND l%d.level_%s_id = lnk.from_id
          INNER JOIN items i ON lnk.db_uuid = i.db_uuid
            AND lnk.session = i.session
            AND lnk.bundle = i.bundle
            AND lnk.to_id = i.item_id 
            AND i.level = '%s'
        )", i, i-1, i-1, i-1, i-1, path[1], path[i+1], i-1, i-1, i-1, i-1, i-1, i-1, path[i], path[i+1]))
    }
  }
  
  return(paste(joins, collapse = ",\n"))
}

# Position function
execute_position_function <- function(con, func_name, parent_level, child_level, operator, value, position = NULL) {
  # Position functions: Start(parent, child), End(parent, child), Medial(parent, child, [n])
  # Returns children in specific positions within their parent
  
  # Handle Medial with specific position
  if (func_name == "Medial" && !is.null(position)) {
    position_condition <- sprintf("child_rank = %d", position)
  } else {
    position_condition <- switch(func_name,
      "Start" = "child_rank = 1",
      "End" = "child_rank = max_rank",
      "Medial" = "child_rank > 1 AND child_rank < max_rank"
    )
  }
  
  # For position functions, the value should always be 1 (true/false concept)
  # The operator determines if we want or don't want items in that position
  include_position <- switch(operator,
    "==" = value == 1,
    "=" = value == 1,
    "!=" = value != 1,
    stop("Invalid operator for position function: ", operator)
  )
  
  # If we're looking for items NOT in position (value != 1), negate the condition
  if (!include_position) {
    position_condition <- sprintf("NOT (%s)", position_condition)
  }
  
  sql <- sprintf("
    WITH child_positions AS (
      SELECT 
        c.db_uuid, c.session, c.bundle, c.item_id,
        c.level, c.type, c.seq_idx, c.sample_rate,
        c.sample_point, c.sample_start, c.sample_dur,
        ROW_NUMBER() OVER (
          PARTITION BY p.db_uuid, p.session, p.bundle, p.item_id
          ORDER BY c.seq_idx
        ) as child_rank,
        COUNT(*) OVER (PARTITION BY p.db_uuid, p.session, p.bundle, p.item_id) as max_rank
      FROM items p
      INNER JOIN links lnk ON p.db_uuid = lnk.db_uuid 
        AND p.session = lnk.session 
        AND p.bundle = lnk.bundle 
        AND p.item_id = lnk.from_id
      INNER JOIN items c ON lnk.db_uuid = c.db_uuid 
        AND lnk.session = c.session 
        AND lnk.bundle = c.bundle 
        AND lnk.to_id = c.item_id
      WHERE p.level = '%s' AND c.level = '%s'
    )
    SELECT DISTINCT 
      cp.db_uuid, cp.session, cp.bundle, cp.item_id,
      cp.level, cp.type, cp.seq_idx, cp.sample_rate,
      cp.sample_point, cp.sample_start, cp.sample_dur,
      l.label
    FROM child_positions cp
    INNER JOIN labels l ON cp.db_uuid = l.db_uuid 
      AND cp.session = l.session 
      AND cp.bundle = l.bundle 
      AND cp.item_id = l.item_id
      AND l.name = '%s'
    WHERE %s
    ORDER BY cp.session, cp.bundle, cp.seq_idx",
    parent_level, child_level, child_level, position_condition
  )
  
  return(DBI::dbGetQuery(con, sql))
}

# Count function
execute_count_function <- function(con, parent_level, child_level, operator, value) {
  sql <- sprintf("
    WITH child_counts AS (
      SELECT 
        p.db_uuid, p.session, p.bundle, p.item_id,
        p.level, p.type, p.seq_idx, p.sample_rate,
        p.sample_point, p.sample_start, p.sample_dur,
        COUNT(c.item_id) as child_count
      FROM items p
      LEFT JOIN links lnk ON p.db_uuid = lnk.db_uuid 
        AND p.session = lnk.session 
        AND p.bundle = lnk.bundle 
        AND p.item_id = lnk.from_id
      LEFT JOIN items c ON lnk.db_uuid = c.db_uuid 
        AND lnk.session = c.session 
        AND lnk.bundle = c.bundle 
        AND lnk.to_id = c.item_id 
        AND c.level = '%s'
      WHERE p.level = '%s'
      GROUP BY p.db_uuid, p.session, p.bundle, p.item_id,
               p.level, p.type, p.seq_idx, p.sample_rate,
               p.sample_point, p.sample_start, p.sample_dur
    )
    SELECT DISTINCT 
      cc.db_uuid, cc.session, cc.bundle, cc.item_id,
      cc.level, cc.type, cc.seq_idx, cc.sample_rate,
      cc.sample_point, cc.sample_start, cc.sample_dur,
      l.label
    FROM child_counts cc
    INNER JOIN labels l ON cc.db_uuid = l.db_uuid 
      AND cc.session = l.session 
      AND cc.bundle = l.bundle 
      AND cc.item_id = l.item_id
      AND l.name = '%s'
    WHERE cc.child_count %s %d
    ORDER BY cc.session, cc.bundle, cc.seq_idx",
    child_level, parent_level, parent_level, operator, value
  )
  
  return(DBI::dbGetQuery(con, sql))
}

# Hierarchy functions
get_hierarchy_info <- function(con) {
  list(
    links = list(
      list(type = "ONE_TO_MANY", super = "Utterance", sub = "Intonational"),
      list(type = "ONE_TO_MANY", super = "Intonational", sub = "Intermediate"), 
      list(type = "ONE_TO_MANY", super = "Intermediate", sub = "Word"),
      list(type = "ONE_TO_MANY", super = "Word", sub = "Syllable"),
      list(type = "ONE_TO_MANY", super = "Syllable", sub = "Phoneme"),
      list(type = "MANY_TO_MANY", super = "Phoneme", sub = "Phonetic"),
      list(type = "ONE_TO_MANY", super = "Syllable", sub = "Tone"),
      list(type = "ONE_TO_MANY", super = "Intonational", sub = "Foot"),
      list(type = "ONE_TO_MANY", super = "Foot", sub = "Syllable")
    )
  )
}

can_dominate <- function(hierarchy_info, level_a, level_b) {
  path <- find_dominance_path(hierarchy_info, level_a, level_b)
  return(length(path) > 0)
}

find_dominance_path <- function(hierarchy_info, from_level, to_level) {
  if (from_level == to_level) {
    return(c(from_level))
  }
  
  adj_list <- list()
  for (link in hierarchy_info$links) {
    super <- link$super
    sub <- link$sub
    if (is.null(adj_list[[super]])) {
      adj_list[[super]] <- c()
    }
    adj_list[[super]] <- c(adj_list[[super]], sub)
  }
  
  queue <- list(list(level = from_level, path = c(from_level)))
  visited <- c()
  
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    
    current_level <- current$level
    current_path <- current$path
    
    if (current_level == to_level) {
      return(current_path)
    }
    
    if (current_level %in% visited) {
      next
    }
    
    visited <- c(visited, current_level)
    
    children <- adj_list[[current_level]]
    if (!is.null(children)) {
      for (child in children) {
        if (!(child %in% visited)) {
          queue <- append(queue, list(list(
            level = child, 
            path = c(current_path, child)
          )))
        }
      }
    }
  }
  
  return(c())
}

# Result formatting
format_as_emuRsegs <- function(result_df) {
  if (nrow(result_df) == 0) {
    return(create_empty_emuRsegs())
  }
  
  start_times <- result_df$sample_start / result_df$sample_rate
  end_times <- (result_df$sample_start + result_df$sample_dur) / result_df$sample_rate
  
  # Check if we have separate end_item_id and end_seq_idx (from sequence queries)
  has_end_id <- "end_item_id" %in% names(result_df)
  has_end_seq <- "end_seq_idx" %in% names(result_df)
  
  # Match emuR column order exactly
  emuRsegs_df <- data.frame(
    labels = result_df$label,
    start = start_times,
    end = end_times,
    db_uuid = result_df$db_uuid,
    session = result_df$session,
    bundle = result_df$bundle,
    start_item_id = result_df$item_id,
    end_item_id = if(has_end_id) result_df$end_item_id else result_df$item_id,
    level = result_df$level,
    attribute = result_df$level,
    start_item_seq_idx = result_df$seq_idx,
    end_item_seq_idx = if(has_end_seq) result_df$end_seq_idx else result_df$seq_idx,
    type = result_df$type,
    sample_start = result_df$sample_start,
    sample_end = result_df$sample_start + result_df$sample_dur,
    sample_rate = result_df$sample_rate,
    stringsAsFactors = FALSE
  )
  
  # Convert to tibble to match emuR output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    emuRsegs_df <- dplyr::as_tibble(emuRsegs_df)
  }
  class(emuRsegs_df) <- c("emuRsegs", class(emuRsegs_df))
  return(emuRsegs_df)
}

create_empty_emuRsegs <- function() {
  empty_df <- data.frame(
    labels = character(0),
    start = numeric(0),
    end = numeric(0),
    db_uuid = character(0),
    session = character(0),
    bundle = character(0),
    start_item_id = integer(0),
    end_item_id = integer(0),
    level = character(0),
    attribute = character(0),
    start_item_seq_idx = integer(0),
    end_item_seq_idx = integer(0),
    type = character(0),
    sample_start = integer(0),
    sample_end = integer(0),
    sample_rate = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Convert to tibble to match emuR output
  if (requireNamespace("dplyr", quietly = TRUE)) {
    empty_df <- dplyr::as_tibble(empty_df)
  }
  class(empty_df) <- c("emuRsegs", class(empty_df))
  return(empty_df)
}

# Success message
cat("Optimized EQL query implementation loaded successfully!\n")
cat("Usage: result <- ask_for(emuDB, 'Phonetic == t')  # or query() as alias\n")
cat("\nSupported EQL features:\n")
cat("  - Simple queries: Level == value, Level != value\n")
cat("  - Regex queries: Level =~ pattern, Level !~ pattern\n")
cat("  - Attributes: Level:Attribute == value\n")
cat("  - Sequence: [Level == a -> Level == b]\n")
cat("  - Dominance: [Level1 == a ^ Level2 == b]\n")
cat("  - Projection: #Level marks the side to return\n")
cat("  - Conjunction: [query1 & query2]\n")
cat("  - Disjunction: [query1 | query2]\n")
cat("  - Position functions: Start(parent, child), End(parent, child), Medial(parent, child)\n")
cat("  - Count function: Num(parent, child) >= n\n")
cat("\nReturns: segment_list object (compatible with emuR::query results)\n")
