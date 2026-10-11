

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
#' query(corpus, "Phonetic == t")
#' query(corpus, "Phonetic == t")  # alias
#' 
#' # Sequence with projection
#' query(corpus, "[#Phoneme == n -> Phoneme == t]")
#' 
#' # Dominance query
#' query(corpus, "[Word == the ^ Phoneme == D]")
#' 
#' # Count function
#' query(corpus, "Num(Syllable, Phoneme) >= 3")
#' }
#' @name reindeer-query-parser
#' @keywords internal
NULL

# Expand label-group names in `==` / `!=` predicates, as emuR does.
# Attribute-level groups win over database-level ones; a group name shadows a
# literal label of the same name. Regex operators are left untouched.
resolve_label_groups <- function(parsed, config) {
  if (!is.null(parsed$left))  parsed$left  <- resolve_label_groups(parsed$left, config)
  if (!is.null(parsed$right)) parsed$right <- resolve_label_groups(parsed$right, config)
  if (!identical(parsed$type, "simple") || !parsed$operator %in% c("==", "=", "!=")) {
    return(parsed)
  }

  groups <- config$labelGroups
  for (lvl in config$levelDefinitions) {
    for (attr_def in lvl$attributeDefinitions) {
      if (identical(attr_def$name, parsed$attribute)) {
        groups <- c(attr_def$labelGroups, groups)
      }
    }
  }
  if (!length(groups)) return(parsed)
  group_values <- stats::setNames(lapply(groups, function(g) unlist(g$values)),
                                  vapply(groups, `[[`, "", "name"))

  labels <- if (is.null(parsed$alternatives)) parsed$value else parsed$alternatives
  # `[[` on a name list takes the first match, i.e. the attribute-level group.
  expanded <- unique(unlist(lapply(labels, function(l) {
    if (l %in% names(group_values)) group_values[[l]] else l
  })))
  if (identical(expanded, labels)) return(parsed)

  parsed$value <- expanded[1]
  parsed$alternatives <- if (length(expanded) > 1) expanded else NULL
  parsed
}

# Parse EQL and resolve label groups from the DBconfig next to the cache.
.parse_eql_for_db <- function(query_string, db_path) {
  parsed <- parse_eql_query(query_string)
  config <- tryCatch(load_DBconfig(dirname(db_path)), error = function(e) NULL)
  if (is.null(config)) parsed else resolve_label_groups(parsed, config)
}

parse_simple_query <- function(query_string) {
  has_projection <- grepl("^#", query_string)
  if (has_projection) {
    query_string <- sub("^#", "", query_string)
  }

  # Scope predicates: `Session == X` / `Bundle == X` (and variants) filter on
  # the segment's owning session/bundle rather than on item labels. Parser
  # detects these early so executors can route them through a dedicated
  # WHERE clause instead of trying to resolve "Session" as an annotation level.
  scope_pattern <- "^(Session|Bundle)\\s*(==|!=|=~|!~)\\s*(?:'([^']*)'|\"([^\"]*)\"|(.+))$"
  scope_match <- regexec(scope_pattern, query_string)
  scope_matches <- regmatches(query_string, scope_match)[[1]]
  if (length(scope_matches) >= 4 && scope_matches[1] != "") {
    kind  <- tolower(scope_matches[2])  # "session" or "bundle"
    operator <- scope_matches[3]
    value <- if (scope_matches[4] != "") {
      scope_matches[4]
    } else if (scope_matches[5] != "") {
      scope_matches[5]
    } else {
      trimws(scope_matches[6])
    }
    alternatives <- NULL
    # Quoted values are literal, as in emuR: `Bundle == 'a|b'` is one name.
    if (operator %in% c("==", "!=") && scope_matches[6] != "" && grepl("\\|", value)) {
      alternatives <- trimws(strsplit(value, "\\|")[[1]])
      value <- alternatives[1]
    }
    return(list(
      type = "scope_filter",
      kind = kind,
      operator = operator,
      value = value,
      alternatives = alternatives,
      projection = has_projection
    ))
  }

  # Handle bare level name (no operator) - means "all items on this level"
  bare_level_pattern <- "^([A-Za-z_]+)(?::([A-Za-z_]+))?$"
  bare_match <- regexec(bare_level_pattern, query_string)
  bare_matches <- regmatches(query_string, bare_match)[[1]]
  if (length(bare_matches) >= 1 && bare_matches[1] != "") {
    level <- bare_matches[2]
    attribute <- if (length(bare_matches) >= 3 && bare_matches[3] != "") bare_matches[3] else level
    return(list(
      type = "simple",
      level = level,
      attribute = attribute,
      operator = "=~",
      value = ".*",
      alternatives = NULL,
      projection = has_projection
    ))
  }

  # Match attribute definition: Level:Attribute or just Level
  # Value can contain pipe-separated alternatives: m | n | p
  # Allow embedded apostrophes/quotes in unquoted values
  pattern <- "^([A-Za-z_]+)(?::([A-Za-z_]+))?\\s*(==|!=|=~|!~|=)\\s*(?:'([^']*)'|\"([^\"]*)\"|(.+))$"
  match <- regexec(pattern, query_string)
  matches <- regmatches(query_string, match)[[1]]

  if (length(matches) < 4) {
    .query_abort(c(
      "Cannot parse simple query.",
      .eql_caret(query_string, pos = 1L, label = "Query:")
    ))
  }

  # EQL has single-quoted strings only; a double-quoted value would be a
  # literal label with quote characters, never what the author meant.
  if (length(matches) >= 6 && matches[6] != "") {
    .query_abort(c(
      "Labels are quoted with single quotes in EQL.",
      "i" = "Use {.code {matches[4]} '{matches[6]}'} instead of double quotes.",
      .eql_caret(query_string, pos = regexpr("\"", query_string), label = "Query:")))
  }

  level <- matches[2]
  attribute <- if (length(matches) >= 3 && matches[3] != "") matches[3] else level
  operator <- matches[4]
  # Value is in group 5 (single-quoted), 6 (double-quoted), or 7 (unquoted)
  value <- if (matches[5] != "") {
    matches[5]
  } else if (matches[6] != "") {
    matches[6]
  } else {
    trimws(matches[7])
  }

  # Reject a value that is itself an operator fragment - happens when the
  # user wrote e.g. "Phonetic ==" and the regex backtracked, matching
  # `=` as the operator and the trailing `=` as the value. Surface the
  # parse error at query() time instead of letting an empty query run.
  if (operator == "=" && value %in% c("=", "~")) {
    op_pos <- regexpr("==|=~|!~|!=|=", query_string)
    .query_abort(c(
      "Cannot parse simple query: missing right-hand value.",
      .eql_caret(query_string, pos = op_pos, label = "Query:")
    ))
  }
  if (!nzchar(value)) {
    op_pos <- regexpr("==|=~|!~|!=|=", query_string)
    .query_abort(c(
      "Cannot parse simple query: empty right-hand value.",
      .eql_caret(query_string, pos = op_pos, label = "Query:")
    ))
  }

  # Check for label alternatives (pipe-separated): m | n | p
  # Only for unquoted == and != values (not regex); quoted values are
  # literal labels, as in emuR: `Phonetic == 'p|t'` matches the label "p|t".
  alternatives <- NULL
  if (operator %in% c("==", "=", "!=") && matches[7] != "" && grepl("\\|", value)) {
    alternatives <- trimws(strsplit(value, "\\|")[[1]])
    # Each alternative may be quoted: 'm'|'n'
    alternatives <- sub("^(['\"])(.*)\\1$", "\\2", alternatives)
    value <- alternatives[1]  # Keep first as primary
  }

  # emuR accepts a single `=` as `==`.
  if (operator == "=") operator <- "=="

  return(list(
    type = "simple",
    level = level,
    attribute = attribute,
    operator = operator,
    value = value,
    alternatives = alternatives,
    projection = has_projection
  ))
}

parse_function_query <- function(query_string) {
  # Normalize TRUE/FALSE/T/F to 1/0
  .normalize_bool_value <- function(v) {
    v <- trimws(v)
    if (v %in% c("TRUE", "T")) return("1")
    if (v %in% c("FALSE", "F")) return("0")
    v
  }

  # Try 3-parameter pattern first (for Medial with position)
  pattern_3param <- "^(Medial)\\(([A-Za-z_]+),\\s*([A-Za-z_]+),\\s*([0-9]+|TRUE|FALSE|T|F)\\)$"
  match_3 <- regexec(pattern_3param, query_string)
  matches_3 <- regmatches(query_string, match_3)[[1]]

  if (length(matches_3) == 5) {
    val <- .normalize_bool_value(matches_3[5])
    return(list(
      type = "function",
      func_name = matches_3[2],
      level1 = matches_3[3],
      level2 = matches_3[4],
      operator = "==",
      value = val,
      position = as.numeric(val)
    ))
  }

  # Try 2-parameter pattern with comparison
  pattern_2param <- "^(Start|End|Medial|Num)\\(([A-Za-z_]+),\\s*([A-Za-z_]+)\\)\\s*(==|!=|=|>|<|>=|<=)\\s*([0-9]+|TRUE|FALSE|T|F)$"
  match_2 <- regexec(pattern_2param, query_string)
  matches_2 <- regmatches(query_string, match_2)[[1]]

  if (length(matches_2) == 6) {
    val <- .normalize_bool_value(matches_2[6])
    return(list(
      type = "function",
      func_name = matches_2[2],
      level1 = matches_2[3],
      level2 = matches_2[4],
      operator = matches_2[5],
      value = val,
      position = NULL
    ))
  }

  .query_abort(c(
    "Cannot parse function query.",
    .eql_caret(query_string,
               pos = regexpr("\\(", query_string),
               label = "Query:")
  ))
}

# Open a query connection with REGEXP support
.open_query_connection <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  # Register R's grepl as SQLite REGEXP function for =~/!~ operators
  RSQLite::initRegExp(con)
  con
}

# Resolve level/attribute: if "level" is actually an attribute name, find the real level
.resolve_level_attribute <- function(con, level, attribute) {
  # Check if level exists in items table
  check <- DBI::dbGetQuery(con,
    "SELECT 1 FROM items WHERE level = ? LIMIT 1", params = list(level))
  if (nrow(check) > 0) return(list(level = level, attribute = attribute))

  # Level not found - check if it's an attribute name in labels
  attr_check <- DBI::dbGetQuery(con,
    "SELECT DISTINCT i.level FROM items i JOIN labels l ON i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.item_id=l.item_id WHERE l.name=? LIMIT 1",
    params = list(level))
  if (nrow(attr_check) > 0) {
    return(list(level = attr_check$level[1], attribute = level))
  }

  # Fall back to original
  list(level = level, attribute = attribute)
}



# Sentinel SQL that returns zero rows with the standard column shape.
# Used when an in-build sub-query materialises empty.
.empty_query_sql <- function() {
  list(
    sql = "SELECT NULL AS db_uuid, NULL AS session, NULL AS bundle, NULL AS item_id,
           NULL AS level, NULL AS type, NULL AS seq_idx, NULL AS sample_rate,
           NULL AS sample_point, NULL AS sample_start, NULL AS sample_dur,
           NULL AS label, NULL AS attribute WHERE 0",
    params = list()
  )
}

# Build a self-contained WHERE fragment matching items against a set of
# (db_uuid, session, bundle, item_id) tuples. Uses a control-char-separated
# composite key in a single deduplicated IN list instead of a per-row
# 4-column OR list, keeping the SQL compact and still self-contained so the
# lazy path can defer it. Values are bound via dbQuoteLiteral (no injection).
.subquery_match <- function(db_path, query, con) {
  # Compound operands are embedded as sub-selects (nothing is executed while
  # compiling): rows of `i` whose identity appears in the operand's result.
  # The operand's level is derived from the query tree, not from its rows.
  q <- build_base_sql(db_path, query, list(), con = con)
  if (is.null(q)) return(NULL)
  list(
    sql = paste0("(i.db_uuid, i.session, i.bundle, i.item_id) IN (",
                 "SELECT db_uuid, session, bundle, item_id FROM (", q$sql, "))"),
    params = q$params,
    level = .resolve_level_attribute(con, extract_level_from_query(query),
                                     extract_level_from_query(query))
  )
}

# Build SQL for a sequence query. Returns list(sql, params) ready for
# DBI::dbGetQuery, or NULL if a non-simple sub-query was pre-executed and
# returned no rows (caller should treat as empty result).
#
# Sub-query handling: any non-simple branch is materialised here via
# sub-selects (see .subquery_match()); the resulting
# item IDs are embedded as quoted literals. The OUTER sequence query
# remains a single SQL statement that the lazy path can defer to collect().
# @keywords internal
build_sequence_query_sql_impl <- function(db_path, parsed_query, result_level = NULL, con) {
  left_query <- parsed_query$left
  right_query <- parsed_query$right

  left_level_raw <- extract_level_from_query(left_query)
  right_level_raw <- extract_level_from_query(right_query)

  # Resolve attribute names to actual level names (e.g. "Text" → "Word")
  left_resolved <- .resolve_level_attribute(con, left_level_raw, left_level_raw)
  right_resolved <- .resolve_level_attribute(con, right_level_raw, right_level_raw)
  left_level <- left_resolved$level
  right_level <- right_resolved$level
  # Track original names as attribute fallbacks
  left_attr_default <- left_resolved$attribute
  right_attr_default <- right_resolved$attribute

  # Pre-execute non-simple sub-queries BEFORE level check - compound sub-queries
  # (e.g., dominance) may resolve to a different level than extract_level_from_query reports
  left_preexec <- NULL
  right_preexec <- NULL
  left_cond <- NULL
  right_cond <- NULL

  if (left_query$type != "simple") {
    left_preexec <- .subquery_match(db_path, left_query, con)
    if (is.null(left_preexec)) return(NULL)
    left_level <- left_preexec$level$level
    left_attr_default <- left_preexec$level$attribute
  } else {
    left_cond <- extract_condition_from_query(left_query)
  }
  if (right_query$type != "simple") {
    right_preexec <- .subquery_match(db_path, right_query, con)
    if (is.null(right_preexec)) return(NULL)
    right_level <- right_preexec$level$level
    right_attr_default <- right_preexec$level$attribute
  } else {
    right_cond <- extract_condition_from_query(right_query)
  }

  if (left_level != right_level) {
    .query_abort("Sequence queries require both sides to be from the same level")
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
  
  # Validate result_side to prevent column name injection
  if (!result_side %in% c("left", "right", "both")) {
    .query_abort("Internal error: invalid result_side {.val {result_side}}")
  }

  # Build CTE WHERE clauses with parameterized conditions
  # Returns list(sql = "...", params = list(...))
  build_match_where <- function(level, cond, preexec) {
    if (!is.null(preexec)) {
      return(list(
        sql = paste0("i.level = ? AND ", preexec$sql),
        params = c(list(level), preexec$params)
      ))
    } else {
      return(list(
        sql = paste0("i.level = ? AND ", cond$sql),
        params = c(list(level), cond$params)
      ))
    }
  }

  left_needs_label_join <- is.null(left_preexec)
  right_needs_label_join <- is.null(right_preexec)

  left_w <- build_match_where(left_level, left_cond, left_preexec)
  right_w <- build_match_where(right_level, right_cond, right_preexec)

  # Check if we return both elements or just one
  return_both <- (is.null(left_query$projection) || !left_query$projection) &&
                  (is.null(right_query$projection) || !right_query$projection)

  # Collect all params in order as we build the SQL
  all_params <- list()

  if (return_both) {
    left_attr <- if (!is.null(left_query$attribute)) left_query$attribute else left_attr_default
    right_attr <- if (!is.null(right_query$attribute)) right_query$attribute else right_attr_default

    # Build left_matches CTE
    left_cte_sql <- if (left_needs_label_join) {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx,
              i.sample_start, i.sample_dur, i.sample_rate
       FROM items i
       INNER JOIN labels l ON i.db_uuid = l.db_uuid
         AND i.session = l.session AND i.bundle = l.bundle AND i.item_id = l.item_id
       WHERE ", left_w$sql)
    } else {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx,
              i.sample_start, i.sample_dur, i.sample_rate
       FROM items i
       WHERE ", left_w$sql)
    }
    all_params <- c(all_params, left_w$params)

    # Build right_matches CTE
    right_cte_sql <- if (right_needs_label_join) {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx,
              i.sample_start, i.sample_dur, i.sample_rate
       FROM items i
       INNER JOIN labels l ON i.db_uuid = l.db_uuid
         AND i.session = l.session AND i.bundle = l.bundle AND i.item_id = l.item_id
       WHERE ", right_w$sql)
    } else {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx,
              i.sample_start, i.sample_dur, i.sample_rate
       FROM items i
       WHERE ", right_w$sql)
    }
    all_params <- c(all_params, right_w$params)

    # level, left_attr, right_attr are structural (from parser), parameterize them too
    sql <- paste0("
      WITH left_matches AS (", left_cte_sql, "),
      right_matches AS (", right_cte_sql, "),
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
        ? as level, li.type as type,
        sp.left_seq as seq_idx,
        sp.right_seq as end_seq_idx,
        sp.sample_rate,
        NULL as sample_point, sp.start_sample as sample_start,
        sp.end_sample - sp.start_sample as sample_dur,
        ll.label || '->' || rl.label as label,
        ll.name as attribute
      FROM sequence_pairs sp
      INNER JOIN items li ON sp.db_uuid = li.db_uuid
        AND sp.session = li.session
        AND sp.bundle = li.bundle
        AND sp.left_id = li.item_id
      INNER JOIN labels ll ON sp.db_uuid = ll.db_uuid
        AND sp.session = ll.session
        AND sp.bundle = ll.bundle
        AND sp.left_id = ll.item_id
        AND ll.name = ?
      INNER JOIN labels rl ON sp.db_uuid = rl.db_uuid
        AND sp.session = rl.session
        AND sp.bundle = rl.bundle
        AND sp.right_id = rl.item_id
        AND rl.name = ?
      ORDER BY sp.session, sp.bundle, sp.left_seq")
    all_params <- c(all_params, list(left_level, left_attr, right_attr))
  } else {
    # Use result_side (not level equality) to determine which side's attribute to display,
    # since both sides may resolve to the same level (e.g., "Word") but with different attributes
    result_attr <- if (result_side == "left") {
      extract_attribute_from_query(left_query)
    } else {
      extract_attribute_from_query(right_query)
    }

    # Build left_matches CTE
    left_cte_sql <- if (left_needs_label_join) {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx
       FROM items i
       INNER JOIN labels l ON i.db_uuid = l.db_uuid
         AND i.session = l.session AND i.bundle = l.bundle AND i.item_id = l.item_id
       WHERE ", left_w$sql)
    } else {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx
       FROM items i
       WHERE ", left_w$sql)
    }
    all_params <- c(all_params, left_w$params)

    # Build right_matches CTE
    right_cte_sql <- if (right_needs_label_join) {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx
       FROM items i
       INNER JOIN labels l ON i.db_uuid = l.db_uuid
         AND i.session = l.session AND i.bundle = l.bundle AND i.item_id = l.item_id
       WHERE ", right_w$sql)
    } else {
      paste0("SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id, i.seq_idx
       FROM items i
       WHERE ", right_w$sql)
    }
    all_params <- c(all_params, right_w$params)

    sql <- paste0("
      WITH left_matches AS (", left_cte_sql, "),
      right_matches AS (", right_cte_sql, "),
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
        l.label, l.name as attribute
      FROM sequence_pairs sp
      INNER JOIN items i ON sp.", result_side, "_id = i.item_id
        AND sp.db_uuid = i.db_uuid
        AND sp.session = i.session
        AND sp.bundle = i.bundle
      INNER JOIN labels l ON i.db_uuid = l.db_uuid
        AND i.session = l.session
        AND i.bundle = l.bundle
        AND i.item_id = l.item_id
        AND l.name = ?
      ORDER BY i.session, i.bundle, i.seq_idx")
    all_params <- c(all_params, list(result_attr))
  }

  list(sql = sql, params = all_params)
}


# Build SQL for a dominance query. Returns list(sql, params) or NULL if a
# non-simple sub-query was pre-executed and returned no rows. Sub-query
# handling: same Option B as sequence - non-simple branches are
# materialised here; the outer dominance CTE chain is a single SQL
# statement that the lazy path can defer to collect().
# @keywords internal
build_dominance_query_sql_impl <- function(db_path, parsed_query, result_level = NULL, con) {
  left_query <- parsed_query$left
  right_query <- parsed_query$right

  left_level <- extract_level_from_query(left_query)
  right_level <- extract_level_from_query(right_query)

  # Resolve attribute names to actual level names (e.g. "Text" → "Word")
  left_resolved <- .resolve_level_attribute(con, left_level, left_level)
  right_resolved <- .resolve_level_attribute(con, right_level, right_level)
  left_level <- left_resolved$level
  right_level <- right_resolved$level

  # Determine result level based on projection
  if (is.null(result_level)) {
    if (!is.null(left_query$projection) && left_query$projection) {
      result_level <- left_level
    } else if (!is.null(right_query$projection) && right_query$projection) {
      result_level <- right_level
    } else {
      result_level <- left_level
    }
  }

  db_dir <- dirname(db_path)
  hierarchy_info <- get_hierarchy_info(db_dir)

  if (!can_dominate(hierarchy_info, left_level, right_level)) {
    cli::cli_warn("No dominance relationship possible between {.val {left_level}} and {.val {right_level}}")
    return(NULL)
  }

  # A sequence operand is a span of items, not an item: dominance over spans
  # has its own (all-in-SQL) compilation.
  if (identical(left_query$type, "sequence") || identical(right_query$type, "sequence")) {
    return(build_span_dominance_sql(db_path, parsed_query, result_level, con))
  }

  # Compound operands are embedded as sub-selects.
  left_item_ids <- NULL
  right_item_ids <- NULL
  if (left_query$type != "simple") {
    left_item_ids <- .subquery_match(db_path, left_query, con)
    if (is.null(left_item_ids)) return(NULL)
  }
  if (right_query$type != "simple") {
    right_item_ids <- .subquery_match(db_path, right_query, con)
    if (is.null(right_item_ids)) return(NULL)
  }

  build_corrected_dominance_sql(
    con, left_query, right_query, left_level, right_level, result_level, hierarchy_info,
    left_item_ids = left_item_ids, right_item_ids = right_item_ids
  )
}





# Helper functions
extract_condition_from_query <- function(query) {
  if (is.list(query)) {
    operator <- query$operator
    value <- query$value
    attribute <- if (!is.null(query$attribute)) query$attribute else query$level

    # Check for label alternatives
    alternatives <- query$alternatives

    # Build parameterized condition (sql fragment + params list)
    if (operator %in% c("==", "=")) {
      if (!is.null(alternatives)) {
        placeholders <- paste(rep("?", length(alternatives)), collapse = ", ")
        return(list(
          sql = sprintf("l.label IN (%s) AND l.name = ?", placeholders),
          params = c(as.list(alternatives), list(attribute))
        ))
      }
      return(list(sql = "l.label = ? AND l.name = ?", params = list(value, attribute)))
    } else if (operator == "!=") {
      if (!is.null(alternatives)) {
        placeholders <- paste(rep("?", length(alternatives)), collapse = ", ")
        return(list(
          sql = sprintf("l.label NOT IN (%s) AND l.name = ?", placeholders),
          params = c(as.list(alternatives), list(attribute))
        ))
      }
      return(list(sql = "l.label != ? AND l.name = ?", params = list(value, attribute)))
    } else if (operator == "=~") {
      return(list(sql = "l.label REGEXP ? AND l.name = ?", params = list(value, attribute)))
    } else if (operator == "!~") {
      return(list(sql = "l.label NOT REGEXP ? AND l.name = ?", params = list(value, attribute)))
    }
  }

  .query_abort("Cannot extract condition from query")
}

extract_level_from_query <- function(query) {
  if (is.list(query)) {
    if (!is.null(query$level)) return(query$level)
    # Function queries: the result level is level1 (parent) for Num, level2 (child) for position
    if (query$type == "function") {
      if (query$func_name == "Num") return(query$level1)
      return(query$level2)  # Start/End/Medial return child items
    }
    # Dominance: result level depends on projection
    if (query$type == "dominance") {
      if (!is.null(query$right$projection) && query$right$projection) {
        return(extract_level_from_query(query$right))
      }
      return(extract_level_from_query(query$left))
    }
    # Other compound queries: extract from left side
    if (query$type %in% c("conjunction", "disjunction", "sequence")) {
      return(extract_level_from_query(query$left))
    }
  }
  .query_abort("Cannot extract level from query")
}

# Extract the display attribute name from any query type.
# For simple queries: attribute field (e.g. "Text" from "Text == his")
# For function queries: level1 for Num, level2 for position functions
# For dominance/sequence/conjunction: recurse based on projection
extract_attribute_from_query <- function(query) {
  if (is.list(query)) {
    if (!is.null(query$attribute)) return(query$attribute)
    if (query$type == "function") {
      if (query$func_name == "Num") return(query$level1)
      return(query$level2)
    }
    if (query$type == "dominance") {
      if (!is.null(query$right$projection) && query$right$projection) {
        return(extract_attribute_from_query(query$right))
      }
      return(extract_attribute_from_query(query$left))
    }
    if (query$type %in% c("conjunction", "disjunction", "sequence")) {
      return(extract_attribute_from_query(query$left))
    }
  }
  # Fallback: use level
  return(extract_level_from_query(query))
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
    attribute = character(0),
    stringsAsFactors = FALSE
  ))
}

# Dominance SQL builder - returns list(sql, params) for parameterized execution
build_corrected_dominance_sql <- function(con, left_query, right_query, left_level, right_level,
                                          result_level, hierarchy_info,
                                          left_item_ids = NULL, right_item_ids = NULL) {
  all_params <- list()

  # Build conditions: parameterized for simple queries, dbQuoteLiteral for pre-executed
  left_cond <- if (is.null(left_item_ids)) extract_condition_from_query(left_query) else
    list(sql = left_item_ids$sql, params = left_item_ids$params)
  right_cond <- if (is.null(right_item_ids)) extract_condition_from_query(right_query) else
    list(sql = right_item_ids$sql, params = right_item_ids$params)

  # Get attribute for final label display - use result_level to pick side,
  # then extract_attribute_from_query handles function/dominance/compound queries
  result_attr <- if (result_level == left_level) {
    extract_attribute_from_query(left_query)
  } else {
    extract_attribute_from_query(right_query)
  }

  path_info <- find_dominance_path(hierarchy_info, left_level, right_level)

  if (length(path_info$path) == 0) {
    .query_abort("No dominance path found")
  }

  cte_result <- build_dominance_chain_cte(path_info, left_cond, right_cond,
                                           left_preexecuted = !is.null(left_item_ids),
                                           right_preexecuted = !is.null(right_item_ids))
  all_params <- c(all_params, cte_result$params)

  result_side <- if(result_level == left_level) "left" else "right"

  # Validate result_side to prevent column name injection
  if (!result_side %in% c("left", "right")) {
    .query_abort("Internal error: invalid result_side {.val {result_side}}")
  }

  main_sql <- paste0("
    SELECT DISTINCT
      i.db_uuid, i.session, i.bundle, i.item_id,
      i.level, i.type, i.seq_idx, i.sample_rate,
      i.sample_point, i.sample_start, i.sample_dur,
      l.label, l.name as attribute
    FROM dominance_pairs dp
    INNER JOIN items i ON dp.", result_side, "_id = i.item_id
      AND dp.db_uuid = i.db_uuid
      AND dp.session = i.session
      AND dp.bundle = i.bundle
    INNER JOIN labels l ON i.db_uuid = l.db_uuid
      AND i.session = l.session
      AND i.bundle = l.bundle
      AND i.item_id = l.item_id
      AND l.name = ?
    WHERE i.level = ?
    ORDER BY i.session, i.bundle, i.seq_idx")
  all_params <- c(all_params, list(result_attr, result_level))

  return(list(sql = paste(cte_result$sql, main_sql, sep = "\n"), params = all_params))
}

# Returns list(sql = "WITH ...", params = list(...))
build_dominance_chain_cte <- function(path_info, left_cond, right_cond,
                                      left_preexecuted = FALSE, right_preexecuted = FALSE) {
  path <- path_info$path
  directions <- path_info$directions
  ctes <- c()
  all_params <- list()

  # Build left_matches CTE
  left_level <- path[1]
  if (left_preexecuted) {
    ctes <- c(ctes, paste0("
      left_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id
        FROM items i
        WHERE i.level = ? AND ", left_cond$sql, "
      )"))
  } else {
    ctes <- c(ctes, paste0("
      left_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id
        FROM items i
        INNER JOIN labels l ON i.db_uuid = l.db_uuid
          AND i.session = l.session
          AND i.bundle = l.bundle
          AND i.item_id = l.item_id
        WHERE i.level = ? AND ", left_cond$sql, "
      )"))
  }
  all_params <- c(all_params, list(left_level), left_cond$params)

  # Build right_matches CTE
  right_level <- path[length(path)]
  if (right_preexecuted) {
    ctes <- c(ctes, paste0("
      right_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id
        FROM items i
        WHERE i.level = ? AND ", right_cond$sql, "
      )"))
  } else {
    ctes <- c(ctes, paste0("
      right_matches AS (
        SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id
        FROM items i
        INNER JOIN labels l ON i.db_uuid = l.db_uuid
          AND i.session = l.session
          AND i.bundle = l.bundle
          AND i.item_id = l.item_id
        WHERE i.level = ? AND ", right_cond$sql, "
      )"))
  }
  all_params <- c(all_params, list(right_level), right_cond$params)

  if (length(path) == 2) {
    dir <- directions[1]
    if (dir == "down") {
      lm_col <- "from_id"
      rm_col <- "to_id"
    } else {
      lm_col <- "to_id"
      rm_col <- "from_id"
    }
    ctes <- c(ctes, paste0("
      dominance_pairs AS (
        SELECT lm.db_uuid, lm.session, lm.bundle,
               lm.item_id as left_id, rm.item_id as right_id
        FROM left_matches lm
        INNER JOIN links lnk ON lm.db_uuid = lnk.db_uuid
          AND lm.session = lnk.session
          AND lm.bundle = lnk.bundle
          AND lm.item_id = lnk.", lm_col, "
        INNER JOIN right_matches rm ON lnk.db_uuid = rm.db_uuid
          AND lnk.session = rm.session
          AND lnk.bundle = rm.bundle
          AND lnk.", rm_col, " = rm.item_id
      )"))
  } else {
    chain_sql <- build_recursive_dominance_chain(path_info)
    ctes <- c(ctes, chain_sql)
  }

  return(list(sql = paste("WITH", paste(ctes, collapse = ",\n")), params = all_params))
}

build_recursive_dominance_chain <- function(path_info) {
  path <- path_info$path
  directions <- path_info$directions

  if (length(path) <= 2) {
    .query_abort("build_recursive_dominance_chain called for direct dominance")
  }

  # Validate level names - they're used as SQL identifiers and values.
  # Level names come from DBconfig JSON (trusted) but we still validate
  # to prevent any injection through malformed config files.
  for (lvl in path) {
    if (grepl("[^A-Za-z0-9_]", lvl)) {
      .query_abort("Invalid level name in hierarchy path: {.val {lvl}}")
    }
  }

  # Validate direction values
  for (d in directions) {
    if (!d %in% c("down", "up")) {
      .query_abort("Invalid direction in hierarchy path: {.val {d}}")
    }
  }

  # Helper: for a given direction, determine which link columns to join
  # "down" = parent->child: from_id is current, to_id is next
  # "up" = child->parent: to_id is current, from_id is next
  link_cols <- function(dir) {
    if (dir == "down") list(match = "from_id", next_col = "to_id")
    else list(match = "to_id", next_col = "from_id")
  }

  joins <- c()

  for (i in 1:(length(path) - 1)) {
    lc <- link_cols(directions[i])
    if (i == 1) {
      joins <- c(joins, sprintf("
        link%d AS (
          SELECT lm.db_uuid, lm.session, lm.bundle,
                 lm.item_id as level_%s_id, lnk.%s as level_%s_id
          FROM left_matches lm
          INNER JOIN links lnk ON lm.db_uuid = lnk.db_uuid
            AND lm.session = lnk.session
            AND lm.bundle = lnk.bundle
            AND lm.item_id = lnk.%s
          INNER JOIN items i ON lnk.db_uuid = i.db_uuid
            AND lnk.session = i.session
            AND lnk.bundle = i.bundle
            AND lnk.%s = i.item_id
            AND i.level = '%s'
        )", i, path[i], lc$next_col, path[i+1], lc$match, lc$next_col, path[i+1]))
    } else if (i == length(path) - 1) {
      joins <- c(joins, sprintf("
        dominance_pairs AS (
          SELECT l%d.db_uuid, l%d.session, l%d.bundle,
                 l%d.level_%s_id as left_id, rm.item_id as right_id
          FROM link%d l%d
          INNER JOIN links lnk ON l%d.db_uuid = lnk.db_uuid
            AND l%d.session = lnk.session
            AND l%d.bundle = lnk.bundle
            AND l%d.level_%s_id = lnk.%s
          INNER JOIN right_matches rm ON lnk.db_uuid = rm.db_uuid
            AND lnk.session = rm.session
            AND lnk.bundle = rm.bundle
            AND lnk.%s = rm.item_id
        )", i-1, i-1, i-1, i-1, path[1], i-1, i-1, i-1, i-1, i-1, i-1, path[i], lc$match, lc$next_col))
    } else {
      joins <- c(joins, sprintf("
        link%d AS (
          SELECT l%d.db_uuid, l%d.session, l%d.bundle,
                 l%d.level_%s_id, lnk.%s as level_%s_id
          FROM link%d l%d
          INNER JOIN links lnk ON l%d.db_uuid = lnk.db_uuid
            AND l%d.session = lnk.session
            AND l%d.bundle = lnk.bundle
            AND l%d.level_%s_id = lnk.%s
          INNER JOIN items i ON lnk.db_uuid = i.db_uuid
            AND lnk.session = i.session
            AND lnk.bundle = i.bundle
            AND lnk.%s = i.item_id
            AND i.level = '%s'
        )", i, i-1, i-1, i-1, i-1, path[1], lc$next_col, path[i+1], i-1, i-1, i-1, i-1, i-1, i-1, path[i], lc$match, lc$next_col, path[i+1]))
    }
  }

  return(paste(joins, collapse = ",\n"))
}

# FROM-clause fragment joining every item of `parent_level` to its descendants at
# `child_level`, however many hierarchy levels lie between them (aliases `p` and
# `c`). Intermediate levels come from the DBconfig link definitions, so
# Start(Syllable, Phonetic) works through Phoneme. `hier` is get_hierarchy_info().
# @keywords internal
.hier_from_clause <- function(parent_level, child_level, hier, kind = "INNER",
                              child_cond = "") {
  path_info <- if (is.null(hier)) NULL else find_dominance_path(hier, parent_level, child_level)
  path <- if (is.null(path_info) || length(path_info$path) < 2L) c(parent_level, child_level) else path_info$path
  if (!is.null(path_info) && length(path_info$path) >= 2L && !all(path_info$directions == "down")) {
    .query_abort("{.val {parent_level}} does not dominate {.val {child_level}}.")
  }
  for (lvl in path) {
    if (grepl("[^A-Za-z0-9_]", lvl)) .query_abort("Invalid level name in hierarchy path: {.val {lvl}}")
  }
  from <- "FROM items p"
  prev <- "p"
  for (k in seq_len(length(path) - 1L)) {
    cur <- if (k == length(path) - 1L) "c" else sprintf("i%d", k)
    cond <- if (cur == "c") child_cond else sprintf(" AND %s.level = '%s'", cur, path[k + 1L])
    from <- paste0(from, sprintf("
      %s JOIN links l%d ON %s.db_uuid = l%d.db_uuid
        AND %s.session = l%d.session AND %s.bundle = l%d.bundle
        AND %s.item_id = l%d.from_id
      %s JOIN items %s ON l%d.db_uuid = %s.db_uuid
        AND l%d.session = %s.session AND l%d.bundle = %s.bundle
        AND l%d.to_id = %s.item_id%s",
      kind, k, prev, k, prev, k, prev, k, prev, k,
      kind, cur, k, cur, k, cur, k, cur, k, cur, cond))
    prev <- cur
  }
  from
}

# Position function
# Build SQL for a position function (Start / End / Medial). Returns
# list(sql, params) so the same builder is shared by the eager and lazy
# execution paths.
# @keywords internal
build_position_function_sql <- function(func_name, parent_level, child_level,
                                        operator, value, position = NULL, hier = NULL) {
  if (func_name == "Medial" && !is.null(position)) {
    position_condition <- sprintf("child_rank = %d", position)
  } else {
    position_condition <- switch(func_name,
      "Start" = "child_rank = 1",
      "End" = "child_rank = max_rank",
      "Medial" = "child_rank > 1 AND child_rank < max_rank"
    )
  }

  include_position <- switch(operator,
    "==" = value == 1,
    "=" = value == 1,
    "!=" = value != 1,
    .query_abort("Invalid operator for position function: {.val {operator}}")
  )
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
      %s
      WHERE p.level = ? AND c.level = ?
    )
    SELECT DISTINCT
      cp.db_uuid, cp.session, cp.bundle, cp.item_id,
      cp.level, cp.type, cp.seq_idx, cp.sample_rate,
      cp.sample_point, cp.sample_start, cp.sample_dur,
      l.label, l.name as attribute
    FROM child_positions cp
    INNER JOIN labels l ON cp.db_uuid = l.db_uuid
      AND cp.session = l.session
      AND cp.bundle = l.bundle
      AND cp.item_id = l.item_id
      AND l.name = ?
    WHERE %s
    ORDER BY cp.session, cp.bundle, cp.seq_idx",
    .hier_from_clause(parent_level, child_level, hier), position_condition
  )
  list(sql = sql, params = list(parent_level, child_level, child_level))
}

# Build SQL for a count function (Num). Returns list(sql, params).
# @keywords internal
build_count_function_sql <- function(parent_level, child_level, operator, value, hier = NULL) {
  valid_operators <- c("=", "==", "!=", ">", "<", ">=", "<=")
  if (!operator %in% valid_operators) {
    .query_abort("Invalid operator for count function: {.val {operator}}")
  }
  sql_op <- if (operator == "==") "=" else operator

  sql <- sprintf("
    WITH child_counts AS (
      SELECT
        p.db_uuid, p.session, p.bundle, p.item_id,
        p.level, p.type, p.seq_idx, p.sample_rate,
        p.sample_point, p.sample_start, p.sample_dur,
        COUNT(c.item_id) as child_count
      %s
      WHERE p.level = ?
      GROUP BY p.db_uuid, p.session, p.bundle, p.item_id,
               p.level, p.type, p.seq_idx, p.sample_rate,
               p.sample_point, p.sample_start, p.sample_dur
    )
    SELECT DISTINCT
      cc.db_uuid, cc.session, cc.bundle, cc.item_id,
      cc.level, cc.type, cc.seq_idx, cc.sample_rate,
      cc.sample_point, cc.sample_start, cc.sample_dur,
      l.label, l.name as attribute
    FROM child_counts cc
    INNER JOIN labels l ON cc.db_uuid = l.db_uuid
      AND cc.session = l.session
      AND cc.bundle = l.bundle
      AND cc.item_id = l.item_id
      AND l.name = ?
    WHERE cc.child_count %s ?
    ORDER BY cc.session, cc.bundle, cc.seq_idx",
    .hier_from_clause(parent_level, child_level, hier, kind = "LEFT", child_cond = " AND c.level = ?"),
    sql_op
  )
  list(
    sql = sql,
    params = list(child_level, parent_level, parent_level, as.integer(value))
  )
}



# Hierarchy functions
get_hierarchy_info <- function(db_dir) {
  # Load hierarchy dynamically from DBconfig JSON
  dbconfig <- load_DBconfig(db_dir)
  link_defs <- dbconfig$linkDefinitions
  if (is.null(link_defs) || length(link_defs) == 0) {
    .query_abort("No linkDefinitions found in DBconfig for {.path {db_dir}}")
  }
  links <- lapply(link_defs, function(ld) {
    list(type = ld$type, super = ld$superlevelName, sub = ld$sublevelName)
  })
  list(links = links)
}

can_dominate <- function(hierarchy_info, level_a, level_b) {
  result <- find_dominance_path(hierarchy_info, level_a, level_b)
  return(length(result$path) > 0)
}

find_dominance_path <- function(hierarchy_info, from_level, to_level) {
  if (from_level == to_level) {
    return(list(path = c(from_level), directions = character(0)))
  }

  # Build set of direct links for direction lookup
  link_set <- list()
  adj_list <- list()
  for (link in hierarchy_info$links) {
    super <- link$super
    sub <- link$sub
    # parent → child (down)
    if (is.null(adj_list[[super]])) adj_list[[super]] <- c()
    adj_list[[super]] <- c(adj_list[[super]], sub)
    # child → parent (up)
    if (is.null(adj_list[[sub]])) adj_list[[sub]] <- c()
    adj_list[[sub]] <- c(adj_list[[sub]], super)
    # Record link direction
    link_set[[paste0(super, "->", sub)]] <- "down"
    link_set[[paste0(sub, "->", super)]] <- "up"
  }

  queue <- list(list(level = from_level, path = c(from_level), directions = character(0)))
  visited <- c()

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    current_level <- current$level
    current_path <- current$path
    current_dirs <- current$directions

    if (current_level == to_level) {
      return(list(path = current_path, directions = current_dirs))
    }

    if (current_level %in% visited) next
    visited <- c(visited, current_level)

    neighbors <- adj_list[[current_level]]
    if (!is.null(neighbors)) {
      for (neighbor in neighbors) {
        if (!(neighbor %in% visited)) {
          dir <- link_set[[paste0(current_level, "->", neighbor)]]
          queue <- append(queue, list(list(
            level = neighbor,
            path = c(current_path, neighbor),
            directions = c(current_dirs, dir)
          )))
        }
      }
    }
  }

  return(list(path = c(), directions = c()))
}

# Deduce times for ITEM-type levels by finding time-bearing descendants
deduce_item_times <- function(result_df, db_path) {
  if (nrow(result_df) == 0) return(result_df)

  # Find rows needing time deduction (ITEM type with NULL sample_start)
  needs_times <- which(result_df$type == "ITEM" & is.na(result_df$sample_start))
  if (length(needs_times) == 0) return(result_df)

  con <- .open_query_connection(db_path)
  on.exit(DBI::dbDisconnect(con))

  has_end_id <- "end_item_id" %in% names(result_df)

  # Collect all unique (db_uuid, session, bundle, item_id) tuples that need resolution
  needed <- result_df[needs_times, c("db_uuid", "session", "bundle", "item_id"), drop = FALSE]
  if (has_end_id) {
    end_rows <- result_df[needs_times, , drop = FALSE]
    end_mask <- !is.na(end_rows[["end_item_id"]]) & end_rows[["end_item_id"]] != end_rows$item_id
    if (any(end_mask)) {
      end_needed <- end_rows[end_mask, c("db_uuid", "session", "bundle"), drop = FALSE]
      end_needed$item_id <- end_rows[["end_item_id"]][end_mask]
      needed <- rbind(needed, end_needed)
    }
  }
  needed <- unique(needed)

  # Populate temp table with only the item_ids we need

  DBI::dbExecute(con, "CREATE TEMP TABLE needed_items (db_uuid TEXT, session TEXT, bundle TEXT, item_id INTEGER)")
  if (nrow(needed) > 0) {
    DBI::dbAppendTable(con, "needed_items", needed)
  }

  # Scoped recursive CTE: only traverse from needed item_ids
  sql <- "
    WITH RECURSIVE descendants AS (
      SELECT lnk.from_id AS ancestor_id, lnk.to_id AS item_id,
             lnk.db_uuid, lnk.session, lnk.bundle
      FROM links lnk
      INNER JOIN needed_items ni ON lnk.db_uuid = ni.db_uuid
        AND lnk.session = ni.session
        AND lnk.bundle = ni.bundle
        AND lnk.from_id = ni.item_id
      UNION ALL
      SELECT d.ancestor_id, lnk.to_id,
             lnk.db_uuid, lnk.session, lnk.bundle
      FROM descendants d
      INNER JOIN links lnk ON d.db_uuid = lnk.db_uuid
        AND d.session = lnk.session
        AND d.bundle = lnk.bundle
        AND d.item_id = lnk.from_id
    )
    SELECT d.ancestor_id, d.db_uuid, d.session, d.bundle,
           MIN(i.sample_start) AS min_sample_start,
           MAX(CASE
             WHEN i.type = 'EVENT' THEN i.sample_point
             ELSE i.sample_start + i.sample_dur
           END) AS max_sample_end,
           MAX(i.sample_rate) AS sample_rate
    FROM descendants d
    INNER JOIN items i ON d.db_uuid = i.db_uuid
      AND d.session = i.session
      AND d.bundle = i.bundle
      AND d.item_id = i.item_id
    WHERE i.sample_start IS NOT NULL OR i.sample_point IS NOT NULL
    GROUP BY d.ancestor_id, d.db_uuid, d.session, d.bundle
  "

  time_info <- DBI::dbGetQuery(con, sql)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS needed_items")
  if (nrow(time_info) == 0) return(result_df)

  # Vectorized merge instead of row-by-row lookup
  # Build a key for fast matching
  ti_key <- paste(time_info$ancestor_id, time_info$db_uuid, time_info$session, time_info$bundle, sep = "\x1f")
  ti_lookup <- match  # alias for clarity

  # Process all needs_times rows vectorized
  res_ids <- result_df$item_id[needs_times]
  res_keys <- paste(res_ids, result_df$db_uuid[needs_times],
                    result_df$session[needs_times], result_df$bundle[needs_times], sep = "\x1f")
  start_match <- match(res_keys, ti_key)

  # Determine which rows are sequence spans needing end_item_id lookup
  if (has_end_id) {
    end_item_ids <- result_df[["end_item_id"]][needs_times]
    is_span <- !is.na(end_item_ids) & end_item_ids != res_ids
  } else {
    is_span <- rep(FALSE, length(needs_times))
  }

  # Single-item rows (not spans): direct assignment
  single <- !is_span & !is.na(start_match)
  if (any(single)) {
    si <- start_match[single]
    idx <- needs_times[single]
    result_df$sample_start[idx] <- time_info$min_sample_start[si]
    result_df$sample_dur[idx] <- time_info$max_sample_end[si] - time_info$min_sample_start[si]
    na_rate <- is.na(result_df$sample_rate[idx]) | result_df$sample_rate[idx] == 0
    result_df$sample_rate[idx[na_rate]] <- time_info$sample_rate[si[na_rate]]
  }

  # Sequence span rows: lookup both start and end
  if (any(is_span)) {
    span_idx <- needs_times[is_span]
    span_start_match <- start_match[is_span]
    end_keys <- paste(end_item_ids[is_span], result_df$db_uuid[span_idx],
                      result_df$session[span_idx], result_df$bundle[span_idx], sep = "\x1f")
    span_end_match <- match(end_keys, ti_key)

    both_found <- !is.na(span_start_match) & !is.na(span_end_match)
    if (any(both_found)) {
      bf_idx <- span_idx[both_found]
      bf_si <- span_start_match[both_found]
      bf_ei <- span_end_match[both_found]
      result_df$sample_start[bf_idx] <- time_info$min_sample_start[bf_si]
      result_df$sample_dur[bf_idx] <- time_info$max_sample_end[bf_ei] - time_info$min_sample_start[bf_si]
      na_rate <- is.na(result_df$sample_rate[bf_idx]) | result_df$sample_rate[bf_idx] == 0
      result_df$sample_rate[bf_idx[na_rate]] <- time_info$sample_rate[bf_si[na_rate]]
    }
  }

  result_df
}

# Result formatting
format_as_emuRsegs <- function(result_df) {
  if (nrow(result_df) == 0) {
    return(create_empty_emuRsegs())
  }
  
  # Compute times in milliseconds (emuR convention)
  # Handle EVENT type (uses sample_point), SEGMENT (sample_start/sample_dur), ITEM (may be NULL)
  is_event <- !is.na(result_df$sample_point)

  start_times <- ifelse(
    is_event,
    (result_df$sample_point / result_df$sample_rate) * 1000,
    # emuR: a SEGMENT starts half a sample before its first sample ...
    ((result_df$sample_start - 0.5) / result_df$sample_rate) * 1000
  )
  # emuR returns 0 for EVENT end times
  end_times <- ifelse(
    is_event,
    0,
    # ... and ends half a sample after its last one.
    ((result_df$sample_start + result_df$sample_dur + 0.5) / result_df$sample_rate) * 1000
  )
  
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
    attribute = if ("attribute" %in% names(result_df)) result_df$attribute else result_df$level,
    start_item_seq_idx = result_df$seq_idx,
    end_item_seq_idx = if(has_end_seq) result_df$end_seq_idx else result_df$seq_idx,
    type = result_df$type,
    sample_start = ifelse(is_event, result_df$sample_point, result_df$sample_start),
    sample_end = ifelse(is_event, result_df$sample_point, result_df$sample_start + result_df$sample_dur),
    sample_rate = result_df$sample_rate,
    stringsAsFactors = FALSE
  )
  
  # Convert to tibble to match emuR output
  emuRsegs_df <- tibble::as_tibble(emuRsegs_df)
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
  empty_df <- tibble::as_tibble(empty_df)
  class(empty_df) <- c("emuRsegs", class(empty_df))
  return(empty_df)
}

# ---------------------------------------------------------------------------
# Dominance with sequence operands
#
# A sequence result is a SPAN of consecutive items on one level, not a single
# item. A span on the dominating side dominates whatever any of its items
# dominates; a span on the dominated side is dominated by an item only if the
# item dominates ALL of its members. Compiled entirely in SQL: each operand
# becomes a `units` relation (one row per item or span), expanded to its
# member items, joined through the hierarchy, and mapped back to units.
# ---------------------------------------------------------------------------

.dominance_operand <- function(db_path, query, con, tag) {
  level <- .resolve_level_attribute(con, extract_level_from_query(query),
                                    extract_level_from_query(query))
  if (query$type == "sequence") {
    plain <- query
    plain$left$projection <- FALSE
    plain$right$projection <- FALSE
    span <- build_base_sql(db_path, plain, list(), con = con)
    if (is.null(span)) return(NULL)
    proj <- if (isTRUE(query$left$projection)) "start" else if (isTRUE(query$right$projection)) "end" else "both"
    return(list(
      tag = tag, level = level$level, attribute = level$attribute, kind = "span", proj = proj,
      span_sql = span$sql, span_params = span$params,
      units_sql = paste0("SELECT db_uuid, session, bundle, item_id AS unit_id, end_item_id AS unit_end FROM (",
                         span$sql, ")"),
      units_params = span$params))
  }
  if (query$type == "simple") {
    cond <- extract_condition_from_query(query)
    return(list(
      tag = tag, level = level$level, attribute = level$attribute, kind = "item",
      units_sql = paste0(
        "SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id AS unit_id, i.item_id AS unit_end ",
        "FROM items i INNER JOIN labels l ON i.db_uuid = l.db_uuid AND i.session = l.session ",
        "AND i.bundle = l.bundle AND i.item_id = l.item_id WHERE i.level = ? AND ", cond$sql),
      units_params = c(list(level$level), cond$params)))
  }
  sub <- build_base_sql(db_path, query, list(), con = con)
  if (is.null(sub)) return(NULL)
  list(
    tag = tag, level = level$level, attribute = level$attribute, kind = "item",
    units_sql = paste0("SELECT db_uuid, session, bundle, item_id AS unit_id, item_id AS unit_end FROM (",
                       sub$sql, ")"),
    units_params = sub$params)
}

build_span_dominance_sql <- function(db_path, parsed_query, result_level, con) {
  left <- .dominance_operand(db_path, parsed_query$left, con, "l")
  right <- .dominance_operand(db_path, parsed_query$right, con, "r")
  if (is.null(left) || is.null(right)) return(NULL)

  hier <- get_hierarchy_info(dirname(db_path))
  path <- find_dominance_path(hier, left$level, right$level)
  if (length(path$path) < 2L || !all(path$directions == "down" | path$directions == "up")) {
    .query_abort("No dominance path between {.val {left$level}} and {.val {right$level}}")
  }
  left_is_parent <- all(path$directions == "down")
  parent_level <- if (left_is_parent) left$level else right$level
  child_level <- if (left_is_parent) right$level else left$level
  lcol <- if (left_is_parent) "parent_id" else "child_id"
  rcol <- if (left_is_parent) "child_id" else "parent_id"

  members <- function(o) sprintf("
    %s_members AS (
      SELECT u.db_uuid, u.session, u.bundle, u.unit_id, u.unit_end, m.item_id AS item_id
      FROM %s_units u
      JOIN items a ON a.db_uuid = u.db_uuid AND a.session = u.session
        AND a.bundle = u.bundle AND a.item_id = u.unit_id
      JOIN items b ON b.db_uuid = u.db_uuid AND b.session = u.session
        AND b.bundle = u.bundle AND b.item_id = u.unit_end
      JOIN items m ON m.db_uuid = u.db_uuid AND m.session = u.session
        AND m.bundle = u.bundle AND m.level = a.level
        AND m.seq_idx BETWEEN a.seq_idx AND b.seq_idx
    )", o$tag, o$tag)

  # The dominated (child) side must be dominated as a whole: every member of
  # its span by the same parent unit. The parent side needs only one member.
  ctag <- if (left_is_parent) "r" else "l"
  cm <- if (left_is_parent) "rm" else "lm"

  ctes <- c(
    sprintf("l_units AS (%s)", left$units_sql),
    sprintf("r_units AS (%s)", right$units_sql),
    members(left), members(right),
    sprintf("
    pairs AS (
      SELECT p.db_uuid, p.session, p.bundle, p.item_id AS parent_id, c.item_id AS child_id
      %s
      WHERE p.level = '%s' AND c.level = '%s'
    )", .hier_from_clause(parent_level, child_level, hier), parent_level, child_level),
    sprintf("
    unit_pairs AS (
      SELECT lm.db_uuid, lm.session, lm.bundle, lm.unit_id AS l_unit, lm.unit_end AS l_end,
             rm.unit_id AS r_unit, rm.unit_end AS r_end
      FROM pairs pr
      JOIN l_members lm ON lm.db_uuid = pr.db_uuid AND lm.session = pr.session
        AND lm.bundle = pr.bundle AND lm.item_id = pr.%s
      JOIN r_members rm ON rm.db_uuid = pr.db_uuid AND rm.session = pr.session
        AND rm.bundle = pr.bundle AND rm.item_id = pr.%s
      GROUP BY lm.db_uuid, lm.session, lm.bundle, lm.unit_id, lm.unit_end, rm.unit_id, rm.unit_end
      HAVING COUNT(DISTINCT %s.item_id) = (
        SELECT COUNT(*) FROM %s_members c2
        WHERE c2.db_uuid = %s.db_uuid AND c2.session = %s.session
          AND c2.bundle = %s.bundle AND c2.unit_id = %s.unit_id)
    )", lcol, rcol, cm, ctag, cm, cm, cm, cm))
  params <- c(left$units_params, right$units_params)

  # Which operand supplies the result rows (same rule as plain dominance).
  res_left <- if (is.null(result_level)) {
    !(isTRUE(parsed_query$right$projection) && !isTRUE(parsed_query$left$projection))
  } else result_level == left$level
  res <- if (res_left) left else right
  unit <- if (res_left) "l_unit" else "r_unit"
  unit_end <- if (res_left) "l_end" else "r_end"

  head <- paste("WITH", paste(ctes, collapse = ",\n"))
  if (res$kind == "span" && res$proj == "both") {
    sql <- paste0(head, "
      SELECT s.* FROM (", res$span_sql, ") s
      WHERE (s.db_uuid, s.session, s.bundle, s.item_id) IN (
        SELECT db_uuid, session, bundle, ", unit, " FROM unit_pairs)
      ORDER BY s.session, s.bundle, s.seq_idx")
    params <- c(params, res$span_params)
  } else {
    pick <- if (res$kind == "span" && res$proj == "end") unit_end else unit
    sql <- paste0(head, "
      SELECT DISTINCT i.db_uuid, i.session, i.bundle, i.item_id,
        i.level, i.type, i.seq_idx, i.sample_rate,
        i.sample_point, i.sample_start, i.sample_dur,
        l.label, l.name as attribute
      FROM unit_pairs up
      INNER JOIN items i ON i.db_uuid = up.db_uuid AND i.session = up.session
        AND i.bundle = up.bundle AND i.item_id = up.", pick, "
      INNER JOIN labels l ON i.db_uuid = l.db_uuid AND i.session = l.session
        AND i.bundle = l.bundle AND i.item_id = l.item_id AND l.name = ?
      WHERE i.level = ?
      ORDER BY i.session, i.bundle, i.seq_idx")
    params <- c(params, list(res$attribute, res$level))
  }
  list(sql = sql, params = params)
}
