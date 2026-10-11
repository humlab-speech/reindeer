# EQL tokenizer + recursive-descent parser.
#
#   expr    := operand ( OP operand )*          precedence, weakest first:
#                                                 &  <  |  <  ->  <  ^   (left associative)
#   operand := '[' expr ']' | TERM
#   TERM    := [#]Level[:Attr] (==|!=|=~|!~|=) value
#            | Start|End|Medial|Num '(' Level ',' Level [',' n] ')' [cmp n]
#            | Session|Bundle cmp value | bare Level
#
# Result: the nested-list AST the executors consume (type = simple /
# scope_filter / function / sequence / dominance / conjunction / disjunction).
# An operator only separates operands when what follows it starts an operand,
# so label lists (`p|t|k`), regexes (`=~ ^a|b`), quoted values and labels such
# as `R&B` stay values. `->` and `^` are binary: chains must be bracketed.

# What can start an operand: `Level[:Attr] <cmp>`, a bracket, or a function.
.eql_operand_start <- "^\\s*(#?[A-Za-z_]+(:[A-Za-z_]+)?\\s*(==|!=|=~|!~|=)|\\[|(Start|End|Medial|Num)\\()"

.eql_ops <- c("&" = 1L, "|" = 2L, "->" = 3L, "^" = 4L)
.eql_node_type <- c("&" = "conjunction", "|" = "disjunction",
                    "->" = "sequence", "^" = "dominance")

# Does an operator at position `k` of `q` really separate operands? Returns
# its length, or 0.
.eql_op_at <- function(q, k) {
  rest <- substr(q, k, nchar(q))
  op <- if (startsWith(rest, "->")) "->" else if (substr(rest, 1, 1) %in% c("&", "|", "^")) substr(rest, 1, 1) else ""
  if (!nzchar(op)) return(0L)
  after <- substr(rest, nchar(op) + 1L, nchar(rest))
  # A dangling operator (nothing, or a closing bracket, follows) is still an
  # operator: it is reported as such instead of being swallowed into a value.
  if (grepl("^\\s*\\]*\\s*$", after)) return(nchar(op))
  if (grepl(.eql_operand_start, after) ||
      (op %in% c("->", "^") && grepl("^\\s*#?[A-Za-z_]+(:[A-Za-z_]+)?\\s*\\]*\\s*$", after))) {
    return(nchar(op))
  }
  0L
}

# Index just past a quoted value that starts at `j`.
.eql_skip_quoted <- function(chars, j) {
  n <- length(chars)
  close <- j + 1L
  while (close <= n && chars[close] != chars[j]) close <- close + 1L
  min(close, n) + 1L
}

# End index (inclusive) of the operand that starts at `i`.
.eql_operand_end <- function(q, chars, i) {
  n <- length(chars)
  j <- i
  paren <- 0L
  in_value <- FALSE
  vbracket <- 0L
  while (j <= n) {
    ch <- chars[j]
    if (!in_value) {
      cmp <- if (paren == 0L && j < n && substr(q, j, j + 1L) %in% c("==", "!=", "=~", "!~")) 2L
             else if (paren == 0L && ch == "=") 1L else 0L
      if (ch == "(") {
        paren <- paren + 1L
      } else if (ch == ")") {
        paren <- max(0L, paren - 1L)
      } else if (paren == 0L && ch == "]") {
        return(j - 1L)
      } else if (cmp > 0L) {
        in_value <- TRUE
        j <- j + cmp
        while (j <= n && chars[j] %in% c(" ", "\t")) j <- j + 1L
        value_start <- j
        if (j <= n && chars[j] %in% c("'", "\"")) j <- .eql_skip_quoted(chars, j)
        next
      } else if (paren == 0L && ch %in% c(" ", "\t")) {
        k <- j
        while (k <= n && chars[k] %in% c(" ", "\t")) k <- k + 1L
        if (k > n) return(j - 1L)
        if (.eql_op_at(q, k) > 0L || chars[k] == "]") return(j - 1L)
      }
    } else {
      if (vbracket == 0L && j > value_start && ch %in% c("&", "|", "^", "-")) {
        prev <- j - 1L
        while (prev > value_start && chars[prev] %in% c(" ", "\t")) prev <- prev - 1L
        if (!chars[prev] %in% c("=", "~", "|") && .eql_op_at(q, j) > 0L) return(j - 1L)
      }
      if (ch == "[") {
        vbracket <- vbracket + 1L
      } else if (ch == "]") {
        if (vbracket == 0L) return(j - 1L)
        vbracket <- vbracket - 1L
      } else if (ch %in% c(" ", "\t") && vbracket == 0L) {
        k <- j
        while (k <= n && chars[k] %in% c(" ", "\t")) k <- k + 1L
        if (k > n) return(j - 1L)
        if (.eql_op_at(q, k) > 0L || chars[k] == "]") return(j - 1L)
      }
    }
    j <- j + 1L
  }
  n
}

.eql_tokenize <- function(q) {
  chars <- strsplit(q, "")[[1]]
  n <- length(chars)
  toks <- list()
  push <- function(type, text, pos) {
    toks[[length(toks) + 1L]] <<- list(type = type, text = text, pos = pos)
  }
  missing_operand <- function(pos) {
    .query_abort(c("Operator is missing an operand.", .eql_caret(q, pos = pos, label = "Query:")))
  }
  i <- 1L
  want_operand <- TRUE
  while (i <= n) {
    ch <- chars[i]
    if (ch %in% c(" ", "\t", "\n")) { i <- i + 1L; next }
    if (ch == "[" && want_operand) { push("LB", "[", i); i <- i + 1L; next }
    if (ch == "]") {
      if (want_operand && length(toks) > 0L && toks[[length(toks)]]$type == "OP") {
        missing_operand(toks[[length(toks)]]$pos)
      }
      push("RB", "]", i)
      i <- i + 1L
      want_operand <- FALSE
      next
    }
    if (!want_operand) {
      len <- .eql_op_at(q, i)
      if (len == 0L) {
        .query_abort(c("Unexpected text in query.", .eql_caret(q, pos = i, label = "Query:")))
      }
      push("OP", substr(q, i, i + len - 1L), i)
      i <- i + len
      want_operand <- TRUE
      next
    }
    if (ch %in% c("&", "|", "^") || substr(q, i, i + 1L) == "->") missing_operand(i)
    j <- .eql_operand_end(q, chars, i)
    push("TERM", trimws(substr(q, i, j)), i)
    i <- j + 1L
    want_operand <- FALSE
  }
  if (length(toks) > 0L && toks[[length(toks)]]$type == "OP") {
    missing_operand(toks[[length(toks)]]$pos)
  }
  toks
}

.eql_parse_term <- function(text) {
  if (grepl("^(Start|End|Medial|Num)\\(", text)) return(parse_function_query(text))
  parse_simple_query(text)
}

parse_eql_query <- function(query_string) {
  q <- trimws(query_string)
  toks <- .eql_tokenize(q)
  if (length(toks) == 0L) {
    .query_abort(c("Empty query.", .eql_caret(q, pos = 1L, label = "Query:")))
  }
  pos <- 1L
  peek <- function() if (pos <= length(toks)) toks[[pos]] else NULL
  advance <- function() { t <- toks[[pos]]; pos <<- pos + 1L; t }

  parse_operand <- function() {
    t <- peek()
    if (is.null(t)) {
      .query_abort(c("Unexpected end of query.",
                     .eql_caret(q, pos = nchar(q) + 1L, label = "Query:")))
    }
    if (t$type == "LB") {
      advance()
      node <- parse_expr(1L)
      close <- peek()
      if (is.null(close) || close$type != "RB") {
        .query_abort(c("Missing closing bracket {.val ]}.",
                       .eql_caret(q, pos = t$pos, label = "Query:")))
      }
      advance()
      return(node)
    }
    if (t$type == "TERM") {
      advance()
      return(.eql_parse_term(t$text))
    }
    .query_abort(c("Unexpected {.val {t$text}}.", .eql_caret(q, pos = t$pos, label = "Query:")))
  }

  parse_expr <- function(min_prec) {
    left <- parse_operand()
    combined_by <- ""
    repeat {
      t <- peek()
      if (is.null(t) || t$type != "OP" || .eql_ops[[t$text]] < min_prec) break
      # Sequences and dominance are binary: `A -> B -> C` has no single
      # reading, so nesting must be explicit (like emuR, which rejects it).
      if (t$text %in% c("->", "^") && identical(combined_by, t$text)) {
        op <- t$text
        .query_abort(c(
          "{.val {op}} is binary; bracket the operands to chain it.",
          "i" = "Write {.code [[A {op} B] {op} C]} or {.code [A {op} [B {op} C]]}.",
          .eql_caret(q, pos = t$pos, label = "Query:")))
      }
      advance()
      right <- parse_expr(.eql_ops[[t$text]] + 1L)
      left <- list(type = .eql_node_type[[t$text]], left = left, right = right)
      combined_by <- t$text
    }
    left
  }

  node <- parse_expr(1L)
  t <- peek()
  if (!is.null(t)) {
    .query_abort(c(
      if (t$type == "RB") "Unbalanced closing bracket." else "Unexpected input after the query.",
      .eql_caret(q, pos = t$pos, label = "Query:")))
  }
  node
}
