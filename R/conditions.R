# ============================================================================
# Classed conditions for reindeer
# ----------------------------------------------------------------------------
# Three thin wrappers around cli::cli_abort that attach a stable subclass:
#
#   reindeer_query_error   - EQL parsing, query execution, schema mismatches
#                            in the query path.
#   reindeer_schema_error  - JSON-schema / DBconfig validation failures.
#   reindeer_cache_error   - SQLite cache, quantify cache, persistent cache
#                            issues.
#
# Downstream packages and user code can catch these with tryCatch():
#
#   tryCatch(query(corp, "bad EQL"),
#            reindeer_query_error = function(e) { ... })
#
# All three inherit from "reindeer_error" so a single handler can catch
# any reindeer-originated abort.
# ============================================================================

#' @keywords internal
.query_abort <- function(message, ..., call = rlang::caller_env(),
                         .envir = rlang::caller_env()) {
  cli::cli_abort(message, ..., call = call, .envir = .envir,
                 class = c("reindeer_query_error", "reindeer_error"))
}

#' @keywords internal
.schema_abort <- function(message, ..., call = rlang::caller_env(),
                          .envir = rlang::caller_env()) {
  cli::cli_abort(message, ..., call = call, .envir = .envir,
                 class = c("reindeer_schema_error", "reindeer_error"))
}

#' @keywords internal
.cache_abort <- function(message, ..., call = rlang::caller_env(),
                         .envir = rlang::caller_env()) {
  cli::cli_abort(message, ..., call = call, .envir = .envir,
                 class = c("reindeer_cache_error", "reindeer_error"))
}

#' @keywords internal
.query_warn <- function(message, ..., call = rlang::caller_env(),
                        .envir = rlang::caller_env()) {
  cli::cli_warn(message, ..., call = call, .envir = .envir,
                class = c("reindeer_query_warning", "reindeer_warning"))
}

#' Render an EQL string with a caret pointer at `pos` (1-indexed).
#'
#' Used inside `.query_abort` calls so parse failures show *where* in the
#' input the parser gave up. `pos = NA` falls back to a plain string.
#' Returns a character vector with three lines: a label, the input, and
#' the caret line. Designed to be passed straight into cli::cli_abort.
#'
#' @keywords internal
.eql_caret <- function(query_string, pos = NA_integer_,
                       label = "Input:") {
  if (is.na(pos) || pos < 1L || pos > nchar(query_string) + 1L) {
    return(c("i" = sprintf("%s {.code %s}", label, query_string)))
  }
  caret <- paste0(strrep(" ", pos - 1L), "^")
  c(
    "i" = sprintf("%s {.code %s}", label, query_string),
    " " = sprintf("       %s", caret)
  )
}
