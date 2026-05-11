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
