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

#' Abort with a "missing companion package" condition.
#'
#' Raised when a reindeer entry point depends on a companion package
#' (erodex, protoscribe, eggstract, superassp) that is not installed.
#' Catch with `tryCatch(..., reindeer_missing_companion_error = ...)`.
#'
#' @param pkg Character; the missing package name.
#' @param purpose Optional short string describing what the user was
#'   trying to do (e.g. "draft annotation generation").
#' @keywords internal
.companion_abort <- function(pkg, purpose = NULL,
                             call = rlang::caller_env()) {
  url <- switch(pkg,
    erodex     = "https://github.com/humlab-speech/erodex",
    protoscribe = "https://github.com/humlab-speech/protoscribe",
    eggstract  = "https://github.com/humlab-speech/eggstract",
    superassp  = "https://github.com/humlab-speech/superassp",
    NULL
  )
  msg <- c(
    sprintf("The {.pkg %s} package is required%s but is not installed.",
            pkg, if (!is.null(purpose)) sprintf(" for %s", purpose) else ""),
    "i" = if (!is.null(url)) sprintf("Install: {.code remotes::install_github(\"%s\")}",
                                      sub("^https://github.com/", "", url))
          else sprintf("Install: {.code install.packages(\"%s\")}", pkg)
  )
  cli::cli_abort(msg, call = call,
                 class = c("reindeer_missing_companion_error", "reindeer_error"))
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
