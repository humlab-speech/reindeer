# ==============================================================================
# PARALLELISM POLICY
#
# reindeer never creates or changes a `future::plan()`. Parallel sections use
# `future.apply::future_lapply()`, so the caller's plan decides: sequential by
# default, parallel after e.g. `future::plan(future::multisession)`. Worker
# payloads carry file paths and plain data, never database connections.
# ==============================================================================

#' Warn once about the removed .parallel/.workers/.optimize arguments
#'
#' @param ... Named arguments as supplied by the caller (NULL = not supplied).
#' @noRd
.warn_ignored_parallel_args <- function(...) {
  args <- list(...)
  given <- names(args)[!vapply(args, is.null, logical(1))]
  if (length(given) > 0) {
    cli::cli_warn(
      c("{.arg {given}} {?is/are} ignored.",
        "i" = "Set the number of workers with {.code future::plan()} instead."),
      class = "reindeer_deprecated"
    )
  }
  invisible(NULL)
}
