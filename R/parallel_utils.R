# ==============================================================================
# WORKER POLICY
#
# Every parallel entry point funnels through these helpers. Worker processes are
# full R sessions that re-load reindeer and its dependencies, which costs about
# 1.1 s for nine workers on a 10-core machine - far more than the work itself
# for a few dozen segments or a handful of bundles. The rule is therefore:
# never spawn more workers than there are units of work, and never spawn any
# when the work is in-process anyway.
# ==============================================================================

#' Can a fresh R session load reindeer?
#'
#' `future::multisession` workers start clean sessions and resolve packages
#' through `.libPaths()`. Under `devtools::load_all()` the package is loaded but
#' not installed, so worker-based code paths cannot run and must fall back to
#' the sequential branch instead of erroring.
#'
#' @return Logical
#' @noRd
.workers_can_load_reindeer <- function() {
  any(file.exists(file.path(.libPaths(), "reindeer", "DESCRIPTION")))
}

#' Worker count for a unit of work
#'
#' @param n_units Number of independent work units (files, bundles, segments).
#' @param requested Explicit count, e.g. from a `.workers` argument.
#' @return Integer; 1 means "stay in this process".
#' @noRd
.reindeer_workers <- function(n_units, requested = NULL) {
  n_units <- suppressWarnings(as.integer(n_units)[1])
  if (is.na(n_units) || n_units < 2L) {
    return(1L)
  }

  requested <- requested %||% getOption("reindeer.workers", NULL)
  cores <- if (is.null(requested)) {
    suppressWarnings(as.integer(future::availableCores())[1])
  } else {
    suppressWarnings(as.integer(requested)[1])
  }
  if (is.na(cores)) cores <- 1L
  if (cores < 1L) cores <- 1L

  max(1L, min(cores, n_units))
}

#' Should this workload run in parallel?
#'
#' @param n_units Number of independent work units.
#' @param requested Explicit worker count.
#' @return Logical scalar.
#' @noRd
.use_parallel_workers <- function(n_units, requested = NULL) {
  .reindeer_workers(n_units, requested) > 1L && .workers_can_load_reindeer()
}

#' Run an expression under a multisession plan sized to the work
#'
#' The plan is restored afterwards, so callers keep whatever plan they had. When
#' there is nothing to parallelise (or nothing that can run in parallel), the
#' expression runs in-process with no plan churn at all.
#'
#' @param n_units Number of independent work units.
#' @param requested Explicit worker count.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#' @noRd
.with_worker_plan <- function(n_units, requested = NULL, expr) {
  if (!.use_parallel_workers(n_units, requested)) {
    return(expr)
  }
  workers <- .reindeer_workers(n_units, requested)
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
  expr
}
