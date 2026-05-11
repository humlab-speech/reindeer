# ==============================================================================
# Pipe-loss provenance for segment_list (Item 2)
# ==============================================================================
#
# Records the row count delta across every verb that touches a segment_list,
# so users can answer "where did 27 of my 100 segments go?" without manual
# bookkeeping. Provenance lives as a tibble on the `reindeer_provenance`
# attribute of the segment_list and is preserved through tidyverse verbs via
# the dplyr_reconstruct hook.

# Internal: empty provenance tibble with the canonical column structure
.empty_provenance <- function() {
  tibble::tibble(
    step = integer(),
    verb = character(),
    call = character(),
    rows_in = integer(),
    rows_out = integer(),
    rows_lost = integer(),
    ts = as.POSIXct(character())
  )
}

# Internal: append one row to a provenance tibble, applying the truncation cap
.append_provenance_row <- function(prov, verb, call, rows_in, rows_out) {
  if (is.null(prov)) prov <- .empty_provenance()
  call_str <- if (is.null(call)) NA_character_ else paste(deparse(call), collapse = " ")
  new_row <- tibble::tibble(
    step = nrow(prov) + 1L,
    verb = as.character(verb),
    call = call_str,
    rows_in = as.integer(rows_in),
    rows_out = as.integer(rows_out),
    rows_lost = as.integer(rows_in) - as.integer(rows_out),
    ts = Sys.time()
  )
  out <- vctrs::vec_rbind(prov, new_row)
  cap <- getOption("reindeer.provenance_max", 1000L)
  if (nrow(out) > cap) {
    out <- out[(nrow(out) - cap + 1L):nrow(out), ]
    cli::cli_warn(c(
      "Provenance log truncated to last {cap} entries.",
      "i" = "Set {.code options(reindeer.provenance_max = N)} to raise the cap."
    ))
  }
  out
}

# Internal: row count or NA for arbitrary objects (handles non-data.frame inputs)
.nrow_or_na <- function(x) {
  if (is.data.frame(x)) nrow(x) else NA_integer_
}

# Internal: warn when row loss exceeds threshold (default 25%)
.maybe_warn_loss <- function(verb, rows_in, rows_out) {
  if (is.na(rows_in) || rows_in <= 0L) return(invisible())
  thr <- getOption("reindeer.loss_warn", 0.25)
  lost <- rows_in - rows_out
  if (lost / rows_in > thr) {
    cli::cli_warn(
      "{verb}: {lost} of {rows_in} row{?s} lost ({sprintf('%.1f', 100 * lost / rows_in)}%)"
    )
  }
}

# Internal: append a provenance step to `out`, copying the log from `from`
# so dplyr/vctrs reconstructs preserve the running history.
#
# `warn` controls whether loss exceeding the threshold emits a cli warning.
# Navigation verbs (scout/ascend_to/descend_to) warn by default because their
# row drops are silent; user-explicit ops (dplyr verbs, bracket subsetting)
# default to silent because the user already chose to filter.
.record_step <- function(out, from, verb, call = NULL, warn = TRUE) {
  prov <- if (!is.null(from)) attr(from, "reindeer_provenance") else NULL
  rows_in <- .nrow_or_na(from)
  rows_out <- .nrow_or_na(out)
  attr(out, "reindeer_provenance") <- .append_provenance_row(
    prov, verb, call, rows_in, rows_out
  )
  if (isTRUE(warn)) {
    .maybe_warn_loss(verb, rows_in, rows_out)
  }
  out
}

# Internal: seed the provenance log on the first step (no input segment)
.seed_provenance <- function(out, verb, call = NULL) {
  attr(out, "reindeer_provenance") <- .append_provenance_row(
    NULL, verb, call, NA_integer_, .nrow_or_na(out)
  )
  out
}

#' Inspect the provenance log of a segment_list
#'
#' Returns a tibble describing each pipeline step that touched the
#' `segment_list`: the verb name, the deparsed call, input/output row counts,
#' and the timestamp. Use this to surface silent row drops introduced by
#' navigation verbs (`scout`, `ascend_to`, `descend_to`) or dplyr filters.
#'
#' @param seg A `segment_list` or `extended_segment_list`.
#' @return A tibble with columns `step`, `verb`, `call`, `rows_in`,
#'   `rows_out`, `rows_lost`, `ts`. Empty if no provenance has been recorded.
#' @examplesIf interactive()
#' segs <- query(corp, "Phonetic == t")
#' segs <- scout(segs, 1)
#' provenance(segs)
#' @export
provenance <- function(seg) {
  prov <- attr(seg, "reindeer_provenance")
  if (is.null(prov)) return(.empty_provenance())
  prov
}

#' Cumulative or per-step row loss for a segment_list pipeline
#'
#' @param seg A `segment_list`.
#' @param step Optional integer step index. If NULL (default), returns the
#'   cumulative number of rows lost across the entire pipeline. Otherwise
#'   returns the rows lost at the given step.
#' @return An integer count.
#' @examplesIf interactive()
#' segs <- query(corp, "Phonetic == t") |> scout(1) |> ascend_to("Word")
#' dropped(segs)        # cumulative
#' dropped(segs, 2L)    # how many segs scout() dropped
#' @export
dropped <- function(seg, step = NULL) {
  prov <- provenance(seg)
  if (nrow(prov) == 0L) return(0L)
  if (is.null(step)) {
    return(sum(prov$rows_lost, na.rm = TRUE))
  }
  if (!is.numeric(step) || length(step) != 1L) {
    cli::cli_abort("{.arg step} must be a single integer")
  }
  step <- as.integer(step)
  if (step < 1L || step > nrow(prov)) {
    cli::cli_abort("{.arg step} {step} out of range (1:{nrow(prov)})")
  }
  prov$rows_lost[step]
}
