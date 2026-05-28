# ==============================================================================
# Pipe-loss provenance for segment_list (Item 2)
# ==============================================================================
#
# Records the row count delta across every verb that touches a segment_list,
# so users can answer "where did 27 of my 100 segments go?" without manual
# bookkeeping. Provenance is stored internally as a list-of-row-records on
# the `reindeer_provenance` attribute (cheap to append to). When the user
# calls provenance() / dropped() / dropped_rows() we materialise that list
# into a tibble on demand. The list form replaces the earlier tibble-per-
# step vctrs::vec_rbind which copied the entire history on every verb.

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

# Internal: coerce whatever the attribute currently holds into the list form
# used by the new append path. Accepts:
#   - NULL                -> empty list
#   - a list of records   -> returned as-is
#   - a tibble (legacy)   -> converted row-by-row into a list of records
.coerce_provenance_to_list <- function(prov) {
  if (is.null(prov)) return(list())
  if (is.list(prov) && !is.data.frame(prov)) return(prov)
  if (is.data.frame(prov) && nrow(prov) > 0L) {
    return(lapply(seq_len(nrow(prov)), function(i) {
      list(
        verb = as.character(prov$verb[i]),
        call = as.character(prov$call[i]),
        rows_in = as.integer(prov$rows_in[i]),
        rows_out = as.integer(prov$rows_out[i]),
        rows_lost = as.integer(prov$rows_lost[i]),
        ts = prov$ts[i]
      )
    }))
  }
  list()
}

# Internal: convert the list-of-records form back into the public tibble form
.provenance_list_to_tibble <- function(prov_list) {
  if (!is.list(prov_list) || length(prov_list) == 0L) return(.empty_provenance())
  tibble::tibble(
    step = seq_along(prov_list),
    verb = vapply(prov_list, function(e) as.character(e$verb), character(1)),
    call = vapply(prov_list, function(e) {
      v <- e$call
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1)),
    rows_in = vapply(prov_list, function(e) {
      v <- e$rows_in
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    rows_out = vapply(prov_list, function(e) {
      v <- e$rows_out
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    rows_lost = vapply(prov_list, function(e) {
      v <- e$rows_lost
      if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1)),
    ts = do.call(c, lapply(prov_list, function(e) {
      if (is.null(e$ts)) as.POSIXct(NA) else e$ts
    }))
  )
}

# Internal: append one record to a provenance list, applying the truncation cap.
# Returns the updated list (list-mode storage avoids vctrs::vec_rbind copying
# the full history on every verb).
.append_provenance_row <- function(prov, verb, call, rows_in, rows_out) {
  prov_list <- .coerce_provenance_to_list(prov)
  call_str <- if (is.null(call)) NA_character_ else paste(deparse(call), collapse = " ")
  new_entry <- list(
    verb = as.character(verb),
    call = call_str,
    rows_in = as.integer(rows_in),
    rows_out = as.integer(rows_out),
    rows_lost = as.integer(rows_in) - as.integer(rows_out),
    ts = Sys.time()
  )
  prov_list[[length(prov_list) + 1L]] <- new_entry

  cap <- getOption("reindeer.provenance_max", 1000L)
  if (length(prov_list) > cap) {
    prov_list <- prov_list[(length(prov_list) - cap + 1L):length(prov_list)]
    cli::cli_warn(c(
      "Provenance log truncated to last {cap} entries.",
      "i" = "Set {.code options(reindeer.provenance_max = N)} to raise the cap."
    ))
  }
  prov_list
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
  if (S7::S7_inherits(seg, lazy_segment_list)) {
    seg <- collect(seg)
  }
  prov <- attr(seg, "reindeer_provenance")
  if (is.null(prov)) return(.empty_provenance())
  # Internal storage is list-of-records; the public form is a tibble.
  # Legacy tibble-form attributes still round-trip cleanly through
  # the list->tibble materialisation path.
  if (is.data.frame(prov)) return(prov)
  .provenance_list_to_tibble(prov)
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

#' Per-step row-loss summary for a segment_list pipeline
#'
#' Tidy companion to [provenance()] and [dropped()] for debugging
#' filter/scout/ascend chains. Returns a one-row-per-step tibble with
#' the verb that ran, how many rows entered and left, and how many were
#' dropped. Use this when [dropped()] tells you something was lost and
#' you want to know *where* in the pipeline.
#'
#' @param seg A `segment_list` (or `extended_segment_list` /
#'   `lazy_segment_list`, which is collected first).
#' @return A tibble with one row per step: `step`, `verb`, `call`,
#'   `rows_in`, `rows_out`, `rows_lost`, `pct_lost`.
#' @examplesIf interactive()
#' segs <- query(corp, "Phonetic == t") |>
#'         dplyr::filter(label != "never_matches") |>
#'         scout(1)
#' dropped_rows(segs)
#' @export
dropped_rows <- function(seg) {
  prov <- provenance(seg)
  if (nrow(prov) == 0L) {
    return(tibble::tibble(
      step = integer(),
      verb = character(),
      call = character(),
      rows_in = integer(),
      rows_out = integer(),
      rows_lost = integer(),
      pct_lost = numeric()
    ))
  }
  pct <- ifelse(is.na(prov$rows_in) | prov$rows_in == 0,
                NA_real_,
                100 * prov$rows_lost / prov$rows_in)
  tibble::tibble(
    step = prov$step,
    verb = prov$verb,
    call = prov$call,
    rows_in = prov$rows_in,
    rows_out = prov$rows_out,
    rows_lost = prov$rows_lost,
    pct_lost = round(pct, 1)
  )
}
