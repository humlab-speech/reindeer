# ============================================================================
# Deprecated stubs for functionality moved to companion packages
# ============================================================================
#
# Provides a helpful redirect for users upgrading from older reindeer
# versions whose scripts still call these symbols unqualified.

# --- eggstract (retired in v1.3, never had a working companion target) ------

#' @rdname deprecated-moved-functions
#' @export
enrich_egg <- function(...) {
  cli::cli_abort(c(
    "{.fn enrich_egg} has been removed from {.pkg reindeer}.",
    "i" = "It forwarded to {.code eggstract::enrich_with_egg}, which {.pkg eggstract} has never exported.",
    "i" = "Use {.fn quantify_egg} for EGG-track measurement via {.pkg eggstract}'s {.fn trk_f0} (and friends).",
    "i" = "See {.url https://github.com/humlab-speech/eggstract}."
  ), class = c("reindeer_moved_error", "reindeer_error"))
}

# --- reindeer itself (quantify unification, v1.2.0) --------------------------

#' @rdname deprecated-moved-functions
#' @export
enrich <- function(...) {
  cli::cli_abort(c(
    "{.fn enrich} has been removed from {.pkg reindeer}.",
    "i" = "Corpus-level DSP (write + register a track): use {.fn quantify} \\
           instead, e.g. {.code quantify(corp, .using = fn)}.",
    "i" = "Segment-level metadata join: use {.fn biographize} instead.",
    "i" = "Segment-level DSP: use {.fn quantify} instead (same call shape \\
           you already use)."
  ), class = c("reindeer_moved_error", "reindeer_error"))
}

#' Functions moved to companion packages
#'
#' These names were exported from earlier versions of `reindeer` but have
#' been relocated to dedicated companion packages. Calling them now
#' aborts with a `reindeer_moved_error` and a pointer to the correct
#' install + namespace. New code should call them directly from the
#' companion package.
#'
#' * `enrich_egg` is removed outright (its `eggstract::enrich_with_egg`
#'   target never existed). Use [quantify_egg()] instead.
#' * `enrich()` is removed outright; corpus-level DSP now goes through
#'   [quantify()] (`quantify(corp, .using = fn)`), segment-level metadata
#'   joins through [biographize()], segment-level DSP through [quantify()].
#'
#' @name deprecated-moved-functions
#' @param ... Ignored - the stub never executes the call.
#' @return Never returns; always errors with a redirect message.
#' @keywords internal
NULL
