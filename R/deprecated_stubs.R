# ============================================================================
# Deprecated stubs for functionality moved to companion packages
# ============================================================================
#
# Provides a helpful redirect for users upgrading from older reindeer
# versions whose scripts still call these symbols unqualified.

.companion_redirect <- function(fn_name, package, install_url) {
  cli::cli_abort(c(
    "{.fn {fn_name}} moved out of {.pkg reindeer} into {.pkg {package}}.",
    "i" = "Install with {.code remotes::install_github(\"{install_url}\")} ",
    "i" = "then call {.code {package}::{fn_name}(...)} instead.",
    "i" = "See {.url https://github.com/{install_url}}.",
    .envir = environment()
  ), class = c("reindeer_moved_error", "reindeer_error"))
}

# --- protoscribe (moved in v0.3) --------------------------------------------

#' @rdname deprecated-moved-functions
#' @export
draft_vad <- function(...) .companion_redirect("draft_vad", "protoscribe",
                                                "humlab-speech/protoscribe")

#' @rdname deprecated-moved-functions
#' @export
draft_vot <- function(...) .companion_redirect("draft_vot", "protoscribe",
                                                "humlab-speech/protoscribe")

#' @rdname deprecated-moved-functions
#' @export
draft_periods <- function(...) .companion_redirect("draft_periods", "protoscribe",
                                                    "humlab-speech/protoscribe")

#' @rdname deprecated-moved-functions
#' @export
draft_momel_intsint <- function(...) .companion_redirect(
  "draft_momel_intsint", "protoscribe", "humlab-speech/protoscribe")

# --- erodex (moved in v0.7) -------------------------------------------------

#' @rdname deprecated-moved-functions
#' @export
quantify_simulate <- function(...) .companion_redirect(
  "quantify_simulate", "erodex", "humlab-speech/erodex")

#' @rdname deprecated-moved-functions
#' @export
enrich_simulate <- function(...) .companion_redirect(
  "enrich_simulate", "erodex", "humlab-speech/erodex")

#' @rdname deprecated-moved-functions
#' @export
reminisce <- function(...) .companion_redirect(
  "reminisce", "erodex", "humlab-speech/erodex")

#' @rdname deprecated-moved-functions
#' @export
reminisce_tracks <- function(...) .companion_redirect(
  "reminisce_tracks", "erodex", "humlab-speech/erodex")

#' @rdname deprecated-moved-functions
#' @export
list_simulations <- function(...) .companion_redirect(
  "list_simulations", "erodex", "humlab-speech/erodex")

#' Functions moved to companion packages
#'
#' These names were exported from earlier versions of `reindeer` but have
#' been relocated to dedicated companion packages. Calling them now
#' aborts with a `reindeer_moved_error` and a pointer to the correct
#' install + namespace. New code should call them directly from the
#' companion package.
#'
#' * Draft-annotation generators (`draft_vad`, `draft_vot`,
#'   `draft_periods`, `draft_momel_intsint`) live in
#'   [protoscribe](https://github.com/humlab-speech/protoscribe).
#'   Use [propose_annotations()] for the reindeer-side wrapper.
#' * Parameter-grid simulation (`quantify_simulate`, `enrich_simulate`,
#'   `reminisce`, `reminisce_tracks`, `list_simulations`) lives in
#'   [erodex](https://github.com/humlab-speech/erodex).
#'
#' @name deprecated-moved-functions
#' @param ... Ignored — the stub never executes the call.
#' @return Never returns; always errors with a redirect message.
#' @keywords internal
NULL
