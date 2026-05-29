# ==============================================================================
# protoscribe companion glue
# ==============================================================================
#
# Thin wrapper that exposes protoscribe's draft-annotation generators from a
# reindeer pipeline. protoscribe lives in Suggests; the wrapper gates on
# availability and aborts with reindeer_missing_companion_error otherwise.
#
# By default, suggestions are returned as a segment_list-shaped tibble for
# inspection. When .review = TRUE, the EMU-webApp is launched (via
# serve_app()) so the user can accept/refine the proposed annotations
# before they're written to the corpus.

.protoscribe_dispatch <- list(
  vad      = "draft_vad",
  vot      = "draft_vot",
  periods  = "draft_periods",
  momel    = "draft_momel_intsint",
  slam     = "draft_slam",
  slam_plus = "draft_slam_plus",
  slamp3   = "draft_slamp3"
)

#' Propose draft annotations via the protoscribe companion package
#'
#' Routes one of protoscribe's draft generators (VAD, VOT, periods,
#' MOMEL/INTSINT) over the corpus and either returns the proposed
#' annotations as a segment_list-shaped tibble for inspection or launches
#' the EMU-webApp via [serve_app()] for interactive review.
#'
#' @param corpus A `corpus` object.
#' @param type One of `"vad"`, `"vot"`, `"periods"`, `"momel"`.
#' @param .review Logical. When `TRUE`, spawn `serve_app(corpus,
#'   seglist = suggestions)` so the user can accept/refine; when
#'   `FALSE`, return the suggestions silently.
#' @param .commit Logical. Reserved for a future option to write back
#'   accepted suggestions automatically. Default `FALSE` — review
#'   only.
#' @param ... Forwarded to the underlying protoscribe `draft_*`
#'   function.
#' @return A `segment_list`-shaped tibble of suggestions, invisibly
#'   when `.review = TRUE`.
#' @examplesIf interactive()
#' library(protoscribe)
#' propose_annotations(corp, type = "vad")
#' @export
propose_annotations <- function(corpus, type = c("vad", "vot", "periods", "momel",
                                                 "slam", "slam_plus", "slamp3"),
                                  .review = TRUE, .commit = FALSE, ...) {
  if (!requireNamespace("protoscribe", quietly = TRUE)) {
    .companion_abort("protoscribe", purpose = "draft annotation generation")
  }
  type <- match.arg(type)
  fn_name <- .protoscribe_dispatch[[type]]
  fn <- tryCatch(
    get(fn_name, envir = asNamespace("protoscribe")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    cli::cli_abort(c(
      paste0("protoscribe is installed but does not export {.fn ", fn_name, "}."),
      "i" = "Upgrade protoscribe or open an issue at {.url https://github.com/humlab-speech/protoscribe}."
    ))
  }

  suggestions <- fn(corpus = corpus, ...)

  if (isTRUE(.commit)) {
    cli::cli_alert_warning(
      paste0("{.arg .commit = TRUE} is reserved for a future release; ",
             "suggestions returned for review only.")
    )
  }

  if (isTRUE(.review)) {
    if (S7::S7_inherits(suggestions, segment_list)) {
      serve_app(corpus, seglist = suggestions)
    } else {
      cli::cli_alert_warning(
        "Suggestion result is not a segment_list; skipping review UI."
      )
    }
    return(invisible(suggestions))
  }
  suggestions
}
