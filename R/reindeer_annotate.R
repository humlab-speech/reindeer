# ==============================================================================
# annotate() and review(): open a corpus in the Artic annotation web app
# ==============================================================================

#' Annotate a corpus in Artic
#'
#' Serves a corpus to the Artic web application in editing mode: all bundles
#' (or a session/bundle-pattern subset) can be inspected, annotated, and saved
#' straight back to the database. The Artic web app is bundled with the package.
#'
#' \code{annotate()} shares its name with \code{ggplot2::annotate()}. Calls
#' whose first argument is not a \code{corpus} are forwarded verbatim to
#' ggplot2, so both verbs keep working regardless of attach order. With
#' \pkg{conflicted}, prefer \code{reindeer::annotate} explicitly.
#'
#' @param geom A \code{corpus} (editing session), a \code{segment_list} (opens
#'   as a review playlist, with the corpus taken from it) or,
#'   for ggplot2, the geom to annotate with.
#' @param ... For a corpus: \code{session_pattern}, \code{bundle_pattern},
#'   \code{host}, \code{port}, \code{auto_open_url},
#'   \code{browser}, \code{use_viewer}, \code{app_dir}, \code{debug},
#'   \code{debug_level}. The camelCase spellings still work with a warning. For ggplot2: forwarded to \code{ggplot2::annotate()}.
#' @details When given a segment list, Artic opens a playlist with one time
#'   anchor per row, in the current row order (sort with \code{dplyr::arrange()}
#'   first to decide it). Extra arguments for that case: \code{tracks}
#'   (registered tracks to overlay; default the \code{dsp_columns} of a
#'   \code{quantify()} result), \code{perspective} and \code{canvas}
#'   (where to draw them; default the first perspective and \code{"SPEC"}).
#' @return Invisibly \code{TRUE} for a corpus. The server keeps running until
#'   the *clear* button is used, the tab is closed, or
#'   \code{httpuv::stopServer(getOption("reindeer.serve_handle"))} is called.
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' annotate(corp)
#' annotate(corp, session_pattern = "Session.*")
#' @seealso [query()], [quantify()]
#' @export
annotate <- function(geom = NULL, ...) {
  if (S7::S7_inherits(geom, corpus)) {
    return(do.call(.artic_session, c(list(geom, dbconfig_overlay = .artic_edit_overlay),
                                     .artic_args(list(...), "annotate"))))
  }
  # A segment list opens as a playlist; its corpus is recovered from it.
  if (is_segment_list(geom)) {
    seg <- if (S7::S7_inherits(geom, lazy_segment_list)) dplyr::collect(geom) else geom
    return(review(get_corpus_cached(seg), seg, ...))
  }
  .require_ggplot2()
  ggplot2::annotate(geom = geom, ...)
}

#' Review a segment list in Artic as a playlist
#'
#' Serves a corpus restricted to the bundles and segments in \code{seglist},
#' with one time anchor per segment. Artic steps through the anchors (and, when
#' the build supports it, on to the next bundle), so a query result, an
#' \code{extended_segment_list} from \code{quantify()}, or any augmented segment
#' list becomes a review playlist. Rows are played in their current order, so
#' \code{dplyr::arrange()} before \code{review()} sets the order (e.g. worst
#' measurement first).
#'
#' @param corpus A \code{corpus} object.
#' @param ... Server options forwarded to the Artic session: \code{host},
#'   \code{port}, \code{autoOpenURL}, \code{browser}, \code{useViewer},
#'   \code{debug}, \code{debugLevel}.
#' @section Method arguments - corpus:
#' \describe{
#'   \item{`seglist`}{A \code{segment_list}, \code{extended_segment_list},
#'     \code{lazy_segment_list}, or data.frame with \code{session} and
#'     \code{bundle} plus anchor columns.}
#'   \item{`tracks`}{Registered SSFF tracks to overlay while reviewing.
#'     Defaults to the \code{dsp_columns} of an \code{extended_segment_list},
#'     else none.}
#'   \item{`perspective`}{Perspective to attach track overlays to (name or
#'     index; defaults to the first perspective).}
#'   \item{`canvas`}{Signal canvas to overlay tracks on (default `"SPEC"`).}
#'   \item{`app_dir`}{Optional explicit Artic dist directory.}
#' }
#' @usage review(corpus, ...)
#' @return Invisibly \code{TRUE}. Stop the server as for [annotate()].
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' segs <- query(corp, "Phonetic == t")
#' review(corp, segs)
#' review(corp, quantify(segs, dsp_function = superassp::trk_formant_forest))
#' @seealso [annotate()], [query()], [quantify()]
#' @noRd
review <- S7::new_generic("review", "corpus", fun = function(corpus, ...) {
    S7::S7_dispatch()
})

S7::method(review, corpus) <- function(corpus, seglist, tracks = NULL,
                                       perspective = NULL, canvas = "SPEC",
                                       app_dir = NULL, ...) {
  playlist <- .build_review_playlist(seglist, corpus = corpus)
  resolved <- .artic_resolve_tracks(corpus, seglist, tracks)
  resolved <- .artic_tracks_available(corpus, resolved, playlist)

  overlay <- function(cfg) {
    .artic_assign_tracks(
      .artic_edit_overlay(cfg),
      resolved,
      perspective = perspective,
      canvas = canvas
    )
  }

  n_anchors <- sum(vapply(playlist, function(e) nrow(e$timeAnchors), integer(1)))
  cli::cli_alert_info(
    "Artic review playlist: {length(playlist)} bundle{?s}, {n_anchors} anchor{?s}."
  )
  if (length(resolved) > 0L) {
    cli::cli_alert_info("Track overlay{?s}: {.val {resolved}}")
  }

  args <- .artic_args(list(...), "review")
  if (!is.null(args$appDir)) { app_dir <- args$appDir; args$appDir <- NULL }
  do.call(.artic_session, c(
    list(corpus, bundle_entries = playlist, dbconfig_overlay = overlay,
         appDir = app_dir, require_artic = .artic_required_features()),
    args
  ))
}

# Public (snake_case) option names of annotate()/review() -> .artic_session().
.artic_arg_map <- c(session_pattern = "sessionPattern", bundle_pattern = "bundlePattern",
                    host = "host", port = "port", auto_open_url = "autoOpenURL",
                    browser = "browser", use_viewer = "useViewer", app_dir = "appDir",
                    debug = "debug", debug_level = "debugLevel")

.artic_args <- function(dots, fn) {
  dots <- .rename_args(dots, stats::setNames(names(.artic_arg_map), unname(.artic_arg_map)), fn)
  unknown <- setdiff(names(dots), names(.artic_arg_map))
  if (length(unknown) > 0L) {
    cli::cli_abort(c("Unknown argument(s) to {.fn {fn}}: {.arg {unknown}}.",
                     "i" = "Supported: {.arg {names(.artic_arg_map)}}."),
                   class = c("reindeer_error"))
  }
  stats::setNames(dots, unname(.artic_arg_map[names(dots)]))
}
