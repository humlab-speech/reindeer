# ==============================================================================
# annotate() and review(): open a corpus in the Artic annotation web app
# ==============================================================================

#' Annotate a corpus in Artic
#'
#' Serves a corpus to the Artic web application in editing mode: all bundles
#' (or a session/bundle-pattern subset) can be inspected, annotated, and saved
#' straight back to the database. Artic must be available locally; see
#' \code{\link{install_artic}} and \code{\link{artic_info}}.
#'
#' \code{annotate()} shares its name with \code{ggplot2::annotate()}. Calls
#' whose first argument is not a \code{corpus} are forwarded verbatim to
#' ggplot2, so both verbs keep working regardless of attach order. With
#' \pkg{conflicted}, prefer \code{reindeer::annotate} explicitly.
#'
#' @param geom A \code{corpus} (editing session) or, for ggplot2, the geom to
#'   annotate with.
#' @param ... For a corpus: \code{sessionPattern}, \code{bundlePattern},
#'   \code{bundleListName}, \code{host}, \code{port}, \code{autoOpenURL},
#'   \code{browser}, \code{useViewer}, \code{appDir}, \code{debug},
#'   \code{debugLevel}. For ggplot2: forwarded to \code{ggplot2::annotate()}.
#' @return Invisibly \code{TRUE} for a corpus. The server keeps running until
#'   the *clear* button is used, the tab is closed, or
#'   \code{httpuv::stopServer(getOption("reindeer.serve_handle"))} is called.
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' annotate(corp)
#' annotate(corp, sessionPattern = "Session.*")
#' @seealso [review()], [install_artic()]
#' @export
annotate <- function(geom = NULL, ...) {
  if (S7::S7_inherits(geom, corpus)) {
    return(.artic_session(geom, dbconfig_overlay = .artic_edit_overlay, ...))
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
#'   \item{`bundleListName`}{Optional bundle-list name used to persist
#'     per-bundle \code{comment}/\code{finishedEditing} progress.}
#'   \item{`appDir`}{Optional explicit Artic dist directory.}
#' }
#' @usage review(corpus, ...)
#' @return Invisibly \code{TRUE}. Stop the server as for [annotate()].
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' segs <- query(corp, "Phonetic == t")
#' review(corp, segs)
#' review(corp, quantify(segs, dsp_function = superassp::trk_formant_forest))
#' @seealso [annotate()], [query()], [quantify()]
#' @export
review <- S7::new_generic("review", "corpus")

#' @export
S7::method(review, corpus) <- function(corpus, seglist, tracks = NULL,
                                       perspective = NULL, canvas = "SPEC",
                                       bundleListName = NULL, appDir = NULL, ...) {
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

  .artic_session(
    corpus,
    bundle_entries = playlist,
    dbconfig_overlay = overlay,
    bundleListName = bundleListName,
    appDir = appDir,
    require_artic = .artic_required_features(),
    ...
  )
}
