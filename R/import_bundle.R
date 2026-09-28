#' @include corpus_creation.R corpus_metadata_io.R quantify_corpus.R
NULL

# ==============================================================================
# import_bundle() / import_session(): consistent import with auto track
# generation. See specs/2026-09-28-import-bundle-session-design.md.
# ==============================================================================

#' Import a bundle into a corpus: create, tag with metadata, import media,
#' and generate any already-registered SSFF tracks
#'
#' Creates the session (if new) and bundle, applies `metadata`, imports
#' `media`, then replays every corpus-registered `ssffTrackDefinitions`
#' entry that has a resolvable `generator` recipe for this one bundle -
#' the same tracks every other bundle in the corpus already has, computed
#' with this bundle's own Age/Gender norms.
#'
#' @param corpus_obj A corpus object.
#' @param session Session name (literal, created if it doesn't exist yet).
#' @param bundle Bundle name (literal).
#' @param media Media file spec, as accepted by `corpus[session, bundle] <-
#'   media` (single path, or a channel-mapped vector).
#' @param metadata Named list of bundle metadata (e.g. `list(Age = 64,
#'   Gender = "F")`), applied before track generation so DSP parameter
#'   derivation sees it.
#' @param tracks Logical; replay registered track generators for this
#'   bundle. Default `TRUE`.
#' @param verbose Show progress messages.
#' @return The corpus, invisibly.
#' @export
import_bundle <- function(corpus_obj, session, bundle, media,
                          metadata = list(), tracks = TRUE, verbose = TRUE) {
  create_session_and_bundle(corpus_obj, session, bundle, verbose = verbose)

  if (length(metadata) > 0) {
    corpus_assign_metadata(corpus_obj, session, bundle, metadata)
  }

  corpus_import_media(corpus_obj, session, bundle, media)

  if (isTRUE(tracks)) {
    .replay_registered_tracks_for_bundle(corpus_obj, session, bundle, verbose = verbose)
  }

  invisible(corpus_obj)
}

#' Replay every registered SSFF track generator for one bundle
#'
#' Only entries with a resolvable `generator$package`/`generator$function`
#' are replayed; connect-only definitions (no `generator`) are skipped.
#' `overwrite = TRUE` only replaces the JSON recipe entry for that track
#' name in `_DBconfig.json` - `sessionPattern`/`bundlePattern` are anchored
#' to exactly this bundle, so no other bundle's on-disk SSFF file is ever
#' touched by this call.
#'
#' @noRd
.replay_registered_tracks_for_bundle <- function(corpus_obj, session, bundle, verbose = TRUE) {
  dbConfig <- load_DBconfig(corpus_obj)
  track_defs <- dbConfig$ssffTrackDefinitions %||% list()

  failures <- character(0)
  for (track_def in track_defs) {
    gen <- track_def$generator
    if (is.null(gen) || is.null(gen$package) || is.na(gen$package) ||
        is.null(gen$`function`) || is.na(gen$`function`)) {
      next
    }

    # Splice an unevaluated `pkg::fn` call (not a resolved function value) into
    # the `.using` argument - quantify.corpus() identifies the DSP routine via
    # deparse(substitute(.using)), which only sees a clean "pkg::fn" string
    # when the argument expression is itself that unevaluated call. A resolved
    # closure passed through do.call() instead deparses to the function's
    # entire source text.
    qualified_fn <- as.call(list(quote(`::`), as.name(gen$package), as.name(gen$`function`)))
    call_expr <- as.call(c(
      quote(quantify),
      list(object = corpus_obj, .using = qualified_fn,
           name = track_def$name, columnName = track_def$columnName,
           fileExtension = track_def$fileExtension, overwrite = TRUE,
           sessionPattern = paste0("^", session, "$"),
           bundlePattern = paste0("^", bundle, "$"),
           .metadata_fields = c("Gender", "Age"),
           .verbose = verbose, .parallel = FALSE),
      gen$args %||% list()
    ))

    result <- tryCatch({
      eval(call_expr)
      NULL
    }, error = function(e) conditionMessage(e))

    if (!is.null(result)) {
      # Double any literal braces so an arbitrary DSP error message can't be
      # misread as cli glue syntax.
      safe_result <- gsub("([{}])", "\\1\\1", result)
      failures[[length(failures) + 1L]] <- sprintf("%s: %s", track_def$name, safe_result)
    }
  }

  if (length(failures) > 0) {
    cli::cli_warn(c(
      "Failed to generate {length(failures)} track{?s} for {.val {session}/{bundle}}.",
      stats::setNames(failures, rep("i", length(failures)))
    ))
  }

  invisible(corpus_obj)
}

#' Import every bundle of a recording session into a corpus
#'
#' Session-level convenience for [import_bundle()]. `metadata` supplies the
#' session-level default (e.g. one speaker's Age/Gender for every bundle in
#' the session); a bundle's own `metadata` entry overrides it for that
#' bundle only. The session is created transparently by the first bundle
#' imported.
#'
#' @param corpus_obj A corpus object.
#' @param session Session name (literal, created if it doesn't exist yet).
#' @param bundles List of per-bundle specs, each a list with `bundle`,
#'   `media`, and optionally `metadata` (overrides the session-level
#'   `metadata` for that bundle).
#' @param metadata Named list of session-level default metadata.
#' @param tracks Logical; replay registered track generators for each
#'   bundle. Default `TRUE`.
#' @param verbose Show progress messages.
#' @return The corpus, invisibly.
#' @export
import_session <- function(corpus_obj, session, bundles,
                           metadata = list(), tracks = TRUE, verbose = TRUE) {
  for (spec in bundles) {
    bundle_metadata <- utils::modifyList(metadata, spec$metadata %||% list())
    tryCatch({
      import_bundle(corpus_obj, session, spec$bundle, media = spec$media,
                    metadata = bundle_metadata, tracks = tracks, verbose = verbose)
    }, error = function(e) {
      cli::cli_warn("Failed to import bundle {.val {spec$bundle}}: {conditionMessage(e)}")
    })
  }

  invisible(corpus_obj)
}
