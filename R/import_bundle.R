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
  track_defs <- dbConfig$ssffTrackDefinitions %or_empty% list()

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
           name = track_def$name, column_name = track_def$columnName,
           file_extension = track_def$fileExtension, overwrite = TRUE,
           session_pattern = paste0("^", session, "$"),
           bundle_pattern = paste0("^", bundle, "$"),
           .metadata_fields = c("Gender", "Age"),
           .verbose = verbose),
      gen$args %or_empty% list()
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
    bundle_metadata <- utils::modifyList(metadata, spec$metadata %or_empty% list())
    tryCatch({
      import_bundle(corpus_obj, session, spec$bundle, media = spec$media,
                    metadata = bundle_metadata, tracks = tracks, verbose = verbose)
    }, error = function(e) {
      cli::cli_warn("Failed to import bundle {.val {spec$bundle}}: {conditionMessage(e)}")
    })
  }

  invisible(corpus_obj)
}

#' Import media files into a session, one bundle per file
#'
#' Each file becomes a bundle named after the file (without extension) in
#' `session`, which is created if needed. Registered signal tracks are
#' generated for the new bundles. All files are checked before anything is
#' written: they must exist, have distinct, filesystem-safe names, and must
#' not already be bundles of the session.
#'
#' @param corpus A [corpus()].
#' @param files Character vector of audio file paths.
#' @param session Session name.
#' @param metadata `NULL`; a named list applied to every bundle; or a data
#'   frame with a `bundle` column (one row per file) of per-bundle metadata.
#' @param annotations Reserved for per-file TextGrid/EAF import; must be
#'   `NULL` for now.
#' @param tracks Generate the corpus' registered tracks for the new bundles
#'   (default `TRUE`).
#' @param verbose Print progress.
#' @return A tibble of `session`, `bundle`, `file`, invisibly.
#' @examplesIf interactive()
#' corp <- corpus("my_emuDB", create = TRUE)
#' import_media(corp, c("a.wav", "b.wav"), session = "S1",
#'              metadata = list(Gender = "f"))
#' @export
import_media <- function(corpus, files, session, metadata = NULL,
                         annotations = NULL, tracks = TRUE, verbose = FALSE) {
  if (!S7::S7_inherits(corpus, reindeer::corpus)) {
    cli::cli_abort("{.arg corpus} must be a corpus")
  }
  if (!is.null(annotations)) {
    cli::cli_abort("{.arg annotations} is not supported yet; import the media first, then annotate.")
  }
  files <- as.character(files)
  fail <- function(msg) cli::cli_abort(msg, class = c("reindeer_import_error", "reindeer_error"))
  if (length(files) == 0L) fail("No files to import.")
  missing <- files[!file.exists(files)]
  if (length(missing) > 0L) fail(c("Files not found:", stats::setNames(missing, rep("x", length(missing)))))
  bundles <- tools::file_path_sans_ext(basename(files))
  if (anyDuplicated(bundles)) fail("Two files would become the same bundle name.")
  if (!all(vapply(c(session, bundles), .safe_part, logical(1)))) {
    fail("Session and file names must not contain path separators or '..'.")
  }
  existing <- list_bundles_from_cache(get_corpus_connection(corpus), corpus@.uuid)
  clash <- bundles[bundles %in% existing$name[existing$session == session]]
  if (length(clash) > 0L) fail(c("Bundles already exist in this session:", stats::setNames(clash, rep("x", length(clash)))))

  per_bundle <- function(b) {
    if (is.null(metadata)) return(list())
    if (is.data.frame(metadata)) {
      row <- metadata[metadata$bundle == b, setdiff(names(metadata), c("session", "bundle")), drop = FALSE]
      if (nrow(row) == 0L) return(list())
      row <- as.list(row[1, , drop = FALSE])
      return(row[!vapply(row, function(v) is.na(v[1]), logical(1))])
    }
    as.list(metadata)
  }
  for (i in seq_along(files)) {
    import_bundle(corpus, session, bundles[i], media = files[i],
                  metadata = per_bundle(bundles[i]), tracks = tracks, verbose = verbose)
  }
  invisible(tibble::tibble(session = session, bundle = bundles, file = files))
}
