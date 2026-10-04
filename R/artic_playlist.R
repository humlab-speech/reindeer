# ==============================================================================
# Review playlists, DBconfig overlays, and track selection for Artic
# ==============================================================================

#' Column of a data.frame as character, tolerating list-columns
#' @keywords internal
#' @noRd
.artic_as_label <- function(x, n) {
  if (is.null(x)) {
    return(rep(NA_character_, n))
  }
  if (is.list(x)) {
    return(vapply(x, function(v) {
      if (length(v) == 0L) NA_character_ else paste(as.character(v), collapse = "; ")
    }, character(1)))
  }
  as.character(x)
}

#' Build an ordered review playlist from a segment list
#'
#' Produces Artic bundle entries with per-segment time anchors in the order the
#' rows appear in \code{seglist}, so \code{dplyr::arrange()} before
#' \code{review()} controls the review order. Sessions are grouped by first
#' appearance, then bundles within a session, matching Artic's sidebar so the
#' flat playlist order equals the on-screen order.
#'
#' @param seglist A \code{segment_list}, \code{extended_segment_list},
#'   \code{lazy_segment_list}, or data.frame with session/bundle and anchor
#'   columns.
#' @param corpus The corpus under review, used to validate session/bundle names.
#' @return A list of bundle entries, each with \code{session}, \code{name}, and
#'   \code{timeAnchors}.
#' @keywords internal
#' @noRd
.build_review_playlist <- function(seglist, corpus = NULL) {
  if (S7::S7_inherits(seglist, lazy_segment_list)) {
    seglist <- collect(seglist)
  }
  if (!is.data.frame(seglist)) {
    .artic_abort(c(
      "{.arg seglist} must be a segment list.",
      "i" = "Pass the result of {.fn query} or {.fn quantify}."
    ))
  }
  if (nrow(seglist) == 0L) {
    .artic_abort("The review playlist is empty; nothing to review.")
  }

  cols <- names(seglist)
  if (!all(c("session", "bundle") %in% cols)) {
    .artic_abort(c(
      "{.arg seglist} is missing {.field session} and/or {.field bundle}.",
      "i" = "Required columns: {.field session}, {.field bundle}."
    ))
  }
  ses <- as.character(seglist$session)
  bnd <- as.character(seglist$bundle)

  if (all(c("sample_start", "sample_end") %in% cols)) {
    start_s <- as.numeric(seglist$sample_start)
    end_s <- as.numeric(seglist$sample_end)
  } else if (all(c("start", "end", "sample_rate") %in% cols)) {
    sr <- as.numeric(seglist$sample_rate)
    start_s <- round(as.numeric(seglist$start) / 1000 * sr)
    end_s <- round(as.numeric(seglist$end) / 1000 * sr)
  } else {
    .artic_abort(c(
      "{.arg seglist} has no usable anchor columns.",
      "i" = "Provide {.field sample_start}/{.field sample_end}, or {.field start}/{.field end}/{.field sample_rate}."
    ))
  }

  lbl <- .artic_as_label(
    if ("labels" %in% cols) seglist$labels else if ("label" %in% cols) seglist$label else NULL,
    nrow(seglist)
  )

  keep <- !is.na(start_s) & !is.na(end_s) & nzchar(ses) & nzchar(bnd)
  if (any(!keep)) {
    cli::cli_warn("Dropping {sum(!keep)} row{?s} without a usable anchor.")
    ses <- ses[keep]; bnd <- bnd[keep]; lbl <- lbl[keep]
    start_s <- start_s[keep]; end_s <- end_s[keep]
  }
  if (length(ses) == 0L) {
    .artic_abort("No rows in {.arg seglist} have usable anchors.")
  }

  if (!is.null(corpus) && "db_uuid" %in% cols && inherits(corpus, "corpus")) {
    uu <- unique(as.character(seglist$db_uuid))
    uu <- uu[!is.na(uu) & nzchar(uu)]
    if (length(uu) > 0L && any(uu != corpus@.uuid)) {
      .artic_abort(c(
        "{.arg seglist} belongs to a different database.",
        "x" = "seglist db_uuid: {.val {uu}}",
        "i" = "Corpus db_uuid: {.val {corpus@.uuid}}."
      ))
    }
  }

  if (!is.null(corpus)) {
    known <- .list_bundles(corpus)
    known_keys <- paste(known$session, known$name, sep = "\r")
    unknown <- setdiff(unique(paste(ses, bnd, sep = "\r")), known_keys)
    if (length(unknown) > 0L) {
      shown <- sub("\r", "/", unknown)
      .artic_abort(c(
        "{.arg seglist} references bundle{?s} not in the corpus: {.val {shown}}.",
        "i" = "Re-run {.fn query} against this corpus."
      ))
    }
  }

  with_label <- any(!is.na(lbl))
  entries <- list()
  for (s in unique(ses)) {
    for (b in unique(bnd[ses == s])) {
      rows <- which(ses == s & bnd == b)
      anchors <- data.frame(
        sample_start = as.integer(round(start_s[rows])),
        sample_end = as.integer(round(end_s[rows])),
        stringsAsFactors = FALSE
      )
      if (with_label) {
        anchors$label <- lbl[rows]
      }
      entries[[length(entries) + 1L]] <- list(
        session = s,
        name = b,
        timeAnchors = anchors
      )
    }
  }
  entries
}

#' Non-ITEM level names, in DBconfig order
#' @keywords internal
#' @noRd
.artic_time_levels <- function(cfg) {
  defs <- cfg$levelDefinitions %||% list()
  vapply(
    Filter(function(d) !identical(d$type, "ITEM"), defs),
    function(d) as.character(d$name %||% ""),
    character(1)
  )
}

#' Default Artic perspective for a DBconfig without one
#' @keywords internal
#' @noRd
.artic_default_perspective <- function(cfg) {
  list(
    name = "default",
    signalCanvases = list(order = list("OSCI", "SPEC"), assign = list(), contourLims = list()),
    levelCanvases = list(order = as.list(.artic_time_levels(cfg))),
    twoDimCanvases = list(order = list())
  )
}

#' Editing restrictions used by annotate() and review()
#' @keywords internal
#' @noRd
.artic_edit_restrictions <- function() {
  list(
    playback = TRUE, correctionTool = TRUE, editItemSize = TRUE, editItemName = TRUE,
    deleteItemBoundary = TRUE, deleteItem = TRUE, deleteLevel = TRUE, addItem = TRUE,
    drawCrossHairs = TRUE, drawSampleNrs = FALSE, drawZeroLine = TRUE,
    bundleComments = TRUE, bundleFinishedEditing = TRUE,
    showPerspectivesSidebar = TRUE, useLargeTextInputField = FALSE
  )
}

#' Active buttons used by annotate() and review()
#' @keywords internal
#' @noRd
.artic_edit_buttons <- function() {
  list(
    addLevelSeg = TRUE, addLevelEvent = TRUE, renameSelLevel = TRUE,
    downloadTextGrid = FALSE, downloadAnnotation = FALSE, specSettings = TRUE,
    connect = TRUE, search = TRUE, clear = TRUE, deleteSingleLevel = FALSE,
    resizeSingleLevel = TRUE, saveSingleLevel = FALSE, resizePerspectives = TRUE,
    openDemoDB = FALSE, saveBundle = TRUE, openMenu = TRUE, showHierarchy = TRUE,
    editEMUwebAppConfig = FALSE
  )
}

#' Overlay in-memory editing configuration onto a served DBconfig
#'
#' Served to the web app via \code{GETGLOBALDBCONFIG} only; the corpus
#' \code{_DBconfig.json} on disk is never modified.
#'
#' @param dbconfig The corpus DBconfig.
#' @return The overlaid DBconfig.
#' @keywords internal
#' @noRd
.artic_edit_overlay <- function(dbconfig) {
  cfg <- dbconfig
  if (is.null(cfg$EMUwebAppConfig)) {
    cfg$EMUwebAppConfig <- list()
  }
  ea <- cfg$EMUwebAppConfig

  # Our values win over both the corpus config and Artic's defaults: editing is
  # the point of annotate()/review(), and saveBundle must be on or Artic drops
  # unsaved edits when the playlist moves to the next bundle.
  ea$restrictions <- utils::modifyList(ea$restrictions %||% list(), .artic_edit_restrictions())
  ea$activeButtons <- utils::modifyList(ea$activeButtons %||% list(), .artic_edit_buttons())

  if (is.null(ea$perspectives) || length(ea$perspectives) == 0L) {
    ea$perspectives <- list(.artic_default_perspective(cfg))
  }
  for (i in seq_along(ea$perspectives)) {
    p <- ea$perspectives[[i]]
    if (is.null(p$signalCanvases)) {
      p$signalCanvases <- list(order = list(), assign = list(), contourLims = list())
    }
    if (length(p$signalCanvases$order %||% list()) == 0L) {
      p$signalCanvases$order <- list("OSCI", "SPEC")
    }
    if (is.null(p$signalCanvases$assign)) {
      p$signalCanvases$assign <- list()
    }
    if (is.null(p$levelCanvases)) {
      p$levelCanvases <- list(order = list())
    }
    if (length(p$levelCanvases$order %||% list()) == 0L) {
      p$levelCanvases$order <- as.list(.artic_time_levels(cfg))
    }
    if (is.null(p$twoDimCanvases)) {
      p$twoDimCanvases <- list(order = list())
    }
    ea$perspectives[[i]] <- p
  }

  cfg$EMUwebAppConfig <- ea
  cfg
}

#' Resolve requested review tracks to registered SSFF track names
#' @keywords internal
#' @noRd
.artic_resolve_tracks <- function(corpus, seglist, tracks = NULL) {
  if (is.null(tracks)) {
    tracks <- if (is_extended_segment_list(seglist)) unique(seglist@dsp_columns) else character()
  }
  tracks <- as.character(tracks)
  if (length(tracks) == 0L) {
    return(character())
  }
  cfg <- load_DBconfig(get_emuDBhandle(corpus))
  defs <- cfg$ssffTrackDefinitions %||% list()
  known <- vapply(defs, function(d) as.character(d$name %||% ""), character(1))
  unknown <- setdiff(tracks, known)
  if (length(unknown) > 0L) {
    cli::cli_warn(c(
      "Ignoring track{?s} with no {.field ssffTrackDefinitions} entry: {.val {unknown}}.",
      "i" = "Materialize with {.code quantify(corpus, .using = )} to register a track."
    ))
    tracks <- intersect(tracks, known)
  }
  tracks
}

#' Keep only tracks whose files exist for every playlist bundle
#' @keywords internal
#' @noRd
.artic_tracks_available <- function(corpus, tracks, playlist) {
  if (length(tracks) == 0L) {
    return(tracks)
  }
  cfg <- load_DBconfig(get_emuDBhandle(corpus))
  defs <- cfg$ssffTrackDefinitions %||% list()
  basePath <- corpus@basePath
  keep <- vapply(tracks, function(tn) {
    d <- Filter(function(x) identical(as.character(x$name), tn), defs)[[1]]
    fe <- as.character(d$fileExtension %||% "")
    if (!nzchar(fe)) {
      return(FALSE)
    }
    all(vapply(playlist, function(e) {
      file.exists(file.path(
        basePath,
        paste0(e$session, get_session_suffix()),
        paste0(e$name, get_bundle_dir_suffix()),
        paste0(e$name, ".", fe)
      ))
    }, logical(1)))
  }, logical(1))
  if (any(!keep)) {
    cli::cli_warn("Track{?s} {.val {tracks[!keep]}} not on disk for every playlist bundle; not overlaying.")
  }
  tracks[keep]
}

#' Assign SSFF tracks as overlays in the served perspective
#' @keywords internal
#' @noRd
.artic_assign_tracks <- function(dbconfig, tracks, perspective = NULL, canvas = "SPEC") {
  if (length(tracks) == 0L) {
    return(dbconfig)
  }
  cfg <- dbconfig
  pers <- cfg$EMUwebAppConfig$perspectives
  if (is.null(pers) || length(pers) == 0L) {
    return(cfg)
  }
  idx <- if (is.null(perspective)) {
    1L
  } else if (is.numeric(perspective)) {
    as.integer(perspective)
  } else {
    match(perspective, vapply(pers, function(p) as.character(p$name %||% ""), character(1)))
  }
  if (is.na(idx) || idx < 1L || idx > length(pers)) {
    cli::cli_warn("Perspective {.val {perspective}} not found; tracks not overlaid.")
    return(cfg)
  }
  assign <- pers[[idx]]$signalCanvases$assign %||% list()
  for (tn in tracks) {
    exists <- any(vapply(assign, function(a) {
      identical(as.character(a$ssffTrackName), tn) &&
        identical(as.character(a$signalCanvasName), canvas)
    }, logical(1)))
    if (!exists) {
      assign[[length(assign) + 1L]] <- list(signalCanvasName = canvas, ssffTrackName = tn)
    }
  }
  cfg$EMUwebAppConfig$perspectives[[idx]]$signalCanvases$assign <- assign
  cfg
}
