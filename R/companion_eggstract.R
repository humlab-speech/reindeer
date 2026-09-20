# ==============================================================================
# eggstract companion glue
# ==============================================================================
#
# Thin wrappers that route segment_list operations to the eggstract package
# for electroglottography (EGG) measurements. eggstract is in Suggests, so
# the wrappers gate on availability and abort with
# reindeer_missing_companion_error otherwise.
#
# A bundle is treated as having EGG iff bundle-level metadata field HasEGG is
# truthy. The default is to require the flag; pass .require_egg_flag = FALSE
# to bypass the gate (e.g. when EGG signal files exist outside the metadata
# convention).

# Internal: pull the truthy HasEGG flag per bundle from a segment_list.
.has_egg_per_bundle <- function(seg) {
  if (!all(c("session", "bundle") %in% names(seg))) {
    return(rep(NA, nrow(seg)))
  }
  if ("HasEGG" %in% names(seg)) {
    return(isTRUE(as.logical(seg$HasEGG)) | seg$HasEGG %in% c("TRUE", "true", "1", "yes"))
  }
  rep(NA, nrow(seg))
}

# Internal: filter a segment_list to rows whose bundle has HasEGG truthy.
# Returns the segment_list (possibly empty) and warns if everything filters
# out, but never aborts — empty results are valid downstream.
.filter_to_egg_bundles <- function(seg) {
  if (!"HasEGG" %in% names(seg)) {
    cli::cli_alert_warning(
      paste0("{.field HasEGG} not present on segment_list; ",
             "did you forget to {.fn biographize}? ",
             "Returning the segment_list unchanged.")
    )
    return(seg)
  }
  keep <- .has_egg_per_bundle(seg)
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) {
    cli::cli_alert_warning(
      "No bundles have {.field HasEGG = TRUE}; result will be empty."
    )
  }
  seg[keep, ]
}

#' Quantify segments using an eggstract EGG processor
#'
#' Thin wrapper around [quantify()] that delegates DSP to a function from
#' the eggstract companion package (e.g. `eggstract::ksvF0`) and gates
#' availability via the bundle-level `HasEGG` metadata field. Use this
#' when the corpus mixes audio and EGG bundles and only the EGG-equipped
#' ones should be measured.
#'
#' @param seg A `segment_list` or `extended_segment_list`.
#' @param corpus Optional `corpus`. Resolved from `seg@db_path` when NULL.
#' @param .using The eggstract DSP function to apply. Defaults to
#'   `eggstract::trk_f0` when eggstract is installed. Pass any function
#'   compatible with [quantify()].
#' @param ... Forwarded to [quantify()].
#' @param .at Optional relative-time vector.
#' @param .require_egg_flag When `TRUE` (default), restrict to bundles
#'   with bundle-level metadata `HasEGG = TRUE`. Set to `FALSE` to apply
#'   `.using` to every row.
#' @return An `extended_segment_list` with EGG-derived measurements.
#' @examplesIf interactive()
#' library(eggstract)
#' segs <- query(corp, "Phonetic == V") |> biographize(corp)
#' quantify_egg(segs)
#' @export
quantify_egg <- function(seg, corpus = NULL,
                          .using = NULL,
                          ..., .at = NULL,
                          .require_egg_flag = TRUE) {
  if (!requireNamespace("eggstract", quietly = TRUE)) {
    .companion_abort("eggstract", purpose = "EGG-track quantification")
  }
  if (is.null(.using)) {
    .using <- tryCatch(
      get("trk_f0", envir = asNamespace("eggstract")),
      error = function(e) NULL
    )
    if (is.null(.using)) {
      cli::cli_abort(
        "eggstract is installed but does not export {.fn trk_f0}; upgrade eggstract or pass {.arg .using} explicitly."
      )
    }
  }
  if (isTRUE(.require_egg_flag)) {
    seg <- .filter_to_egg_bundles(seg)
    if (nrow(seg) == 0L) {
      # Return an empty extended_segment_list with the right shape.
      return(extended_segment_list(
        seg,
        db_uuid = if (S7::S7_inherits(seg, segment_list)) seg@db_uuid else "",
        db_path = if (S7::S7_inherits(seg, segment_list)) seg@db_path else "",
        dsp_function = "eggstract::ksvF0",
        dsp_columns = character()
      ))
    }
  }
  quantify(seg, dsp_function = .using, ..., .at = .at)
}
