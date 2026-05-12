#' @include metadata_core.R metadata_import_export.R
NULL

#' Load corpus metadata from a single entry point
#'
#' One verb to populate the metadata cache regardless of where the
#' metadata lives. `source = "files"` rescans every `METADATA.json`
#' under the corpus (the former [gather_metadata()] path).
#' `source = "excel"` reads a workbook produced by [export_metadata()]
#' and writes it back through the same channel as
#' [import_metadata()].
#'
#' @param corpus_obj A corpus object.
#' @param source One of `"files"` or `"excel"`. Default `"files"`.
#' @param path Path to the Excel workbook (required when
#'   `source = "excel"`).
#' @param verbose Logical; show progress (forwarded to the underlying
#'   loader when supported). Default `TRUE`.
#' @param parallel Logical; parallel JSON scan (only applies to
#'   `source = "files"`). Default `TRUE`.
#' @return The corpus object, invisibly.
#' @examplesIf interactive()
#' corp <- corpus("path/to/db_emuDB")
#' load_metadata(corp)                                # files (default)
#' load_metadata(corp, source = "excel",
#'               path = "corpus_metadata.xlsx")       # excel round-trip
#' @export
load_metadata <- function(corpus_obj,
                          source = c("files", "excel"),
                          path = NULL,
                          verbose = TRUE,
                          parallel = TRUE) {
  source <- match.arg(source)
  if (!S7::S7_inherits(corpus_obj, corpus)) {
    cli::cli_abort("{.arg corpus_obj} must be a corpus object")
  }

  switch(source,
    files = gather_metadata(corpus_obj, verbose = verbose, parallel = parallel),
    excel = {
      if (is.null(path)) {
        cli::cli_abort("{.arg path} is required when {.code source = \"excel\"}")
      }
      import_metadata(corpus_obj, path)
    }
  )
  invisible(corpus_obj)
}

#' Set metadata on a corpus, session, or bundle
#'
#' `set_metadata()` is the preferred name for writing metadata; it is
#' a thin wrapper around [add_metadata()] kept for `get/set` symmetry
#' with [get_metadata()]. `add_metadata()` remains available as an
#' alias (it predates `set_metadata` and is still used in older code).
#'
#' @param corpus_obj A corpus object.
#' @param metadataList Named list of field/value pairs to write.
#' @param session Optional session name (writes session-level metadata).
#' @param bundle Optional bundle name (requires `session`; writes
#'   bundle-level metadata).
#' @param reset.before.add Logical; if `TRUE`, clear existing fields at
#'   the given scope before writing.
#' @return The corpus object, invisibly.
#' @examplesIf interactive()
#' set_metadata(corp, list(Project = "MyStudy"))
#' set_metadata(corp, list(Age = 25, Gender = "Female"), session = "S1")
#' @export
set_metadata <- function(corpus_obj, metadataList,
                         session = NULL, bundle = NULL,
                         reset.before.add = FALSE) {
  add_metadata(corpus_obj, metadataList,
               session = session, bundle = bundle,
               reset.before.add = reset.before.add)
}
