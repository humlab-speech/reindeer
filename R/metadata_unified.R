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
#' Writes one or more metadata fields at the chosen scope. The scope is
#' chosen by which of `session` and `bundle` you pass:
#' * neither — *database* level (defaults that apply to every bundle).
#' * `session` only — *session* level (overrides the database default
#'   for that session's bundles).
#' * `session` + `bundle` — *bundle* level (overrides both).
#'
#' On read, [get_metadata()] resolves inheritance with
#' **bundle > session > database** precedence: a value set at a more
#' specific scope wins over a less specific one. Writes are stored in
#' the bundle / session / database `METADATA.json` files (the ground
#' truth) and mirrored to the SQLite cache.
#'
#' `add_metadata()` is the older spelling; it remains as an alias of
#' `set_metadata()` for backwards compatibility.
#'
#' @param corpus_obj A corpus object.
#' @param metadataList Named list of field/value pairs to write.
#' @param session Optional session name. Required to write at session
#'   level, and required (together with `bundle`) to write at bundle
#'   level.
#' @param bundle Optional bundle name. When given, `session` must also
#'   be provided; together they identify a bundle-level scope.
#' @param reset.before.add Logical; if `TRUE`, clear existing fields at
#'   the given scope before writing.
#' @return The corpus object, invisibly.
#' @family metadata
#' @seealso [get_metadata()] for the matching reader, [load_metadata()]
#'   for bulk import.
#' @examplesIf interactive()
#' corp <- demo_corpus()
#' set_metadata(corp, list(Project = "MyStudy"))                 # database
#' set_metadata(corp, list(Age = 25, Gender = "Female"),
#'              session = "0000")                                 # session
#' set_metadata(corp, list(Quality = "Good"),
#'              session = "0000", bundle = "msajc003")            # bundle
#' @export
set_metadata <- function(corpus_obj, metadataList,
                         session = NULL, bundle = NULL,
                         reset.before.add = FALSE) {
  add_metadata(corpus_obj, metadataList,
               session = session, bundle = bundle,
               reset.before.add = reset.before.add)
}
