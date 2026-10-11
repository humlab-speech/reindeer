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
  .deprecated_alias("load_metadata", "nothing: metadata syncs automatically", parent.frame())
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
#' * neither - *database* level (defaults that apply to every bundle).
#' * `session` only - *session* level (overrides the database default
#'   for that session's bundles).
#' * `session` + `bundle` - *bundle* level (overrides both).
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
#' @param x What to write: a named list of field/value pairs; a data.frame
#'   (with `session`/`bundle` columns, one row per scope, applied all-or-nothing);
#'   or the path of a workbook written by [export_metadata()].
#' @param metadataList Deprecated spelling of `x`.
#' @param session Optional session name. Required to write at session
#'   level, and required (together with `bundle`) to write at bundle
#'   level.
#' @param bundle Optional bundle name. When given, `session` must also
#'   be provided; together they identify a bundle-level scope.
#' @param reset Logical; if `TRUE`, clear existing fields at the given scope
#'   before writing. (`reset.before.add` is the deprecated spelling.)
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
set_metadata <- function(corpus_obj, x, session = NULL, bundle = NULL,
                         reset = FALSE, reset.before.add = NULL, metadataList = NULL) {
  if (!is.null(metadataList)) {                          # old argument names
    .rename_args(list(metadataList = 1), c(metadataList = "x"), "set_metadata")
    x <- metadataList
  }
  if (!is.null(reset.before.add)) {
    .rename_args(list(reset.before.add = 1), c(reset.before.add = "reset"), "set_metadata")
    reset <- reset.before.add
  }
  reset.before.add <- reset
  if (is.character(x) && length(x) == 1L) {              # an exported workbook
    return(import_metadata(corpus_obj, x))
  }
  if (!is.data.frame(x)) {
    return(add_metadata(corpus_obj, x, session = session, bundle = bundle,
                        reset.before.add = reset.before.add))
  }

  # data.frame: one row per scope. `session` + `bundle` columns -> bundle
  # level, `session` only -> session level, neither -> database level (first
  # row). The whole table is checked before anything is written.
  keys <- intersect(c("session", "bundle"), names(x))
  level <- if (all(c("session", "bundle") %in% names(x))) "bundle"
           else if ("session" %in% names(x)) "session" else "database"
  if (level == "database" && nrow(x) != 1L) {
    cli::cli_abort(c("A metadata table without {.field session}/{.field bundle} columns must have one row.",
                     "x" = "Got {nrow(x)} rows."),
                   class = c("reindeer_metadata_error", "reindeer_error"))
  }
  ops <- lapply(seq_len(nrow(x)), function(i) {
    row <- as.list(x[i, setdiff(names(x), keys), drop = FALSE])
    row <- row[!vapply(row, function(v) length(v) == 0L || is.na(v[1]), logical(1))]
    list(level = level, session = if (level != "database") x$session[i],
         bundle = if (level == "bundle") x$bundle[i], meta = row)
  })
  ops <- Filter(function(o) length(o$meta) > 0L, ops)
  .validate_import_plan(corpus_obj, ops,
                        bundle_meta = data.frame(session = character(), bundle = character()),
                        session_meta = data.frame(session = character()), file = "<data.frame>")
  old <- getOption("reindeer._auto_cmdi_suppress", FALSE)
  options(reindeer._auto_cmdi_suppress = TRUE)
  on.exit(options(reindeer._auto_cmdi_suppress = old), add = TRUE)
  for (o in ops) {
    add_metadata(corpus_obj, o$meta, session = o$session, bundle = o$bundle,
                 reset.before.add = reset.before.add)
  }
  options(reindeer._auto_cmdi_suppress = old)
  .auto_describe(corpus_obj)
  invisible(corpus_obj)
}
