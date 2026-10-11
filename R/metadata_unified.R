#' @include metadata_core.R metadata_import_export.R
NULL

#' Load corpus metadata from a single entry point
#'
#' One verb to populate the metadata cache regardless of where the
#' metadata lives. `source = "files"` rescans every `METADATA.json`
#' under the corpus (the former `gather_metadata()` path).
#' `source = "excel"` reads a workbook produced by [export_metadata()]
#' and writes it back through the same channel as
#' `import_metadata()`.
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
#' @noRd
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
#' Writes metadata fields to the `METADATA.json` file at the scope you choose
#' and keeps the cache in step. Which scope is decided by `session` and
#' `bundle`:
#' * neither: the whole corpus (defaults for every bundle);
#' * `session`: that session (overrides the corpus default);
#' * `session` and `bundle`: that bundle (overrides both).
#'
#' [get_metadata()] reads the result with the most specific value winning.
#' Values keep their type, arrays such as `team` replace the old value, and
#' everything is validated before anything is written.
#'
#' @param corpus_obj A [corpus()].
#' @param x What to write: a named list of fields; a data frame (one row per
#'   scope, with `session` and/or `bundle` columns, applied all-or-nothing); or
#'   the path of a workbook written by [export_metadata()].
#' @param metadataList Deprecated spelling of `x`.
#' @param session,bundle Scope to write to (see above). `bundle` needs
#'   `session`.
#' @param reset.before.add Deprecated spelling of `reset`.
#' @param reset If `TRUE`, clear the existing fields at the scope first.
#'   (`reset.before.add` is the deprecated spelling.)
#' @return `corpus_obj`, invisibly.
#' @family metadata
#' @seealso [get_metadata()], [export_metadata()]
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' # work on a copy so the demo corpus stays untouched
#' src <- demo_corpus()@basePath
#' tmp <- tempfile(); dir.create(tmp); file.copy(src, tmp, recursive = TRUE)
#' corp <- corpus(file.path(tmp, basename(src)))
#'
#' set_metadata(corp, list(Project = "MyStudy"))                      # corpus
#' set_metadata(corp, list(Age = 25, Gender = "Female"), session = "0000")
#' set_metadata(corp, list(Quality = "Good"),
#'              session = "0000", bundle = "msajc003")                # bundle
#' dplyr::select(get_metadata(corp), bundle, Project, Age, Gender, Quality)
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
