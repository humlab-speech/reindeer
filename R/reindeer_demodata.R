#' Load a small bundled demo corpus
#'
#' Returns a ready-to-use [corpus()] object loaded from the `ae` demo
#' speech database that ships inside the package. Intended for examples
#' and quick experiments - no separate download, no `emuR` install
#' required.#'
#' @param verbose Logical; if `TRUE`, pass progress messages from
#'   [corpus()] through. Default `FALSE`.
#' @return A [corpus()] object pointing at a freshly unpacked copy of
#'   the demo database in `tempdir()`. Repeated calls re-use the same
#'   temp directory rather than re-unpacking.
#' @examples
#' corp <- demo_corpus()
#' query(corp, "Phonetic == n")
#' @export
demo_corpus <- function(verbose = FALSE) {
  archive <- "ae.tar.xz"

  demodir <- file.path(tempdir(), "emuR_demoData")
  if (!dir.exists(demodir)) {
    utils::untar(
      system.file("extdata", archive, package = "reindeer"),
      exdir = demodir
    )
  }

  reindeer::corpus(file.path(demodir, "ae_emuDB"), verbose = verbose)
}

