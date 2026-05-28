#' Load a small bundled demo corpus
#'
#' Returns a ready-to-use [corpus()] object loaded from the `ae` demo
#' speech database that ships inside the package. Intended for examples
#' and quick experiments — no separate download, no `emuR` install
#' required. Two flavours of the same data are available:
#' WAV-backed (default) and FLAC-backed.
#'
#' @param verbose Logical; if `TRUE`, pass progress messages from
#'   [corpus()] through. Default `FALSE`.
#' @param format One of `"wav"` (default) or `"flac"`. Selects which
#'   bundled archive to unpack.
#' @return A [corpus()] object pointing at a freshly unpacked copy of
#'   the demo database in `tempdir()`. Repeated calls re-use the same
#'   temp directory rather than re-unpacking.
#' @examples
#' corp <- demo_corpus()
#' query(corp, "Phonetic == n")
#' @export
demo_corpus <- function(verbose = FALSE, format = c("wav", "flac")) {
  format <- match.arg(format)
  archive <- switch(format,
    wav  = "ae.tar.xz",
    flac = "aeflac.tar.xz"
  )

  demodir <- file.path(tempdir(), "emuR_demoData")
  if (!dir.exists(demodir)) {
    utils::untar(
      system.file("extdata", archive, package = "reindeer"),
      exdir = demodir
    )
  }

  reindeer::corpus(file.path(demodir, "ae_emuDB"), verbose = verbose)
}

# Legacy internal aliases kept so existing examples and tests keep working.
# Prefer demo_corpus() in new code.
emu_ae <- function(verbose = FALSE) demo_corpus(verbose = verbose, format = "wav")
ae     <- function(verbose = FALSE) demo_corpus(verbose = verbose, format = "flac")
