#' Read an annotation file (ELAN or Praat TextGrid)
#'
#' The format is taken from the file extension (`.eaf`, `.TextGrid`). Both
#' formats come back in one schema: a tibble with `level`, `labels`, `start`
#' and `end` in milliseconds, the same names and units a [segment_list] uses.
#'
#' @param path Path to a `.eaf` or `.TextGrid` file.
#' @return A tibble with columns `level`, `labels`, `start`, `end` (ms).
#' @seealso [write_annotations()]
#' @examplesIf interactive()
#' ann <- read_annotations("speech.TextGrid")
#' @export
read_annotations <- function(path) {
  ext <- tolower(tools::file_ext(path))
  raw <- switch(ext,
    eaf = read_eaf(path),
    textgrid = {
      tg <- read_textgrid(path)
      tg$start <- tg$start * 1000
      tg$end <- tg$end * 1000
      tg
    },
    cli::cli_abort(c("Unsupported annotation format {.val .{ext}}.",
                     "i" = "Use a {.val .eaf} or {.val .TextGrid} file.")))
  tibble::tibble(level = raw$tier, labels = raw$label, start = raw$start, end = raw$end)
}

#' Write a segment list as an ELAN or Praat TextGrid file
#'
#' The format is taken from the file extension (`.eaf`, `.TextGrid`).
#' `segs` needs `level`, `labels`, `start` and `end` (ms), as in the output of
#' [read_annotations()] and every [segment_list].
#'
#' @param segs A `segment_list` or data frame with `level`, `labels`, `start`,
#'   `end`.
#' @param path Output path ending in `.eaf` or `.TextGrid`.
#' @param ... Passed to the format writer ([write_eaf()] / [write_textgrid()]).
#' @return `path`, invisibly.
#' @seealso [read_annotations()]
#' @examplesIf interactive()
#' write_annotations(query(demo_corpus(), "Phonetic == t") |> collect(), "t.TextGrid")
#' @export
write_annotations <- function(segs, path, ...) {
  if (S7::S7_inherits(segs, lazy_segment_list)) segs <- dplyr::collect(segs)
  switch(tolower(tools::file_ext(path)),
    eaf = write_eaf(segs, path, ...),
    textgrid = write_textgrid(segs, path, ...),
    cli::cli_abort(c("Unsupported annotation format {.val .{tools::file_ext(path)}}.",
                     "i" = "Use a {.val .eaf} or {.val .TextGrid} path.")))
}
