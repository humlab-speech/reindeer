#' Convert frequency (Hz) to semitones (deprecated)
#'
#' Moved to the superassp package. This wrapper calls
#' `superassp::ucnv_hz_to_semitone(x, ref_freq = ref)` and will be removed in
#' reindeer 3.0.0.
#'
#' @param x Numeric vector of frequency values in Hz.
#' @param ref Reference frequency in Hz. Default `16.35160` (C\eqn{_0}).
#' @return Numeric vector of semitone values.
#' @seealso [erb()]
#' @export
semitones <- function(x, ref = 16.35160) {
  .moved_to_superassp("semitones", "ucnv_hz_to_semitone")
  superassp::ucnv_hz_to_semitone(x, ref_freq = ref)
}

#' Convert frequency (Hz) to ERB number (deprecated)
#'
#' Moved to the superassp package. This wrapper calls
#' `superassp::ucnv_hz_to_erb(f, method = "moore1983")` and will be removed in
#' reindeer 3.0.0.
#'
#' @section Bug fixed:
#' Up to and including reindeer 2.2.0 this function returned wrong values: it
#' applied the Moore & Glasberg (1983) constants, which are defined for
#' frequency in kHz, to frequency in Hz, so the result was almost constant
#' (`erb(c(500, 1000, 2000))` gave 42.7, 42.8, 42.9 instead of 10.3, 15.3,
#' 20.9).
#'
#' @param f Numeric vector of frequencies in Hz.
#' @return Numeric vector of ERB-numbers.
#' @seealso [semitones()]
#' @export
erb <- function(f) {
  .moved_to_superassp("erb", "ucnv_hz_to_erb")
  superassp::ucnv_hz_to_erb(f, method = "moore1983")
}

.moved_to_superassp <- function(old, new) {
  rlang::check_installed("superassp", reason = paste0("for ", old, "()."))
  cli::cli_warn(
    "{.fn {old}} has moved to {.pkg superassp} as {.fn {new}} and will be removed in reindeer 3.0.0.",
    class = c("reindeer_deprecated", "reindeer_warning"))
}
