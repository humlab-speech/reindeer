#' Convert frequency (Hz) to semitones
#'
#' Maps frequency values (in Hz) to semitones above a reference frequency.
#' The default reference is C\eqn{_0} (16.35160 Hz), which matches the
#' base frequency used by a Phonogram
#' \insertCite{Schutte.1983.10.1159/000265703}{reindeer}. For Praat-
#' compatible output pass `ref = 1`, `100`, `200`, or `440`.
#'
#' @param x Numeric vector of frequency values in Hz.
#' @param ref Reference frequency in Hz. Default `16.35160` (C\eqn{_0}).
#' @return Numeric vector of semitone values.
#' @references \insertAllCited{}
#' @examples
#' # Semitones relative to 100 Hz (Praat-style)
#' semitones(c(200, 400, 800), ref = 100)
#' # Phonogram range C0..C4 expressed in semitones
#' semitones(261.6256)
#' @seealso [erb()]
#' @export
semitones <- function(x, ref = 16.35160) {
  12 * log(x / ref) / log(2)
}

#' @rdname semitones
#' @export
st <- function(x, ref = 16.35160) {
  .Deprecated("semitones", package = "reindeer")
  semitones(x, ref = ref)
}




#' Convert frequency (Hz) to Equivalent Rectangular Bandwidth (ERB) number
#'
#' Maps physical frequencies (Hz) to the number of ERBs below them, using
#' the rectangular-bandpass model of human auditory filters from
#' \insertCite{Moore:1982ha}{reindeer}. Useful for psychoacoustic
#' analyses where the perceptual scale matters more than the raw Hz.
#'
#' @param f Numeric vector of frequencies in Hz.
#' @return Numeric vector of ERB-numbers.
#' @references \insertAllCited{}
#' @examples
#' erb(c(500, 1000, 2000, 4000))
#' # ERB vs Bark scale
#' f <- seq(50, 8000, 50)
#' plot(f, erb(f), type = "l", ylab = "ERB / Bark")
#' lines(f, emuR::bark(f), lty = 2)
#' @seealso [semitones()]
#' @export
erb <- function(f) {
  11.17 * log((f + 0.312) / (f + 14.675)) + 43
}
