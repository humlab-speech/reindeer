#' Convert Herz to Semitones
#'
#' This function takes a vector of frequencies (in Hz) and convert them to semitones. By default, the frequency of the note C$_1$ is used as the reference for the conversion, so that the output is compatible with the defined base of a Phonogram \insertCite{Schutte.1983.10.1159/000265703}{reindeer}.
#'
#' For Praat-compatible output please use one of 1, 100, 200, or 440 Hz as the reference frequency.
#'
#' @author Fredrik Karlsson
#'
#' @param x A vector of frequency values (in Hz)
#' @param ref The reference frequency. By default C$_1$ (32.703 Hz) which is the base frequency setting of a phonogram.
#'
#' @return Returns a vector of semitone scaled frequency values.
#' @export
#'
#' @references
#'  \insertAllCited{}
#' @examples
#' h <- seq(100,5000,100)
#' s100 <- st(h)
#' # Seimtone distance from C_0
#' sC0 <- st(h,ref=16.352)
#' # C1-C5 range in Semitones
#' strange <- st(523.251, ref=32.703)
#' strange
st <- function(x,ref=32.703){
  out <- 12 * log( x / ref ) / log(2)
  return(out)
}

