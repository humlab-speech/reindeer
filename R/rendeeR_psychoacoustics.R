#' Convert Herz to Semitones
#'
#' This function takes a vector of frequencies (in Hz) and convert them to
#' semitones. By default, the frequency of the note C$_0$ is used as the
#' reference for the conversion, so that the output is compatible with the
#' defined base of a Phonogram
#' \insertCite{Schutte.1983.10.1159/000265703}{reindeer}.
#'
#' For Praat-compatible output please use one of 1, 100, 200, or 440 Hz as the
#' reference frequency.
#'
#' @author Fredrik Karlsson
#'
#' @param x A vector of frequency values (in Hz)
#' @param ref The reference frequency. By default 16.35160 Hz (C$_0$) which is
#'   the base frequency setting of a Phonogram.
#'
#' @return Returns a vector of semitone scaled frequency values.
#' @export
#'
#' @references \insertAllCited{}
#' @examples
#' h <- seq(100,5000,100)
#' # Praat has 100 Hz as one of the reference frequency options for semitone calculations
#' s100 <- st(h,ref=100)
#' # Semitone distance from C_0
#' sC0 <- st(h)
#' #The differ between semitones relative to C0 and a 100 Hz frequency
#' st(h,ref=100) - st(h)
#' # phonogram (C0-C4) range in Semitones
#' strange <- st(261.6256)
#' strange
st <- function(x,ref=16.35160){
  out <- 12 * log( x / ref ) / log(2)
  return(out)
}




#' Convert an Hz frequency to number of
#'
#' Convert physical frequencies (in Hz) to number of Equivalent rectangular
#' bandwidth (ERB) below the frequency. The ERB is  an approximation computed
#' from the bandwidths of filters in human hearing, as modeled by rectangular
#' band-pass filters.
#'
#' The formula for Number of ERBs given in
#' \insertCite{Moore:1982ha}{reindeer} is used
#' in the calculations.
#'
#' @param x A vector of frequency values (in Hz)
#'
#' @return A vector of ERB values
#' @export
#' @references \insertAllCited{}
#' @examples
#' f <- seq(1,10000,10)
#' plot(f,erb(f))
#  # Compare with the bark scale
#' plot(erb(f),emuR::bark(f),lty=2,ylab="Zwicker et al. Bark scale")
#' abline(a = 0, b=1,col="blue")

erb <- function(f){

    nerb <- 11.17 * log( (f + 0.312 ) / (f + 14.675))  + 43

  return(nerb)
}
