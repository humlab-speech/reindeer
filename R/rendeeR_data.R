#' The set of Default Signal Processing Parameters
#'
#' This data frame provides default parameters for signal processing functions
#' that may depend on speaker metadata. Currently, only Gender is considered.
#' The default settings defined in this collection is based on the
#' recommendations found in the literature for the signal processing functions
#' defined in [superassp] (and by extension [wrassp]).
#'
#' If the user defines their own signal processing function, it may be
#' advantageous to take advantage of the naming convention set here so that the
#' `DSPP` data set may simply be reused for the new function. For instance, it
#' is likely that a newly created f0 detection algorithm will still want some
#' guidance on the most probable frequency range for a particular speaker
#' knowing their gender, and naming this parameter `minF` and `maxF` will likely
#' simplify usage of the functions in this package as the `DSPP` may then be
#' used unaltered.
#'
#' The default settings contained within `DSPP` is collected directly from, or
#' estimated from, a number of sources. The age independent but gender dependent
#' parameters are generally found in the Praat
#' \insertCite{Praatdoingphoneti:2022}{reindeer} or [wrassp]
#' \insertCite{Winkelmann:2017cl}{reindeer} manuals. Nominal F1, F2 and F3 for
#' young and adolescent male and female speakers were estimated as the mid point
#' of the age dependent range published by
#' \insertCite{Lee.1999.10.1121/1.426686,Perry:2001it,Linville:2001io,Goy:2013bp,Kent.2018.10.1016/j.jcomdis.2018.05.004}{reindeer}.
#' The f0 ranges (`minF` and `maxF`) for children and adults have been
#' idenfified from previous studies
#' \insertCite{Graddol.1983.10.1177/002383098302600403,Goy:2013bp,Brockmann-Bauser.2015.10.1016/j.ijporl.2015.09.005}{reindeer}.
#' Analysis window length (`windowSize`) used when searching for formants have
#' been adopted from studies that include a wide range of participants
#' \insertCite{Huber.1999.10.1121/1.427150,Gonzalez.2004.10.1016/s0095-4470(03)00049-4,Cartei.2013.10.1371/journal.pone.0081022}{reindeer}.
#'
#' By default, the DSPP settings will ask formant tracking functions to find
#' five (5) formants within the frequence range of 50-`maxhzformant` Hz. The
#' default setting for the `maxhzformant` parameter follows the Praat manual
#' (that is, its set to 5000 Hz for male speakers and 5500 Hz for female
#' speakers). Some additional information age specific values for the
#' `maxhzformant` parameter has been obtained from
#' \insertCite{Escudero.2009.10.1121/1.3180321}{reindeer} for participants aged
#' 20-30 years.
#'
#' The user should be aware that the `numFormants` and `maxhzformant` interact,
#' so the user should take care when chaning either one of them as adjustment
#' also of the other will then be necessary.
#'
#'
#'
#' @format A [tibble::tibble] with three columns \describe{ \item{Gender}{The
#'   three values that should be used are : Male, Female and `NA`. `NA` row
#'   settings will be applied if Gender is not defined in the metadata for a
#'   speaker. } \item{Parameter}{The name of the parameter that should be
#'   supplied to the signal processing function} \item{Setting}{Defines the
#'   default setting for the parameter. Since some signal processing functions
#'   take character arguments, this column is defined as a character vector. The
#'   parameter values will however be converted to numeric if possible before
#'   applying them.} }
#' @references \insertAllCited{}
#'
"DSPP"
