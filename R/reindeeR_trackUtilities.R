

#' A function that may be used to divide up a range of times into bins
#'
#' This is a simple utility function to just create simple bins for a set of
#' time indicators (vector). The user supplies the number of bins needed, and this function
#' than returns a factor as long as the original vector, but with labels from
#' $1..bins$. That is, if the user sets [bins=10], then the user gets a factor
#' where each value in the original vector has been given a number 1 to 10
#' according to what bin it belongs to. If the user wants percentage labels
#' instead, then the bins are renamed accordingly. For instance, if
#' \code{bins=4,percent=FALSE}, then the user gets four bins named 1 to 4. If
#' \code{bins=4,percent=TRUE}, then the user gets four bins labeled  25%, 50%,
#' 75% and 100% respectively. The bins are always inclusive to the left, so that
#' the first bin will always include the lowest value. The 30% bin will include
#' values up until and including the value indicating the 30% cutoff of the
#' interval.
#'
#' The function is just a convenience function and a wrapper around [base::cut]
#' (with \code{include.lowest = TRUE,right = TRUE}), but is intended to ensure
#' uniform and reliable binning of track data by time so that data belonging to
#' the same segment may be combined even if extracted using different analysis
#' windows.
#'
#' @param x The vector of values that need to be placed into discrete bins
#' @param bins The number of bins.
#' @param percent Should the bin labels instead indicate the (high) cutoff percentage?
#'
#' @return a factor with \code{bins} bins, into which all items in \code{x} have been places.
#' @export
#'
#'
bin_time <- function(x,bins=10,percent=FALSE){
  if(length(x) < bins ) warning("The number of bins are too large considering the number of data points.\nMissing values (NA) will be produced. Please ask for a smaller number of bins.")
  labs <- seq(1,bins,1)

  if(percent){
    labs <- paste0(labs * (100 / bins),"%")
  }

  out <- cut(x,breaks=bins,include.lowest = TRUE,right = TRUE,labels=labs,ordered_result=TRUE)
  return(out)
}


#' Cut a vector of ages into age ranges
#'
#' The function makes it easier to construct well presented range factors from a
#' vector of ages, by stating explicitly the first ages that should be included
#' in a new factor level (as a vector). Alternatively, the used may specify that
#' a new level should start every `new.at` (integer) years.
#'
#' @details The function does not perform any rounding up or down, truncating or
#'   any other transformation on the vector of ages before cutting it into
#'   discrete age ranges. So, if the user wants a person of age 10 years and 8
#'   months to be considered part of a "11-13 yrs" age range rather than a
#'   preceding "9-10 yrs" range, then the user should round the ages to whole
#'   integers (e.g. using `round(ages,0)` before applying this function.
#'
#' @param ages The vector of ages.
#' @param new.at A vector of ages at which a new age range should be started,
#'   *or* a single integer indicating the interval where a new interval is
#'   started. For instance, `new.at=10` indicates that a range will be
#'   constructed for "20-29 yrs", "30-39 yrs", and so on.
#' @param suffix The string suffix that is appended to the age range (defaults
#'   to " yrs"). The user has to add a space to the suffix if required for the
#'   application.
#' @param start.at.zero Should the first interval start at zero (the default).
#'
#' @return A factor of age ranges.
#' @export
#'
#' @examples
#' ages <- c(1,1.99999,2,4,10,11)
#' # Unevenly spaced age ranges
#' age_ranges(ages, new.at=c(0,2,4,6,8,seq(10,130,10)))
#' # Evenly spaced age ranges
#' age_ranges(ages, new.at=5)
#'
age_ranges <- function(ages, new.at,suffix=" yrs",start.at.zero=TRUE){

  if(length(new.at) == 1) new.at <- c(0,seq(new.at,((max(ages) %/% new.at) +1 )* new.at, new.at))

  #This makes sure teh age ranges are always complete, even when not starting from zero
  if(! start.at.zero) new.at <- c(min(ages)-1, new.at)

  #Make sure the age range starts at 0
  if(new.at[[1]] != 0 && start.at.zero) new.at <- c(0,new.at)
  labs <- paste0(c(paste(head(new.at,n=-1),c(new.at[-1]-1),sep="-"), paste0(" ≥",tail(new.at,n=1))),suffix)

  cut(ages,breaks = c(new.at,max(ages)+1),right = FALSE,include.lowest = TRUE,labels=labs)
}

