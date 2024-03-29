

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

