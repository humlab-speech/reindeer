% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_database.R,
%   R/reindeeR_emuR_re-export.R
\name{rename_bundles}
\alias{rename_bundles}
\title{Batch rename bundles}
\usage{
rename_bundles(emuDBhandle, bundles)

rename_bundles(emuDBhandle, bundles)
}
\arguments{
\item{emuDBhandle}{An emuR database handle}

\item{bundles}{data.frame like object with the columns
\itemize{
\item \code{session}: name of sessions containing bundle
\item \code{name}: name of bundle
\item \code{name_new}: new name given to bundle
}
It is worth noting that \code{session} and \code{name} are the columns returned by 
\code{\link[emuR]{list_bundles}}.}

\item{from}{A tibble or data.frame with columns "session" and "name",
identifying the bundle to rename. The usual procedure is to first invoke
\link{list_bundles} and then filter out the bundles the user wants to
rename.}

\item{to}{A vector of strings containing new names of the bundles in \code{from}.
The vector has to be of the same length as the number of rows in \code{from}.}

\item{simulate}{Boolean; Should the bundle renaming just be simulated, or
actually performed?}
}
\value{
Either the updated bundle listing, or a tibble showing the file
renaming plan.
}
\description{
This function simplifies renaming of bundles.
}
\details{
The user it expected to supply a data frame and tibble with "session" and
"name" columns, and a vector of new bundle names with the length as the
number of rows in the \code{from} data.frame. The function will then rename all
bundle directories and all files in the bundle directories to the new name
one by one.

Bundle renaming is a sensitive process and many things can go wrong.
Therefore the function will by default simulate the file renaming and return
a summary of the plan for the user to review. The user may then enter the
same arguments again and set \code{simylate=TRUE} to actually perform the renaming.

If the database is part of a git repository, the changes will be committed and pushed
}
\examples{

\dontrun{
library(dplyr)
reindeer:::create_ae_db() -> emuDBhandle
r <- list_bundles(emuDBhandle) \%>\%
  dplyr::mutate(to=toupper(name)) \%>\%
  dplyr::mutate(to=stringr::str_replace(to,"([A-Z]+)([0-9]+)","\\\\1-\\\\2")) \%>\%
  dplyr::mutate(to=tolower(to))

 print(rename_bundles(emuDBhandle,r[1:2,c("session","name")],r[1:2,"to"],simulate=TRUE))
}

}
\seealso{
\link[emuR:rename_bundles]{emuR::rename_bundles}
}
