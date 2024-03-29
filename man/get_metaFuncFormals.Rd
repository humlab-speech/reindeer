% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_signalextensions.R
\name{get_metaFuncFormals}
\alias{get_metaFuncFormals}
\title{A utility function that computes appropriate parameters for a bundle based on metadata}
\usage{
get_metaFuncFormals(
  emuDBhandle,
  session,
  bundle,
  onTheFlyFunctionName,
  onTheFlyParams = list(),
  metadata.defaults = list(Gender = NA, Age = 35),
  recompute = FALSE,
  package = "superassp"
)
}
\arguments{
\item{emuDBhandle}{The Emu database handle.}

\item{session}{Only consider this session.}

\item{bundle}{The bundle in the session to build an argument list for.}

\item{onTheFlyFunctionName}{The name of the digital speech processing function}

\item{onTheFlyParams}{If default parameters are given, they are applied to all bundles / files, overwriting parameters set based on metadata.}

\item{metadata.defaults}{Default values for metadata. Usually used for setting Age and Gender for bundles missing that information.}

\item{recompute}{If \code{TRUE} the Age and Gender specific DSP settings will be recomputed from the database. Please see \link{get_metadataParameters} for more information.}

\item{package}{}
}
\value{
A list which may be supplied to a \code{do.call} call for the DSP function.
}
\description{
This function may be used to derive what parameters would be appropriate to use for a single bundle (in a session) bases on metadata set for the bundle.
}
\seealso{
do.call
}
