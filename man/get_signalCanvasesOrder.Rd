% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_emuR_re-export.R
\name{get_signalCanvasesOrder}
\alias{get_signalCanvasesOrder}
\title{Get which signals are displayed in a perspective, and in which order. (A copy of the emuR function)}
\usage{
get_signalCanvasesOrder(emuDBhandle, perspectiveName)
}
\arguments{
\item{emuDBhandle}{emuDB handle as returned by \code{\link[emuR]{load_emuDB}}}

\item{perspectiveName}{name of perspective}
}
\description{
Set / Get signalCanvasesOrder array that specifies which signals are 
displayed in the according perspective by the EMU-webApp. An entry in this character vector 
refers to either the name of an ssffTrackDefinition or a predefined string: \code{"OSCI"} which 
represents the oscillogram or \code{"SPEC"} which represents the 
spectrogram. For more information on the structural elements of an emuDB 
see \code{vignette{emuDB}}.
}
\seealso{
\link[emuR:SetGetSignalCanvasesOrder]{emuR::get_signalCanvasesOrder}
}
