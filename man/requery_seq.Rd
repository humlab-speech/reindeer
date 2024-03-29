% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_emuR_re-export.R
\name{requery_seq}
\alias{requery_seq}
\title{Requery the sequential context of a segment list. (A copy of the emuR function)}
\usage{
requery_seq(
  emuDBhandle,
  seglist,
  offset = 0,
  offsetRef = "START",
  length = 1,
  ignoreOutOfBounds = FALSE,
  resultType = "tibble",
  calcTimes = TRUE,
  timeRefSegmentLevel = NULL,
  verbose = FALSE
)
}
\arguments{
\item{emuDBhandle}{emuDB handle as returned by \code{\link[emuR]{load_emuDB}}}

\item{seglist}{segment list to requery on (type: 'tibble' or 'emuRsegs')}

\item{offset}{start item offset in sequence (default is 0, meaning the start 
or end item of the input segment)}

\item{offsetRef}{reference item for offset: 'START' for first and 'END' 
for last item of segment}

\item{length}{item length of segments in the returned segment list}

\item{ignoreOutOfBounds}{ignore result segments that are out of bundle bounds}

\item{resultType}{type of result (either 'tibble' == default, 'emuRsegs')}

\item{calcTimes}{calculate times for resulting segments (results in \code{NA} 
values for start and end times in emuseg/emuRsegs). As it can be very 
computationally expensive to calculate the times for large nested hierarchies, 
it can be turned off via this boolean parameter.}

\item{timeRefSegmentLevel}{set time segment level from which to derive time 
information. It is only necessary to set this parameter if more than one 
child level contains time information and the queried parent level is of type ITEM.}

\item{verbose}{be verbose. Set this to \code{TRUE} if you wish to choose which 
path to traverse on intersecting hierarchies. If set to \code{FALSE} (the 
default) all paths will be traversed (= legacy EMU behaviour).}
}
\value{
result set object of class \link[emuR]{emuRsegs} or \link{tibble}
}
\description{
Function to requery sequential context of a segment list queried 
from an emuDB
}
\details{
Builds a new segment list on the same hierarchical level 
and the same length as the segment list given in \code{seglist}. The 
resulting segments usually have different start position and length (in 
terms of items of the respective level) controlled by the \code{offset},
\code{offsetRef} and \code{length} parameters.
A segment here is defined as a single item or a chain of items from the 
respective level, e.g. if a level in a bundle instance has labels 'a', 'b' 
and 'c' in that order, 'a' or 'a->b' oder 'a->b->c' are all valid segments, 
but not 'a->c'.
\code{offsetRef} determines if the position offset is referenced to the 
start or the end item of the segments in the input list \code{seglist}; 
parameter \code{offset} determines the offset of the resulting item start 
position to this reference item; parameter \code{length} sets the item 
length of the result segments. If the requested segments are out of bundle 
item boundaries and parameter \code{ignoreOutOfBounds} is \code{FALSE} 
(the default), an error is generated. To get residual resulting segments 
that lie within the bounds the \code{ignoreOutOfBounds} parameter can be 
set to \code{TRUE}. The returned segment list is usually of the same 
length and order as the input \code{seglist}; if \code{ignoreOutOfBounds=FALSE}, 
the resulting segment list may be out of sync.
}
\seealso{
\link[emuR:requery_seq]{emuR::requery_seq}
}
