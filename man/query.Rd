% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_emuR_re-export.R
\name{query}
\alias{query}
\title{Query an emuDB. (A copy of the emuR function)}
\usage{
query(
  emuDBhandle,
  query,
  sessionPattern = ".*",
  bundlePattern = ".*",
  queryLang = "EQL2",
  timeRefSegmentLevel = NULL,
  resultType = "tibble",
  calcTimes = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{emuDBhandle}{emuDB handle object (see \link[emuR]{load_emuDB})}

\item{query}{string (see vignette \url{https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html})}

\item{sessionPattern}{A regular expression pattern matching session names to 
be searched from the database}

\item{bundlePattern}{A regular expression pattern matching bundle names to be 
searched from the database}

\item{queryLang}{query language used for evaluating the query string}

\item{timeRefSegmentLevel}{set time segment level from which to derive time 
information. It is only necessary to set this parameter if more than one child 
level contains time information and the queried parent level is of type ITEM.}

\item{resultType}{type (class name) of result (either 'tibble', 'emuRsegs' or 
'emusegs' (use 'emusegs' for legacy compatablility only))}

\item{calcTimes}{calculate times for resulting segments (results in 
\code{NA} values for start and end times in emuseg/emuRsegs). As it can be 
very computationally expensive to 
calculate the times for large nested hierarchies, it can be turned off via this 
parameter.}

\item{verbose}{be verbose. Set this to \code{TRUE} if you wish to choose which 
path to traverse on intersecting hierarchies. If set to \code{FALSE} (the default) 
all paths will be traversed (= legacy EMU behavior).}
}
\value{
result set object of class resultType (default: \link{tibble}, 
compatible to legacy types \link[emuR]{emuRsegs} and \link[emuR]{emusegs})
}
\description{
Function to query annotation items/structures in an emuDB
}
\details{
Evaluates a query string of query language queryLang on an 
emuDB referenced by \code{emuDBhandle} and returns a segment list of the desired type resultType.  
For details of the query language please refer to the EMU-SDMS manual's query 
system chapter (\url{https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html}).
This function extracts a list of segments which meet the conditions given by the query string. 
A segment can consist of one (e.g. 's') or more (e.g. 's->t') items from 
the specified emuDB level. Segment objects (type 'SEGMENT') contain the label 
string and the start and end time information of the segment (in ms). 
The \code{tibble} return type (now the defaults) objects additionally contain 
sample position of start and end item. 
Time information of symbolic elements (type 'ITEM') are derived from linked SEGMENT 
levels if available. If multiple linked SEGMENT levels exist, you can specify the 
level with the \code{timeRefSegmentLevel} argument. If time and sample values cannot be 
derived they will be set to \code{\link{NA}}. \link{tibble}s will 
be ordered by the columns UUID, session, bundle and sequence index (seq_idx). 
Legacy \link[emuR]{emusegs} lists are ordered by the columns utts and start.
The query may be limited to session and/or bundle names specified by regular 
expression pattern strings (see \link{regex}) in parameters \code{sessionPattern} 
respectively \code{bundlePattern}.
}
\seealso{
\link[emuR:query]{emuR::query}
}
