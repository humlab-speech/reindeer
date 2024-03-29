% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_emuR_re-export.R
\name{list_labelGroups}
\alias{list_labelGroups}
\title{List global label groups. (A copy of the emuR function)}
\usage{
list_labelGroups(emuDBhandle)
}
\arguments{
\item{emuDBhandle}{emuDB handle as returned by \code{\link[emuR]{load_emuDB}}}
}
\description{
Add / List / Remove label group that can be used as a short hand  
to reference groups of labels that are globally defined
for the entire database (compared to attribute definition
specific label groups that 
are added by \code{\link[emuR]{add_attrDefLabelGroup}}) in a 
\code{\link[emuR]{query}}. A common example would be to
add a label group for something like the phonetic
category of nasals to be able to reference them 
as "nasals" in a \code{\link[emuR]{query}}. 
In theory you could use a labelGroupName as a label instance within the
level, but since this could lead to serious confusion, it is better avoided.
For users transitioning from the legacy EMU system: Do not confuse a 
labelGroup with legal labels: a labelGroup 
had the unfortunate name 'legal labels' in the legacy EMU system.  
For more information on the structural elements of an emuDB 
see \code{vignette{emuDB}}.
}
\seealso{
\link[emuR:AddListRemoveLabelGroup]{emuR::list_labelGroups}
}
