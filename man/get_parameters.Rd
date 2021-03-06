% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reindeeR_signalextensions.R
\name{get_parameters}
\alias{get_parameters}
\title{Provides the user with speaker dependent signal processing parameters}
\usage{
get_parameters(file = NULL)
}
\arguments{
\item{file}{If the file exists, the settings stored in the file are read in and returned. If it does not exist, one will be created and the \link{DSPP} parameters inserted into it for the user to edit. If \code{NULL}, the \link{DSPP} structure will be returned directly}
}
\value{
A \link{tibble:tibble} containing some mandatory columns
\item{Gender}{Either "Male", "Female", or \code{NA}. \code{NA} parameters will be used in cases where the gender of a speaker is not known}
\item{Parameter}{The name of the parameter written exactly as expected by the called function}
\item{Default}{The value that should be given to functions that take the parameter indicated in the \code{Parameter} column. Most often, this will be a number, but in rare cases a single character is also possible. The whole column is therefore formated as a \code{character} list, although the value will likely be converted to a numeric before use.}
}
\description{
The source of the default signal processing parameters may be a spreadsheet
file in the OOXML Workbook ISO/IEC 29500:2008 standard format.The simplest
way to obtain such a file is to provide this function with a file name that
does not exist. This function will then write the \link{DSPP} set to the file, and
the user may then edit the file by hand using Microsoft Excel, Libreoffice
Calc or some other standard compliant software. Once edited, this function
may be used to read in the spreadsheet file to a tibble that may be used for
signal processing purposes.
}
\details{
Alternatively, the user may use this function to just get the defaults stored
in \link{DSPP} returned directly by not supplying a file name.
}
\examples{
xlsf <- tempfile(fileext = ".xlsx")
#Use the function to write out the default parameters DSPP to the temp file
get_parameters(xlsf)

#Now the user may modify the default settings to their liking using a spreadsheet program
# and then read the settings in again to a data frame using the same command again, and get a
# data frame that may be used by functions in this package.
#Just get the DSPP structure directly
data(DSPP)
newDSPP2 <- get_parameters()

}
