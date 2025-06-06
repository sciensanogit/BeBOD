% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_cb}
\alias{read_cb}
\title{Read from Windows Clipboard}
\usage{
read_cb(dec = ",", sep = "\\t", ...)
}
\arguments{
\item{dec}{Decimal separator.}

\item{sep}{Field separator.}

\item{...}{Additional arguments passed to `read.table`.}
}
\value{
A data frame read from the clipboard.
}
\description{
Reads a table from the Windows clipboard.
}
