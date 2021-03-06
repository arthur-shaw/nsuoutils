% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{ingest_dta_file}
\alias{ingest_dta_file}
\title{Read Stata file into memory}
\usage{
ingest_dta_file(dir, file_name)
}
\arguments{
\item{dir}{Character vector. File path to directory}

\item{file_name}{Character vector. File name with \code{.dta} extension}
}
\value{
Data frame
}
\description{
First, ingest data file.
Then, write it to an object in the global environment, where the object name is the file name minus the \code{.dta} extension.
These names follow the same pattern as the input files in order to facilitate merging.
}
