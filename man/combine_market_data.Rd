% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{combine_market_data}
\alias{combine_market_data}
\title{Fusionner les données relatives aux marchés}
\usage{
combine_market_data(dir_in, dir_regexp = "_STATA_", data_type, dir_out)
}
\arguments{
\item{dir_in}{Character. Root directory where food group sub-directories are located.}

\item{dir_regexp}{Character. Regular expression to identify folders over which to iterate.}

\item{data_type}{Character. One of: "consumption", "production".
For data from markets, "consumption"; for data from farmers, "production".}

\item{dir_out}{Character. Directory where combined files should be saved.}
}
\description{
Créer un fichier \code{marches.dta} dans le répertoire désigné dans \code{dir_out}.
}
