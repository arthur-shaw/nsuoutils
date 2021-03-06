% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{fix_misc_issues}
\alias{fix_misc_issues}
\title{Fix miscellaneous issues with input data}
\usage{
fix_misc_issues(df, df_type)
}
\arguments{
\item{df}{Data frame whose disparate issues to fix.}

\item{df_type}{Character. For consumption data, one of : "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
For production data, one of: "unitesFixes", "unitesAutre1", "unitesAutre2"}
}
\value{
Data frame with corrections, where applicable.
}
\description{
Master fix-it function that applies solutions to known data issues
}
\details{
Can easily be extended to new issues. Fixes to issues are organized as follows:
\itemize{
\item At the top level, group by file type
\item Within file type, group by particular files by checking for file-specific column names
}
}
