% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_pull.R
\name{sql_pull}
\alias{sql_pull}
\title{sql_pull}
\usage{
sql_pull(.df, x, na.rm = T, quote = "single", comma = T)
}
\arguments{
\item{.df}{A .df frame object}

\item{x}{Column to copy}

\item{na.rm}{Option to exclude NA values}

\item{quote}{Double or single quote to wrap around each observation}

\item{comma}{Option to add a comma after each observation}
}
\value{
A .df frame object with the new column in the first position
}
\description{
Pull, quote and copy column as vector to clipboard
}
\examples{
dplyr::starwars \%>\%
  sql_pull(homeworld)
}
