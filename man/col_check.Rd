% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col_check.R
\name{col_check}
\alias{col_check}
\title{col_check}
\usage{
col_check(.df, ...)
}
\arguments{
\item{.df}{A data frame object}

\item{...}{Columns to check}
}
\value{
A tibble with the percent of NA values in each column
}
\description{
col_check
}
\examples{
a <- c(1, 2, NA, NA)
b <- c(NA, NA, NA, 4)
c <- tibble(a, b)
c \%>\% col_check()
}
