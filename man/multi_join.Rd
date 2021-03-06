% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_join.R
\name{multi_join}
\alias{multi_join}
\title{multi_join}
\usage{
multi_join(
  df1,
  df2,
  .by = NULL,
  .fun = dplyr::left_join,
  .soft = T,
  .coalesce = T
)
}
\arguments{
\item{df1}{A data frame object}

\item{df2}{A data frame to join}

\item{.by}{A string or character vector delimited with \code{=} as the key(s)}

\item{.fun}{Function to call}

\item{.soft}{Case insensitive function}

\item{.coalesce}{Coalesce like columns}
}
\value{
A data frame object 1 joined with data frame object 2
}
\description{
Use \code{dplyr::join} to join with additional parameters like case insensitivity and coalesce
}
\examples{
a <- starwars \%>\%
  select(1:5)
b <- starwars \%>\%
  select(1:5) \%>\%
  mutate(name = str_to_upper(name)) \%>\%
  mutate(new_column = "hello")

t <- a \%>\%
  multi_join(b, .by = "name")
}
