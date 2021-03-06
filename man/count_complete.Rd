% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_complete.R
\name{count_complete}
\alias{count_complete}
\title{count_complete}
\usage{
count_complete(.df, ..., boolean = F, name = "completeFields")
}
\arguments{
\item{.df}{.df frame like object}

\item{...}{Columns to count}

\item{boolean}{Option to count Boolean fields with a \code{FALSE} value as NA}

\item{name}{Option to name the new variable}
}
\description{
Count the columns per row that contain .df
}
\examples{
\dontrun{
mpg \%>\%
group_by(model) \%>\%
 mutate(dummy = floor(runif(1, min=min(cyl), max=max(cyl))),
        cyl = ifelse(cyl == dummy, NA_real_, cyl)) \%>\%
 ungroup() \%>\%
 select(-dummy) \%>\%
count_complete(starts_with('c'), name = 'hello')
}
}
