% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTIONS.R
\name{c_bind}
\alias{c_bind}
\title{cbind dataframes with unequal lengths or row lengths}
\usage{
c_bind(..., first = TRUE)
}
\arguments{
\item{...}{dataframes or vectors to bind}

\item{first}{Logical}
}
\description{
cbind dataframes with unequal lengths or row lengths
}
\examples{
c_bind(rnorm(10),rnorm(11),rnorm(12),rnorm(13))
}
\author{
Ananda Mahto
}
\keyword{functions}
