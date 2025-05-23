% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_IRT_T.R
\name{response_dimension}
\alias{response_dimension}
\title{index parameter and items relative to their dimensions}
\usage{
response_dimension(response, dimensions, items)
}
\arguments{
\item{response}{vector one to number of items}

\item{dimensions}{number of dimensions}

\item{items}{item comparisons}
}
\description{
index parameter and items relative to their dimensions
}
\examples{
response_dimension(c(1:18),3,c(1,2))
response_dimension(c(1:18),3,c(1,3))
response_dimension(c(1:18),3,c(2,3))
}
\keyword{IRT}
\keyword{Thurstonian}
