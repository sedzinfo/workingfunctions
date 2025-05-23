% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EXPLORE_DESCRIPTIVES.R
\name{compute_descriptives}
\alias{compute_descriptives}
\title{Descriptive statistics}
\usage{
compute_descriptives(df, dv, iv = NULL, file = NULL)
}
\arguments{
\item{df}{dataframe}

\item{dv}{index of dependent variables}

\item{iv}{index of independent variables}

\item{file}{output filename}
}
\description{
uses psych
}
\details{
returns xlsx
}
\examples{
compute_descriptives(df=mtcars,dv=1:5)
compute_descriptives(df=mtcars,dv=1:2,iv=9:10)
compute_descriptives(df=mtcars,dv=1:2,file="descriptives_no_factor")
compute_descriptives(df=mtcars,dv=1:2,iv=9:10,file="descriptives_factor")
}
\keyword{descriptives}
