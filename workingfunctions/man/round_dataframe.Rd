% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTIONS.R
\name{round_dataframe}
\alias{round_dataframe}
\title{Round dataframe}
\usage{
round_dataframe(df, digits = 0, type = "round")
}
\arguments{
\item{df}{dataframe}

\item{digits}{decimal points to return. It works only with "round" type}

\item{type}{"round" "ceiling" "floor" "tenth"}
}
\description{
It only processes numeric values in a dataframe
}
\examples{
round_dataframe(df=change_data_type(df=mtcars,type="factor"),digits=0)
round_dataframe(df=change_data_type(df=mtcars,type="character"),digits=0)
round_dataframe(df=mtcars,digits=0)
round_dataframe(df=mtcars,digits=0,type="ceiling")
round_dataframe(df=mtcars,digits=0,type="floor")
round_dataframe(df=mtcars*100,digits=2,type="tenth")
}
\keyword{functions}
