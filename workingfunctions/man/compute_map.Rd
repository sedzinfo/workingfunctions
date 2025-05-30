% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_IRT_T.R
\name{compute_map}
\alias{compute_map}
\title{Simulate prior distribution}
\usage{
compute_map(eta, mean = 0, sd = 1)
}
\arguments{
\item{eta}{vector}

\item{mean}{numeric}

\item{sd}{numeric}
}
\description{
Simulate prior distribution
}
\examples{
eta<-seq(-6,6,by=0.1)
compute_map(eta=eta,mean=0,sd=1)
}
\keyword{IRT}
\keyword{Thurstonian}
