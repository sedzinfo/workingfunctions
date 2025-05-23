% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EXPLORE_TIME_SERIES.R
\name{ts_smoothing}
\alias{ts_smoothing}
\title{Smoothing}
\usage{
ts_smoothing(
  df,
  start = 0.01,
  stop = 2,
  step = 0.001,
  title = "",
  type = "kernel"
)
}
\arguments{
\item{df}{ts object}

\item{start}{start value}

\item{stop}{stop value}

\item{step}{step}

\item{title}{plot title}

\item{type}{"default" "kernel" "lowess" "friedman" "splines" "polynomial" "linear"}
}
\description{
smoothing for timeseries. uses base plot
}
\details{
returns plot
}
\examples{
ts_data<-ts(UKDriverDeaths,start=1969,end=1984,frequency=12)
par(mfrow=c(2,2))
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="default")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="polynomial")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="linear")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="kernel")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="lowess")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="friedman")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="splines")
}
\keyword{series}
\keyword{time}
