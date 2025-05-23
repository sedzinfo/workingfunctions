% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTIONS_UNIX_TIME.R
\name{decompose_datetime}
\alias{decompose_datetime}
\title{Decompose datetime objects to dataframe collumns}
\usage{
decompose_datetime(
  x,
  format = "",
  origin = "1970-01-01",
  tz = "GMT",
  extended = FALSE,
  breaks = c(-1, 5, 13, 16, 20, 23),
  ...
)
}
\arguments{
\item{x}{datetime object}

\item{format}{date time format}

\item{origin}{Starting date. The default is the unix time origin "1970-01-01"}

\item{tz}{Timezone}

\item{extended}{if TRUE it will display additional day time categories \cr
WEEKDAY MONTH JULIAN QUARTER DAY_PERIOD}

\item{breaks}{Numeric vector Breaks define hour of day for classifiying into  \cr
"Night", "Morning", "Noon", "Afternoon", "Evening". \cr}

\item{...}{arguments passed to as.POSIXct
This argument is used if extended=TRUE}
}
\description{
Decompose datetime objects to dataframe collumns
}
\examples{
timestamp1<-as.numeric(as.POSIXct(Sys.Date()))
timestamp2<-as.numeric(as.POSIXct(Sys.time()))
d1<-Sys.Date()
d2<-Sys.time()
decompose_datetime(x=d1)
decompose_datetime(x=d2)
decompose_datetime(x=d1,extended=TRUE)
decompose_datetime(x=d2,extended=TRUE)
decompose_datetime(x="01/15/1900",format="\%m/\%e/\%Y")
decompose_datetime(x="01/15/1900",format="\%m/\%e/\%Y",extended=TRUE)
decompose_datetime(x=as.Date(as.POSIXct(10000,origin="1970-01-01")))
decompose_datetime(x=as.Date(as.POSIXct(timestamp1,origin="1970-01-01")),
                   format="\%m/\%e/\%Y")
decompose_datetime(x=as.Date(as.POSIXct(timestamp2,origin="1970-01-01")),
                   format="\%m/\%e/\%Y")
}
\keyword{timestamp}
