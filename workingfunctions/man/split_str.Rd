% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTIONS_STRINGS.R
\name{split_str}
\alias{split_str}
\title{Split string to dataframe}
\usage{
split_str(vector, split = "/", include_original = FALSE)
}
\arguments{
\item{vector}{String}

\item{split}{Separation character}

\item{include_original}{if TRUE it will return the input on a separate collumn}
}
\description{
Split string to dataframe
}
\examples{
string<-paste0(1:10,"/",
               generate_string(nchar=2,vector_length=10),"/",
               generate_string(nchar=2,vector_length=10),"/",
               generate_string(nchar=2,vector_length=10))
split_str(string,split="/")
}
\keyword{functions}
\keyword{strings}
