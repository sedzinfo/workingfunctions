% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_RELIABILITY.R
\name{report_alpha}
\alias{report_alpha}
\title{Estimate alpha for several dimensions and export results to xlsx}
\usage{
report_alpha(
  df,
  key = NULL,
  questions = NULL,
  reverse = NULL,
  mini = NULL,
  maxi = NULL,
  file = NULL,
  ...
)
}
\arguments{
\item{df}{dataframe}

\item{key}{index of trait names and items constituting a trait}

\item{questions}{trait names and items constituting a trait}

\item{reverse}{index of trait names and index for reversal}

\item{mini}{minimum rating in scale if NULL reversal will be performed using the empirical minimum}

\item{maxi}{maximum rating in scale if NULL reversal will be performed using the empirical maximum}

\item{file}{output filename}

\item{...}{arguments passed to psych::alpha}
}
\description{
Uses an arbitrary input
}
\examples{
set.seed(12345)
df<-data.frame(matrix(.5,ncol=6,nrow=6))
correlation_martix<-as.matrix(df)
diag(correlation_martix)<-1
df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
key<-list(f1=c("X1","X2","X3"),
          f2=c("X4","X5","X6"))
reverse<-list(f1=c(1,1,1),
              f2=c(1,1,1))
report_alpha(df=df,key=key,cumulative=TRUE,n.iter=1)
report_alpha(df=df,key=key,reverse=reverse,check.keys=FALSE,n.iter=2)
report_alpha(df=df,key=key,check.keys=FALSE,n.iter=2,file="alpha")
}
\keyword{reliability}
