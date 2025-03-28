% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_SEM.R
\name{report_cfa}
\alias{report_cfa}
\title{Report}
\usage{
report_cfa(model, file = NULL, w = 10, h = 10)
}
\arguments{
\item{model}{lavaan object}

\item{file}{output filename}

\item{w}{width of pdf file}

\item{h}{height of pdf file}
}
\description{
Report
}
\examples{
model='LATENT=~ITEM1+ITEM2+ITEM3+ITEM4+ITEM5'
df<-lavaan::simulateData(model=model,model.type="cfa",
                             return.type="data.frame",sample.nobs=100)
df<-generate_missing(df)
fit<-lavaan::cfa(model,data=df,missing="ML")
report_cfa(fit)
report_cfa(fit,file="cfa")
}
\keyword{SEM}
