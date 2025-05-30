% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_IRT.R
\name{report_irt}
\alias{report_irt}
\title{Output for irt model}
\usage{
report_irt(model, m2 = TRUE, file = NULL)
}
\arguments{
\item{model}{object mirt}

\item{m2}{if TRUE report m2 statistics}

\item{file}{output filename}
}
\description{
Output for irt model
}
\examples{
set.seed(12345)
cormatrix<-psych::sim.rasch(nvar=5,n=50000,low=-4,high=4,d=NULL,a=1,mu=0,sd=1)$items
irt_onefactor<-mirt::mirt(cormatrix,1,empiricalhist=TRUE,calcNull=TRUE)
irt_twofactor<-mirt::mirt(cormatrix,2,empiricalhist=TRUE,calcNull=TRUE)
irt_threefactor<-mirt::mirt(cormatrix,3,empiricalhist=TRUE,calcNull=TRUE)
report_irt(model=irt_onefactor,file="one_factor")
report_irt(model=irt_twofactor,file="two_factors")
report_irt(model=irt_threefactor,file="three_factors")
}
\keyword{IRT}
