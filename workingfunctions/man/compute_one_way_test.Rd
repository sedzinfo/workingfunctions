% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLM_ANOVA.R
\name{compute_one_way_test}
\alias{compute_one_way_test}
\title{one way test}
\usage{
compute_one_way_test(formula, df, var.equal = TRUE)
}
\arguments{
\item{formula}{one way formula in form of y~x. It will ignore more complex formulas}

\item{df}{dataframe
eta squared ranges between 0 and 1 \cr
epsilon squared ranges between 0 and 1 \cr
eta squared multiplied by 100 indicates the percentage of variance in the dependent variable explained by the independent variable \cr}

\item{var.equal}{if TRUE it assumes equal variances}
}
\description{
one way test
}
\note{
eta and omega for Welch statistics are not adequately tested and they should not be consulted
}
\examples{
form<-formula(bp_before~agegrp)
compute_one_way_test(formula=form,df=df_blood_pressure,var.equal=TRUE)
compute_one_way_test(formula=form,df=df_blood_pressure,var.equal=FALSE)
oneway.test(formula=form,data=df_blood_pressure,var.equal=TRUE)
oneway.test(formula=form,data=df_blood_pressure,var.equal=FALSE)
car::Anova(aov(form,data=df_blood_pressure),type=2)
model<-lm(form,data=df_blood_pressure)
lsr::etaSquared(aov(form,data=df_blood_pressure),type=3,anova=TRUE)
sjstats::anova_stats(model,digits=22)
}
\keyword{ANOVA}
