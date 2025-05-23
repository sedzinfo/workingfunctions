% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUNCTIONS_TRAIN_TEST.R
\name{proportion_accurate}
\alias{proportion_accurate}
\title{Proportion overall accuracy of a confusion matrix}
\usage{
proportion_accurate(observed, predicted)
}
\arguments{
\item{observed}{Vector of observed variables. These are the true class labels.}

\item{predicted}{Vector of predicted variables. These are the predicted class labels.}
}
\description{
Calculates the overall accuracy and Cohen's kappa statistics of a confusion matrix.
}
\details{
This function evaluates the performance of a confusion matrix by calculating the overall accuracy and Cohen's kappa statistics.

The function performs the following steps:
1. Computes the confusion matrix from the observed and predicted values.
2. Calculates the diagonal proportion (overall accuracy) and the off-diagonal proportion.
3. Computes Cohen's kappa statistics (unweighted,linear,and squared weights).

The output is a data.frame containing the following metrics:
-`cm_diagonal`: Proportion of correct classifications (diagonal elements).
-`cm_off_diagonal`: Proportion of misclassified observations (off-diagonal elements).
-`kappa_unweighted`: Cohen's kappa statistic with no weights.
-`kappa_linear`: Cohen's kappa statistic with linear weights.
-`kappa_squared`: Cohen's kappa statistic with squared weights.
}
\examples{
# Example with numeric observed and predicted values
proportion_accurate(observed=c(1,2,3,4,5,10),predicted=c(1,2,3,4,5,11))

}
\keyword{functions}
