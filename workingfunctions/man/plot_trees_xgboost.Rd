% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ML_XGBOOST.R
\name{plot_trees_xgboost}
\alias{plot_trees_xgboost}
\title{Plot trees for xgboost::xgb.train}
\usage{
plot_trees_xgboost(model, train, file = "xgboost")
}
\arguments{
\item{model}{object from xgboost::xgb.train}

\item{train}{Train dataset}

\item{file}{output filename}
}
\description{
Plot trees for xgboost::xgb.train
}
\examples{
infert_formula<-formula(case~education+spontaneous+induced)
boston_formula<-formula(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat)
train_test_classification<-k_fold(df=infert,model_formula=infert_formula)
train_test_regression<-k_fold(df=MASS::Boston,model_formula=boston_formula)
xgb_classification<-xgboost::xgb.train(
                    data=train_test_classification$xgb$f1$train,
                    watchlist=train_test_classification$xgb$f1$watchlist,
                    eta=.1,
                    nthread=8,
                    nround=20,
                    objective="binary:logistic")
xgb_regression<-xgboost::xgb.train(
                data=train_test_regression$xgb$f1$train,
                watchlist=train_test_regression$xgb$f1$watchlist,
                eta=.3,
                nthread=8,
                nround=20)
# xgboost::xgb.plot.multi.trees(model=xgb_classification,features_keep=2)
# plot_trees_xgboost(model=xgb_classification,
#                    train=train_test_classification$xgb$f1,
#                    file="Classification")
# plot_trees_xgboost(model=xgb_regression,
#                    train=train_test_regression$xbg$f1,
#                    file="Regression")
}
\keyword{ML}
