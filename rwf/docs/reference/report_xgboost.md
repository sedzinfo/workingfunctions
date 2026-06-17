# Report for xgboost::xgb.train

Report for xgboost::xgb.train

## Usage

``` r
report_xgboost(
  model,
  validation_data = NULL,
  label = NULL,
  file = "xgboost",
  w = 10,
  h = 10,
  base_size = 10,
  title = "",
  fast = FALSE
)
```

## Arguments

- model:

  object from xgboost::xgb.train

- validation_data:

  validation data

- label:

  outcome variable name

- file:

  output filename

- w:

  width of pdf file

- h:

  height of pdf file

- base_size:

  base font size

- title:

  plot title

- fast:

  if TRUE error values are not saved in output

## Examples

``` r
infert_formula<-formula(case~education+spontaneous+induced)
boston_formula<-formula(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat)
train_test_classification<-k_fold(df=infert,model_formula=infert_formula)
#> Fold Cases: 1 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 2 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 3 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 4 Train: 224 Test: 24 Total: 248 Unique Train: 224 Unique Test: 24 
#> Fold Cases: 5 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 6 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 7 Train: 224 Test: 24 Total: 248 Unique Train: 224 Unique Test: 24 
#> Fold Cases: 8 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 9 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
#> Fold Cases: 10 Train: 223 Test: 25 Total: 248 Unique Train: 223 Unique Test: 25 
train_test_regression<-k_fold(df=MASS::Boston,model_formula=boston_formula)
#> Fold Cases: 1 Train: 455 Test: 51 Total: 506 Unique Train: 455 Unique Test: 51 
#> Fold Cases: 2 Train: 455 Test: 51 Total: 506 Unique Train: 455 Unique Test: 51 
#> Fold Cases: 3 Train: 456 Test: 50 Total: 506 Unique Train: 456 Unique Test: 50 
#> Fold Cases: 4 Train: 455 Test: 51 Total: 506 Unique Train: 455 Unique Test: 51 
#> Fold Cases: 5 Train: 456 Test: 50 Total: 506 Unique Train: 456 Unique Test: 50 
#> Fold Cases: 6 Train: 455 Test: 51 Total: 506 Unique Train: 455 Unique Test: 51 
#> Fold Cases: 7 Train: 456 Test: 50 Total: 506 Unique Train: 456 Unique Test: 50 
#> Fold Cases: 8 Train: 455 Test: 51 Total: 506 Unique Train: 455 Unique Test: 51 
#> Fold Cases: 9 Train: 456 Test: 50 Total: 506 Unique Train: 456 Unique Test: 50 
#> Fold Cases: 10 Train: 455 Test: 51 Total: 506 Unique Train: 455 Unique Test: 51 
xgb_classification<-xgboost::xgb.train(
                    params=xgboost::xgb.params(objective="binary:logistic"),
                    data=train_test_classification$xgb$f1$train,
                    evals=train_test_classification$xgb$f1$watchlist,
                    nround=20)
#> [1]  train-logloss:0.605650  test-logloss:0.510043 
#> [2]  train-logloss:0.581905  test-logloss:0.479165 
#> [3]  train-logloss:0.567342  test-logloss:0.458180 
#> [4]  train-logloss:0.558139  test-logloss:0.445293 
#> [5]  train-logloss:0.552146  test-logloss:0.436102 
#> [6]  train-logloss:0.548177  test-logloss:0.429405 
#> [7]  train-logloss:0.545518  test-logloss:0.424525 
#> [8]  train-logloss:0.543717  test-logloss:0.420934 
#> [9]  train-logloss:0.542515  test-logloss:0.418269 
#> [10] train-logloss:0.541637  test-logloss:0.416274 
#> [11] train-logloss:0.541022  test-logloss:0.414769 
#> [12] train-logloss:0.540482  test-logloss:0.415183 
#> [13] train-logloss:0.540138  test-logloss:0.415564 
#> [14] train-logloss:0.539898  test-logloss:0.415916 
#> [15] train-logloss:0.539619  test-logloss:0.415344 
#> [16] train-logloss:0.539465  test-logloss:0.415745 
#> [17] train-logloss:0.539090  test-logloss:0.413128 
#> [18] train-logloss:0.538914  test-logloss:0.413898 
#> [19] train-logloss:0.538612  test-logloss:0.411544 
#> [20] train-logloss:0.538448  test-logloss:0.412395 
xgb_regression<-xgboost::xgb.train(
                data=train_test_regression$xgb$f1$train,
                evals=train_test_regression$xgb$f1$watchlist,
                nround=20)
#> [1]  train-rmse:6.870495 test-rmse:6.140053 
#> [2]  train-rmse:5.174978 test-rmse:4.766540 
#> [3]  train-rmse:3.989148 test-rmse:3.912938 
#> [4]  train-rmse:3.164143 test-rmse:3.379425 
#> [5]  train-rmse:2.550406 test-rmse:3.113676 
#> [6]  train-rmse:2.125212 test-rmse:2.908214 
#> [7]  train-rmse:1.772546 test-rmse:2.832399 
#> [8]  train-rmse:1.503942 test-rmse:2.749718 
#> [9]  train-rmse:1.318182 test-rmse:2.715418 
#> [10] train-rmse:1.196557 test-rmse:2.703126 
#> [11] train-rmse:1.076994 test-rmse:2.665380 
#> [12] train-rmse:0.985250 test-rmse:2.638356 
#> [13] train-rmse:0.921539 test-rmse:2.628089 
#> [14] train-rmse:0.861602 test-rmse:2.613303 
#> [15] train-rmse:0.816591 test-rmse:2.608229 
#> [16] train-rmse:0.768588 test-rmse:2.585280 
#> [17] train-rmse:0.730496 test-rmse:2.577266 
#> [18] train-rmse:0.689913 test-rmse:2.566552 
#> [19] train-rmse:0.667255 test-rmse:2.578248 
#> [20] train-rmse:0.648527 test-rmse:2.588789 
if (FALSE) { # \dontrun{
report_xgboost(model=xgb_classification,
               validation_data=train_test_classification$f$test$f1,
               label=train_test_classification$outcome,
               file="Classification")
report_xgboost(model=xgb_regression,
               validation_data=train_test_regression$f$test$f1,
               label=train_test_regression$outcome,
               file="Regression")
} # }
```
