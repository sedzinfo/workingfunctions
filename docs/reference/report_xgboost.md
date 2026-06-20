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
#> [1]  train-logloss:0.596514  test-logloss:0.560550 
#> [2]  train-logloss:0.569360  test-logloss:0.556052 
#> [3]  train-logloss:0.553321  test-logloss:0.557129 
#> [4]  train-logloss:0.543112  test-logloss:0.561428 
#> [5]  train-logloss:0.537027  test-logloss:0.564347 
#> [6]  train-logloss:0.532440  test-logloss:0.569725 
#> [7]  train-logloss:0.529694  test-logloss:0.572771 
#> [8]  train-logloss:0.527461  test-logloss:0.577550 
#> [9]  train-logloss:0.526133  test-logloss:0.578102 
#> [10] train-logloss:0.524974  test-logloss:0.582957 
#> [11] train-logloss:0.524261  test-logloss:0.584912 
#> [12] train-logloss:0.523564  test-logloss:0.585951 
#> [13] train-logloss:0.522776  test-logloss:0.590383 
#> [14] train-logloss:0.522043  test-logloss:0.591078 
#> [15] train-logloss:0.521643  test-logloss:0.592417 
#> [16] train-logloss:0.521196  test-logloss:0.593165 
#> [17] train-logloss:0.520913  test-logloss:0.593832 
#> [18] train-logloss:0.520448  test-logloss:0.595736 
#> [19] train-logloss:0.520109  test-logloss:0.597917 
#> [20] train-logloss:0.519697  test-logloss:0.601429 
xgb_regression<-xgboost::xgb.train(
                data=train_test_regression$xgb$f1$train,
                evals=train_test_regression$xgb$f1$watchlist,
                nround=20)
#> [1]  train-rmse:6.779953 test-rmse:7.883347 
#> [2]  train-rmse:5.127539 test-rmse:6.805361 
#> [3]  train-rmse:3.979874 test-rmse:6.234165 
#> [4]  train-rmse:3.123993 test-rmse:5.462813 
#> [5]  train-rmse:2.556359 test-rmse:5.253136 
#> [6]  train-rmse:2.092734 test-rmse:4.789814 
#> [7]  train-rmse:1.769000 test-rmse:4.495935 
#> [8]  train-rmse:1.531660 test-rmse:4.194864 
#> [9]  train-rmse:1.365019 test-rmse:3.970766 
#> [10] train-rmse:1.222465 test-rmse:3.856196 
#> [11] train-rmse:1.129154 test-rmse:3.867602 
#> [12] train-rmse:1.046333 test-rmse:3.842552 
#> [13] train-rmse:0.997830 test-rmse:3.839760 
#> [14] train-rmse:0.929822 test-rmse:3.861990 
#> [15] train-rmse:0.886331 test-rmse:3.845626 
#> [16] train-rmse:0.847879 test-rmse:3.822468 
#> [17] train-rmse:0.770942 test-rmse:3.743698 
#> [18] train-rmse:0.738418 test-rmse:3.738625 
#> [19] train-rmse:0.686716 test-rmse:3.728543 
#> [20] train-rmse:0.671315 test-rmse:3.736515 
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
