# Plot trees for xgboost::xgb.train

Plot trees for xgboost::xgb.train

## Usage

``` r
plot_trees_xgboost(model, train, file = "xgboost")
```

## Arguments

- model:

  object from xgboost::xgb.train

- train:

  Train dataset

- file:

  output filename

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
                    data=train_test_classification$xgb$f1$train,
                    watchlist=train_test_classification$xgb$f1$watchlist,
                    eta=.1,
                    nthread=8,
                    nround=20,
                    objective="binary:logistic")
#> Warning: Passed invalid function arguments: eta, nthread. These should be passed as a list to argument 'params'. Conversion from argument to 'params' entry will be done automatically, but this behavior will become an error in a future version.
#> Warning: Parameter 'watchlist' has been renamed to 'evals'. This warning will become an error in a future version.
#> Warning: Argument 'objective' is only for custom objectives. For built-in objectives, pass the objective under 'params'. This warning will become an error in a future version.
#> [1]  train-logloss:0.625324  test-logloss:0.577357 
#> [2]  train-logloss:0.609407  test-logloss:0.578811 
#> [3]  train-logloss:0.596090  test-logloss:0.581177 
#> [4]  train-logloss:0.584889  test-logloss:0.584222 
#> [5]  train-logloss:0.575429  test-logloss:0.587767 
#> [6]  train-logloss:0.567278  test-logloss:0.590590 
#> [7]  train-logloss:0.560241  test-logloss:0.593789 
#> [8]  train-logloss:0.554152  test-logloss:0.597280 
#> [9]  train-logloss:0.548872  test-logloss:0.600992 
#> [10] train-logloss:0.544284  test-logloss:0.604868 
#> [11] train-logloss:0.540290  test-logloss:0.608861 
#> [12] train-logloss:0.536796  test-logloss:0.612990 
#> [13] train-logloss:0.533719  test-logloss:0.617153 
#> [14] train-logloss:0.531472  test-logloss:0.619229 
#> [15] train-logloss:0.529491  test-logloss:0.622395 
#> [16] train-logloss:0.527729  test-logloss:0.624184 
#> [17] train-logloss:0.526169  test-logloss:0.626005 
#> [18] train-logloss:0.524785  test-logloss:0.629182 
#> [19] train-logloss:0.523546  test-logloss:0.630941 
#> [20] train-logloss:0.522450  test-logloss:0.634052 
xgb_regression<-xgboost::xgb.train(
                data=train_test_regression$xgb$f1$train,
                watchlist=train_test_regression$xgb$f1$watchlist,
                eta=.3,
                nthread=8,
                nround=20)
#> Warning: Passed invalid function arguments: eta, nthread. These should be passed as a list to argument 'params'. Conversion from argument to 'params' entry will be done automatically, but this behavior will become an error in a future version.
#> Warning: Parameter 'watchlist' has been renamed to 'evals'. This warning will become an error in a future version.
#> [1]  train-rmse:6.774317 test-rmse:7.212093 
#> [2]  train-rmse:5.085121 test-rmse:5.597765 
#> [3]  train-rmse:3.892655 test-rmse:4.537569 
#> [4]  train-rmse:3.059144 test-rmse:4.123293 
#> [5]  train-rmse:2.427202 test-rmse:3.765254 
#> [6]  train-rmse:2.003078 test-rmse:3.670677 
#> [7]  train-rmse:1.705477 test-rmse:3.618568 
#> [8]  train-rmse:1.434696 test-rmse:3.619467 
#> [9]  train-rmse:1.262284 test-rmse:3.565357 
#> [10] train-rmse:1.123011 test-rmse:3.509978 
#> [11] train-rmse:1.009275 test-rmse:3.521042 
#> [12] train-rmse:0.953967 test-rmse:3.514362 
#> [13] train-rmse:0.882562 test-rmse:3.489201 
#> [14] train-rmse:0.818008 test-rmse:3.489720 
#> [15] train-rmse:0.762033 test-rmse:3.491879 
#> [16] train-rmse:0.715732 test-rmse:3.474819 
#> [17] train-rmse:0.687252 test-rmse:3.465708 
#> [18] train-rmse:0.642179 test-rmse:3.471169 
#> [19] train-rmse:0.613953 test-rmse:3.457612 
#> [20] train-rmse:0.559664 test-rmse:3.454021 
# xgboost::xgb.plot.multi.trees(model=xgb_classification,features_keep=2)
# plot_trees_xgboost(model=xgb_classification,
#                    train=train_test_classification$xgb$f1,
#                    file="Classification")
# plot_trees_xgboost(model=xgb_regression,
#                    train=train_test_regression$xbg$f1,
#                    file="Regression")
```
