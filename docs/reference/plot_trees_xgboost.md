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
#> [1]  train-logloss:0.623700  test-logloss:0.610367 
#> [2]  train-logloss:0.611266  test-logloss:0.595618 
#> [3]  train-logloss:0.600792  test-logloss:0.582165 
#> [4]  train-logloss:0.591913  test-logloss:0.570386 
#> [5]  train-logloss:0.584361  test-logloss:0.560029 
#> [6]  train-logloss:0.577734  test-logloss:0.552103 
#> [7]  train-logloss:0.572031  test-logloss:0.545050 
#> [8]  train-logloss:0.567115  test-logloss:0.538758 
#> [9]  train-logloss:0.562765  test-logloss:0.533669 
#> [10] train-logloss:0.559003  test-logloss:0.529157 
#> [11] train-logloss:0.555745  test-logloss:0.525152 
#> [12] train-logloss:0.552922  test-logloss:0.521592 
#> [13] train-logloss:0.550432  test-logloss:0.518169 
#> [14] train-logloss:0.548251  test-logloss:0.515073 
#> [15] train-logloss:0.546337  test-logloss:0.512311 
#> [16] train-logloss:0.544657  test-logloss:0.509825 
#> [17] train-logloss:0.543180  test-logloss:0.507585 
#> [18] train-logloss:0.541881  test-logloss:0.505565 
#> [19] train-logloss:0.540737  test-logloss:0.503741 
#> [20] train-logloss:0.539709  test-logloss:0.502292 
xgb_regression<-xgboost::xgb.train(
                data=train_test_regression$xgb$f1$train,
                watchlist=train_test_regression$xgb$f1$watchlist,
                eta=.3,
                nthread=8,
                nround=20)
#> Warning: Passed invalid function arguments: eta, nthread. These should be passed as a list to argument 'params'. Conversion from argument to 'params' entry will be done automatically, but this behavior will become an error in a future version.
#> Warning: Parameter 'watchlist' has been renamed to 'evals'. This warning will become an error in a future version.
#> [1]  train-rmse:6.854962 test-rmse:6.579069 
#> [2]  train-rmse:5.174794 test-rmse:5.042426 
#> [3]  train-rmse:3.989909 test-rmse:4.037522 
#> [4]  train-rmse:3.149278 test-rmse:3.455290 
#> [5]  train-rmse:2.534004 test-rmse:3.004020 
#> [6]  train-rmse:2.124534 test-rmse:2.763111 
#> [7]  train-rmse:1.795526 test-rmse:2.650973 
#> [8]  train-rmse:1.576032 test-rmse:2.607866 
#> [9]  train-rmse:1.411462 test-rmse:2.543309 
#> [10] train-rmse:1.293250 test-rmse:2.473808 
#> [11] train-rmse:1.200497 test-rmse:2.432415 
#> [12] train-rmse:1.090752 test-rmse:2.439590 
#> [13] train-rmse:1.001736 test-rmse:2.404787 
#> [14] train-rmse:0.903884 test-rmse:2.416750 
#> [15] train-rmse:0.853224 test-rmse:2.420861 
#> [16] train-rmse:0.799464 test-rmse:2.433400 
#> [17] train-rmse:0.738400 test-rmse:2.418773 
#> [18] train-rmse:0.718290 test-rmse:2.408886 
#> [19] train-rmse:0.679221 test-rmse:2.419082 
#> [20] train-rmse:0.619782 test-rmse:2.409803 
# xgboost::xgb.plot.multi.trees(model=xgb_classification,features_keep=2)
# plot_trees_xgboost(model=xgb_classification,
#                    train=train_test_classification$xgb$f1,
#                    file="Classification")
# plot_trees_xgboost(model=xgb_regression,
#                    train=train_test_regression$xbg$f1,
#                    file="Regression")
```
