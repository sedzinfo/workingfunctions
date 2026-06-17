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
#> [1]  train-logloss:0.635107  test-logloss:0.514424 
#> [2]  train-logloss:0.622560  test-logloss:0.498000 
#> [3]  train-logloss:0.611975  test-logloss:0.483719 
#> [4]  train-logloss:0.603015  test-logloss:0.471243 
#> [5]  train-logloss:0.595410  test-logloss:0.460300 
#> [6]  train-logloss:0.588832  test-logloss:0.451036 
#> [7]  train-logloss:0.583184  test-logloss:0.442839 
#> [8]  train-logloss:0.578329  test-logloss:0.435565 
#> [9]  train-logloss:0.574150  test-logloss:0.429096 
#> [10] train-logloss:0.570536  test-logloss:0.423894 
#> [11] train-logloss:0.567393  test-logloss:0.419223 
#> [12] train-logloss:0.564655  test-logloss:0.415124 
#> [13] train-logloss:0.562267  test-logloss:0.411434 
#> [14] train-logloss:0.560167  test-logloss:0.408180 
#> [15] train-logloss:0.558331  test-logloss:0.405398 
#> [16] train-logloss:0.556711  test-logloss:0.402750 
#> [17] train-logloss:0.555291  test-logloss:0.400581 
#> [18] train-logloss:0.554036  test-logloss:0.398404 
#> [19] train-logloss:0.552949  test-logloss:0.396450 
#> [20] train-logloss:0.551973  test-logloss:0.394888 
xgb_regression<-xgboost::xgb.train(
                data=train_test_regression$xgb$f1$train,
                watchlist=train_test_regression$xgb$f1$watchlist,
                eta=.3,
                nthread=8,
                nround=20)
#> Warning: Passed invalid function arguments: eta, nthread. These should be passed as a list to argument 'params'. Conversion from argument to 'params' entry will be done automatically, but this behavior will become an error in a future version.
#> Warning: Parameter 'watchlist' has been renamed to 'evals'. This warning will become an error in a future version.
#> [1]  train-rmse:6.758981 test-rmse:7.739382 
#> [2]  train-rmse:5.124587 test-rmse:6.360189 
#> [3]  train-rmse:3.935361 test-rmse:5.227996 
#> [4]  train-rmse:3.096693 test-rmse:4.601976 
#> [5]  train-rmse:2.470598 test-rmse:4.128856 
#> [6]  train-rmse:2.035462 test-rmse:3.937343 
#> [7]  train-rmse:1.707423 test-rmse:3.729041 
#> [8]  train-rmse:1.466699 test-rmse:3.645010 
#> [9]  train-rmse:1.302169 test-rmse:3.567995 
#> [10] train-rmse:1.182896 test-rmse:3.477296 
#> [11] train-rmse:1.088969 test-rmse:3.423951 
#> [12] train-rmse:1.033987 test-rmse:3.407407 
#> [13] train-rmse:0.983244 test-rmse:3.411465 
#> [14] train-rmse:0.915706 test-rmse:3.359106 
#> [15] train-rmse:0.859692 test-rmse:3.328143 
#> [16] train-rmse:0.826796 test-rmse:3.301269 
#> [17] train-rmse:0.797502 test-rmse:3.273505 
#> [18] train-rmse:0.725271 test-rmse:3.271714 
#> [19] train-rmse:0.696356 test-rmse:3.243753 
#> [20] train-rmse:0.658296 test-rmse:3.224496 
# xgboost::xgb.plot.multi.trees(model=xgb_classification,features_keep=2)
# plot_trees_xgboost(model=xgb_classification,
#                    train=train_test_classification$xgb$f1,
#                    file="Classification")
# plot_trees_xgboost(model=xgb_regression,
#                    train=train_test_regression$xbg$f1,
#                    file="Regression")
```
