# Proportion overall accuracy of a confusion matrix

Calculates the overall accuracy and Cohen's kappa statistics of a
confusion matrix.

## Usage

``` r
proportion_accurate(observed, predicted)
```

## Arguments

- observed:

  Vector of observed variables. These are the true class labels.

- predicted:

  Vector of predicted variables. These are the predicted class labels.

## Details

This function evaluates the performance of a confusion matrix by
calculating the overall accuracy and Cohen's kappa statistics.

The function performs the following steps: 1. Computes the confusion
matrix from the observed and predicted values. 2. Calculates the
diagonal proportion (overall accuracy) and the off-diagonal proportion.
3. Computes Cohen's kappa statistics (unweighted,linear,and squared
weights).

The output is a data.frame containing the following metrics:
-\`cm_diagonal\`: Proportion of correct classifications (diagonal
elements). -\`cm_off_diagonal\`: Proportion of misclassified
observations (off-diagonal elements). -\`kappa_unweighted\`: Cohen's
kappa statistic with no weights. -\`kappa_linear\`: Cohen's kappa
statistic with linear weights. -\`kappa_squared\`: Cohen's kappa
statistic with squared weights.

## Examples

``` r
# Example with numeric observed and predicted values
proportion_accurate(observed=c(1,2,3,4,5,10),predicted=c(1,2,3,4,5,11))
#>   cm_diagonal cm_off_diagonal kappa_unweighted kappa_linear kappa_squared
#> 1      0.8333               1           0.8065       0.9286        0.9801
```
