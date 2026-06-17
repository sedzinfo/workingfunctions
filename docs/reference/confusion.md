# Create a confusion matrix from observed and predicted vectors

Generates a confusion matrix from observed and predicted values.

## Usage

``` r
confusion(observed, predicted)
```

## Arguments

- observed:

  Vector of observed variables. These are the true class labels.

- predicted:

  Vector of predicted variables. These are the predicted class labels.

## Details

This function creates a confusion matrix by comparing the observed
(true) class labels with the predicted class labels. The confusion
matrix is a table that is often used to describe the performance of a
classification model. The function performs the following steps: 1.
Identifies the unique class labels from both the observed and predicted
vectors. 2. Sorts the class labels in a mixed order (if they are
character variables) using \`gtools::mixedsort\`. 3. Constructs a table
to represent the confusion matrix with the sorted class labels as
levels. The output is a confusion matrix,where rows represent the
predicted class labels and columns represent the observed class labels.

## Examples

``` r
# Example with numeric observed and predicted values
confusion(observed=c(1,2,3,4,5,10),predicted=c(1,2,3,4,5,11))
#>          observed
#> predicted 1 2 3 4 5 10 11
#>        1  1 0 0 0 0  0  0
#>        2  0 1 0 0 0  0  0
#>        3  0 0 1 0 0  0  0
#>        4  0 0 0 1 0  0  0
#>        5  0 0 0 0 1  0  0
#>        10 0 0 0 0 0  0  0
#>        11 0 0 0 0 0  1  0
# Example with repeated observed and predicted values
confusion(observed=c(1,2,2,2,2),predicted=c(1,1,2,2,2))
#>          observed
#> predicted 1 2
#>         1 1 1
#>         2 0 3
```
