# Mean and SD of scale scores

Computes the mean and standard deviation of respondent scale scores.
When `divisor` is `NULL` the scale score is the row mean across items.
When `divisor` is supplied the scale score is the row sum divided by
`divisor`, which is useful when scores need to be expressed on a custom
metric (e.g. dividing a sum score by the maximum possible score to
obtain a proportion).

## Usage

``` r
mean_sd_alpha(df, divisor = NULL)
```

## Arguments

- df:

  A data frame whose columns are the items of a single scale. All
  columns must be numeric.

- divisor:

  Numeric scalar used to divide the row sum before computing the mean
  and SD. When `NULL` (default) row means are used instead.

## Value

A one-row data frame with columns `MEAN` and `SD` containing the mean
and standard deviation of the scale scores across respondents.

## Examples

``` r
set.seed(12345)
df <- data.frame(matrix(.5, ncol = 6, nrow = 6))
correlation_martix <- as.matrix(df)
diag(correlation_martix) <- 1
df <- round(generate_correlation_matrix(correlation_martix, nrows = 1000), 0) + 5
mean_sd_alpha(df)
#>    MEAN     SD
#> 1 5.007 0.7683
mean_sd_alpha(df, divisor = 100)
#>     Mean     SD
#> 1 0.3004 0.0461
```
