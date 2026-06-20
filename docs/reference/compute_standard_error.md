# Compute the standard error of the mean

Compute the standard error of the mean

## Usage

``` r
compute_standard_error(vector)
```

## Arguments

- vector:

  Numeric vector. Missing values are removed before computation.

## Value

A numeric scalar. The standard error of the mean.

## Examples

``` r
set.seed(1)
vector <- rnorm(1000)
compute_standard_error(vector)
#> [1] 0.03272691
```
