# Compute skewness of a numeric vector

Calculates the skewness of a numeric vector using the \\b_1\\ formula
consistent with MINITAB and BMDP. Missing values are removed before
computation.

## Usage

``` r
compute_skewness(vector)
```

## Arguments

- vector:

  Numeric vector.

## Value

A numeric scalar. Positive values indicate right skew, negative values
indicate left skew.

## Note

Formula used: \\b_1 = m_3 / s^3 = g_1 ((n-1)/n)^{3/2}\\. Used in MINITAB
and BMDP. Results match
[`e1071::skewness()`](https://rdrr.io/pkg/e1071/man/skewness.html) with
`type = 2`.

## Examples

``` r
set.seed(1)
vector <- rnorm(1000)
compute_skewness(vector)
#> [1] -0.01913836
e1071::skewness(vector)
#> [1] -0.01913836
```
