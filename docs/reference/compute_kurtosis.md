# Compute kurtosis of a numeric vector

Calculates the excess kurtosis of a numeric vector using the \\b_2\\
formula consistent with MINITAB and BMDP. Missing values are removed
before computation.

## Usage

``` r
compute_kurtosis(vector)
```

## Arguments

- vector:

  Numeric vector.

## Value

A numeric scalar. A value of 0 indicates a normal distribution; positive
values indicate heavier tails (leptokurtic); negative values indicate
lighter tails (platykurtic).

## Note

Formula used: \\b_2 = m_4 / s^4 - 3 = (g_2 + 3)(1 - 1/n)^2 - 3\\. Used
in MINITAB and BMDP. Results match
[`e1071::kurtosis()`](https://rdrr.io/pkg/e1071/man/kurtosis.html) with
`type = 2`.

## Examples

``` r
set.seed(1)
vector <- rnorm(1000)
compute_kurtosis(vector)
#> [1] -0.007768915
e1071::kurtosis(vector)
#> [1] -0.007768915
```
