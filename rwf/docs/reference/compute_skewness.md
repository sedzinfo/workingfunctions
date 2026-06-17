# Compute skewness

Compute skewness

## Usage

``` r
compute_skewness(vector)
```

## Arguments

- vector:

  vector

## Note

b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2). Used in MINITAB and BMDP.

## Examples

``` r
set.seed(1)
vector<-rnorm(1000)
compute_skewness(vector)
#> [1] -0.01913836
e1071::skewness(vector)
#> [1] -0.01913836
```
