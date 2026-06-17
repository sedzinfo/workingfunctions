# Compute kurtosis

Compute kurtosis

## Usage

``` r
compute_kurtosis(vector)
```

## Arguments

- vector:

  vector

## Note

b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3. Used in MINITAB and
BMDP.

## Examples

``` r
set.seed(1)
vector<-rnorm(1000)
compute_kurtosis(vector)
#> [1] -0.007768915
e1071::kurtosis(vector)
#> [1] -0.007768915
```
