# Compute dissatenuation

Compute dissatenuation

## Usage

``` r
compute_dissatenuation(variable1, error1, variable2, error2)
```

## Arguments

- variable1:

  vector

- error1:

  vector error measurement for variable1

- variable2:

  vector

- error2:

  vector error measurement for variable2

## Examples

``` r
set.seed(1)
compute_dissatenuation(rnorm(10),rnorm(10),rnorm(10),rnorm(10))
#> [1] 0.1306254
```
