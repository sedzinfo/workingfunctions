# Compute adjustments

Compute adjustments

## Usage

``` r
compute_adjustment(a, ntests)
```

## Arguments

- a:

  alpha criterion

- ntests:

  number of tests

## Examples

``` r
compute_adjustment(0.05,100)
#> $sidak
#> [1] 0.0005128014
#> 
#> $bonferroni
#> [1] 5e-04
#> 
```
