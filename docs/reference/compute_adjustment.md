# Compute multiple comparison alpha adjustments

Calculates Bonferroni and Šidák corrected alpha thresholds for a given
family-wise alpha level and number of tests.

## Usage

``` r
compute_adjustment(a, ntests)
```

## Arguments

- a:

  Numeric. The desired family-wise alpha level (e.g. `0.05`).

- ntests:

  Integer. The number of tests (comparisons) being performed.

## Value

A named list with two elements:

- sidak:

  Šidák corrected alpha: \\1 - (1 - \alpha)^{1/k}\\.

- bonferroni:

  Bonferroni corrected alpha: \\\alpha / k\\.

## Examples

``` r
compute_adjustment(0.05, 100)
#> $sidak
#> [1] 0.0005128014
#> 
#> $bonferroni
#> [1] 5e-04
#> 
```
