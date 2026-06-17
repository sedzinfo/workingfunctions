# Compute number of dummy comparisons

Compute number of dummy comparisons

## Usage

``` r
compute_dummy_comparisons(items)
```

## Arguments

- items:

  number of items per block

## Examples

``` r
compute_dummy_comparisons(1)
#> [1] 0
compute_dummy_comparisons(2)
#> [1] 1
compute_dummy_comparisons(3)
#> [1] 3
compute_dummy_comparisons(4)
#> [1] 6
compute_dummy_comparisons(5)
#> [1] 10
compute_dummy_comparisons(6)
#> [1] 15
```
