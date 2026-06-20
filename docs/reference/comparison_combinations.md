# All pairwise column name combinations

Generates a data frame of all pairwise combinations of column names from
a data frame. Useful for programmatically specifying variable pairs to
pass to functions like
[`compute_crosstable`](https://sedzinfo.github.io/rwf/reference/compute_crosstable.md)
or
[`plot_crosstable`](https://sedzinfo.github.io/rwf/reference/plot_crosstable.md).

## Usage

``` r
comparison_combinations(df, all_orders = TRUE)
```

## Arguments

- df:

  A data frame whose column names will be combined.

- all_orders:

  Logical. When `TRUE` (default) both orderings of each pair are
  included (e.g. `(X1, X2)` and `(X2, X1)`), producing \\n(n-1)\\ rows
  for \\n\\ columns. When `FALSE` only unique unordered pairs are
  returned, producing \\n(n-1)/2\\ rows.

## Value

A data frame with two character columns `X1` and `X2`, each row
representing one variable pair.

## Examples

``` r
comparison_combinations(generate_correlation_matrix(n = 10)[, 1:4])
#>    X1 X2
#> 1  X1 X2
#> 2  X1 X3
#> 3  X1 X4
#> 7  X2 X1
#> 4  X2 X3
#> 5  X2 X4
#> 8  X3 X1
#> 10 X3 X2
#> 6  X3 X4
#> 9  X4 X1
#> 11 X4 X2
#> 12 X4 X3
```
