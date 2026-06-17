# Produce combinations for comparisons from dataframe names

Produce combinations for comparisons from dataframe names

## Usage

``` r
comparison_combinations(df, all_orders = TRUE)
```

## Arguments

- df:

  dataframe

- all_orders:

  if TRUE the order of combination is considered i.e. the combination X1
  X2 also appears as X2 X1 if FALSE it is assumed that X1 X2 and X2 X1
  are the same and only one of them appears

## Examples

``` r
comparison_combinations(generate_correlation_matrix(n=10)[,1:4])
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
