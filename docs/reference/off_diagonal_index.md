# Get off-diagonal indices for a square matrix

Returns a data frame of row/column index pairs for navigating just above
and below the diagonal, useful for accessing or modifying off-diagonal
neighbours.

## Usage

``` r
off_diagonal_index(length)
```

## Arguments

- length:

  Integer. The size of the diagonal (i.e. number of rows/columns in the
  square matrix).

## Value

A data frame with `length` rows and four columns:

- x1:

  Row index.

- x2:

  Column index (same as `x1`, i.e. the diagonal position).

- x3:

  Index of the element just above (`i + 1`).

- x4:

  Index of the element just below (`i - 1`).

## Examples

``` r
off_diagonal_index(length=6)
#>   x1 x2 x3 x4
#> 1  1  1  2  0
#> 2  2  2  3  1
#> 3  3  3  4  2
#> 4  4  4  5  3
#> 5  5  5  6  4
#> 6  6  6  7  5
```
