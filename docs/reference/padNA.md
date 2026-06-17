# Pad a data frame to a target number of rows with NAs

Extends a data frame to `rowsneeded` rows by appending (or prepending)
`NA`-filled rows. Internal helper used by
[`c_bind`](https://sedzinfo.github.io/rwf/reference/c_bind.md).

## Usage

``` r
padNA(df, rowsneeded, first = TRUE)
```

## Arguments

- df:

  A data frame to pad.

- rowsneeded:

  Integer target row count. Must be greater than or equal to `nrow(df)`.

- first:

  Logical. When `TRUE` (default) `NA` rows are appended at the bottom;
  when `FALSE` they are prepended at the top.

## Value

A data frame with `rowsneeded` rows and the same columns as `df`.

## Author

Ananda Mahto
