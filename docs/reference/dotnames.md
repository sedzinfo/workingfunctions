# Pad a data frame to a target number of rows with NAs

Extends a data frame to `rowsneeded` rows by appending (or prepending)
`NA`-filled rows. Internal helper used by
[`c_bind`](https://sedzinfo.github.io/rwf/reference/c_bind.md).

## Usage

``` r
dotnames(...)
```

## Value

A data frame with `rowsneeded` rows and the same columns as `df`.

## Author

Ananda Mahto
