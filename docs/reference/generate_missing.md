# Introduce missing values into a vector or data frame

Randomly replaces a fixed number of values with `NA`, either in a vector
or across every column of a data frame independently.

Randomly replaces a fixed number of values with `NA`, either in a vector
or across every column of a data frame independently.

## Usage

``` r
generate_missing(df, missing = 5)

generate_missing(df, missing = 5)
```

## Arguments

- df:

  A numeric vector or data frame. The object into which missing values
  are introduced.

- missing:

  Integer. Number of values to replace with `NA` per vector or per
  column. Must not exceed the length of the vector or `nrow(df)`.
  Default is `5`.

## Value

The input object with `missing` values replaced by `NA`. Returns the
same type as the input (vector or data frame).

The input object with `missing` values replaced by `NA`. Returns the
same type as the input (vector or data frame).

## Examples

``` r
generate_missing(rnorm(10), missing = 5)
#>  [1]     NA     NA     NA -2.582  1.923  1.691  1.622     NA  0.368     NA
generate_missing(generate_data(nrow = 10, ncol = 2), missing = 5)
#>          X1       X2
#> 1   0.21332       NA
#> 2        NA       NA
#> 3        NA -0.33764
#> 4   0.28134       NA
#> 5   0.16287 -0.07383
#> 6        NA       NA
#> 7        NA       NA
#> 8        NA  0.46596
#> 9  -0.01284 -0.90834
#> 10  0.76875  0.89846
generate_missing(rnorm(10), missing = 5)
#>  [1] -0.63339       NA       NA       NA       NA -0.70332 -0.33182  0.01996  0.80527       NA
generate_missing(generate_data(nrow = 10, ncol = 2), missing = 5)
#>         X1      X2
#> 1       NA      NA
#> 2   0.9935      NA
#> 3  -0.9332      NA
#> 4       NA      NA
#> 5       NA -0.8902
#> 6   0.6013      NA
#> 7       NA -0.4856
#> 8  -0.5206  1.3180
#> 9       NA  0.7152
#> 10  2.0209 -1.1239
```
