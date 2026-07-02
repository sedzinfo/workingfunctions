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
#>  [1]  0.090207        NA -0.561522  0.683311        NA -0.645514        NA        NA        NA  0.008565
generate_missing(generate_data(nrow = 10, ncol = 2), missing = 5)
#>         X1      X2
#> 1       NA      NA
#> 2       NA      NA
#> 3  -1.1520  0.2694
#> 4       NA      NA
#> 5   1.6673      NA
#> 6   0.0589      NA
#> 7  -1.0617 -1.2569
#> 8   1.6567  0.5571
#> 9       NA  0.5223
#> 10      NA  1.4599
generate_missing(rnorm(10), missing = 5)
#>  [1]  0.77274       NA       NA       NA       NA       NA  1.13411  1.43307 -0.07661 -0.15840
generate_missing(generate_data(nrow = 10, ncol = 2), missing = 5)
#>         X1      X2
#> 1       NA  0.9592
#> 2       NA      NA
#> 3   0.2522      NA
#> 4  -2.5484  1.5866
#> 5  -1.9926  0.3509
#> 6       NA      NA
#> 7   2.6308      NA
#> 8       NA -0.2920
#> 9   2.1364 -0.4831
#> 10      NA      NA
```
