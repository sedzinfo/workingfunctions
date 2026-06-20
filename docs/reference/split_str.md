# Split a string vector into a data frame of parts

Splits each element of a character vector by a separator and returns the
parts as columns of a data frame, one row per input element.

## Usage

``` r
split_str(vector, split = "/", include_original = FALSE)
```

## Arguments

- vector:

  Character vector to split.

- split:

  Character. The separator to split on. Default is `"/"`.

- include_original:

  Logical. If `TRUE`, appends the original input as a final column.
  Default is `FALSE`.

## Value

A data frame with one row per element of `vector` and one column per
split part. Assumes all elements produce the same number of parts.

## Examples

``` r
string<-paste0(1:10,"/",
               generate_string(nchar=2,vector_length=10),"/",
               generate_string(nchar=2,vector_length=10),"/",
               generate_string(nchar=2,vector_length=10))
split_str(string,split="/")
#>    X1 X2 X3 X4
#> 1   1 Ij Mj tk
#> 2   2 Oz NA yQ
#> 3   3 YZ 7d zy
#> 4   4 5k fL nE
#> 5   5 8a qH jK
#> 6   6 Rk aa Ic
#> 7   7 Tx jr 5M
#> 8   8 Ur XH re
#> 9   9 KK fC 5M
#> 10 10 cM FO 2e
```
