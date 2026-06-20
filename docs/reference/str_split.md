# Split a string vector into a data frame of parts

Splits each element of a character vector by a separator and returns
theparts as columns of a data frame, one row per input element.

## Usage

``` r
str_split(vector, split = "/", include_original = FALSE)
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
string <- paste0(
  1:10, "/",
  generate_string(nchar = 2, vector_length = 10), "/",
  generate_string(nchar = 2, vector_length = 10), "/",
  generate_string(nchar = 2, vector_length = 10)
)
str_split(string, split = "/")
#>    X1 X2 X3 X4
#> 1   1 eU qY le
#> 2   2 Cx hp 6p
#> 3   3 GW LB Du
#> 4   4 Fm sl Ux
#> 5   5 aC ql c7
#> 6   6 R8 hG rV
#> 7   7 49 TV pr
#> 8   8 LB 0X DY
#> 9   9 YE 0S KO
#> 10 10 NU VG NF
```
