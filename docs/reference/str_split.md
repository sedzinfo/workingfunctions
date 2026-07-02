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
#> 1   1 5N 0o Pf
#> 2   2 Ei 4k sG
#> 3   3 Ww IJ 7p
#> 4   4 y1 0e P1
#> 5   5 g7 zF hK
#> 6   6 Xv ek DS
#> 7   7 Cu 1e Kv
#> 8   8 tB 31 5t
#> 9   9 Mz Dd ym
#> 10 10 vn rx zz
```
