# Generate random strings

Produces a character vector of random strings by sampling from a
character pool.

Produces a character vector of random strings by sampling from a
character pool.

## Usage

``` r
generate_string(
  vector = c(LETTERS, letters, 0:9),
  vector_length = 1,
  nchar = 5
)

generate_string(
  vector = c(LETTERS, letters, 0:9),
  vector_length = 1,
  nchar = 5
)
```

## Arguments

- vector:

  Character vector. The pool of characters to sample from. Default is
  `c(LETTERS, letters, 0:9)`.

- vector_length:

  Integer. Number of strings to generate. Default is `1`.

- nchar:

  Integer. Length of each generated string. Default is `5`.

## Value

A character vector of length `vector_length`.

A character vector of length `vector_length`.

## Examples

``` r
generate_string(nchar = 10)
#> [1] "T51fqSuRoI"
generate_string(nchar = 10, vector_length = 10)
#>  [1] "tpCrKOQAHD" "80ATj8fCOU" "6mhFSjr5y0" "Ldv3TFkxSd" "kDCpDXXMwS" "qbYdxJtYPZ" "nGY00Ilcn0" "rWrKJowe9e" "3PoQe0YRgc" "qzDnXoeJCY"
generate_string(nchar = 10)
#> [1] "yljC3nSxNk"
generate_string(nchar = 10, vector_length = 10)
#>  [1] "Zr4vWHInBN" "KUkVhbg0iG" "tzs70jlhOz" "W6NvIMeXRP" "gdFQce2f1n" "NTzy4XyQUm" "LAOxLj9rrC" "7nE2936r4H" "4lhotTEKCR" "iL9citi0xR"
```
