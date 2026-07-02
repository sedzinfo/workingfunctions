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
#> [1] "Qpbiq0V3gy"
generate_string(nchar = 10, vector_length = 10)
#>  [1] "BDKtiMfokM" "e88uujSJKN" "8t9x17soEE" "w3R1T6xuEu" "zM1MlY0n8Y" "K4dlxt5T51" "fqSuRoItpC" "rKOQAHD80A" "Tj8fCOU6mh" "FSjr5y0Ldv"
generate_string(nchar = 10)
#> [1] "3TFkxSdkDC"
generate_string(nchar = 10, vector_length = 10)
#>  [1] "pDXXMwSqbY" "dxJtYPZnGY" "00Ilcn0rWr" "KJowe9e3Po" "Qe0YRgcqzD" "nXoeJCYylj" "C3nSxNkZr4" "vWHInBNKUk" "Vhbg0iGtzs" "70jlhOzW6N"
```
