# Generate random strings

Generate random strings

## Usage

``` r
generate_string(
  vector = c(LETTERS, letters, 0:9),
  vector_length = 1,
  nchar = 5
)
```

## Arguments

- vector:

  character pool

- vector_length:

  number of strings to generate

- nchar:

  Length of generated strings

## Examples

``` r
generate_string(nchar=10)
#> [1] "bZwJJG1Gav"
generate_string(nchar=10,vector_length=10)
#>  [1] "YtLe9GN1XU" "CI0EAwKbBQ" "LmJ04xj5ea" "10xHL3hx9z" "rmJfWwTPfw"
#>  [6] "2HTluAbipN" "UJhCHzb05b" "oyCmso6QPQ" "WylfR1nPUg" "m6YC7WoVOI"
```
