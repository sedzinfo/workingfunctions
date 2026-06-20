# Extract n characters from the left or right of a string

Extract n characters from the left or right of a string

## Usage

``` r
sub_str(x, n = 2, type)
```

## Arguments

- x:

  Character vector.

- n:

  Integer. Number of characters to extract. Default is `2`.

- type:

  Character. One of `"left"` or `"right"`.

## Value

A character vector of the same length as `x`.

## Examples

``` r
sub_str("12345",n=2,type="right")
#> [1] "45"
sub_str("12345",n=2,type="left")
#> [1] "12"
```
