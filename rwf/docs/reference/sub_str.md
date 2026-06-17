# Return n characters from left or right

Return n characters from left or right

## Usage

``` r
sub_str(x, n = 2, type)
```

## Arguments

- x:

  Character

- n:

  Number of characters to return

- type:

  "right" "left"

## Examples

``` r
sub_str("12345",n=2,type="right")
#> [1] "45"
sub_str("12345",n=2,type="left")
#> [1] "12"
```
