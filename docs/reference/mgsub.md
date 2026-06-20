# Apply gsub for multiple patterns with a single replacement

Iterates over a vector of patterns, applying
[`gsub`](https://rdrr.io/r/base/grep.html) sequentially with the same
replacement string for each.

## Usage

``` r
mgsub(mydata, pattern, replacement, ...)
```

## Arguments

- mydata:

  Character vector to search within.

- pattern:

  Character vector of patterns to search for.

- replacement:

  Character. The replacement string applied for all patterns.

- ...:

  Additional arguments passed to
  [`gsub`](https://rdrr.io/r/base/grep.html), such as `fixed` or
  `ignore.case`.

## Value

A character vector with all pattern matches replaced.

## Examples

``` r
mgsub(mydata="#$%^&*_+",pattern=c("%","*"),"REPLACE",fixed=TRUE)
#> [1] "#$REPLACE^&REPLACE_+"
```
