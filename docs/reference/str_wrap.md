# Wrap long strings to a specified line width

Breaks a character string into multiple lines so that no line exceeds
`width` characters. Words are kept intact; lines are joined with `"\n"`.

Wraps a character string at a given width and collapses the result into
a single newline-delimited string. Useful for formatting long plot
titles or labels.

Breaks a character string into multiple lines so that no line exceeds
`width` characters. Words are kept intact; lines are joined with `"\n"`.

## Usage

``` r
str_wrap(string, width = 80)

str_wrap(string, width = 80)

str_wrap(string, width = 80)
```

## Arguments

- string:

  A character vector.

- width:

  Maximum number of characters per line. Default `80`.

- x:

  Character. The string to wrap.

- ...:

  Additional arguments passed to
  [`strwrap`](https://rdrr.io/r/base/strwrap.html), such as `width`.

## Value

A character vector the same length as `string`, with embedded newlines
inserted at word boundaries.

A single character string with newlines inserted at wrap points.

A character vector the same length as `string`, with embedded newlines
inserted at word boundaries.

## Examples

``` r
# Wrap at 30 characters
cat(str_wrap("The quick brown fox jumped over the lazy dog", width=30))
#> The quick brown fox jumped
#> over the lazy dog

# Wrap a vector of strings
labels <- c("Short label", "A much longer label that needs wrapping")
str_wrap(labels, width=20)
#> [1] "Short label"                              "A much longer label\nthat needs wrapping"
str_wrap(rep("sting",50),30)
#>  [1] "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting"
#> [25] "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting" "sting"
#> [49] "sting" "sting"
# Wrap at 30 characters
cat(str_wrap("The quick brown fox jumped over the lazy dog", width = 30))
#> The quick brown fox jumped
#> over the lazy dog

# Wrap a vector of strings
labels <- c("Short label", "A much longer label that needs wrapping")
str_wrap(labels, width = 20)
#> [1] "Short label"                              "A much longer label\nthat needs wrapping"
```
