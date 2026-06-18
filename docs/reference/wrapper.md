# Wrap a string to a specified width

Wraps a character string at a given width and collapses the result into
a single newline-delimited string. Useful for formatting long plot
titles or labels.

## Usage

``` r
wrapper(x, ...)
```

## Arguments

- x:

  Character. The string to wrap.

- ...:

  Additional arguments passed to
  [`strwrap`](https://rdrr.io/r/base/strwrap.html), such as `width`.

## Value

A single character string with newlines inserted at wrap points.

## Examples

``` r
wrapper(rep("sting",50),30)
#> [1] "sting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting\nsting"
```
