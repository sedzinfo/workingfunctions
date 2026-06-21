# Reverse-score a numeric vector

Reverses the order of values in a vector by mapping each value to its
mirror equivalent based on the observed levels. Useful for
reverse-scoring Likert scale items.

## Usage

``` r
swap(vector)
```

## Arguments

- vector:

  Numeric vector to reverse-score.

## Value

A numeric vector of the same length with values reverse-mapped across
the observed range.

## Examples

``` r
swap(c(1:10, 1, 2, 3))
#>  [1] 10  9  8  7  6  5  4  3  2  1 10  9  8
```
