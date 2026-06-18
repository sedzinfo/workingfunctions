# Flatten a two-dimensional list into a data frame

Converts a two-dimensional list to a data frame by applying
[`ldply`](https://rdrr.io/pkg/plyr/man/ldply.html) across the top-level
elements.

## Usage

``` r
flatten_list(mydata)
```

## Arguments

- mydata:

  A list where each element can be coerced to a data frame.

## Value

A data frame combining all list elements row-wise, with an additional
`.id` column containing the top-level list names.
