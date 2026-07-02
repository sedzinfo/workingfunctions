# Generate a data frame of random factor vectors

Creates a data frame (or single factor) populated with factor values
sampled from a supplied pool, either randomly or in a balanced
distribution across levels.

Creates a data frame (or single factor) populated with factor values
sampled from a supplied pool, either randomly or in a balanced
distribution across levels.

## Usage

``` r
generate_factor(vector = LETTERS[1:5], nrows = 2, ncols = 10, type = "random")

generate_factor(vector = LETTERS[1:5], nrows = 2, ncols = 10, type = "random")
```

## Arguments

- vector:

  Character vector. The pool of factor levels to sample from. Default is
  `LETTERS[1:5]`.

- nrows:

  Integer. Number of rows to generate. For `type = "balanced"`, `nrows`
  should be divisible by `length(vector)`. Default is `2`.

- ncols:

  Integer. Number of columns to generate. When `ncols = 1`, a single
  factor vector is returned instead of a data frame. Default is `10`.

- type:

  Character. Sampling method. One of:

  - `"random"` — each value is sampled independently with replacement.

  - `"balanced"` — each level appears exactly `nrows / length(vector)`
    times per column.

  Default is `"random"`.

## Value

A data frame of factors with `nrows` rows and `ncols` columns, or a
single factor vector when `ncols = 1`.

A data frame of factors with `nrows` rows and `ncols` columns, or a
single factor vector when `ncols = 1`.

## Examples

``` r
generate_factor(vector = LETTERS[1:5], ncols = 5, nrows = 10, type = "random")
#>    X1 X2 X3 X4 X5
#> 1   D  C  C  B  A
#> 2   E  A  D  B  B
#> 3   E  A  E  E  B
#> 4   D  E  C  D  D
#> 5   D  E  B  A  B
#> 6   A  A  A  B  E
#> 7   C  A  E  A  D
#> 8   E  A  B  C  B
#> 9   A  B  B  D  B
#> 10  C  B  D  B  E
generate_factor(vector = LETTERS[1:5], ncols = 5, nrows = 10, type = "balanced")
#>    X1 X2 X3 X4 X5
#> 1   A  A  A  A  A
#> 2   A  A  A  A  A
#> 3   B  B  B  B  B
#> 4   B  B  B  B  B
#> 5   C  C  C  C  C
#> 6   C  C  C  C  C
#> 7   D  D  D  D  D
#> 8   D  D  D  D  D
#> 9   E  E  E  E  E
#> 10  E  E  E  E  E
generate_factor(vector = LETTERS[1:5], ncols = 1, nrows = 10, type = "balanced")
#>  [1] A A B B C C D D E E
#> Levels: A B C D E
generate_factor(vector = LETTERS[1:5], ncols = 1, nrows = 10, type = "random")
#>  [1] C D D B D A D C A E
#> Levels: A B C D E
generate_factor(vector = LETTERS[1:5], ncols = 5, nrows = 10, type = "random")
#>    X1 X2 X3 X4 X5
#> 1   C  D  A  D  B
#> 2   D  D  E  D  A
#> 3   D  A  E  E  E
#> 4   A  A  D  C  E
#> 5   A  A  C  C  A
#> 6   D  C  D  C  E
#> 7   A  A  B  D  E
#> 8   C  C  A  D  B
#> 9   D  E  C  C  A
#> 10  C  A  C  D  A
generate_factor(vector = LETTERS[1:5], ncols = 5, nrows = 10, type = "balanced")
#>    X1 X2 X3 X4 X5
#> 1   A  A  A  A  A
#> 2   A  A  A  A  A
#> 3   B  B  B  B  B
#> 4   B  B  B  B  B
#> 5   C  C  C  C  C
#> 6   C  C  C  C  C
#> 7   D  D  D  D  D
#> 8   D  D  D  D  D
#> 9   E  E  E  E  E
#> 10  E  E  E  E  E
generate_factor(vector = LETTERS[1:5], ncols = 1, nrows = 10, type = "balanced")
#>  [1] A A B B C C D D E E
#> Levels: A B C D E
generate_factor(vector = LETTERS[1:5], ncols = 1, nrows = 10, type = "random")
#>  [1] A B A B D A E D D B
#> Levels: A B C D E
```
