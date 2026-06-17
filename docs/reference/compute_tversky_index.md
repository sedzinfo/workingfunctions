# Compute the Tversky index

Computes the Tversky index between two sets, a generalisation of the
Jaccard and Sørensen–Dice similarity coefficients. The index measures
the overlap between `x` and `y` relative to their differences, weighted
by `alpha` and `beta`.

The Tversky index is defined as:

\$\$T(x, y) = \frac{\|x \cap y\|}{\|x \cap y\| + \alpha\|x \setminus
y\| + \beta\|y \setminus x\|}\$\$

Special cases:

- `alpha = beta = 0.5` — Sørensen–Dice coefficient

- `alpha = beta = 1.0` — Jaccard index

## Usage

``` r
compute_tversky_index(x, y, alpha = 0.5, beta = 0.5)
```

## Arguments

- x:

  A vector. Coerced to character before comparison.

- y:

  A vector. Coerced to character before comparison.

- alpha:

  Non-negative numeric. Weight applied to elements in `x` but not in
  `y`. Defaults to `0.5`.

- beta:

  Non-negative numeric. Weight applied to elements in `y` but not in
  `x`. Defaults to `0.5`.

## Value

A single numeric value in the range `[0, 1]`, where `0` indicates no
overlap and `1` indicates identical sets.

## Note

Both `x` and `y` are treated as *sets* — duplicate elements within each
vector are ignored. Inputs are coerced to character before comparison,
so `1L` and `"1"` are treated as equal.

## See also

[`intersect`](https://rdrr.io/r/base/sets.html),
[`setdiff`](https://rdrr.io/r/base/sets.html),
[`cdf`](https://sedzinfo.github.io/rwf/reference/cdf.md)

## Examples

``` r
x <- c("a", "b", "c", "d")
y <- c("b", "c", "d", "e")

# default (Sorensen-Dice)
compute_tversky_index(x, y)
#> [1] 0.75

# Jaccard index
compute_tversky_index(x, y, alpha = 1, beta = 1)
#> [1] 0.6

# asymmetric: penalise x-only elements more heavily
compute_tversky_index(x, y, alpha = 0.9, beta = 0.1)
#> [1] 0.75

# identical sets → 1
compute_tversky_index(x, x)
#> [1] 1

# disjoint sets → 0
compute_tversky_index(c("a", "b"), c("c", "d"))
#> [1] 0
```
