# Combine upper and lower triangles from two matrices

Merges two matrices by taking the upper triangle from one and the lower
triangle from the other, with flexible control over the diagonal. Useful
for displaying two related statistics (e.g. correlations and p-values)
in a single compact matrix.

## Usage

``` r
symmetric_matrix(matrix, duplicate = "lower", diagonal = NULL)
```

## Arguments

- diagonal:

  Controls the diagonal of the returned matrix. One of:

  - `"upper"` — use the diagonal of `m_upper`.

  - `"lower"` — use the diagonal of `m_lower`.

  - `NA` — fill the diagonal with `NA`.

  - A numeric or character vector of length `nrow(m_upper)` — use the
    supplied values directly.

  Default is `NA`.

- m_upper:

  A numeric matrix. Its upper triangle is used in the result.

- m_lower:

  A numeric matrix of the same dimensions as `m_upper`. Its lower
  triangle is used in the result.

## Value

A matrix of the same dimensions as the inputs, combining the upper
triangle of `m_upper` and the lower triangle of `m_lower`.

## See also

[`matrix_triangle`](https://sedzinfo.github.io/rwf/reference/matrix_triangle.md)

## Examples

``` r
m_lower<-matrix_triangle(matrix(1:9,nrow=3,ncol=3),type="lower",diagonal=NA)
m_upper<-matrix_triangle(matrix(11:19,nrow=3,ncol=3),type="upper",diagonal=NA)
symmetric_matrix(matrix=m_lower,duplicate="lower",diagonal=NA)
#>      [,1] [,2] [,3]
#> [1,]   NA    2    3
#> [2,]    2   NA    6
#> [3,]    3    6   NA
symmetric_matrix(matrix=m_upper,duplicate="upper",diagonal=NA)
#>      [,1] [,2] [,3]
#> [1,]   NA   14   17
#> [2,]   14   NA   18
#> [3,]   17   18   NA
```
