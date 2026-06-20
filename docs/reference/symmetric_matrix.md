# Make a symmetric matrix by duplicating one triangle

Mirrors either the lower or upper triangle of a matrix to the opposite
side, producing a symmetric matrix. Optionally sets the diagonal.

## Usage

``` r
symmetric_matrix(matrix, duplicate = "lower", diagonal = NULL)
```

## Arguments

- matrix:

  A square numeric matrix.

- duplicate:

  Character. Which triangle to use as the source. One of: `"lower"`
  mirrors the lower triangle to the upper, or `"upper"` mirrors the
  upper triangle to the lower. Default is `"lower"`.

- diagonal:

  Value to place on the diagonal. If omitted, the original diagonal of
  `matrix` is preserved. Pass `NA` to fill with `NA`.

## Value

A symmetric matrix of the same dimensions as the input.

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
