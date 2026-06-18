# Extract the upper or lower triangle of a matrix

Returns a matrix with the off-triangle values replaced by a fill value,
optionally overriding the diagonal. Useful for displaying correlation or
covariance matrices without redundant values.

## Usage

``` r
matrix_triangle(m, off_diagonal = NA, diagonal = NULL, type = "lower")
```

## Arguments

- m:

  A numeric matrix or object coercible to one.

- off_diagonal:

  Value to fill the suppressed triangle with. Default is `NA`.

- diagonal:

  Value to place on the diagonal. If `NULL`, the original diagonal of
  `m` is preserved. Default is `NULL`.

- type:

  Character. Which triangle to retain. One of `"lower"` or `"upper"`.
  Default is `"lower"`.

## Value

A matrix of the same dimensions as `m`, with the off-triangle filled by
`off_diagonal` and the diagonal set by `diagonal`.

## Examples

``` r
m<-matrix(1:9,nrow=3,ncol=3)
matrix_triangle(m=m)
#>      [,1] [,2] [,3]
#> [1,]    1   NA   NA
#> [2,]    2    5   NA
#> [3,]    3    6    9
matrix_triangle(m=m,diagonal=NA,type="lower")
#>      [,1] [,2] [,3]
#> [1,]   NA   NA   NA
#> [2,]    2   NA   NA
#> [3,]    3    6   NA
matrix_triangle(m=m,diagonal=NULL,type="lower")
#>      [,1] [,2] [,3]
#> [1,]    1   NA   NA
#> [2,]    2    5   NA
#> [3,]    3    6    9
matrix_triangle(m=m,diagonal=NA,type="upper")
#>      [,1] [,2] [,3]
#> [1,]   NA    4    7
#> [2,]   NA   NA    8
#> [3,]   NA   NA   NA
matrix_triangle(m=m,diagonal=NULL,type="upper")
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]   NA    5    8
#> [3,]   NA   NA    9
```
