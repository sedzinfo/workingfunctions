# Return upper or lower matrix triangle

Return upper or lower matrix triangle

## Usage

``` r
matrix_triangle(m, off_diagonal = NA, diagonal = NULL, type = "lower")
```

## Arguments

- m:

  matrix

- off_diagonal:

  off diagonal value

- diagonal:

  diagonal value. If NULL it returns the diagonal of the input matrix

- type:

  "upper" displays upper triangle, "lower" displays lower triangle

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
