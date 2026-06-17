# Symmetric Matrix

Symmetric Matrix

## Usage

``` r
symmetric_matrix(matrix, duplicate = "lower", diagonal = NULL)
```

## Arguments

- matrix:

  matrix

- duplicate:

  "upper" duplicates upper triangle "lower" duplicates lower triangle

- diagonal:

  diagonal values

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
