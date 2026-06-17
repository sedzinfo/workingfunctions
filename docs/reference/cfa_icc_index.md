# index of items to convert from lavaan to thurstonian order for analysis

index of items to convert from lavaan to thurstonian order for analysis

## Usage

``` r
cfa_icc_index(nitems, nfactors = 3)
```

## Arguments

- nitems:

  number of items in the questionnaire

- nfactors:

  number of factors

## Examples

``` r
cfa_icc_index(nitems=18,nfactors=3)
#> $index_vector
#>  [1]  1  7 13  2  8 14  3  9 15  4 10 16  5 11 17  6 12 18
#> 
#> $index_matrix
#>      [,1] [,2] [,3]
#> [1,]    1    7   13
#> [2,]    2    8   14
#> [3,]    3    9   15
#> [4,]    4   10   16
#> [5,]    5   11   17
#> [6,]    6   12   18
#> 
```
