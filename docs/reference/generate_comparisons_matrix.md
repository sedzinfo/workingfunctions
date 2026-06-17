# Generate comparisons matrix

Generate comparisons matrix

## Usage

``` r
generate_comparisons_matrix(items)
```

## Arguments

- items:

  number of items

## Examples

``` r
generate_comparisons_matrix(2)
#>      [,1] [,2]
#> [1,]    1   -1
generate_comparisons_matrix(3)
#>      [,1] [,2] [,3]
#> [1,]    1   -1    0
#> [2,]    1    0   -1
#> [3,]    0    1   -1
generate_comparisons_matrix(4)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1   -1    0    0
#> [2,]    1    0   -1    0
#> [3,]    1    0    0   -1
#> [4,]    0    1   -1    0
#> [5,]    0    1    0   -1
#> [6,]    0    0    1   -1
generate_comparisons_matrix(5)
#>       [,1] [,2] [,3] [,4] [,5]
#>  [1,]    1   -1    0    0    0
#>  [2,]    1    0   -1    0    0
#>  [3,]    1    0    0   -1    0
#>  [4,]    1    0    0    0   -1
#>  [5,]    0    1   -1    0    0
#>  [6,]    0    1    0   -1    0
#>  [7,]    0    1    0    0   -1
#>  [8,]    0    0    1   -1    0
#>  [9,]    0    0    1    0   -1
#> [10,]    0    0    0    1   -1
generate_comparisons_matrix(6)
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,]    1   -1    0    0    0    0
#>  [2,]    1    0   -1    0    0    0
#>  [3,]    1    0    0   -1    0    0
#>  [4,]    1    0    0    0   -1    0
#>  [5,]    1    0    0    0    0   -1
#>  [6,]    0    1   -1    0    0    0
#>  [7,]    0    1    0   -1    0    0
#>  [8,]    0    1    0    0   -1    0
#>  [9,]    0    1    0    0    0   -1
#> [10,]    0    0    1   -1    0    0
#> [11,]    0    0    1    0   -1    0
#> [12,]    0    0    1    0    0   -1
#> [13,]    0    0    0    1   -1    0
#> [14,]    0    0    0    1    0   -1
#> [15,]    0    0    0    0    1   -1
```
