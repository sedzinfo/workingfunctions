# Generate Matrix A

Generate Matrix A

## Usage

``` r
generate_matrix_A(blocks = 3, items = 3)
```

## Arguments

- blocks:

  number of blocks

- items:

  number of items per block

## Examples

``` r
generate_matrix_A(blocks=3,items=3)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#>  [1,]    1   -1    0    0    0    0    0    0    0
#>  [2,]    1    0   -1    0    0    0    0    0    0
#>  [3,]    0    1   -1    0    0    0    0    0    0
#>  [4,]    0    0    0    1   -1    0    0    0    0
#>  [5,]    0    0    0    1    0   -1    0    0    0
#>  [6,]    0    0    0    0    1   -1    0    0    0
#>  [7,]    0    0    0    0    0    0    1   -1    0
#>  [8,]    0    0    0    0    0    0    1    0   -1
#>  [9,]    0    0    0    0    0    0    0    1   -1
```
