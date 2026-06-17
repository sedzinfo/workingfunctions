# index parameter and items relative to their dimensions

index parameter and items relative to their dimensions

## Usage

``` r
response_dimension(response, dimensions, items)
```

## Arguments

- response:

  vector one to number of items

- dimensions:

  number of dimensions

- items:

  item comparisons

## Examples

``` r
response_dimension(c(1:18),3,c(1,2))
#>  [1]  1  2  4  5  7  8 10 11 13 14 16 17
response_dimension(c(1:18),3,c(1,3))
#>  [1]  1  3  4  6  7  9 10 12 13 15 16 18
response_dimension(c(1:18),3,c(2,3))
#>  [1]  2  3  5  6  8  9 11 12 14 15 17 18
```
