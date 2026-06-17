# Convert key to index list

Convert key to index list

## Usage

``` r
questions_by_keys(key)
```

## Arguments

- key:

  a vector indicating the dimension of each question. The order of the
  elements in the key represents the order of the questions, the numeric
  values represent the dimension the question belongs to

## Examples

``` r
key<-c(1,2,3,4,5,1,2,3,4,5)
questions_by_keys(key)
#> [[1]]
#> [1] 1 6
#> 
#> [[2]]
#> [1] 2 7
#> 
#> [[3]]
#> [1] 3 8
#> 
#> [[4]]
#> [1] 4 9
#> 
#> [[5]]
#> [1]  5 10
#> 
```
