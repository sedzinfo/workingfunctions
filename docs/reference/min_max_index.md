# Indices of the minimum and maximum values in a vector

Returns the positions of the minimum and maximum values in a vector.
When there are ties all tied positions are returned.

## Usage

``` r
min_max_index(vector)
```

## Arguments

- vector:

  A numeric vector.

## Value

A named list with two elements:

- max_index:

  Integer vector of positions where the maximum value occurs.

- min_index:

  Integer vector of positions where the minimum value occurs.

## Examples

``` r
vector1<-c(1,2,3,4,5,4,3,2,1)
vector2<-c(1,2,3,4,5,5,3,2,1)
vector3<-c(1,2,3,5,5,4,3,2,1)
vector4<-c(1,2,3,4,6,4,3,2,1)
vector5<-c(1,6,3,4,6,4,3,2,1)
vector<-vector1
which(vector==max(vector),arr.ind=TRUE)
#> [1] 5
which(vector==min(vector),arr.ind=TRUE)
#> [1] 1 9
min_max_index(vector1)
#> $max_index
#> [1] 5
#> 
#> $min_index
#> [1] 1 9
#> 
min_max_index(vector2)
#> $max_index
#> [1] 5 6
#> 
#> $min_index
#> [1] 1 9
#> 
min_max_index(vector3)
#> $max_index
#> [1] 4 5
#> 
#> $min_index
#> [1] 1 9
#> 
min_max_index(vector4)
#> $max_index
#> [1] 5
#> 
#> $min_index
#> [1] 1 9
#> 
min_max_index(vector5)
#> $max_index
#> [1] 2 5
#> 
#> $min_index
#> [1] 1 9
#> 
```
