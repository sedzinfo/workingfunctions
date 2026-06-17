# Takes a vector with multiple responses and dummy arranges it in a dataframe

Takes a vector with multiple responses and dummy arranges it in a
dataframe

## Usage

``` r
dummy_arrange(vector)
```

## Arguments

- vector:

  Vector

## Examples

``` r
vector1<-gsub(" ","",
             generate_multiple_responce_vector(responces=c("Agree","Hi","All"),
             responded=1:3,length=10),fixed=TRUE)
vector2<-gsub(" ","",
             generate_multiple_responce_vector(responces=1:4,responded=1:4,length=10),
             fixed=TRUE)
vector3<-sample(1:4,10,replace=TRUE)
vector4<-sample(LETTERS[1:3],10,replace=TRUE)
dummy_arrange(vector1)
#>    Agree All Hi
#> 1      1   1  1
#> 2      0   1  0
#> 3      1   0  1
#> 4      1   0  1
#> 5      1   1  0
#> 6      0   1  0
#> 7      1   1  1
#> 8      1   1  1
#> 9      1   1  1
#> 10     1   1  1
dummy_arrange(vector2)
#>    1 2 3 4
#> 1  0 1 0 0
#> 2  1 1 0 0
#> 3  0 1 0 0
#> 4  0 0 0 1
#> 5  1 0 1 0
#> 6  1 1 1 1
#> 7  0 1 0 1
#> 8  0 1 1 1
#> 9  0 1 0 0
#> 10 0 1 1 1
dummy_arrange(vector3)
#>    1 3 4
#> 1  0 0 1
#> 2  0 0 1
#> 3  0 1 0
#> 4  0 1 0
#> 5  0 1 0
#> 6  1 0 0
#> 7  1 0 0
#> 8  0 1 0
#> 9  0 0 1
#> 10 0 1 0
dummy_arrange(vector4)
#>    A B C
#> 1  0 0 1
#> 2  0 1 0
#> 3  1 0 0
#> 4  0 1 0
#> 5  1 0 0
#> 6  0 0 1
#> 7  0 0 1
#> 8  0 0 1
#> 9  0 0 1
#> 10 1 0 0
```
