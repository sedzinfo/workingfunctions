# Convert scale to thurstonian binary with n items per ranking block

Convert scale to thurstonian binary with n items per ranking block

## Usage

``` r
rank_to_binary(mydata, items, reverse = TRUE)
```

## Arguments

- mydata:

  dataframe

- items:

  number of items in block

- reverse:

  if TRUE assumes that the highest value is first item in rank if FALSE
  the lowest value is the first item in rank

## Examples

``` r
set.seed(12345)
mydata<-data.frame(i1=round(rnorm(10,mean=2,sd=1),2),
                   i2=round(rnorm(10,mean=2,sd=1),2),
                   i3=round(rnorm(10,mean=2,sd=1),2),
                   i4=round(rnorm(10,mean=2,sd=1),2),
                   i5=round(rnorm(10,mean=2,sd=1),2),
                   i6=round(rnorm(10,mean=2,sd=1),2))
rank_to_binary(mydata[,c("i1","i2","i3")],items=3)
#>       i12 i13 i23
#>  [1,]   1   0   0
#>  [2,]   0   0   1
#>  [3,]   0   1   1
#>  [4,]   0   1   1
#>  [5,]   1   1   1
#>  [6,]   0   0   0
#>  [7,]   1   1   0
#>  [8,]   1   0   0
#>  [9,]   0   0   1
#> [10,]   0   0   1
rank_to_binary(mydata[,c("i1","i2","i3")],items=3,reverse=FALSE)
#>       i12 i13 i23
#>  [1,]   0   1   1
#>  [2,]   1   1   0
#>  [3,]   1   0   0
#>  [4,]   1   0   0
#>  [5,]   0   0   0
#>  [6,]   1   1   1
#>  [7,]   0   0   1
#>  [8,]   0   1   1
#>  [9,]   1   1   0
#> [10,]   1   1   0
rank_to_binary(mydata,items=3)
#>       i12 i13 i23
#>  [1,]   1   0   0
#>  [2,]   0   0   1
#>  [3,]   0   1   1
#>  [4,]   0   1   1
#>  [5,]   1   1   1
#>  [6,]   0   0   0
#>  [7,]   1   1   0
#>  [8,]   1   0   0
#>  [9,]   0   0   1
#> [10,]   0   0   1
```
