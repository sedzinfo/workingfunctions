# Convert thurstonian binary triplets to scale

Convert thurstonian binary triplets to scale

## Usage

``` r
rank3_to_triplets(mydata)
```

## Arguments

- mydata:

  dataframe

## Examples

``` r
set.seed(12345)
mydata<-data.frame(i1=rnorm(10,mean=2,sd=.5),
                   i2=rnorm(10,mean=2,sd=.5),
                   i3=rnorm(10,mean=2,sd=.5),
                   i4=rnorm(10,mean=2,sd=.5),
                   i5=rnorm(10,mean=2,sd=.5),
                   i6=rnorm(10,mean=2,sd=.5))
result<-rank_to_binary(mydata[,1:3])
rank3_to_triplets(result)
#>    item1 item2 item3
#> 1      2     1     3
#> 2      1     3     2
#> 3      2     3     1
#> 4      2     3     1
#> 5      3     2     1
#> 6      1     2     3
#> 7      3     1     2
#> 8      2     1     3
#> 9      1     3     2
#> 10     1     3     2
```
