# Convert scale to thurstonian binary with n items per block and n blocks

Convert scale to thurstonian binary with n items per block and n blocks

## Usage

``` r
rank_df_to_binary(mydata, items, reverse = TRUE)
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
mydata<-data.frame(i1=rnorm(10,mean=2,sd=.5),
                   i2=rnorm(10,mean=2,sd=.5),
                   i3=rnorm(10,mean=2,sd=.5),
                   i4=rnorm(10,mean=2,sd=.5),
                   i5=rnorm(10,mean=2,sd=.5),
                   i6=rnorm(10,mean=2,sd=.5))
rank_df_to_binary(mydata[,c("i1","i2","i3","i4")],4)
#>    i12 i13 i14 i23 i24 i34
#> 1    1   0   0   0   0   0
#> 2    0   0   0   1   0   0
#> 3    0   1   0   1   0   0
#> 4    0   1   0   1   0   0
#> 5    1   1   1   1   0   0
#> 6    0   0   0   0   1   1
#> 7    1   1   1   0   0   0
#> 8    1   0   1   0   1   1
#> 9    0   0   0   1   0   0
#> 10   0   0   0   1   1   0
rank_df_to_binary(mydata,3)
#>    i12 i13 i23 i12.1 i13.1 i23.1
#> 1    1   0   0     0     1     1
#> 2    0   0   1     1     1     0
#> 3    0   1   1     1     1     0
#> 4    0   1   1     1     1     1
#> 5    1   1   1     0     1     1
#> 6    0   0   0     0     1     1
#> 7    1   1   0     1     0     0
#> 8    1   0   0     0     0     0
#> 9    0   0   1     1     0     0
#> 10   0   0   1     1     1     1
```
