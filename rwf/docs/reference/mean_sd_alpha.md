# Mean and SD

Mean and SD

## Usage

``` r
mean_sd_alpha(df, divisor = NULL)
```

## Arguments

- df:

  dataframe with one dimension

- divisor:

  number to use for dividing the rowsums

## Examples

``` r
set.seed(12345)
df<-data.frame(matrix(.5,ncol=6,nrow=6))
correlation_martix<-as.matrix(df)
diag(correlation_martix)<-1
df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
mean_sd_alpha(df)
#>    MEAN     SD
#> 1 5.007 0.7683
mean_sd_alpha(df,divisor=100)
#>     Mean     SD
#> 1 0.3004 0.0461
```
