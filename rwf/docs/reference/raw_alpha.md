# Raw alpha

Raw alpha

## Usage

``` r
raw_alpha(df)
```

## Arguments

- df:

  dataframe with one dimension

## Examples

``` r
set.seed(12345)
df<-data.frame(matrix(.5,ncol=6,nrow=6))
correlation_martix<-as.matrix(df)
diag(correlation_martix)<-1
df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
psych::alpha(df)
#> 
#> Reliability analysis   
#> Call: psych::alpha(x = df)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
#>       0.84      0.84    0.81      0.46 5.2 0.008    5 0.77     0.45
#> 
#>     95% confidence boundaries 
#>          lower alpha upper
#> Feldt     0.82  0.84  0.85
#> Duhachek  0.82  0.84  0.85
#> 
#>  Reliability if an item is dropped:
#>    raw_alpha std.alpha G6(smc) average_r S/N alpha se   var.r med.r
#> X1      0.82      0.82    0.78      0.47 4.5   0.0091 0.00059  0.48
#> X2      0.81      0.81    0.77      0.46 4.2   0.0096 0.00065  0.45
#> X3      0.81      0.80    0.77      0.45 4.1   0.0097 0.00051  0.45
#> X4      0.81      0.81    0.78      0.47 4.4   0.0093 0.00061  0.46
#> X5      0.81      0.81    0.78      0.46 4.3   0.0093 0.00072  0.47
#> X6      0.81      0.81    0.77      0.46 4.3   0.0095 0.00063  0.45
#> 
#>  Item statistics 
#>       n raw.r std.r r.cor r.drop mean  sd
#> X1 1000  0.72  0.72  0.63   0.58    5 1.0
#> X2 1000  0.76  0.76  0.69   0.63    5 1.0
#> X3 1000  0.77  0.76  0.70   0.64    5 1.1
#> X4 1000  0.73  0.73  0.66   0.60    5 1.0
#> X5 1000  0.74  0.74  0.66   0.60    5 1.0
#> X6 1000  0.74  0.74  0.67   0.61    5 1.0
#> 
#> Non missing response frequency for each item
#>    1    2    3    4    5    6    7    8 9 miss
#> X1 0 0.00 0.07 0.22 0.39 0.26 0.06 0.00 0    0
#> X2 0 0.01 0.05 0.23 0.37 0.26 0.06 0.00 0    0
#> X3 0 0.00 0.07 0.23 0.39 0.23 0.06 0.01 0    0
#> X4 0 0.01 0.05 0.25 0.39 0.24 0.06 0.01 0    0
#> X5 0 0.01 0.05 0.25 0.39 0.23 0.07 0.00 0    0
#> X6 0 0.01 0.06 0.24 0.40 0.23 0.05 0.01 0    0
raw_alpha(df=df)
#> [1] 0.8375
```
