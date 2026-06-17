# Percent of outliers in vector

Percent of outliers in vector

## Usage

``` r
outlier_summary(vector)
```

## Arguments

- vector:

  numeric vector

## Details

returns dataframe

## Examples

``` r
vector<-generate_missing(rnorm(1000))
df<-generate_missing(mtcars[,1:2])
outlier_summary(vector)
#>   abs_z_1.96 abs_z_2.58 abs_z_3.29
#> 1     5.03 %     1.01 %      0.1 %
data.frame(sapply(mtcars,outlier_summary))
#>               mpg cyl disp     hp   drat     wt   qsec  vs  am gear   carb
#> abs_z_1.96 6.25 % 0 %  0 % 3.12 % 3.12 % 9.38 % 3.12 % 0 % 0 %  0 % 6.25 %
#> abs_z_2.58    0 % 0 %  0 % 3.12 %    0 %    0 % 3.12 % 0 % 0 %  0 % 3.12 %
#> abs_z_3.29    0 % 0 %  0 %    0 %    0 %    0 %    0 % 0 % 0 %  0 %    0 %
```
