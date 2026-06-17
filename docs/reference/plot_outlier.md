# Outlier graph using mean median and boxplot algorythms

Outlier graph using mean median and boxplot algorythms

## Usage

``` r
plot_outlier(df, method = "mean", title = "", base_size = 10)
```

## Arguments

- df:

  dataframe or vector with continous or ordinal data

- method:

  "mean" "median" "boxplot"

- title:

  plot title

- base_size:

  base font size

## Author

unknown

## Examples

``` r
vector<-generate_missing(rnorm(1000))
df<-generate_missing(mtcars[,1:2])
plot_outlier(df=vector,method="mean",title="random vector")
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $vector

#> 
plot_outlier(df=vector,method="median")
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $vector

#> 
plot_outlier(df=vector,method="boxplot")
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $vector

#> 
plot_outlier(df=df,method="mean",title="random vector")
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> $mpg

#> 
#> $cyl

#> 
plot_outlier(df=df,method="median")
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> $mpg

#> 
#> $cyl

#> 
plot_outlier(df=df,method="boxplot")
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> $mpg

#> 
#> $cyl

#> 
plot_multiplot(plotlist=plot_outlier(df=mtcars[,2:5],method="mean"),cols=2)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

#> [[1]]
#> 
```
