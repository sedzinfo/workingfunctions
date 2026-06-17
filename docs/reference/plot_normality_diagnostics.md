# Normality plots

plot histogram density boxplot qq plot

## Usage

``` r
plot_normality_diagnostics(
  df,
  breaks = NULL,
  title = "",
  file = NULL,
  w = 10,
  h = 10
)
```

## Arguments

- df:

  dataframe or vector with continous or ordinal data

- breaks:

  number of bars to display

- title:

  plot title

- file:

  output filename

- w:

  width of pdf file

- h:

  height of pdf file

## Details

uses plot base

## Examples

``` r
vector<-generate_missing(rnorm(1000))
df<-generate_missing(mtcars[,1:2])
plot_normality_diagnostics(df=vector,title="",file="rnorm",breaks=30)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

#> 
plot_normality_diagnostics(df=vector,title="")
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

#> 
plot_normality_diagnostics(df=df,title="mtcars")
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%

#>   |                                                                              |======================================================================| 100%

#> 
plot_normality_diagnostics(df=df,title="mtcars",file="rnorm")
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%

#>   |                                                                              |======================================================================| 100%

#> 
```
