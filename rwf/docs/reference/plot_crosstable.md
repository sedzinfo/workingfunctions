# Plot crosstables

Plot crosstables

## Usage

``` r
plot_crosstable(
  df,
  factor_index,
  combinations = NULL,
  shape = 16,
  angle = 0,
  base_size = 10,
  title = ""
)
```

## Arguments

- df:

  dataframe

- factor_index:

  index of factors

- combinations:

  index of comparisons

- shape:

  shape of points

- angle:

  angle of xaxis labels

- base_size:

  base font size

- title:

  plot title

## Examples

``` r
combinations<-data.frame(index1=c("vs","am","gear"),index2=c("cyl","cyl","cyl"))
plot_crosstable(df=mtcars,factor_index=8:9)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $am_vs

#> 
plot_crosstable(df=mtcars,combinations=combinations)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
#> $vs_cyl

#> 
#> $am_cyl

#> 
#> $gear_cyl

#> 
```
