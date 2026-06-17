# Plot response frequencies

Plot response frequencies

## Usage

``` r
plot_response_frequencies(
  df,
  factor_index,
  base_size = 10,
  title = "",
  width = 100,
  reorder = FALSE
)
```

## Arguments

- df:

  dataframe

- factor_index:

  index of factors

- base_size:

  base font size

- title:

  plot title

- width:

  Numeric, wrap width for x-axis title

- reorder:

  Logical, whether to reorder factors based on frequency

## Examples

``` r
plot_response_frequencies(df=mtcars,factor_index=1:10)
#> $mpg

#> 
#> $cyl

#> 
#> $disp

#> 
#> $hp

#> 
#> $drat

#> 
#> $wt

#> 
#> $qsec

#> 
#> $vs

#> 
#> $am

#> 
#> $gear

#> 
```
