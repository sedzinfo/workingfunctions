# Plot autocorrelation function of correlation covariance and partial correlation

uses ggplot

## Usage

``` r
plot_acf(df, lag.max = length(df), base_size = 10, title = "")
```

## Arguments

- df:

  ts object

- lag.max:

  maximum lags to include

- base_size:

  base font size

- title:

  plot title

## Details

returns plot

## Examples

``` r
ts_data<-ts(UKDriverDeaths,start=1969,end=1984,frequency=12)
plot_acf(df=ts_data,base_size=20)
```
