# Smoothing

smoothing for timeseries. uses base plot

## Usage

``` r
ts_smoothing(
  df,
  start = 0.01,
  stop = 2,
  step = 0.001,
  title = "",
  type = "kernel"
)
```

## Arguments

- df:

  ts object

- start:

  start value

- stop:

  stop value

- step:

  step

- title:

  plot title

- type:

  "default" "kernel" "lowess" "friedman" "splines" "polynomial" "linear"

## Details

returns plot

## Examples

``` r
ts_data<-ts(UKDriverDeaths,start=1969,end=1984,frequency=12)
par(mfrow=c(2,2))
ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="default")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="polynomial")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="linear")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="kernel")

ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="lowess")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="friedman")
ts_smoothing(ts_data,start=.01,stop=2,step=.01,
             title="Driver Deaths in UK",type="splines")
```
