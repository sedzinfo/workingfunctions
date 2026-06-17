# Compute item information for 3PL model

Compute item information for 3PL model

## Usage

``` r
compute_info_3pl(a, b, g, theta)
```

## Arguments

- a:

  numeric discrimination parameter

- b:

  numeric difficulty parameter

- g:

  numeric guessing parameter

- theta:

  numeric theta

## Examples

``` r
compute_info_3pl(a=1.5,b=1,g=.2,theta=-3)
#> [1] 5.435117e-05
compute_info_3pl(a=1.5,b=1,g=.2,theta=-2)
#> [1] 0.001029247
compute_info_3pl(a=1.5,b=1,g=.2,theta=-1)
#> [1] 0.01620814
compute_info_3pl(a=1.5,b=1,g=.2,theta=0)
#> [1] 0.1415695
compute_info_3pl(a=1.5,b=1,g=.2,theta=1)
#> [1] 0.375
compute_info_3pl(a=1.5,b=1,g=.2,theta=2)
#> [1] 0.2569949
compute_info_3pl(a=1.5,b=1,g=.2,theta=3)
#> [1] 0.08051625
ti<-compute_info_3pl(a=1.5,b=1,g=.2,theta=seq(-6,6,by=.01)) # test information
plot(ti,x=seq(-6,6,by=.01))
```
