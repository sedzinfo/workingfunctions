# Compute item information for 1PL model

Compute item information for 1PL model

## Usage

``` r
compute_info_1pl(b, theta)
```

## Arguments

- b:

  numeric difficulty parameter

- theta:

  numeric theta

## Examples

``` r
compute_info_1pl(b=1,theta=-3)
#> [1] 0.01766271
compute_info_1pl(b=1,theta=-2)
#> [1] 0.04517666
compute_info_1pl(b=1,theta=-1)
#> [1] 0.1049936
compute_info_1pl(b=1,theta=0)
#> [1] 0.1966119
compute_info_1pl(b=1,theta=1)
#> [1] 0.25
compute_info_1pl(b=1,theta=2)
#> [1] 0.1966119
compute_info_1pl(b=1,theta=3)
#> [1] 0.1049936
ti<-compute_info_1pl(b=1,theta=seq(-6,6,by=.01)) # test information
plot(ti,x=seq(-6,6,by=.01))
```
