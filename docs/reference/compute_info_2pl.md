# Compute item information for 2PL model

Compute item information for 2PL model

## Usage

``` r
compute_info_2pl(a, b, theta)
```

## Arguments

- a:

  numeric discrimination parameter

- b:

  numeric difficulty parameter

- theta:

  numeric theta

## Examples

``` r
compute_info_2pl(a=1.5,b=1,theta=-3)
#> [1] 0.005549646
compute_info_2pl(a=1.5,b=1,theta=-2)
#> [1] 0.02444902
compute_info_2pl(a=1.5,b=1,theta=-1)
#> [1] 0.1016475
compute_info_2pl(a=1.5,b=1,theta=0)
#> [1] 0.3355795
compute_info_2pl(a=1.5,b=1,theta=1)
#> [1] 0.5625
compute_info_2pl(a=1.5,b=1,theta=2)
#> [1] 0.3355795
compute_info_2pl(a=1.5,b=1,theta=3)
#> [1] 0.1016475
ti<-compute_info_2pl(a=1,b=-2,theta=seq(-6,6,by=.01)) # test information
plot(ti,x=seq(-6,6,by=.01))

ti<-compute_info_2pl(a=2,b=0,theta=seq(-6,6,by=.01)) # test information
plot(ti,x=seq(-6,6,by=.01))

ti<-compute_info_2pl(a=3,b=2,theta=seq(-6,6,by=.01)) # test information
plot(ti,x=seq(-6,6,by=.01))
```
