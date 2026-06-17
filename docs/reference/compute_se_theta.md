# Compute the SE of theta

Compute the SE of theta

## Usage

``` r
compute_se_theta(info)
```

## Arguments

- info:

  numeric information

## Examples

``` r
compute_se_theta(1)
#> [1] 1
ti<-compute_info_2pl(a=10,b=0,theta=seq(-3,3,by=.01)) # test information
plot(compute_se_theta(ti),x=seq(-3,3,by=.01))
```
