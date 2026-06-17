# Shrout reliability

Shrout reliability

## Usage

``` r
shrout(sperson, spersonitem, stime, spersontime, serror, m, k)
```

## Arguments

- sperson:

  variance component of participant

- spersonitem:

  variance component of participant by item interaction

- stime:

  variance component of time

- spersontime:

  variance component of participant by time interaction

- serror:

  variance component of error

- m:

  m item reports

- k:

  k time points

## Examples

``` r
design<-expand.grid(time=1:3,item=1:2,person=1:10)
design<-change_data_type(design,type="factor")
design$response<-rnorm(30,0,0.1)
model<-mixlm::lm(response~r(time)*r(person)+r(item)*r(person),data=design)
result<-extract_components(model)
vc<-result$components
shrout(sperson=vc[2,3],spersonitem=vc[5,3],stime=vc[1,3],
       spersontime=vc[4,3],serror=vc[6,3],3,3)
#>   measure result
#> 1     r1f 0.3233
#> 2     r1r 0.2841
#> 3      rc 0.1654
#> 4     rkf 0.5891
#> 5     rkr 0.5435
#>                                                                                           description
#> 1                            Reliability (between persons) of measures taken on the same fixed k time
#> 2                           Reliability (between persons) of measures taken on the same random k time
#> 3                                                              Reliability (within persons) of change
#> 4        Reliability (between persons) of average measures taken over fixed m items and fixed k times
#> 5 Reliability (between persons) of different random time with same number of points k between periods
```
