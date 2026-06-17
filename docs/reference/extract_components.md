# Extract variance components from model

Extract variance components from model

## Usage

``` r
extract_components(model, title = "")
```

## Arguments

- model:

  model containing variance components

- title:

  plot title

## Examples

``` r
design<-expand.grid(time=1:3,item=1:3,person=1:10)
design<-change_data_type(design,type="factor")
design$response<-rowSums(change_data_type(design[,1:2],type="numeric"))+rnorm(90,0,0.1)
model<-mixlm::lm(response~r(time)*r(person)+r(item)*r(person),data=design)
extract_components(model)
#> $components
#>     component          VC vc_percent
#> 1        time  1.01979173  50.106421
#> 2      person -0.00011694   0.005746
#> 3        item  1.00340773  49.301410
#> 4 time:person  0.00005982   0.002939
#> 5 person:item -0.00093142   0.045764
#> 6   Residuals  0.01094395   0.537720
#> 
#> $plot

#> 
```
