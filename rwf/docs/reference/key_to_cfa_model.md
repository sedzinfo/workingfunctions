# Converts key to cfa model spesification

This function uses the key spesification used in report_alpha function
and converts the key to a cfa model spesification

## Usage

``` r
key_to_cfa_model(key)
```

## Arguments

- key:

  index of trait names and items constituring a trait

## Examples

``` r
population_model<-'t1=~x1+.5*x2+.5*x3
                   t2=~x4+.5*x5+.5*x6
                   t3=~x7+.5*x8+.5*x9'
model_data<-lavaan::simulateData(population_model,sample.nobs=1000)
key<-list(f1=paste0("x",1:3),f2=paste0("x",4:6),f3=paste0("x",7:9))
model<-key_to_cfa_model(key)
fit<-lavaan::cfa(model,model_data)
```
