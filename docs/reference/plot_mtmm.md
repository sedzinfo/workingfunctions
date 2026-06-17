# Plot multitrait multimethod matrix

Plot multitrait multimethod matrix

## Usage

``` r
plot_mtmm(df, key, method, subject, title = "")
```

## Arguments

- df:

  dataframe

- key:

  List index of trait names and items constituring a trait

- method:

  name of dataframe collumn spesifying the method used for the row
  observed

- subject:

  name of dataframe collumn spesifying subject id

- title:

  plot title

## Examples

``` r
population_model<-'t1=~x1+.9*x2+.9*x3
                   t2=~x4+.9*x5+.9*x6
                   t3=~x7+.9*x8+.9*x9'
model_data<-lavaan::simulateData(population_model,sample.nobs=1000)
model_data<-model_data[sample(1:1000,1000,TRUE),]
model_data<-rbind(model_data,model_data,model_data)
model_data$method<-c(rep("m1",1000),rep("m2",1000),rep("m3",1000))
model_data$id<-rep(1:1000,3)
key<-list(t1=paste0("x",1:3),t2=paste0("x",4:6),t3=paste0("x",7:9))
plot_mtmm(df=model_data,key=key,method="method",subject="id")
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the rwf package.
#>   Please report the issue at <https://github.com/sedzinfo/rwf/issues>.
```
