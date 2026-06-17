# Residuals for matrices

Root Mean Squared Residual Number of absolute residuals \> 0.05
Proportion of absolute residuals \> 0.05. It can either accept a psych
EFA model or it can compare two correlation or covariance matrices

## Usage

``` r
compute_residual_stats(model, data = NULL)
```

## Arguments

- model:

  psych EFA model. It has to be a correlation or covariance matrix if
  data is not NULL

- data:

  correlation or covariance matrix

## Examples

``` r
model<-psych::fa(mtcars,nfactors=2,rotate="oblimin",fm="pa",oblique.scores=TRUE)
compute_residual_stats(model)
#>                       residual_statistics       value critical                              formula
#> 1              Root Mean Squared Residual  0.04419293       NA              sqrt(mean(residuals^2))
#> 2     Number of absolute residuals > 0.05 13.00000000       NA                  abs(residuals)>0.05
#> 3 Proportion of absolute residuals > 0.05  0.23636364      0.5 numberLargeResiduals/nrow(residuals)
```
