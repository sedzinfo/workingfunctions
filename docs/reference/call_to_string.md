# Model call to string

Takes a call object and convert it to string

## Usage

``` r
call_to_string(model)
```

## Arguments

- model:

  Model object

## Examples

``` r
df<-generate_correlation_matrix()
model<-lm(df$X1~df$X2)
call_to_string(model)
#> [1] "lm(formula=df$X1~df$X2)"
```
