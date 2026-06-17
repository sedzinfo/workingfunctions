# Generate dataframe which outputs a predetermined correlation matrix

Generate dataframe which outputs a predetermined correlation matrix

## Usage

``` r
generate_correlation_matrix(correlation_martix, nrows = 10)
```

## Arguments

- correlation_martix:

  correlation matrix of resulting dataframe

- nrows:

  number of rows to generate

## Examples

``` r
df<-data.frame(matrix(.999,ncol=2,nrow=2))
correlation_martix<-as.matrix(df)
diag(correlation_martix)<-1
df<-generate_correlation_matrix(correlation_martix,nrows=100)
stats::cor(df)
#>        X1     X2
#> X1 1.0000 0.9993
#> X2 0.9993 1.0000
```
