# Replace NA with the previous element in a vector

Replace NA with the previous element in a vector

## Usage

``` r
replace_na_with_previous(vector)
```

## Arguments

- vector:

  Vector

## Examples

``` r
df1<-generate_missing(rnorm(10),missing=5)
df2<-generate_missing(rnorm(10),missing=5)
df3<-generate_missing(rnorm(10),missing=5)
df4<-generate_missing(rnorm(10),missing=5)
df5<-generate_missing(rnorm(10),missing=5)
df<-data.frame(df1,df2,df3,df4,df5)
row.names(df)<-paste0("A",row.names(df))
replace_na_with_previous(df1)
#>  [1] -0.7968 -0.2635  0.1099  0.1099 -0.8494 -0.8494 -0.8494 -0.8494 -0.8494 -1.3723
df[]<-lapply(df,replace_na_with_previous)
```
