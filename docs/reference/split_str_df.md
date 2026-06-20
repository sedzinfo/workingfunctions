# Split a string column or row names in a data frame into separate columns

Splits a delimited string — either from row names or a specified column
— and prepends the resulting parts as new columns to the data frame.

## Usage

``` r
split_str_df(df, split = "/", type = "row", index, ...)
```

## Arguments

- df:

  A data frame.

- split:

  Character. The separator to split on. Default is `"/"`.

- type:

  Character. Where to read the string from. One of:

  `"row"`

  :   Splits the row names of `df`.

  `"collumn"`

  :   Splits the column specified by `index`.

  Default is `"row"`.

- index:

  Integer. Column index to split when `type = "collumn"`.

- ...:

  Additional arguments passed to
  [`split_str`](https://sedzinfo.github.io/rwf/reference/split_str.md).

## Value

A data frame with the split parts prepended as new columns, followed by
the original columns of `df`.

## See also

[`split_str`](https://sedzinfo.github.io/rwf/reference/split_str.md)

## Examples

``` r
df<-generate_correlation_matrix()
string<-paste0(1:nrow(df),"/",
               generate_string(nchar=2,vector_length=nrow(df)),"/",
               generate_string(nchar=2,vector_length=nrow(df)),"/",
               generate_string(nchar=2,vector_length=nrow(df)))
row.names(df)<-string
split_str_df(df,split="/",type="row")
#>             X1 X2 X3 X4     X1.1     X2.1    X3.1     X4.1       X5       X6      X7      X8       X9     X10
#> 1/lS/IG/6j   1 lS IG 6j  0.99906  1.21123  0.8995  0.64414  0.21056  1.82092 -0.6279 -0.6098 -0.35991  1.7332
#> 2/36/l9/lU   2 36 l9 lU -0.47599  0.62060  0.3498  1.63266  0.75119 -0.07826  0.5440  0.7785 -0.69674  0.9300
#> 3/xN/Hg/6F   3 xN Hg 6F -0.25266 -0.87823 -0.6045 -1.82607 -0.50380 -0.24917 -2.0538 -0.4064  1.03562 -0.9871
#> 4/Cp/1V/dT   4 Cp 1V dT  0.63591  1.03589  0.7587  0.08931  0.37483 -0.25427  1.6935  1.0676 -0.03758  1.0068
#> 5/0A/Wq/rD   5 0A Wq rD -1.45662  1.01288 -1.3829 -1.51889  0.09287  0.54339 -0.5576 -1.3335  0.45380 -2.6119
#> 6/0M/xh/zZ   6 0M xh zZ -0.49781  0.96090 -1.0961  0.77763 -0.87664  0.15742  1.4443  0.4908  1.08376 -0.6139
#> 7/cl/4a/Qe   7 cl 4a Qe -0.25974  0.15619  0.6793  0.59443  0.51417  0.74907 -1.5983 -0.7983  0.87091 -0.3298
#> 8/ne/k4/Wa   8 ne k4 Wa -0.86672  0.14754 -0.5986  0.74467 -1.61436 -0.05793 -0.6250  0.9740 -0.36662  0.7707
#> 9/zA/Ba/aC   9 zA Ba aC  0.06371  0.03177  0.5437 -0.25066  1.88604 -0.20975 -0.2750 -1.3820 -0.11024  0.3414
#> 10/2M/VO/1P 10 2M VO 1P  0.12008  0.15832  1.0289 -1.12683 -1.63485  0.25595  0.1481 -1.2167 -2.03075  1.1114
df[,1]<-string
split_str_df(df,split="/",type="collumn",index=1)
#>             X1 X2 X3 X4        X1.1     X2.1    X3.1     X4.1       X5       X6      X7      X8       X9     X10
#> 1/lS/IG/6j   1 lS IG 6j  1/lS/IG/6j  1.21123  0.8995  0.64414  0.21056  1.82092 -0.6279 -0.6098 -0.35991  1.7332
#> 2/36/l9/lU   2 36 l9 lU  2/36/l9/lU  0.62060  0.3498  1.63266  0.75119 -0.07826  0.5440  0.7785 -0.69674  0.9300
#> 3/xN/Hg/6F   3 xN Hg 6F  3/xN/Hg/6F -0.87823 -0.6045 -1.82607 -0.50380 -0.24917 -2.0538 -0.4064  1.03562 -0.9871
#> 4/Cp/1V/dT   4 Cp 1V dT  4/Cp/1V/dT  1.03589  0.7587  0.08931  0.37483 -0.25427  1.6935  1.0676 -0.03758  1.0068
#> 5/0A/Wq/rD   5 0A Wq rD  5/0A/Wq/rD  1.01288 -1.3829 -1.51889  0.09287  0.54339 -0.5576 -1.3335  0.45380 -2.6119
#> 6/0M/xh/zZ   6 0M xh zZ  6/0M/xh/zZ  0.96090 -1.0961  0.77763 -0.87664  0.15742  1.4443  0.4908  1.08376 -0.6139
#> 7/cl/4a/Qe   7 cl 4a Qe  7/cl/4a/Qe  0.15619  0.6793  0.59443  0.51417  0.74907 -1.5983 -0.7983  0.87091 -0.3298
#> 8/ne/k4/Wa   8 ne k4 Wa  8/ne/k4/Wa  0.14754 -0.5986  0.74467 -1.61436 -0.05793 -0.6250  0.9740 -0.36662  0.7707
#> 9/zA/Ba/aC   9 zA Ba aC  9/zA/Ba/aC  0.03177  0.5437 -0.25066  1.88604 -0.20975 -0.2750 -1.3820 -0.11024  0.3414
#> 10/2M/VO/1P 10 2M VO 1P 10/2M/VO/1P  0.15832  1.0289 -1.12683 -1.63485  0.25595  0.1481 -1.2167 -2.03075  1.1114
```
