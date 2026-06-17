# Drops unused factor levels

Drops unused factor levels

## Usage

``` r
drop_levels(df, factor_index = NULL, minimum_frequency = 5)
```

## Arguments

- df:

  dataframe

- factor_index:

  numeric index of factors. If NULL the function uses is.factor() to
  discriminate factors

- minimum_frequency:

  the minimum frequency each factor will have, levels with frequency
  bellow or equal to the defined frequency will be renamed "Other"

## Examples

``` r
factor1<-factor(c(rep("A",10),rep("B",10)),levels=c("A","B","C","D"))
factor2<-factor(c(rep("A",10),rep("B",10)),levels=c("A","B","C","D"))
numeric1<-c(1:20)
df<-data.frame(numeric1,factor1,factor2)
df$factor1
#>  [1] A A A A A A A A A A B B B B B B B B B B
#> Levels: A B C D
drop_levels(df=df,minimum_frequency=9)
#>    numeric1 factor1 factor2
#> 1         1       A       A
#> 2         2       A       A
#> 3         3       A       A
#> 4         4       A       A
#> 5         5       A       A
#> 6         6       A       A
#> 7         7       A       A
#> 8         8       A       A
#> 9         9       A       A
#> 10       10       A       A
#> 11       11       B       B
#> 12       12       B       B
#> 13       13       B       B
#> 14       14       B       B
#> 15       15       B       B
#> 16       16       B       B
#> 17       17       B       B
#> 18       18       B       B
#> 19       19       B       B
#> 20       20       B       B
drop_levels(df=df,minimum_frequency=10)
#>    numeric1 factor1 factor2
#> 1         1   Other   Other
#> 2         2   Other   Other
#> 3         3   Other   Other
#> 4         4   Other   Other
#> 5         5   Other   Other
#> 6         6   Other   Other
#> 7         7   Other   Other
#> 8         8   Other   Other
#> 9         9   Other   Other
#> 10       10   Other   Other
#> 11       11   Other   Other
#> 12       12   Other   Other
#> 13       13   Other   Other
#> 14       14   Other   Other
#> 15       15   Other   Other
#> 16       16   Other   Other
#> 17       17   Other   Other
#> 18       18   Other   Other
#> 19       19   Other   Other
#> 20       20   Other   Other
```
