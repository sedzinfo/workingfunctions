# Boxplot

Boxplot

## Usage

``` r
plot_boxplot(df, title = "", base_size = 10)
```

## Arguments

- df:

  dataframe or vector with continous or ordinal data

- title:

  Plot title

- base_size:

  numeric base font size

## Details

uses ggplot

## Examples

``` r
vector<-generate_missing(rnorm(1000))
df<-generate_missing(mtcars[,1:2])
plot_boxplot(df=vector)
#> Warning: Removed 5 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).

plot_boxplot(df=generate_missing(vector))
#> Warning: Removed 10 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).

plot_boxplot(df=df)
#> Warning: Removed 10 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
```
