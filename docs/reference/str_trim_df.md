# Trim whitespace from all character cells in a data frame

Applies [`strwrap`](https://rdrr.io/r/base/strwrap.html) to every
character cell in a data frame, removing leading and trailing
whitespace.

## Usage

``` r
str_trim_df(df)
```

## Arguments

- df:

  A data frame containing one or more character columns.

## Value

A data frame of the same dimensions with whitespace trimmed from all
character cells. Non-character cells are unchanged.

## Examples

``` r
string <- data.frame(
  str1 = rep(paste0(sample(c(LETTERS, rep(" ", 10))), collapse = ""), 10),
  str2 = rep(paste0(sample(c(LETTERS, rep(" ", 10))), collapse = ""), 10),
  num1 = rnorm(10),
  stringsAsFactors = FALSE
)
str_trim_df(string)
#>                                  str1                             str2    num1
#> 1  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY  0.5906
#> 2  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY  0.8039
#> 3  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY  0.9655
#> 4  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY  1.5102
#> 5  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY  0.9960
#> 6  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY  0.7813
#> 7  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY -0.1158
#> 8  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY -0.5693
#> 9  HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY -0.1245
#> 10 HW OMD CLQRSG KAJE B UTY PNZI FX V FMDN G ZRK WOH LVTPA Q ESUXIBJCY -0.9345
```
