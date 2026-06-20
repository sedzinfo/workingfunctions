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
#>                                  str1                                str2    num1
#> 1  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO  0.0899
#> 2  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO  1.1056
#> 3  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO -1.5727
#> 4  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO -0.5486
#> 5  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO  1.1151
#> 6  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO  0.7274
#> 7  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO  0.7016
#> 8  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO -0.3712
#> 9  K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO  0.1284
#> 10 K B YOVZSN U C TE M GIHXPFAQ DLJRW XFC B GQ TJ I ENUK PWLMH D V SZRAYO -2.0811
```
