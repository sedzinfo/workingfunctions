# Split a string column or row names in a data frame into separate columns

Splits a delimited string — either from row names or a specified column
— and prepends the resulting parts as new columns to the data frame.

## Usage

``` r
str_split_df(df, split = "/", type = "row", index, ...)
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
  [`str_split`](https://sedzinfo.github.io/rwf/reference/str_split.md).

## Value

A data frame with the split parts prepended as new columns, followed by
the original columns of `df`.

## See also

[`str_split`](https://sedzinfo.github.io/rwf/reference/str_split.md)

## Examples

``` r
df <- generate_correlation_matrix()
string <- paste0(
  1:nrow(df), "/",
  generate_string(nchar = 2, vector_length = nrow(df)), "/",
  generate_string(nchar = 2, vector_length = nrow(df)), "/",
  generate_string(nchar = 2, vector_length = nrow(df))
)
row.names(df) <- string
str_split_df(df, split = "/", type = "row")
#>             X1 X2 X3 X4    X1.1     X2.1     X3.1    X4.1       X5      X6        X7       X8       X9      X10
#> 1/fQ/hJ/ui   1 fQ hJ ui  0.4308  0.68017 -1.67940 -0.9062  0.76271 -0.4339 -0.925594  0.10072 -0.64128  0.69835
#> 2/Qj/ZD/9s   2 Qj ZD 9s -1.5950 -0.05988 -0.63110  0.9741 -0.35814  0.7838  0.003346 -0.03021  0.47340 -0.32443
#> 3/TP/TS/KD   3 TP TS KD  1.8854 -0.18861 -0.26309 -1.3890 -0.13193  0.3211  0.133241  0.16674  1.02011 -1.13349
#> 4/8g/TH/dF   4 8g TH dF -1.6747  0.19713  0.09626 -1.2774 -2.11806  1.0191 -0.084268 -1.05466 -0.37918 -2.30019
#> 5/YD/Pi/iP   5 YD Pi iP -0.4650  0.18201  1.63690  1.2467  0.93060 -0.9058 -0.089102  0.04361 -1.38608  1.69504
#> 6/PD/0l/24   6 PD 0l 24  0.9992  1.21506 -1.26943  0.7460 -0.20173  0.6734  0.842322  0.91683 -0.43581 -0.49987
#> 7/C5/2U/uR   7 C5 2U uR  2.5392  1.74240  0.18256  0.8834  2.26492 -0.4972  1.357784 -0.49065 -1.85896  0.19958
#> 8/yP/fx/5Z   8 yP fx 5Z -0.8031 -0.67860 -1.49486 -1.7666 -0.78241 -0.7107  0.047130 -2.11570  0.09692 -0.81949
#> 9/vC/Mc/zk   9 vC Mc zk  1.2165  0.98615 -0.91692  0.5910  0.02832  0.6972 -0.051687  0.23814  2.02315 -0.09764
#> 10/4S/ie/zk 10 4S ie zk -1.3484 -0.12793 -0.01146 -1.0350 -0.59123 -0.4356  1.238670  0.42504 -0.21897 -0.95539
df[, 1] <- string
str_split_df(df, split = "/", type = "collumn", index = 1)
#>             X1 X2 X3 X4        X1.1     X2.1     X3.1    X4.1       X5      X6        X7       X8       X9      X10
#> 1/fQ/hJ/ui   1 fQ hJ ui  1/fQ/hJ/ui  0.68017 -1.67940 -0.9062  0.76271 -0.4339 -0.925594  0.10072 -0.64128  0.69835
#> 2/Qj/ZD/9s   2 Qj ZD 9s  2/Qj/ZD/9s -0.05988 -0.63110  0.9741 -0.35814  0.7838  0.003346 -0.03021  0.47340 -0.32443
#> 3/TP/TS/KD   3 TP TS KD  3/TP/TS/KD -0.18861 -0.26309 -1.3890 -0.13193  0.3211  0.133241  0.16674  1.02011 -1.13349
#> 4/8g/TH/dF   4 8g TH dF  4/8g/TH/dF  0.19713  0.09626 -1.2774 -2.11806  1.0191 -0.084268 -1.05466 -0.37918 -2.30019
#> 5/YD/Pi/iP   5 YD Pi iP  5/YD/Pi/iP  0.18201  1.63690  1.2467  0.93060 -0.9058 -0.089102  0.04361 -1.38608  1.69504
#> 6/PD/0l/24   6 PD 0l 24  6/PD/0l/24  1.21506 -1.26943  0.7460 -0.20173  0.6734  0.842322  0.91683 -0.43581 -0.49987
#> 7/C5/2U/uR   7 C5 2U uR  7/C5/2U/uR  1.74240  0.18256  0.8834  2.26492 -0.4972  1.357784 -0.49065 -1.85896  0.19958
#> 8/yP/fx/5Z   8 yP fx 5Z  8/yP/fx/5Z -0.67860 -1.49486 -1.7666 -0.78241 -0.7107  0.047130 -2.11570  0.09692 -0.81949
#> 9/vC/Mc/zk   9 vC Mc zk  9/vC/Mc/zk  0.98615 -0.91692  0.5910  0.02832  0.6972 -0.051687  0.23814  2.02315 -0.09764
#> 10/4S/ie/zk 10 4S ie zk 10/4S/ie/zk -0.12793 -0.01146 -1.0350 -0.59123 -0.4356  1.238670  0.42504 -0.21897 -0.95539
```
