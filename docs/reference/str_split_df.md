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
#>             X1 X2 X3 X4    X1.1     X2.1    X3.1     X4.1      X5       X6      X7      X8       X9      X10
#> 1/CH/ex/yI   1 CH ex yI -0.8604 -1.33733  1.6906  0.01533 -0.9643 -0.25214  1.7688 -1.5272  0.61927  1.08335
#> 2/QD/jQ/7l   2 QD jQ 7l  1.4671 -0.06336 -0.5768 -0.11673 -2.0641 -0.48184  1.7399 -1.1830 -0.92705 -0.92233
#> 3/I5/OG/wp   3 I5 OG wp  0.3812 -0.73938 -1.9399 -0.18874  1.0460  1.79592  2.1881  0.1342  1.14902 -0.80109
#> 4/tg/r5/CN   4 tg r5 CN -0.4331 -0.29023 -0.5906 -0.19266 -0.1687 -0.67487 -0.8593  0.1510 -0.97774 -0.07676
#> 5/45/N9/oW   5 45 N9 oW  0.2544  1.02915  1.1006  0.87717 -0.1901  1.13835 -0.8919  1.4051  0.05701  1.50906
#> 6/3j/3Z/XC   6 3j 3Z XC  1.4711  0.97369 -0.1981  0.06429  0.3339 -0.66801  0.4147 -0.2200  1.15073  2.48201
#> 7/yp/79/yw   7 yp 79 yw -1.5067  0.09428 -0.1490 -0.14370 -0.1069 -0.09073 -0.3133 -1.2426 -0.06327  0.63827
#> 8/2p/xK/RU   8 2p xK RU -1.2428 -0.63388  1.0950 -1.37165  0.2186  1.08843 -1.0926 -0.7081 -0.05094 -0.29377
#> 9/JP/dg/Iw   9 JP dg Iw -2.3344 -1.52596  2.0776 -0.01648 -0.9095 -2.15789 -1.0673  1.1294 -1.48974 -1.64908
#> 10/Ei/0X/AS 10 Ei 0X AS  0.6135  1.04310  1.9135 -0.04616  0.5490  0.25798  0.4558 -0.1409 -1.73948 -0.25559
df[, 1] <- string
str_split_df(df, split = "/", type = "collumn", index = 1)
#>             X1 X2 X3 X4        X1.1     X2.1    X3.1     X4.1      X5       X6      X7      X8       X9      X10
#> 1/CH/ex/yI   1 CH ex yI  1/CH/ex/yI -1.33733  1.6906  0.01533 -0.9643 -0.25214  1.7688 -1.5272  0.61927  1.08335
#> 2/QD/jQ/7l   2 QD jQ 7l  2/QD/jQ/7l -0.06336 -0.5768 -0.11673 -2.0641 -0.48184  1.7399 -1.1830 -0.92705 -0.92233
#> 3/I5/OG/wp   3 I5 OG wp  3/I5/OG/wp -0.73938 -1.9399 -0.18874  1.0460  1.79592  2.1881  0.1342  1.14902 -0.80109
#> 4/tg/r5/CN   4 tg r5 CN  4/tg/r5/CN -0.29023 -0.5906 -0.19266 -0.1687 -0.67487 -0.8593  0.1510 -0.97774 -0.07676
#> 5/45/N9/oW   5 45 N9 oW  5/45/N9/oW  1.02915  1.1006  0.87717 -0.1901  1.13835 -0.8919  1.4051  0.05701  1.50906
#> 6/3j/3Z/XC   6 3j 3Z XC  6/3j/3Z/XC  0.97369 -0.1981  0.06429  0.3339 -0.66801  0.4147 -0.2200  1.15073  2.48201
#> 7/yp/79/yw   7 yp 79 yw  7/yp/79/yw  0.09428 -0.1490 -0.14370 -0.1069 -0.09073 -0.3133 -1.2426 -0.06327  0.63827
#> 8/2p/xK/RU   8 2p xK RU  8/2p/xK/RU -0.63388  1.0950 -1.37165  0.2186  1.08843 -1.0926 -0.7081 -0.05094 -0.29377
#> 9/JP/dg/Iw   9 JP dg Iw  9/JP/dg/Iw -1.52596  2.0776 -0.01648 -0.9095 -2.15789 -1.0673  1.1294 -1.48974 -1.64908
#> 10/Ei/0X/AS 10 Ei 0X AS 10/Ei/0X/AS  1.04310  1.9135 -0.04616  0.5490  0.25798  0.4558 -0.1409 -1.73948 -0.25559
```
