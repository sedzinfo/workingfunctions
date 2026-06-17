# Response frequencies

returns count proportion percent

## Usage

``` r
response_frequency(
  df,
  max = 10,
  uniqueitems = NULL,
  type = "percent",
  file = NULL
)
```

## Arguments

- df:

  dataframe

- max:

  maximum score

- uniqueitems:

  number of unique items

- type:

  "frequency" "proportion" "percent" "all"

- file:

  output filename

## Details

returns dataframe

## Examples

``` r
response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="frequency")
#>        type variable 1  2  3  4 5 6 7 8 miss responses
#> 1 Frequency     gear 0  0 15 12 5 0 0 0    0        32
#> 2 Frequency     carb 7 10  3 10 0 1 0 1    0        32
response_frequency(mtcars[,c("gear")],uniqueitems=1:8,type="proportion")
#>         type variable 1 2      3     4      5 6 7 8 miss responses
#> 1 Proportion       df 0 0 0.4688 0.375 0.1562 0 0 0    0        32
response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="percent")
#>      type variable     1     2      3     4     5     6 7     8 miss responses
#> 1 Percent     gear  0.00  0.00 46.875 37.50 15.62 0.000 0 0.000    0        32
#> 2 Percent     carb 21.88 31.25  9.375 31.25  0.00 3.125 0 3.125    0        32
response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="all")
#>         type variable       1       2        3       4       5       6 7
#> 1  Frequency     gear  0.0000  0.0000 15.00000 12.0000  5.0000 0.00000 0
#> 2  Frequency     carb  7.0000 10.0000  3.00000 10.0000  0.0000 1.00000 0
#> 3 Proportion     gear  0.0000  0.0000  0.46875  0.3750  0.1562 0.00000 0
#> 4 Proportion     carb  0.2188  0.3125  0.09375  0.3125  0.0000 0.03125 0
#> 5    Percent     gear  0.0000  0.0000 46.87500 37.5000 15.6250 0.00000 0
#> 6    Percent     carb 21.8750 31.2500  9.37500 31.2500  0.0000 3.12500 0
#>         8 miss responses
#> 1 0.00000    0        32
#> 2 1.00000    0        32
#> 3 0.00000    0        32
#> 4 0.03125    0        32
#> 5 0.00000    0        32
#> 6 3.12500    0        32
response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="all",
                   file="descriptives")
#>         type variable       1       2        3       4       5       6 7
#> 1  Frequency     gear  0.0000  0.0000 15.00000 12.0000  5.0000 0.00000 0
#> 2  Frequency     carb  7.0000 10.0000  3.00000 10.0000  0.0000 1.00000 0
#> 3 Proportion     gear  0.0000  0.0000  0.46875  0.3750  0.1562 0.00000 0
#> 4 Proportion     carb  0.2188  0.3125  0.09375  0.3125  0.0000 0.03125 0
#> 5    Percent     gear  0.0000  0.0000 46.87500 37.5000 15.6250 0.00000 0
#> 6    Percent     carb 21.8750 31.2500  9.37500 31.2500  0.0000 3.12500 0
#>         8 miss responses
#> 1 0.00000    0        32
#> 2 1.00000    0        32
#> 3 0.00000    0        32
#> 4 0.03125    0        32
#> 5 0.00000    0        32
#> 6 3.12500    0        32
```
