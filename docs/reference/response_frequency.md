# Response frequency table for ordinal or Likert-scale variables

Tabulates how often each response category was chosen for one or more
ordinal variables (e.g. Likert scale items). For each variable the
function returns the count, proportion, or percentage of respondents who
selected each response option, along with the number of missing or
out-of-range responses. The function is a guard against accidental use
on continuous variables: if the number of unique values exceeds `max`
the function returns `NULL`.

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

  A data frame whose columns are the ordinal variables to tabulate.

- max:

  Integer. Maximum number of unique response options allowed before the
  function returns `NULL`. Use this to prevent accidentally tabulating
  continuous variables. Default is `10`.

- uniqueitems:

  Vector of all valid response values (e.g. `1:5` for a 5-point Likert
  scale). When `NULL` (default) the unique values observed in `df` are
  used.

- type:

  Character string controlling the metric returned. One of `"frequency"`
  (raw counts), `"proportion"` (counts divided by valid responses),
  `"percent"` (proportion multiplied by 100, default), or `"all"` (all
  three metrics stacked row-wise).

- file:

  Character string naming the output Excel file (without extension).
  When `NULL` (default) no file is written.

## Value

A data frame with one row per variable (three rows per variable when
`type = "all"`) containing the following columns:

- type:

  `"Frequency"`, `"Proportion"`, or `"Percent"`.

- variable:

  Name of the column from `df`.

- (response columns):

  One column per value in `uniqueitems`, named by the response value,
  containing the frequency, proportion, or percent of respondents who
  chose that category.

- miss:

  Observations with values outside `uniqueitems`. For proportions this
  is the missing rate; for percent it is the missing percentage.

- responses:

  Total number of valid (non-missing) responses.

Returns `NULL` if the number of unique values exceeds `max`.

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
#>         type variable       1       2        3       4       5       6 7       8 miss responses
#> 1  Frequency     gear  0.0000  0.0000 15.00000 12.0000  5.0000 0.00000 0 0.00000    0        32
#> 2  Frequency     carb  7.0000 10.0000  3.00000 10.0000  0.0000 1.00000 0 1.00000    0        32
#> 3 Proportion     gear  0.0000  0.0000  0.46875  0.3750  0.1562 0.00000 0 0.00000    0        32
#> 4 Proportion     carb  0.2188  0.3125  0.09375  0.3125  0.0000 0.03125 0 0.03125    0        32
#> 5    Percent     gear  0.0000  0.0000 46.87500 37.5000 15.6250 0.00000 0 0.00000    0        32
#> 6    Percent     carb 21.8750 31.2500  9.37500 31.2500  0.0000 3.12500 0 3.12500    0        32
response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="all",
                   file="descriptives")
#>         type variable       1       2        3       4       5       6 7       8 miss responses
#> 1  Frequency     gear  0.0000  0.0000 15.00000 12.0000  5.0000 0.00000 0 0.00000    0        32
#> 2  Frequency     carb  7.0000 10.0000  3.00000 10.0000  0.0000 1.00000 0 1.00000    0        32
#> 3 Proportion     gear  0.0000  0.0000  0.46875  0.3750  0.1562 0.00000 0 0.00000    0        32
#> 4 Proportion     carb  0.2188  0.3125  0.09375  0.3125  0.0000 0.03125 0 0.03125    0        32
#> 5    Percent     gear  0.0000  0.0000 46.87500 37.5000 15.6250 0.00000 0 0.00000    0        32
#> 6    Percent     carb 21.8750 31.2500  9.37500 31.2500  0.0000 3.12500 0 3.12500    0        32
```
