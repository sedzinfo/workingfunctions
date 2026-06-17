# Difficile Data

This data set contains information about the impact of different doses
on libido.

## Usage

``` r
df_difficile
```

## Format

A data frame with 15 rows and 3 variables:

- person:

  Unique identifier for each person

- dose:

  Dose received (e.g., 1, 2, 3)

- libido:

  Libido level of the person

## Source

researchpy repo

## Examples

``` r
data(df_difficile)
head(df_difficile)
#>   person dose libido
#> 1      1    1      3
#> 2      2    1      2
#> 3      3    1      1
#> 4      4    1      1
#> 5      5    1      4
#> 6      6    2      5
```
