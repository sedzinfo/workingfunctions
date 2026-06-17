# Crop Yield Data

This data set contains information about crop yields based on different
fertilizer types and water conditions.

## Usage

``` r
df_crop_yield
```

## Format

A data frame with 20 rows and 3 variables:

- Fert:

  Type of fertilizer used (A or B)

- Water:

  Watering condition (High or Low)

- Yield:

  Crop yield (in unspecified units)

## Source

researchpy repo (simulated data, not real)

## Examples

``` r
data(df_crop_yield)
head(df_crop_yield)
#>   Fert Water Yield
#> 1    A  High  27.4
#> 2    A  High  33.6
#> 3    A  High  29.8
#> 4    A  High  35.2
#> 5    A  High  33.0
#> 6    B  High  34.8
```
