# Insurance Data

This data set contains information about insurance charges based on
various factors such as age, sex, BMI, number of children, smoking
status, and region.

## Usage

``` r
df_insurance
```

## Format

A data frame with 19 rows and 7 variables:

- age:

  Age of the individual

- sex:

  Sex of the individual (e.g., male, female)

- bmi:

  Body Mass Index of the individual

- children:

  Number of children covered by the insurance

- smoker:

  Smoking status (yes or no)

- region:

  Region where the individual resides (e.g., southwest, southeast,
  northwest, northeast)

- charges:

  Insurance charges

## Source

researchpy repo

## Examples

``` r
data(df_insurance)
head(df_insurance)
#>   age    sex    bmi children smoker    region   charges
#> 1  19 female 27.900        0    yes southwest 16884.924
#> 2  18   male 33.770        1     no southeast  1725.552
#> 3  28   male 33.000        3     no southeast  4449.462
#> 4  33   male 22.705        0     no northwest 21984.471
#> 5  32   male 28.880        0     no northwest  3866.855
#> 6  31 female 25.740        0     no southeast  3756.622
```
