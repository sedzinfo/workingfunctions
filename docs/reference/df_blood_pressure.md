# Blood Pressure Data

This data set contains blood pressure readings for patients before and
after a certain treatment or intervention.

## Usage

``` r
df_blood_pressure
```

## Format

A data frame with 30 rows and 5 variables:

- patient:

  Unique identifier for each patient

- sex:

  Sex of the patient (e.g., Male, Female)

- agegrp:

  Age group of the patient (e.g., 30-45, 46-59)

- bp_before:

  Blood pressure reading before the intervention

- bp_after:

  Blood pressure reading after the intervention

## Source

researchpy repo

## Examples

``` r
data(df_blood_pressure)
head(df_blood_pressure)
#>   patient  sex agegrp bp_before bp_after
#> 1       1 Male  30-45       143      153
#> 2       2 Male  30-45       163      170
#> 3       3 Male  30-45       153      168
#> 4       4 Male  30-45       153      142
#> 5       5 Male  30-45       146      141
#> 6       6 Male  30-45       150      147
```
