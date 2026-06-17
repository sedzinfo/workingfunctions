# Responses State Data

This data set contains simulated state information paired with
participant numbers from the responses data set.

## Usage

``` r
df_responses_state
```

## Format

A data frame with 28 rows and 2 variables:

- Participant Number:

  Unique identifier for each participant

- State:

  State code where the participant resides (e.g., MI, OH, CO, CA, MA,
  WA)

## Source

researchpy repo (simulated data, not real)

## Examples

``` r
data(df_responses_state)
head(df_responses_state)
#>   Participant Number State
#> 1                  1    MI
#> 2                  2    OH
#> 3                  3    CO
#> 4                  4    CA
#> 5                  5    MA
#> 6                  6    WA
```
