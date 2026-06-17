# Admission Data

This data set contains information about graduate admission, including
GRE scores, GPA, and the ranking of the undergraduate institution.

## Usage

``` r
df_admission
```

## Format

A data frame with 8 rows and 4 variables:

- admit:

  Binary variable indicating admission (0=No, 1=Yes)

- gre:

  GRE (Graduate Record Examination) score

- gpa:

  Grade Point Average

- rank:

  Ranking of the undergraduate institution (1=highest, 4=lowest)

## Source

researchpy repo

## Examples

``` r
data(df_admission)
head(df_admission)
#>   admit gre  gpa rank
#> 1     0 380 3.61    3
#> 2     1 660 3.67    3
#> 3     1 800 4.00    1
#> 4     1 640 3.19    4
#> 5     0 520 2.93    4
#> 6     1 760 3.00    2
```
