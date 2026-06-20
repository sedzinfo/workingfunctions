# Compute the disattenuation correction for measurement error

Estimates the true correlation between two variables by correcting the
observed correlation for attenuation due to measurement error in both
variables.

## Usage

``` r
compute_dissatenuation(variable1, error1, variable2, error2)
```

## Arguments

- variable1:

  Numeric vector. True scores for the first variable.

- error1:

  Numeric vector. Measurement error for `variable1`. Must be the same
  length as `variable1`.

- variable2:

  Numeric vector. True scores for the second variable.

- error2:

  Numeric vector. Measurement error for `variable2`. Must be the same
  length as `variable2`.

## Value

A numeric scalar. The disattenuated (corrected) correlation between
`variable1` and `variable2`.

## Details

The observed correlation is computed from the error-contaminated scores
(`variable + error`). Reliability for each variable is estimated as the
ratio of true score variance to total observed variance. The
disattenuated correlation is then: \$\$\rho = \frac{r\_{obs}}{\sqrt{R_1
\cdot R_2}}\$\$ where \\R_1\\ and \\R_2\\ are the reliability estimates.

## Examples

``` r
set.seed(1)
compute_dissatenuation(rnorm(10), rnorm(10), rnorm(10), rnorm(10))
#> [1] 0.1306254
```
