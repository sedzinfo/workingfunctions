# Compute Cohen's D Effect Size

Computes Cohen's d effect size for a two-group comparison as abs(mean1 -
mean2) / sd_pooled, with sd_pooled = sqrt((sd1^2 + sd2^2) / 2). This is
the same formula used inline by report_ttests, extracted here as a
standalone, reusable function so it can be called on its own.

## Usage

``` r
compute_cohens_d(formula, data)
```

## Arguments

- formula:

  a formula of the form dv ~ group, same as used by stats::t.test's
  formula method. The grouping variable must have exactly two levels.
  Note: as with stats::t.test's formula method, paired tests are not
  supported here; use stats::t.test(x, y, paired = TRUE) directly for
  that case.

- data:

  data frame containing the variables in formula, same as
  stats::t.test's data argument.

## Value

A single numeric value, Cohen's d effect size.

## Note

effect size

- Very small 0.01 Sawilowsky (2009)

- Small 0.20 Cohen (1988)

- Medium 0.50 Cohen (1988)

- Large 0.80 Cohen (1988)

- Very large 1.20 Sawilowsky (2009)

- Huge 12.0 Sawilowsky (2009)

## Examples

``` r
compute_cohens_d(
  formula = bp_before ~ agegrp,
  data = df_blood_pressure[df_blood_pressure$agegrp %in% c("30-45", "46-59"), ]
)
#> [1] 0.3287838
effectsize::cohens_d(
  bp_before ~ agegrp,
  data = df_blood_pressure[df_blood_pressure$agegrp %in% c("30-45", "46-59"), ],
  pooled_sd = TRUE
)
#> Cohen's d |        95% CI
#> -------------------------
#> -0.33     | [-0.77, 0.11]
#> 
#> - Estimated using pooled SD.
```
