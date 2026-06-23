# Run Pairwise Wilcoxon Tests and Return a Reporting Table

Performs Wilcoxon rank-sum tests for each selected dependent variable
against each selected independent variable, across all pairwise level
combinations of the independent variable. Also computes descriptive
statistics, effect sizes, Bartlett homogeneity results, and Bonferroni
adjustment.

In simple terms: this function builds a full nonparametric comparison
table, similar to report_ttests, but using wilcox.test for group
differences.

## Usage

``` r
report_wtests(df, dv, iv, file = NULL, ...)
```

## Arguments

- df:

  A data frame containing both the independent and dependent variables.

- dv:

  Integer vector of column indices for the continuous dependent
  variables.

- iv:

  Integer vector of column indices for the categorical independent
  variables.

- file:

  output filename

- ...:

  Arguments passed on to
  [`stats::wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html)

  `x`

  :   numeric vector of data values. Non-finite (e.g., infinite or
      missing) values will be omitted.

  `y`

  :   an optional numeric vector of data values: as with `x` non-finite
      values will be omitted.

  `alternative`

  :   a character string specifying the alternative hypothesis, must be
      one of `"two.sided"` (default), `"greater"` or `"less"`. You can
      specify just the initial letter.

  `mu`

  :   a number specifying an optional parameter used to form the null
      hypothesis. See ‘Details’.

  `paired`

  :   a logical indicating whether you want a paired test.

  `exact`

  :   a logical indicating whether an exact p-value should be computed.

  `correct`

  :   a logical indicating whether to apply continuity correction in the
      normal approximation for the p-value, or an integer \\k\\ between
      0 and 3 giving the number of correction terms to use from the
      Edgeworth series for the normal approximation.

  `conf.int`

  :   a logical indicating whether a confidence interval should be
      computed.

  `conf.level`

  :   confidence level of the interval.

  `tol.root`

  :   (when `conf.int` is true:) a positive numeric tolerance, used in
      [`uniroot`](https://rdrr.io/r/stats/uniroot.html)`(*, tol=tol.root)`
      calls.

  `digits.rank`

  :   a number; if finite,
      [`rank`](https://rdrr.io/r/base/rank.html)`(`[`signif`](https://rdrr.io/r/base/Round.html)`(r, digits.rank))`
      will be used to compute ranks for the test statistic instead of
      (the default) `rank(r)`.

  `formula`

  :   a formula of the form `lhs ~ rhs` where `lhs` is a numeric
      variable giving the data values and `rhs` either `1` for a
      one-sample or paired test or a factor with two levels giving the
      corresponding groups. If `lhs` is of class
      `"`[`Pair`](https://rdrr.io/r/stats/Pair.html)`"` and `rhs` is
      `1`, a paired test is done, see Examples.

  `data`

  :   an optional matrix or data frame (or similar: see
      [`model.frame`](https://rdrr.io/r/stats/model.frame.html))
      containing the variables in the formula `formula`. By default the
      variables are taken from `environment(formula)`.

  `subset`

  :   an optional vector specifying a subset of observations to be used.

  `na.action`

  :   a function which indicates what should happen when the data
      contain [`NA`](https://rdrr.io/r/base/NA.html)s.

## Value

A data frame where each row is one pairwise level comparison for one
dependent-independent variable combination. Returned columns mean:

- DV: Name stored in the DV column by the current implementation. Note:
  this currently contains the independent variable name.

- IV: Name stored in the IV column by the current implementation. Note:
  this currently contains the dependent variable name.

- level1: First group level being compared.

- level2: Second group level being compared.

- n1: Sample size in level1.

- n2: Sample size in level2.

- W: Wilcoxon test statistic from wilcox.test.

- p: p-value from wilcox.test.

- CI_l: Lower confidence interval bound from wilcox.test.

- CI_u: Upper confidence interval bound from wilcox.test.

- alternative: Alternative hypothesis used by wilcox.test.

- method: Test label from wilcox.test.

- mean1: Mean of the dependent variable in level1.

- mean2: Mean of the dependent variable in level2.

- sd1: Standard deviation in level1.

- sd2: Standard deviation in level2.

- sd_pooled: Pooled standard deviation, sqrt((sd1^2 + sd2^2) / 2).

- d: Cohen d effect size, abs(mean2 - mean1) / sd_pooled.

- r: Wilcoxon effect size from rstatix::wilcox_effsize.

- k_squared\[bartlett\]: Bartlett test statistic for equal variances.

- df\[bartlett\]: Degrees of freedom of Bartlett test.

- p\[bartlett\]: p-value of Bartlett test. Small values suggest
  heteroscedasticity.

- bonferroni_p: Bonferroni-adjusted alpha threshold computed for the
  number of tests in the output table.

- significant: Logical-like character flag (TRUE/FALSE) indicating
  whether p is below bonferroni_p.

## Details

Missing values are removed per analysis pair using complete cases on the
current dependent and independent variables.

For each independent variable, all pairwise level combinations are
tested using utils::combn.

The function calls stats::wilcox.test with conf.int = TRUE and forwards
additional arguments through ....

The function also calls report_dataframe to generate a formatted report.

## Examples

``` r
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2)
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(4)
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2, 4)
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2, 4),
  alternative = "two.sided"
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2, 4),
  alternative = "less"
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2, 4),
  alternative = "greater"
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2, 4),
  var.equal = TRUE
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
report_wtests(
  df = df_insurance,
  dv = which("charges" == names(df_insurance)),
  iv = c(2, 4),
  var.equal = TRUE,
  file = "wilcoxontest"
)
#> Error in required_package("coin"): coin package needed to be installed before using this function. Type this in R: install.packages('coin')
```
