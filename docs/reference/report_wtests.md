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
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2))
#>    DV      IV level1 level2  n1  n2      W      p   CI_l  CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled      d        r k_squared[bartlett]
#> 1 sex charges female   male 662 676 221304 0.7287 -929.1 566.9   two.sided Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.1148 0.009486               15.59
#>   df[bartlett] p[bartlett] bonferroni_p significant
#> 1            1  0.00007887         0.05       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(4))
#>          DV      IV level1 level2  n1  n2     W          p     CI_l    CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1  children charges      0      1 574 324 87794 0.16411342 -1563.03   309.0   two.sided Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 2  children charges      0      3 574 157 35692 0.00006472 -4120.05 -1707.1   two.sided Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 3  children charges      0      2 574 240 57386 0.00017160 -3218.57 -1185.6   two.sided Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 4  children charges      0      5 574  18  5334 0.81466261 -2829.26  4259.1   two.sided Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 5  children charges      0      4 574  25  5666 0.07493225 -5743.00   332.3   two.sided Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 6  children charges      1      3 324 157 20460 0.00050250 -3335.25  -966.4   two.sided Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 7  children charges      1      2 324 240 33821 0.00820085 -2396.95  -345.3   two.sided Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 8  children charges      1      5 324  18  3008 0.82267385 -1886.64  3039.5   two.sided Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 9  children charges      1      4 324  25  3133 0.05935064 -5161.04   106.4   two.sided Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 10 children charges      3      2 157 240 17627 0.27809245 -2020.25   560.8   two.sided Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 11 children charges      3      5 157  18  1812 0.05030207    -1.26  6184.4   two.sided Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 12 children charges      3      4 157  25  1922 0.87012848 -3127.14  2805.0   two.sided Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 13 children charges      2      5 240  18  2549 0.20326617  -683.72  5203.4   two.sided Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 14 children charges      2      4 240  25  2745 0.48527112 -3793.25  1930.0   two.sided Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 15 children charges      5      4  18  25   307 0.04397189   190.18  6389.2   two.sided                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1               0.1153            1 0.7341451329     0.003333       FALSE
#> 2               0.1576            1 0.6913522564     0.003333        TRUE
#> 3               1.6673            1 0.1966253499     0.003333        TRUE
#> 4              23.1372            1 0.0000015084     0.003333       FALSE
#> 5               2.9046            1 0.0883267624     0.003333       FALSE
#> 6               0.3739            1 0.5409114605     0.003333        TRUE
#> 7               2.0650            1 0.1507142694     0.003333       FALSE
#> 8              22.4877            1 0.0000021149     0.003333       FALSE
#> 9               2.5282            1 0.1118298401     0.003333       FALSE
#> 10              0.3696            1 0.5432189483     0.003333       FALSE
#> 11             23.3776            1 0.0000013312     0.003333       FALSE
#> 12              3.1777            1 0.0746510253     0.003333       FALSE
#> 13             24.9594            1 0.0000005855     0.003333       FALSE
#> 14              4.2347            1 0.0396047045     0.003333       FALSE
#> 15             12.2513            1 0.0004649248     0.003333       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4))
#>          DV      IV level1 level2  n1  n2      W          p     CI_l    CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1       sex charges female   male 662 676 221304 0.72865109  -929.07   566.9   two.sided Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.11478 0.009486
#> 2  children charges      0      1 574 324  87794 0.16411342 -1563.03   309.0   two.sided Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 3  children charges      0      3 574 157  35692 0.00006472 -4120.05 -1707.1   two.sided Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 4  children charges      0      2 574 240  57386 0.00017160 -3218.57 -1185.6   two.sided Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 5  children charges      0      5 574  18   5334 0.81466261 -2829.26  4259.1   two.sided Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 6  children charges      0      4 574  25   5666 0.07493225 -5743.00   332.3   two.sided Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 7  children charges      1      3 324 157  20460 0.00050250 -3335.25  -966.4   two.sided Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 8  children charges      1      2 324 240  33821 0.00820085 -2396.95  -345.3   two.sided Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 9  children charges      1      5 324  18   3008 0.82267385 -1886.64  3039.5   two.sided Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 10 children charges      1      4 324  25   3133 0.05935064 -5161.04   106.4   two.sided Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 11 children charges      3      2 157 240  17627 0.27809245 -2020.25   560.8   two.sided Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 12 children charges      3      5 157  18   1812 0.05030207    -1.26  6184.4   two.sided Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 13 children charges      3      4 157  25   1922 0.87012848 -3127.14  2805.0   two.sided Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 14 children charges      2      5 240  18   2549 0.20326617  -683.72  5203.4   two.sided Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 15 children charges      2      4 240  25   2745 0.48527112 -3793.25  1930.0   two.sided Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 16 children charges      5      4  18  25    307 0.04397189   190.18  6389.2   two.sided                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1              15.5852            1 0.0000788678     0.003125       FALSE
#> 2               0.1153            1 0.7341451329     0.003125       FALSE
#> 3               0.1576            1 0.6913522564     0.003125        TRUE
#> 4               1.6673            1 0.1966253499     0.003125        TRUE
#> 5              23.1372            1 0.0000015084     0.003125       FALSE
#> 6               2.9046            1 0.0883267624     0.003125       FALSE
#> 7               0.3739            1 0.5409114605     0.003125        TRUE
#> 8               2.0650            1 0.1507142694     0.003125       FALSE
#> 9              22.4877            1 0.0000021149     0.003125       FALSE
#> 10              2.5282            1 0.1118298401     0.003125       FALSE
#> 11              0.3696            1 0.5432189483     0.003125       FALSE
#> 12             23.3776            1 0.0000013312     0.003125       FALSE
#> 13              3.1777            1 0.0746510253     0.003125       FALSE
#> 14             24.9594            1 0.0000005855     0.003125       FALSE
#> 15              4.2347            1 0.0396047045     0.003125       FALSE
#> 16             12.2513            1 0.0004649248     0.003125       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              alternative="two.sided")
#>          DV      IV level1 level2  n1  n2      W          p     CI_l    CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1       sex charges female   male 662 676 221304 0.72865109  -929.07   566.9   two.sided Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.11478 0.009486
#> 2  children charges      0      1 574 324  87794 0.16411342 -1563.03   309.0   two.sided Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 3  children charges      0      3 574 157  35692 0.00006472 -4120.05 -1707.1   two.sided Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 4  children charges      0      2 574 240  57386 0.00017160 -3218.57 -1185.6   two.sided Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 5  children charges      0      5 574  18   5334 0.81466261 -2829.26  4259.1   two.sided Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 6  children charges      0      4 574  25   5666 0.07493225 -5743.00   332.3   two.sided Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 7  children charges      1      3 324 157  20460 0.00050250 -3335.25  -966.4   two.sided Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 8  children charges      1      2 324 240  33821 0.00820085 -2396.95  -345.3   two.sided Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 9  children charges      1      5 324  18   3008 0.82267385 -1886.64  3039.5   two.sided Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 10 children charges      1      4 324  25   3133 0.05935064 -5161.04   106.4   two.sided Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 11 children charges      3      2 157 240  17627 0.27809245 -2020.25   560.8   two.sided Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 12 children charges      3      5 157  18   1812 0.05030207    -1.26  6184.4   two.sided Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 13 children charges      3      4 157  25   1922 0.87012848 -3127.14  2805.0   two.sided Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 14 children charges      2      5 240  18   2549 0.20326617  -683.72  5203.4   two.sided Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 15 children charges      2      4 240  25   2745 0.48527112 -3793.25  1930.0   two.sided Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 16 children charges      5      4  18  25    307 0.04397189   190.18  6389.2   two.sided                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1              15.5852            1 0.0000788678     0.003125       FALSE
#> 2               0.1153            1 0.7341451329     0.003125       FALSE
#> 3               0.1576            1 0.6913522564     0.003125        TRUE
#> 4               1.6673            1 0.1966253499     0.003125        TRUE
#> 5              23.1372            1 0.0000015084     0.003125       FALSE
#> 6               2.9046            1 0.0883267624     0.003125       FALSE
#> 7               0.3739            1 0.5409114605     0.003125        TRUE
#> 8               2.0650            1 0.1507142694     0.003125       FALSE
#> 9              22.4877            1 0.0000021149     0.003125       FALSE
#> 10              2.5282            1 0.1118298401     0.003125       FALSE
#> 11              0.3696            1 0.5432189483     0.003125       FALSE
#> 12             23.3776            1 0.0000013312     0.003125       FALSE
#> 13              3.1777            1 0.0746510253     0.003125       FALSE
#> 14             24.9594            1 0.0000005855     0.003125       FALSE
#> 15              4.2347            1 0.0396047045     0.003125       FALSE
#> 16             12.2513            1 0.0004649248     0.003125       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              alternative="less")
#>          DV      IV level1 level2  n1  n2      W          p CI_l    CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1       sex charges female   male 662 676 221304 0.36432554 -Inf   480.1        less Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.11478 0.009486
#> 2  children charges      0      1 574 324  87794 0.08205671 -Inf   141.8        less Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 3  children charges      0      3 574 157  35692 0.00003236 -Inf -1894.6        less Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 4  children charges      0      2 574 240  57386 0.00008580 -Inf -1366.7        less Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 5  children charges      0      5 574  18   5334 0.59321179 -Inf  3673.1        less Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 6  children charges      0      4 574  25   5666 0.03746613 -Inf  -237.3        less Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 7  children charges      1      3 324 157  20460 0.00025125 -Inf -1167.0        less Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 8  children charges      1      2 324 240  33821 0.00410042 -Inf  -504.4        less Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 9  children charges      1      5 324  18   3008 0.58961569 -Inf  2591.9        less Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 10 children charges      1      4 324  25   3133 0.02967532 -Inf  -388.9        less Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 11 children charges      3      2 157 240  17627 0.13904623 -Inf   386.0        less Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 12 children charges      3      5 157  18   1812 0.97513611 -Inf  5455.1        less Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 13 children charges      3      4 157  25   1922 0.43506424 -Inf  2187.4        less Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 14 children charges      2      5 240  18   2549 0.89894727 -Inf  4352.8        less Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 15 children charges      2      4 240  25   2745 0.24263556 -Inf  1465.9        less Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 16 children charges      5      4  18  25    307 0.97931829 -Inf  6118.6        less                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1              15.5852            1 0.0000788678     0.003125       FALSE
#> 2               0.1153            1 0.7341451329     0.003125       FALSE
#> 3               0.1576            1 0.6913522564     0.003125        TRUE
#> 4               1.6673            1 0.1966253499     0.003125        TRUE
#> 5              23.1372            1 0.0000015084     0.003125       FALSE
#> 6               2.9046            1 0.0883267624     0.003125       FALSE
#> 7               0.3739            1 0.5409114605     0.003125        TRUE
#> 8               2.0650            1 0.1507142694     0.003125       FALSE
#> 9              22.4877            1 0.0000021149     0.003125       FALSE
#> 10              2.5282            1 0.1118298401     0.003125       FALSE
#> 11              0.3696            1 0.5432189483     0.003125       FALSE
#> 12             23.3776            1 0.0000013312     0.003125       FALSE
#> 13              3.1777            1 0.0746510253     0.003125       FALSE
#> 14             24.9594            1 0.0000005855     0.003125       FALSE
#> 15              4.2347            1 0.0396047045     0.003125       FALSE
#> 16             12.2513            1 0.0004649248     0.003125       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              alternative="greater")
#>          DV      IV level1 level2  n1  n2      W       p    CI_l CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1       sex charges female   male 662 676 221304 0.63573  -791.8  Inf     greater Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.11478 0.009486
#> 2  children charges      0      1 574 324  87794 0.91798 -1412.4  Inf     greater Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 3  children charges      0      3 574 157  35692 0.99997 -3928.2  Inf     greater Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 4  children charges      0      2 574 240  57386 0.99991 -3053.8  Inf     greater Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 5  children charges      0      5 574  18   5334 0.40733 -2491.8  Inf     greater Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 6  children charges      0      4 574  25   5666 0.96263 -5306.0  Inf     greater Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 7  children charges      1      3 324 157  20460 0.99975 -3136.7  Inf     greater Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 8  children charges      1      2 324 240  33821 0.99591 -2229.0  Inf     greater Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 9  children charges      1      5 324  18   3008 0.41134 -1541.0  Inf     greater Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 10 children charges      1      4 324  25   3133 0.97046 -4687.5  Inf     greater Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 11 children charges      3      2 157 240  17627 0.86115 -1808.2  Inf     greater Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 12 children charges      3      5 157  18   1812 0.02515   410.6  Inf     greater Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 13 children charges      3      4 157  25   1922 0.56654 -2665.3  Inf     greater Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 14 children charges      2      5 240  18   2549 0.10163  -358.7  Inf     greater Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 15 children charges      2      4 240  25   2745 0.75822 -3293.3  Inf     greater Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 16 children charges      5      4  18  25    307 0.02199   793.8  Inf     greater                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1              15.5852            1 0.0000788678     0.003125       FALSE
#> 2               0.1153            1 0.7341451329     0.003125       FALSE
#> 3               0.1576            1 0.6913522564     0.003125       FALSE
#> 4               1.6673            1 0.1966253499     0.003125       FALSE
#> 5              23.1372            1 0.0000015084     0.003125       FALSE
#> 6               2.9046            1 0.0883267624     0.003125       FALSE
#> 7               0.3739            1 0.5409114605     0.003125       FALSE
#> 8               2.0650            1 0.1507142694     0.003125       FALSE
#> 9              22.4877            1 0.0000021149     0.003125       FALSE
#> 10              2.5282            1 0.1118298401     0.003125       FALSE
#> 11              0.3696            1 0.5432189483     0.003125       FALSE
#> 12             23.3776            1 0.0000013312     0.003125       FALSE
#> 13              3.1777            1 0.0746510253     0.003125       FALSE
#> 14             24.9594            1 0.0000005855     0.003125       FALSE
#> 15              4.2347            1 0.0396047045     0.003125       FALSE
#> 16             12.2513            1 0.0004649248     0.003125       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              var.equal=TRUE)
#>          DV      IV level1 level2  n1  n2      W          p     CI_l    CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1       sex charges female   male 662 676 221304 0.72865109  -929.07   566.9   two.sided Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.11478 0.009486
#> 2  children charges      0      1 574 324  87794 0.16411342 -1563.03   309.0   two.sided Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 3  children charges      0      3 574 157  35692 0.00006472 -4120.05 -1707.1   two.sided Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 4  children charges      0      2 574 240  57386 0.00017160 -3218.57 -1185.6   two.sided Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 5  children charges      0      5 574  18   5334 0.81466261 -2829.26  4259.1   two.sided Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 6  children charges      0      4 574  25   5666 0.07493225 -5743.00   332.3   two.sided Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 7  children charges      1      3 324 157  20460 0.00050250 -3335.25  -966.4   two.sided Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 8  children charges      1      2 324 240  33821 0.00820085 -2396.95  -345.3   two.sided Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 9  children charges      1      5 324  18   3008 0.82267385 -1886.64  3039.5   two.sided Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 10 children charges      1      4 324  25   3133 0.05935064 -5161.04   106.4   two.sided Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 11 children charges      3      2 157 240  17627 0.27809245 -2020.25   560.8   two.sided Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 12 children charges      3      5 157  18   1812 0.05030207    -1.26  6184.4   two.sided Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 13 children charges      3      4 157  25   1922 0.87012848 -3127.14  2805.0   two.sided Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 14 children charges      2      5 240  18   2549 0.20326617  -683.72  5203.4   two.sided Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 15 children charges      2      4 240  25   2745 0.48527112 -3793.25  1930.0   two.sided Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 16 children charges      5      4  18  25    307 0.04397189   190.18  6389.2   two.sided                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1              15.5852            1 0.0000788678     0.003125       FALSE
#> 2               0.1153            1 0.7341451329     0.003125       FALSE
#> 3               0.1576            1 0.6913522564     0.003125        TRUE
#> 4               1.6673            1 0.1966253499     0.003125        TRUE
#> 5              23.1372            1 0.0000015084     0.003125       FALSE
#> 6               2.9046            1 0.0883267624     0.003125       FALSE
#> 7               0.3739            1 0.5409114605     0.003125        TRUE
#> 8               2.0650            1 0.1507142694     0.003125       FALSE
#> 9              22.4877            1 0.0000021149     0.003125       FALSE
#> 10              2.5282            1 0.1118298401     0.003125       FALSE
#> 11              0.3696            1 0.5432189483     0.003125       FALSE
#> 12             23.3776            1 0.0000013312     0.003125       FALSE
#> 13              3.1777            1 0.0746510253     0.003125       FALSE
#> 14             24.9594            1 0.0000005855     0.003125       FALSE
#> 15              4.2347            1 0.0396047045     0.003125       FALSE
#> 16             12.2513            1 0.0004649248     0.003125       FALSE
report_wtests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              var.equal=TRUE,
              file="wilcoxontest")
#>          DV      IV level1 level2  n1  n2      W          p     CI_l    CI_u alternative                                            method mean1 mean2   sd1   sd2 sd_pooled       d        r
#> 1       sex charges female   male 662 676 221304 0.72865109  -929.07   566.9   two.sided Wilcoxon rank sum test with continuity correction 12570 13957 11129 12971     12085 0.11478 0.009486
#> 2  children charges      0      1 574 324  87794 0.16411342 -1563.03   309.0   two.sided Wilcoxon rank sum test with continuity correction 12366 12731 12023 11824     11924 0.03063 0.046435
#> 3  children charges      0      3 574 157  35692 0.00006472 -4120.05 -1707.1   two.sided Wilcoxon rank sum test with continuity correction 12366 15355 12023 12331     12178 0.24547 0.147765
#> 4  children charges      0      2 574 240  57386 0.00017160 -3218.57 -1185.6   two.sided Wilcoxon rank sum test with continuity correction 12366 15074 12023 12891     12465 0.21722 0.131707
#> 5  children charges      0      5 574  18   5334 0.81466261 -2829.26  4259.1   two.sided Wilcoxon rank sum test with continuity correction 12366  8786 12023  3808      8918 0.40143 0.009663
#> 6  children charges      0      4 574  25   5666 0.07493225 -5743.00   332.3   two.sided Wilcoxon rank sum test with continuity correction 12366 13851 12023  9139     10679 0.13903 0.072789
#> 7  children charges      1      3 324 157  20460 0.00050250 -3335.25  -966.4   two.sided Wilcoxon rank sum test with continuity correction 12731 15355 11824 12331     12080 0.21723 0.158664
#> 8  children charges      1      2 324 240  33821 0.00820085 -2396.95  -345.3   two.sided Wilcoxon rank sum test with continuity correction 12731 15074 11824 12891     12369 0.18938 0.111330
#> 9  children charges      1      5 324  18   3008 0.82267385 -1886.64  3039.5   two.sided Wilcoxon rank sum test with continuity correction 12731  8786 11824  3808      8784 0.44915 0.012185
#> 10 children charges      1      4 324  25   3133 0.05935064 -5161.04   106.4   two.sided Wilcoxon rank sum test with continuity correction 12731 13851 11824  9139     10567 0.10594 0.100988
#> 11 children charges      3      2 157 240  17627 0.27809245 -2020.25   560.8   two.sided Wilcoxon rank sum test with continuity correction 15355 15074 12331 12891     12614 0.02234 0.054458
#> 12 children charges      3      5 157  18   1812 0.05030207    -1.26  6184.4   two.sided Wilcoxon rank sum test with continuity correction 15355  8786 12331  3808      9126 0.71987 0.148150
#> 13 children charges      3      4 157  25   1922 0.87012848 -3127.14  2805.0   two.sided Wilcoxon rank sum test with continuity correction 15355 13851 12331  9139     10853 0.13864 0.012271
#> 14 children charges      2      5 240  18   2549 0.20326617  -683.72  5203.4   two.sided Wilcoxon rank sum test with continuity correction 15074  8786 12891  3808      9505 0.66149 0.079312
#> 15 children charges      2      4 240  25   2745 0.48527112 -3793.25  1930.0   two.sided Wilcoxon rank sum test with continuity correction 15074 13851 12891  9139     11174 0.10944 0.042953
#> 16 children charges      5      4  18  25    307 0.04397189   190.18  6389.2   two.sided                      Wilcoxon rank sum exact test  8786 13851  3808  9139      7001 0.72341 0.307849
#>    k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p significant
#> 1              15.5852            1 0.0000788678     0.003125       FALSE
#> 2               0.1153            1 0.7341451329     0.003125       FALSE
#> 3               0.1576            1 0.6913522564     0.003125        TRUE
#> 4               1.6673            1 0.1966253499     0.003125        TRUE
#> 5              23.1372            1 0.0000015084     0.003125       FALSE
#> 6               2.9046            1 0.0883267624     0.003125       FALSE
#> 7               0.3739            1 0.5409114605     0.003125        TRUE
#> 8               2.0650            1 0.1507142694     0.003125       FALSE
#> 9              22.4877            1 0.0000021149     0.003125       FALSE
#> 10              2.5282            1 0.1118298401     0.003125       FALSE
#> 11              0.3696            1 0.5432189483     0.003125       FALSE
#> 12             23.3776            1 0.0000013312     0.003125       FALSE
#> 13              3.1777            1 0.0746510253     0.003125       FALSE
#> 14             24.9594            1 0.0000005855     0.003125       FALSE
#> 15              4.2347            1 0.0396047045     0.003125       FALSE
#> 16             12.2513            1 0.0004649248     0.003125       FALSE
```
