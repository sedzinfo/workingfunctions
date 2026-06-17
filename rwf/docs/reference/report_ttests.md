# Run Pairwise t-tests and Return a Reporting Table

Performs t-tests for each selected dependent variable against each
selected independent variable, across all pairwise level combinations of
the independent variable. Also computes descriptive statistics, effect
sizes, Bartlett homogeneity test results, and Bonferroni adjustment.

In simple terms: this function creates a full t-test report table you
can export or use in downstream summaries.

## Usage

``` r
report_ttests(df, dv, iv, file = NULL, ...)
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
  [`stats::t.test`](https://rdrr.io/r/stats/t.test.html)

  `x`

  :   a (non-empty) numeric vector of data values.

  `y`

  :   an optional (non-empty) numeric vector of data values.

  `alternative`

  :   a character string specifying the alternative hypothesis, must be
      one of `"two.sided"` (default), `"greater"` or `"less"`. You can
      specify just the initial letter.

  `mu`

  :   a number indicating the true value of the mean (or difference in
      means if you are performing a two sample test).

  `paired`

  :   a logical indicating whether you want a paired t-test.

  `var.equal`

  :   a logical variable indicating whether to treat the two variances
      as being equal. If `TRUE` then the pooled variance is used to
      estimate the variance otherwise the Welch (or Satterthwaite)
      approximation to the degrees of freedom is used.

  `conf.level`

  :   confidence level of the interval.

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

A data frame where each row is one pairwise group comparison for one
dependent-independent variable combination. Returned columns mean:

- DV: Name stored in the DV column by the current implementation. Note:
  this currently contains the independent variable name.

- IV: Name stored in the IV column by the current implementation. Note:
  this currently contains the dependent variable name.

- level1: First group level being compared.

- level2: Second group level being compared.

- n1: Sample size in level1.

- n2: Sample size in level2.

- t: t statistic from t.test.

- df: Degrees of freedom for the t statistic.

- p: p-value from t.test.

- CI_l: Lower confidence interval bound for the mean difference.

- CI_u: Upper confidence interval bound for the mean difference.

- alternative: Alternative hypothesis used by t.test.

- method: Test label from t.test (for example Welch Two Sample t-test).

- mean1: Mean of the dependent variable in level1.

- mean2: Mean of the dependent variable in level2.

- sd1: Standard deviation in level1.

- sd2: Standard deviation in level2.

- sd_pooled: Pooled standard deviation, sqrt((sd1^2 + sd2^2) / 2).

- d: Cohen d effect size, abs(mean2 - mean1) / sd_pooled.

- r: Effect-size r derived from d using the function formula.

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

The function also calls report_dataframe to generate a formatted report.

## Examples

``` r
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2))
#>    DV      IV level1 level2  n1  n2      t   df       p  CI_l   CI_u
#> 1 sex charges female   male 662 676 -2.101 1313 0.03584 -2682 -91.86
#>   alternative                  method mean1 mean2   sd1   sd2 sd_pooled      d
#> 1   two.sided Welch Two Sample t-test 12570 13957 11129 12971     12085 0.1148
#>         r k_squared[bartlett] df[bartlett] p[bartlett] bonferroni_p significant
#> 1 0.02789               15.59            1  0.00007887         0.05        TRUE
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(4))
#>          DV      IV level1 level2  n1  n2       t     df           p  CI_l
#> 1  children charges      0      1 574 324 -0.4418 679.59 0.658783431 -1988
#> 2  children charges      0      3 574 157 -2.7061 243.21 0.007290553 -5165
#> 3  children charges      0      2 574 240 -2.7863 421.22 0.005572108 -4618
#> 4  children charges      0      5 574  18  3.4810  29.20 0.001591976  1477
#> 5  children charges      0      4 574  25 -0.7833  27.75 0.440101809 -5369
#> 6  children charges      1      3 324 157 -2.2178 297.45 0.027320507 -4953
#> 7  children charges      1      2 324 240 -2.2095 489.12 0.027603430 -4425
#> 8  children charges      1      5 324  18  3.5468  39.48 0.001022814  1696
#> 9  children charges      1      4 324  25 -0.5764  30.56 0.568583875 -5083
#> 10 children charges      3      2 157 240 -0.2186 344.03 0.827073078 -2817
#> 11 children charges      3      5 157  18  4.9318  71.21 0.000005148  3913
#> 12 children charges      3      4 157  25  0.7248  39.42 0.472847187 -2693
#> 13 children charges      2      5 240  18  5.1368  55.84 0.000003684  3835
#> 14 children charges      2      4 240  25  0.6089  34.83 0.546534224 -2855
#> 15 children charges      5      4  18  25  2.4871  34.17 0.017921653   927
#>      CI_u alternative                  method mean1 mean2   sd1   sd2 sd_pooled
#> 1  1257.9   two.sided Welch Two Sample t-test 12366 12731 12023 11824     11924
#> 2  -813.4   two.sided Welch Two Sample t-test 12366 15355 12023 12331     12178
#> 3  -797.5   two.sided Welch Two Sample t-test 12366 15074 12023 12891     12465
#> 4  5682.6   two.sided Welch Two Sample t-test 12366  8786 12023  3808      8918
#> 5  2399.6   two.sided Welch Two Sample t-test 12366 13851 12023  9139     10679
#> 6  -295.7   two.sided Welch Two Sample t-test 12731 15355 11824 12331     12080
#> 7  -259.4   two.sided Welch Two Sample t-test 12731 15074 11824 12891     12369
#> 8  6194.1   two.sided Welch Two Sample t-test 12731  8786 11824  3808      8784
#> 9  2844.1   two.sided Welch Two Sample t-test 12731 13851 11824  9139     10567
#> 10 2253.1   two.sided Welch Two Sample t-test 15355 15074 12331 12891     12614
#> 11 9225.1   two.sided Welch Two Sample t-test 15355  8786 12331  3808      9126
#> 12 5702.2   two.sided Welch Two Sample t-test 15355 13851 12331  9139     10853
#> 13 8739.7   two.sided Welch Two Sample t-test 15074  8786 12891  3808      9505
#> 14 5300.8   two.sided Welch Two Sample t-test 15074 13851 12891  9139     11174
#> 15 9202.3   two.sided Welch Two Sample t-test  8786 13851  3808  9139      7001
#>          d        r k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p
#> 1  0.03063 0.007014              0.1153            1 0.7341451329     0.003333
#> 2  0.24547 0.039752              0.1576            1 0.6913522564     0.003333
#> 3  0.21722 0.043210              1.6673            1 0.1966253499     0.003333
#> 4  0.40143 0.011696             23.1372            1 0.0000015084     0.003333
#> 5  0.13903 0.005530              2.9046            1 0.0883267624     0.003333
#> 6  0.21723 0.045584              0.3739            1 0.5409114605     0.003333
#> 7  0.18938 0.044245              2.0650            1 0.1507142694     0.003333
#> 8  0.44915 0.021905             22.4877            1 0.0000021149     0.003333
#> 9  0.10594 0.006996              2.5282            1 0.1118298401     0.003333
#> 10 0.02234 0.005312              0.3696            1 0.5432189483     0.003333
#> 11 0.71987 0.062290             23.3776            1 0.0000013312     0.003333
#> 12 0.13864 0.016163              3.1777            1 0.0746510253     0.003333
#> 13 0.66149 0.041164             24.9594            1 0.0000005855     0.003333
#> 14 0.10944 0.009264              4.2347            1 0.0396047045     0.003333
#> 15 0.72341 0.149703             12.2513            1 0.0004649248     0.003333
#>    significant
#> 1        FALSE
#> 2        FALSE
#> 3        FALSE
#> 4         TRUE
#> 5        FALSE
#> 6        FALSE
#> 7        FALSE
#> 8         TRUE
#> 9        FALSE
#> 10       FALSE
#> 11        TRUE
#> 12       FALSE
#> 13        TRUE
#> 14       FALSE
#> 15       FALSE
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4))
#>          DV      IV level1 level2  n1  n2       t      df           p  CI_l
#> 1       sex charges female   male 662 676 -2.1009 1313.36 0.035841015 -2682
#> 2  children charges      0      1 574 324 -0.4418  679.59 0.658783431 -1988
#> 3  children charges      0      3 574 157 -2.7061  243.21 0.007290553 -5165
#> 4  children charges      0      2 574 240 -2.7863  421.22 0.005572108 -4618
#> 5  children charges      0      5 574  18  3.4810   29.20 0.001591976  1477
#> 6  children charges      0      4 574  25 -0.7833   27.75 0.440101809 -5369
#> 7  children charges      1      3 324 157 -2.2178  297.45 0.027320507 -4953
#> 8  children charges      1      2 324 240 -2.2095  489.12 0.027603430 -4425
#> 9  children charges      1      5 324  18  3.5468   39.48 0.001022814  1696
#> 10 children charges      1      4 324  25 -0.5764   30.56 0.568583875 -5083
#> 11 children charges      3      2 157 240 -0.2186  344.03 0.827073078 -2817
#> 12 children charges      3      5 157  18  4.9318   71.21 0.000005148  3913
#> 13 children charges      3      4 157  25  0.7248   39.42 0.472847187 -2693
#> 14 children charges      2      5 240  18  5.1368   55.84 0.000003684  3835
#> 15 children charges      2      4 240  25  0.6089   34.83 0.546534224 -2855
#> 16 children charges      5      4  18  25  2.4871   34.17 0.017921653   927
#>       CI_u alternative                  method mean1 mean2   sd1   sd2
#> 1   -91.86   two.sided Welch Two Sample t-test 12570 13957 11129 12971
#> 2  1257.87   two.sided Welch Two Sample t-test 12366 12731 12023 11824
#> 3  -813.38   two.sided Welch Two Sample t-test 12366 15355 12023 12331
#> 4  -797.51   two.sided Welch Two Sample t-test 12366 15074 12023 12891
#> 5  5682.65   two.sided Welch Two Sample t-test 12366  8786 12023  3808
#> 6  2399.63   two.sided Welch Two Sample t-test 12366 13851 12023  9139
#> 7  -295.65   two.sided Welch Two Sample t-test 12731 15355 11824 12331
#> 8  -259.38   two.sided Welch Two Sample t-test 12731 15074 11824 12891
#> 9  6194.14   two.sided Welch Two Sample t-test 12731  8786 11824  3808
#> 10 2844.15   two.sided Welch Two Sample t-test 12731 13851 11824  9139
#> 11 2253.10   two.sided Welch Two Sample t-test 15355 15074 12331 12891
#> 12 9225.11   two.sided Welch Two Sample t-test 15355  8786 12331  3808
#> 13 5702.20   two.sided Welch Two Sample t-test 15355 13851 12331  9139
#> 14 8739.70   two.sided Welch Two Sample t-test 15074  8786 12891  3808
#> 15 5300.79   two.sided Welch Two Sample t-test 15074 13851 12891  9139
#> 16 9202.28   two.sided Welch Two Sample t-test  8786 13851  3808  9139
#>    sd_pooled       d        r k_squared[bartlett] df[bartlett]  p[bartlett]
#> 1      12085 0.11478 0.027893             15.5852            1 0.0000788678
#> 2      11924 0.03063 0.007014              0.1153            1 0.7341451329
#> 3      12178 0.24547 0.039752              0.1576            1 0.6913522564
#> 4      12465 0.21722 0.043210              1.6673            1 0.1966253499
#> 5       8918 0.40143 0.011696             23.1372            1 0.0000015084
#> 6      10679 0.13903 0.005530              2.9046            1 0.0883267624
#> 7      12080 0.21723 0.045584              0.3739            1 0.5409114605
#> 8      12369 0.18938 0.044245              2.0650            1 0.1507142694
#> 9       8784 0.44915 0.021905             22.4877            1 0.0000021149
#> 10     10567 0.10594 0.006996              2.5282            1 0.1118298401
#> 11     12614 0.02234 0.005312              0.3696            1 0.5432189483
#> 12      9126 0.71987 0.062290             23.3776            1 0.0000013312
#> 13     10853 0.13864 0.016163              3.1777            1 0.0746510253
#> 14      9505 0.66149 0.041164             24.9594            1 0.0000005855
#> 15     11174 0.10944 0.009264              4.2347            1 0.0396047045
#> 16      7001 0.72341 0.149703             12.2513            1 0.0004649248
#>    bonferroni_p significant
#> 1      0.003125       FALSE
#> 2      0.003125       FALSE
#> 3      0.003125       FALSE
#> 4      0.003125       FALSE
#> 5      0.003125        TRUE
#> 6      0.003125       FALSE
#> 7      0.003125       FALSE
#> 8      0.003125       FALSE
#> 9      0.003125        TRUE
#> 10     0.003125       FALSE
#> 11     0.003125       FALSE
#> 12     0.003125        TRUE
#> 13     0.003125       FALSE
#> 14     0.003125        TRUE
#> 15     0.003125       FALSE
#> 16     0.003125       FALSE
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              alternative="two.sided")
#>          DV      IV level1 level2  n1  n2       t      df           p  CI_l
#> 1       sex charges female   male 662 676 -2.1009 1313.36 0.035841015 -2682
#> 2  children charges      0      1 574 324 -0.4418  679.59 0.658783431 -1988
#> 3  children charges      0      3 574 157 -2.7061  243.21 0.007290553 -5165
#> 4  children charges      0      2 574 240 -2.7863  421.22 0.005572108 -4618
#> 5  children charges      0      5 574  18  3.4810   29.20 0.001591976  1477
#> 6  children charges      0      4 574  25 -0.7833   27.75 0.440101809 -5369
#> 7  children charges      1      3 324 157 -2.2178  297.45 0.027320507 -4953
#> 8  children charges      1      2 324 240 -2.2095  489.12 0.027603430 -4425
#> 9  children charges      1      5 324  18  3.5468   39.48 0.001022814  1696
#> 10 children charges      1      4 324  25 -0.5764   30.56 0.568583875 -5083
#> 11 children charges      3      2 157 240 -0.2186  344.03 0.827073078 -2817
#> 12 children charges      3      5 157  18  4.9318   71.21 0.000005148  3913
#> 13 children charges      3      4 157  25  0.7248   39.42 0.472847187 -2693
#> 14 children charges      2      5 240  18  5.1368   55.84 0.000003684  3835
#> 15 children charges      2      4 240  25  0.6089   34.83 0.546534224 -2855
#> 16 children charges      5      4  18  25  2.4871   34.17 0.017921653   927
#>       CI_u alternative                  method mean1 mean2   sd1   sd2
#> 1   -91.86   two.sided Welch Two Sample t-test 12570 13957 11129 12971
#> 2  1257.87   two.sided Welch Two Sample t-test 12366 12731 12023 11824
#> 3  -813.38   two.sided Welch Two Sample t-test 12366 15355 12023 12331
#> 4  -797.51   two.sided Welch Two Sample t-test 12366 15074 12023 12891
#> 5  5682.65   two.sided Welch Two Sample t-test 12366  8786 12023  3808
#> 6  2399.63   two.sided Welch Two Sample t-test 12366 13851 12023  9139
#> 7  -295.65   two.sided Welch Two Sample t-test 12731 15355 11824 12331
#> 8  -259.38   two.sided Welch Two Sample t-test 12731 15074 11824 12891
#> 9  6194.14   two.sided Welch Two Sample t-test 12731  8786 11824  3808
#> 10 2844.15   two.sided Welch Two Sample t-test 12731 13851 11824  9139
#> 11 2253.10   two.sided Welch Two Sample t-test 15355 15074 12331 12891
#> 12 9225.11   two.sided Welch Two Sample t-test 15355  8786 12331  3808
#> 13 5702.20   two.sided Welch Two Sample t-test 15355 13851 12331  9139
#> 14 8739.70   two.sided Welch Two Sample t-test 15074  8786 12891  3808
#> 15 5300.79   two.sided Welch Two Sample t-test 15074 13851 12891  9139
#> 16 9202.28   two.sided Welch Two Sample t-test  8786 13851  3808  9139
#>    sd_pooled       d        r k_squared[bartlett] df[bartlett]  p[bartlett]
#> 1      12085 0.11478 0.027893             15.5852            1 0.0000788678
#> 2      11924 0.03063 0.007014              0.1153            1 0.7341451329
#> 3      12178 0.24547 0.039752              0.1576            1 0.6913522564
#> 4      12465 0.21722 0.043210              1.6673            1 0.1966253499
#> 5       8918 0.40143 0.011696             23.1372            1 0.0000015084
#> 6      10679 0.13903 0.005530              2.9046            1 0.0883267624
#> 7      12080 0.21723 0.045584              0.3739            1 0.5409114605
#> 8      12369 0.18938 0.044245              2.0650            1 0.1507142694
#> 9       8784 0.44915 0.021905             22.4877            1 0.0000021149
#> 10     10567 0.10594 0.006996              2.5282            1 0.1118298401
#> 11     12614 0.02234 0.005312              0.3696            1 0.5432189483
#> 12      9126 0.71987 0.062290             23.3776            1 0.0000013312
#> 13     10853 0.13864 0.016163              3.1777            1 0.0746510253
#> 14      9505 0.66149 0.041164             24.9594            1 0.0000005855
#> 15     11174 0.10944 0.009264              4.2347            1 0.0396047045
#> 16      7001 0.72341 0.149703             12.2513            1 0.0004649248
#>    bonferroni_p significant
#> 1      0.003125       FALSE
#> 2      0.003125       FALSE
#> 3      0.003125       FALSE
#> 4      0.003125       FALSE
#> 5      0.003125        TRUE
#> 6      0.003125       FALSE
#> 7      0.003125       FALSE
#> 8      0.003125       FALSE
#> 9      0.003125        TRUE
#> 10     0.003125       FALSE
#> 11     0.003125       FALSE
#> 12     0.003125        TRUE
#> 13     0.003125       FALSE
#> 14     0.003125        TRUE
#> 15     0.003125       FALSE
#> 16     0.003125       FALSE
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              alternative="less")
#>          DV      IV level1 level2  n1  n2       t      df        p CI_l    CI_u
#> 1       sex charges female   male 662 676 -2.1009 1313.36 0.017921 -Inf  -300.3
#> 2  children charges      0      1 574 324 -0.4418  679.59 0.329392 -Inf   996.4
#> 3  children charges      0      3 574 157 -2.7061  243.21 0.003645 -Inf -1165.4
#> 4  children charges      0      2 574 240 -2.7863  421.22 0.002786 -Inf -1105.7
#> 5  children charges      0      5 574  18  3.4810   29.20 0.999204 -Inf  5326.9
#> 6  children charges      0      4 574  25 -0.7833   27.75 0.220051 -Inf  1740.8
#> 7  children charges      1      3 324 157 -2.2178  297.45 0.013660 -Inf  -671.9
#> 8  children charges      1      2 324 240 -2.2095  489.12 0.013802 -Inf  -595.3
#> 9  children charges      1      5 324  18  3.5468   39.48 0.999489 -Inf  5818.7
#> 10 children charges      1      4 324  25 -0.5764   30.56 0.284292 -Inf  2175.2
#> 11 children charges      3      2 157 240 -0.2186  344.03 0.413537 -Inf  1843.8
#> 12 children charges      3      5 157  18  4.9318   71.21 0.999997 -Inf  8789.1
#> 13 children charges      3      4 157  25  0.7248   39.42 0.763576 -Inf  5001.4
#> 14 children charges      2      5 240  18  5.1368   55.84 0.999998 -Inf  8334.8
#> 15 children charges      2      4 240  25  0.6089   34.83 0.726733 -Inf  4616.6
#> 16 children charges      5      4  18  25  2.4871   34.17 0.991039 -Inf  8507.5
#>    alternative                  method mean1 mean2   sd1   sd2 sd_pooled
#> 1         less Welch Two Sample t-test 12570 13957 11129 12971     12085
#> 2         less Welch Two Sample t-test 12366 12731 12023 11824     11924
#> 3         less Welch Two Sample t-test 12366 15355 12023 12331     12178
#> 4         less Welch Two Sample t-test 12366 15074 12023 12891     12465
#> 5         less Welch Two Sample t-test 12366  8786 12023  3808      8918
#> 6         less Welch Two Sample t-test 12366 13851 12023  9139     10679
#> 7         less Welch Two Sample t-test 12731 15355 11824 12331     12080
#> 8         less Welch Two Sample t-test 12731 15074 11824 12891     12369
#> 9         less Welch Two Sample t-test 12731  8786 11824  3808      8784
#> 10        less Welch Two Sample t-test 12731 13851 11824  9139     10567
#> 11        less Welch Two Sample t-test 15355 15074 12331 12891     12614
#> 12        less Welch Two Sample t-test 15355  8786 12331  3808      9126
#> 13        less Welch Two Sample t-test 15355 13851 12331  9139     10853
#> 14        less Welch Two Sample t-test 15074  8786 12891  3808      9505
#> 15        less Welch Two Sample t-test 15074 13851 12891  9139     11174
#> 16        less Welch Two Sample t-test  8786 13851  3808  9139      7001
#>          d        r k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p
#> 1  0.11478 0.027893             15.5852            1 0.0000788678     0.003125
#> 2  0.03063 0.007014              0.1153            1 0.7341451329     0.003125
#> 3  0.24547 0.039752              0.1576            1 0.6913522564     0.003125
#> 4  0.21722 0.043210              1.6673            1 0.1966253499     0.003125
#> 5  0.40143 0.011696             23.1372            1 0.0000015084     0.003125
#> 6  0.13903 0.005530              2.9046            1 0.0883267624     0.003125
#> 7  0.21723 0.045584              0.3739            1 0.5409114605     0.003125
#> 8  0.18938 0.044245              2.0650            1 0.1507142694     0.003125
#> 9  0.44915 0.021905             22.4877            1 0.0000021149     0.003125
#> 10 0.10594 0.006996              2.5282            1 0.1118298401     0.003125
#> 11 0.02234 0.005312              0.3696            1 0.5432189483     0.003125
#> 12 0.71987 0.062290             23.3776            1 0.0000013312     0.003125
#> 13 0.13864 0.016163              3.1777            1 0.0746510253     0.003125
#> 14 0.66149 0.041164             24.9594            1 0.0000005855     0.003125
#> 15 0.10944 0.009264              4.2347            1 0.0396047045     0.003125
#> 16 0.72341 0.149703             12.2513            1 0.0004649248     0.003125
#>    significant
#> 1        FALSE
#> 2        FALSE
#> 3        FALSE
#> 4         TRUE
#> 5        FALSE
#> 6        FALSE
#> 7        FALSE
#> 8        FALSE
#> 9        FALSE
#> 10       FALSE
#> 11       FALSE
#> 12       FALSE
#> 13       FALSE
#> 14       FALSE
#> 15       FALSE
#> 16       FALSE
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              alternative="greater")
#>          DV      IV level1 level2  n1  n2       t      df           p  CI_l
#> 1       sex charges female   male 662 676 -2.1009 1313.36 0.982079493 -2474
#> 2  children charges      0      1 574 324 -0.4418  679.59 0.670608284 -1727
#> 3  children charges      0      3 574 157 -2.7061  243.21 0.996354723 -4813
#> 4  children charges      0      2 574 240 -2.7863  421.22 0.997213946 -4309
#> 5  children charges      0      5 574  18  3.4810   29.20 0.000795988  1833
#> 6  children charges      0      4 574  25 -0.7833   27.75 0.779949096 -4710
#> 7  children charges      1      3 324 157 -2.2178  297.45 0.986339747 -4576
#> 8  children charges      1      2 324 240 -2.2095  489.12 0.986198285 -4089
#> 9  children charges      1      5 324  18  3.5468   39.48 0.000511407  2072
#> 10 children charges      1      4 324  25 -0.5764   30.56 0.715708063 -4414
#> 11 children charges      3      2 157 240 -0.2186  344.03 0.586463461 -2407
#> 12 children charges      3      5 157  18  4.9318   71.21 0.000002574  4349
#> 13 children charges      3      4 157  25  0.7248   39.42 0.236423594 -1992
#> 14 children charges      2      5 240  18  5.1368   55.84 0.000001842  4240
#> 15 children charges      2      4 240  25  0.6089   34.83 0.273267112 -2171
#> 16 children charges      5      4  18  25  2.4871   34.17 0.008960826  1622
#>    CI_u alternative                  method mean1 mean2   sd1   sd2 sd_pooled
#> 1   Inf     greater Welch Two Sample t-test 12570 13957 11129 12971     12085
#> 2   Inf     greater Welch Two Sample t-test 12366 12731 12023 11824     11924
#> 3   Inf     greater Welch Two Sample t-test 12366 15355 12023 12331     12178
#> 4   Inf     greater Welch Two Sample t-test 12366 15074 12023 12891     12465
#> 5   Inf     greater Welch Two Sample t-test 12366  8786 12023  3808      8918
#> 6   Inf     greater Welch Two Sample t-test 12366 13851 12023  9139     10679
#> 7   Inf     greater Welch Two Sample t-test 12731 15355 11824 12331     12080
#> 8   Inf     greater Welch Two Sample t-test 12731 15074 11824 12891     12369
#> 9   Inf     greater Welch Two Sample t-test 12731  8786 11824  3808      8784
#> 10  Inf     greater Welch Two Sample t-test 12731 13851 11824  9139     10567
#> 11  Inf     greater Welch Two Sample t-test 15355 15074 12331 12891     12614
#> 12  Inf     greater Welch Two Sample t-test 15355  8786 12331  3808      9126
#> 13  Inf     greater Welch Two Sample t-test 15355 13851 12331  9139     10853
#> 14  Inf     greater Welch Two Sample t-test 15074  8786 12891  3808      9505
#> 15  Inf     greater Welch Two Sample t-test 15074 13851 12891  9139     11174
#> 16  Inf     greater Welch Two Sample t-test  8786 13851  3808  9139      7001
#>          d        r k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p
#> 1  0.11478 0.027893             15.5852            1 0.0000788678     0.003125
#> 2  0.03063 0.007014              0.1153            1 0.7341451329     0.003125
#> 3  0.24547 0.039752              0.1576            1 0.6913522564     0.003125
#> 4  0.21722 0.043210              1.6673            1 0.1966253499     0.003125
#> 5  0.40143 0.011696             23.1372            1 0.0000015084     0.003125
#> 6  0.13903 0.005530              2.9046            1 0.0883267624     0.003125
#> 7  0.21723 0.045584              0.3739            1 0.5409114605     0.003125
#> 8  0.18938 0.044245              2.0650            1 0.1507142694     0.003125
#> 9  0.44915 0.021905             22.4877            1 0.0000021149     0.003125
#> 10 0.10594 0.006996              2.5282            1 0.1118298401     0.003125
#> 11 0.02234 0.005312              0.3696            1 0.5432189483     0.003125
#> 12 0.71987 0.062290             23.3776            1 0.0000013312     0.003125
#> 13 0.13864 0.016163              3.1777            1 0.0746510253     0.003125
#> 14 0.66149 0.041164             24.9594            1 0.0000005855     0.003125
#> 15 0.10944 0.009264              4.2347            1 0.0396047045     0.003125
#> 16 0.72341 0.149703             12.2513            1 0.0004649248     0.003125
#>    significant
#> 1        FALSE
#> 2        FALSE
#> 3        FALSE
#> 4        FALSE
#> 5         TRUE
#> 6        FALSE
#> 7        FALSE
#> 8        FALSE
#> 9         TRUE
#> 10       FALSE
#> 11       FALSE
#> 12        TRUE
#> 13       FALSE
#> 14        TRUE
#> 15       FALSE
#> 16       FALSE
report_ttests(df=df_insurance,
              dv=which("charges"==names(df_insurance)),
              iv=c(2,4),
              var.equal=TRUE)
#>          DV      IV level1 level2  n1  n2       t   df        p    CI_l
#> 1       sex charges female   male 662 676 -2.0975 1336 0.036133 -2684.5
#> 2  children charges      0      1 574 324 -0.4397  896 0.660238 -1995.1
#> 3  children charges      0      3 574 157 -2.7454  729 0.006193 -5127.0
#> 4  children charges      0      2 574 240 -2.8672  812 0.004249 -4561.2
#> 5  children charges      0      5 574  18  1.2603  590 0.208046 -1998.7
#> 6  children charges      0      4 574  25 -0.6096  597 0.542364 -6267.9
#> 7  children charges      1      3 324 157 -2.2505  479 0.024871 -4915.3
#> 8  children charges      1      2 324 240 -2.2381  562 0.025605 -4398.1
#> 9  children charges      1      5 324  18  1.4098  340 0.159508 -1559.1
#> 10 children charges      1      4 324  25 -0.4626  347 0.643924 -5878.9
#> 11 children charges      3      2 157 240 -0.2166  395 0.828634 -2839.2
#> 12 children charges      3      5 157  18  2.2429  173 0.026174   788.2
#> 13 children charges      3      4 157  25  0.5845  180 0.559614 -3575.0
#> 14 children charges      2      5 240  18  2.0592  256 0.040490   274.5
#> 15 children charges      2      4 240  25  0.4620  263 0.644469 -3989.2
#> 16 children charges      5      4  18  25  2.2111   41 0.032665   438.7
#>        CI_u alternative             method mean1 mean2   sd1   sd2 sd_pooled
#> 1    -89.81   two.sided  Two Sample t-test 12570 13957 11129 12971     12085
#> 2   1264.76   two.sided  Two Sample t-test 12366 12731 12023 11824     11924
#> 3   -851.67   two.sided  Two Sample t-test 12366 15355 12023 12331     12178
#> 4   -853.94   two.sided  Two Sample t-test 12366 15074 12023 12891     12465
#> 5   9158.59   two.sided  Two Sample t-test 12366  8786 12023  3808      8918
#> 6   3298.57   two.sided  Two Sample t-test 12366 13851 12023  9139     10679
#> 7   -332.97   two.sided  Two Sample t-test 12731 15355 11824 12331     12080
#> 8   -286.67   two.sided  Two Sample t-test 12731 15074 11824 12891     12369
#> 9   9449.36   two.sided  Two Sample t-test 12731  8786 11824  3808      8784
#> 10  3639.95   two.sided  Two Sample t-test 12731 13851 11824  9139     10567
#> 11  2275.65   two.sided  Two Sample t-test 15355 15074 12331 12891     12614
#> 12 12350.35   two.sided  Two Sample t-test 15355  8786 12331  3808      9126
#> 13  6584.28   two.sided  Two Sample t-test 15355 13851 12331  9139     10853
#> 14 12300.59   two.sided  Two Sample t-test 15074  8786 12891  3808      9505
#> 15  6434.98   two.sided  Two Sample t-test 15074 13851 12891  9139     11174
#> 16  9690.50   two.sided  Two Sample t-test  8786 13851  3808  9139      7001
#>          d        r k_squared[bartlett] df[bartlett]  p[bartlett] bonferroni_p
#> 1  0.11478 0.027893             15.5852            1 0.0000788678     0.003125
#> 2  0.03063 0.007014              0.1153            1 0.7341451329     0.003125
#> 3  0.24547 0.039752              0.1576            1 0.6913522564     0.003125
#> 4  0.21722 0.043210              1.6673            1 0.1966253499     0.003125
#> 5  0.40143 0.011696             23.1372            1 0.0000015084     0.003125
#> 6  0.13903 0.005530              2.9046            1 0.0883267624     0.003125
#> 7  0.21723 0.045584              0.3739            1 0.5409114605     0.003125
#> 8  0.18938 0.044245              2.0650            1 0.1507142694     0.003125
#> 9  0.44915 0.021905             22.4877            1 0.0000021149     0.003125
#> 10 0.10594 0.006996              2.5282            1 0.1118298401     0.003125
#> 11 0.02234 0.005312              0.3696            1 0.5432189483     0.003125
#> 12 0.71987 0.062290             23.3776            1 0.0000013312     0.003125
#> 13 0.13864 0.016163              3.1777            1 0.0746510253     0.003125
#> 14 0.66149 0.041164             24.9594            1 0.0000005855     0.003125
#> 15 0.10944 0.009264              4.2347            1 0.0396047045     0.003125
#> 16 0.72341 0.149703             12.2513            1 0.0004649248     0.003125
#>    significant
#> 1        FALSE
#> 2        FALSE
#> 3        FALSE
#> 4        FALSE
#> 5        FALSE
#> 6        FALSE
#> 7        FALSE
#> 8        FALSE
#> 9        FALSE
#> 10       FALSE
#> 11       FALSE
#> 12       FALSE
#> 13       FALSE
#> 14       FALSE
#> 15       FALSE
#> 16       FALSE
report_ttests(df=mtcars,dv=1:7,iv=8:10,var.equal=TRUE,file="ttest")
#>      DV   IV level1 level2 n1 n2        t df             p       CI_l      CI_u
#> 1    vs  mpg      0      1 18 14 -4.86438 30 0.00003415937  -11.27422  -4.60673
#> 2    am  mpg      1      0 13 19 -4.10613 30 0.00028502074  -10.84837  -3.64151
#> 3  gear  mpg      4      3 12 15 -5.04255 25 0.00003338684  -11.86839  -4.98495
#> 4  gear  mpg      4      5 12  5  1.04328 15 0.31334396688   -3.28904   9.59571
#> 5  gear  mpg      3      5 15  5 -2.36175 18 0.02966104593   -9.96429  -0.58237
#> 6    vs  cyl      0      1 18 14  7.58747 30 0.00000001843    2.09970   3.64633
#> 7    am  cyl      1      0 13 19  3.35741 30 0.00215120692    0.73267   3.00822
#> 8  gear  cyl      4      3 12 15  6.55610 25 0.00000072200    1.92041   3.67959
#> 9  gear  cyl      4      5 12  5 -1.87867 15 0.07986469403   -2.84607   0.17940
#> 10 gear  cyl      3      5 15  5  2.01580 18 0.05899922316   -0.06194   2.99527
#> 11   vs disp      0      1 18 14  5.52885 30 0.00000523501  110.16403 239.22168
#> 12   am disp      1      0 13 19  4.01521 30 0.00036621137   72.15611 221.54025
#> 13 gear disp      4      3 12 15  6.94942 25 0.00000027675  143.03801 263.52865
#> 14 gear disp      4      5 12  5 -2.18523 15 0.04514920473 -156.97099  -1.95568
#> 15 gear disp      3      5 15  5  2.40237 18 0.02729393754   15.53671 232.10329
#> 16   vs   hp      0      1 18 14  5.73374 30 0.00000294090   63.32892 133.40124
#> 17   am   hp      1      0 13 19  1.37332 30 0.17983090461  -16.27768  83.11169
#> 18 gear   hp      4      3 12 15  5.64787 25 0.00000704584   55.04184 118.22483
#> 19 gear   hp      4      5 12  5 -3.46375 15 0.00347245364 -171.38955 -40.81045
#> 20 gear   hp      3      5 15  5 -0.58738 18 0.56424376103  -89.09423  50.16090
#> 21   vs drat      0      1 18 14 -2.68583 30 0.01167552965   -0.82221  -0.11191
#> 22   am drat      1      0 13 19 -5.56510 30 0.00000472679   -1.04394  -0.48343
#> 23 gear drat      4      3 12 15 -8.07077 25 0.00000002000   -1.14305  -0.67828
#> 24 gear drat      4      5 12  5  0.71472 15 0.48575919011   -0.25240   0.50707
#> 25 gear drat      3      5 15  5 -5.00202 18 0.00009243680   -1.11234  -0.45432
#> 26   vs   wt      0      1 18 14  3.65353 30 0.00097984923    0.47509   1.67945
#> 27   am   wt      1      0 13 19  5.25760 30 0.00001125440    0.83043   1.88536
#> 28 gear   wt      4      3 12 15  4.38407 25 0.00018390182    0.67653   1.87534
#> 29 gear   wt      4      5 12  5 -0.04355 15 0.96583525049   -0.79570   0.76383
#> 30 gear   wt      3      5 15  5  2.94013 18 0.00875175983    0.35965   2.16035
#> 31   vs qsec      0      1 18 14 -6.10859 30 0.00000102967   -3.52220  -1.75716
#> 32   am qsec      1      0 13 19  1.29364 30 0.20566214810   -0.47636   2.12268
#> 33 gear qsec      4      3 12 15 -2.23307 25 0.03473297180   -2.44707  -0.09893
#> 34 gear qsec      4      5 12  5  4.16361 15 0.00083189820    1.62285   5.02715
#> 35 gear qsec      3      5 15  5  3.04649 18 0.00694544960    0.63690   3.46710
#>    alternative             method   mean1   mean2      sd1      sd2 sd_pooled
#> 1    two.sided  Two Sample t-test  16.617  24.557   3.8607   5.3790    4.6818
#> 2    two.sided  Two Sample t-test  24.392  17.147   6.1665   3.8340    5.1344
#> 3    two.sided  Two Sample t-test  24.533  16.107   5.2768   3.3716    4.4279
#> 4    two.sided  Two Sample t-test  24.533  21.380   5.2768   6.6590    6.0078
#> 5    two.sided  Two Sample t-test  16.107  21.380   3.3716   6.6590    5.2778
#> 6    two.sided  Two Sample t-test   7.444   4.571   1.1490   0.9376    1.0487
#> 7    two.sided  Two Sample t-test   5.077   6.947   1.5525   1.5447    1.5486
#> 8    two.sided  Two Sample t-test   4.667   7.467   0.9847   1.1872    1.0907
#> 9    two.sided  Two Sample t-test   4.667   6.000   0.9847   2.0000    1.5763
#> 10   two.sided  Two Sample t-test   7.467   6.000   1.1872   2.0000    1.6446
#> 11   two.sided  Two Sample t-test 307.150 132.457 106.7652  56.8932   85.5443
#> 12   two.sided  Two Sample t-test 143.531 290.379  87.2040 110.1716   99.3537
#> 13   two.sided  Two Sample t-test 123.017 326.300  38.9093  94.8527   72.4947
#> 14   two.sided  Two Sample t-test 123.017 202.480  38.9093 115.4906   86.1743
#> 15   two.sided  Two Sample t-test 326.300 202.480  94.8527 115.4906  105.6767
#> 16   two.sided  Two Sample t-test 189.722  91.357  60.2815  24.4245   45.9914
#> 17   two.sided  Two Sample t-test 126.846 160.263  84.0623  53.9082   70.6136
#> 18   two.sided  Two Sample t-test  89.500 176.133  25.8931  47.6893   38.3714
#> 19   two.sided  Two Sample t-test  89.500 195.600  25.8931 102.8338   74.9842
#> 20   two.sided  Two Sample t-test 176.133 195.600  47.6893 102.8338   80.1532
#> 21   two.sided  Two Sample t-test   3.392   3.859   0.4740   0.5058    0.4901
#> 22   two.sided  Two Sample t-test   4.050   3.286   0.3641   0.3923    0.3784
#> 23   two.sided  Two Sample t-test   4.043   3.133   0.3124   0.2737    0.2937
#> 24   two.sided  Two Sample t-test   4.043   3.916   0.3124   0.3895    0.3531
#> 25   two.sided  Two Sample t-test   3.133   3.916   0.2737   0.3895    0.3366
#> 26   two.sided  Two Sample t-test   3.689   2.611   0.9040   0.7150    0.8150
#> 27   two.sided  Two Sample t-test   2.411   3.769   0.6170   0.7774    0.7018
#> 28   two.sided  Two Sample t-test   2.617   3.893   0.6327   0.8330    0.7396
#> 29   two.sided  Two Sample t-test   2.617   2.633   0.6327   0.8189    0.7317
#> 30   two.sided  Two Sample t-test   3.893   2.633   0.8330   0.8189    0.8260
#> 31   two.sided  Two Sample t-test  16.694  19.334   1.0919   1.3544    1.2302
#> 32   two.sided  Two Sample t-test  17.360  18.183   1.7924   1.7513    1.7720
#> 33   two.sided  Two Sample t-test  18.965  17.692   1.6139   1.3499    1.4878
#> 34   two.sided  Two Sample t-test  18.965  15.640   1.6139   1.1305    1.3933
#> 35   two.sided  Two Sample t-test  17.692  15.640   1.3499   1.1305    1.2450
#>          d       r k_squared[bartlett] df[bartlett] p[bartlett] bonferroni_p
#> 1  1.69603 0.29447           1.5848659            1   0.2080612     0.001429
#> 2  1.41105 0.25393           3.2258823            1   0.0724827     0.001429
#> 3  1.90310 0.31968           2.3814678            1   0.1227821     0.001429
#> 4  0.52488 0.09826           0.3107865            1   0.5771980     0.001429
#> 5  0.99916 0.15778           3.2245045            1   0.0725437     0.001429
#> 6  2.73970 0.40271           0.5747405            1   0.4483813     0.001429
#> 7  1.20784 0.22561           0.0003571            1   0.9849238     0.001429
#> 8  2.56718 0.38796           0.4055912            1   0.5242158     0.001429
#> 9  0.84584 0.14938           3.1370062            1   0.0765342     0.001429
#> 10 0.89180 0.14326           1.8299382            1   0.1761348     0.001429
#> 11 2.04213 0.33447           5.0492717            1   0.0246363     0.001429
#> 12 1.47803 0.26282           0.7310694            1   0.3925368     0.001429
#> 13 2.80411 0.40911           7.8911261            1   0.0049678     0.001429
#> 14 0.92212 0.16068           7.4981393            1   0.0061763     0.001429
#> 15 1.17169 0.18012           0.2374158            1   0.6260788     0.001429
#> 16 2.13877 0.34484           9.6677208            1   0.0018753     0.001429
#> 17 0.47324 0.10245           2.8208425            1   0.0930476     0.001429
#> 18 2.25776 0.35793           3.9864395            1   0.0458679     0.001429
#> 19 1.41497 0.22706          11.8428281            1   0.0005788     0.001429
#> 20 0.24287 0.04355           4.1732858            1   0.0410661     0.001429
#> 21 0.95294 0.18996           0.0605277            1   0.8056638     0.001429
#> 22 2.01797 0.32740           0.0768731            1   0.7815805     0.001429
#> 23 3.10102 0.43365           0.2089536            1   0.6475888     0.001429
#> 24 0.36065 0.06966           0.2787561            1   0.5975177     0.001429
#> 25 2.32707 0.30378           0.8016316            1   0.3706060     0.001429
#> 26 1.32177 0.24544           0.7611752            1   0.3829610     0.001429
#> 27 1.93490 0.31821           0.7148260            1   0.3978463     0.001429
#> 28 1.72506 0.29871           0.8659508            1   0.3520787     0.001429
#> 29 0.02177 0.00450           0.3847597            1   0.5350669     0.001429
#> 30 1.52544 0.22241           0.0016476            1   0.9676222     0.001429
#> 31 2.14577 0.34558           0.6690022            1   0.4133997     0.001429
#> 32 0.46455 0.10076           0.0074902            1   0.9310324     0.001429
#> 33 0.85565 0.17442           0.3810023            1   0.5370673     0.001429
#> 34 2.38641 0.33130           0.6031648            1   0.4373731     0.001429
#> 35 1.64813 0.23607           0.1680530            1   0.6818480     0.001429
#>    significant
#> 1         TRUE
#> 2         TRUE
#> 3         TRUE
#> 4        FALSE
#> 5        FALSE
#> 6         TRUE
#> 7        FALSE
#> 8         TRUE
#> 9        FALSE
#> 10       FALSE
#> 11        TRUE
#> 12        TRUE
#> 13        TRUE
#> 14       FALSE
#> 15       FALSE
#> 16        TRUE
#> 17       FALSE
#> 18        TRUE
#> 19       FALSE
#> 20       FALSE
#> 21       FALSE
#> 22        TRUE
#> 23        TRUE
#> 24       FALSE
#> 25        TRUE
#> 26        TRUE
#> 27        TRUE
#> 28        TRUE
#> 29       FALSE
#> 30       FALSE
#> 31        TRUE
#> 32       FALSE
#> 33       FALSE
#> 34        TRUE
#> 35       FALSE
```
