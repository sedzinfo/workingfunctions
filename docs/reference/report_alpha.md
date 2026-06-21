# Cronbach's alpha reliability report for multiple scales

Computes Cronbach's alpha and a comprehensive set of item-level
reliability statistics for one or more scales using
[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html). Supports
item reversal, bootstrap confidence intervals, and optional Excel
export. Scales are classified by their alpha value (Unacceptable \<
0.60, Acceptable 0.60–0.70, Good and Acceptable 0.70–0.80, Good
0.80–0.90, Excellent \> 0.90).

## Usage

``` r
report_alpha(
  df,
  key = NULL,
  questions = NULL,
  reverse = NULL,
  mini = NULL,
  maxi = NULL,
  file = NULL,
  ...
)
```

## Arguments

- df:

  A data frame containing all item columns.

- key:

  A named list where each name is a scale label and each element is a
  character vector of item column names belonging to that scale, e.g.
  `list(f1 = c("X1","X2","X3"))`. When `NULL` (default) all columns in
  `df` are treated as a single scale named `"dimension"`.

- questions:

  A named list (same structure as `key`) of question label strings to
  append to item names in the output tables. When `NULL` (default) only
  column names are used.

- reverse:

  A named list (same structure as `key`) of numeric sign vectors (`1` =
  keep, `-1` = reverse) passed to
  [`psych::reverse.code()`](https://rdrr.io/pkg/psych/man/reverse.code.html).
  When `NULL` (default) no reversal is applied.

- mini:

  Numeric scalar specifying the minimum possible scale rating used for
  item reversal. When `NULL` (default) the empirical minimum is used.

- maxi:

  Numeric scalar specifying the maximum possible scale rating used for
  item reversal. When `NULL` (default) the empirical maximum is used.

- file:

  Character string naming the output Excel file (without extension).
  When `NULL` (default) no file is written. The workbook contains four
  sheets: total statistics, bootstrap CIs (if requested), item
  statistics, and alpha-if-item-removed.

- ...:

  Additional arguments passed to
  [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html), such as
  `cumulative`, `n.iter` (bootstrap iterations), or `check.keys`.

## Value

A named list with the following elements:

- result_total:

  Scale-level statistics including raw alpha, standardised alpha,
  Guttman's Lambda 6, average inter-item correlation, signal-to-noise
  ratio, mean, SD, Kaiser criterion, and alpha classification.

- result_boot:

  Bootstrap confidence intervals for alpha (only populated when
  `n.iter > 1` is passed via `...`).

- result_item_statistics:

  Item-level statistics including corrected and uncorrected item-total
  correlations, item mean and SD, and response frequencies.

- result_dropped:

  Alpha-if-item-removed statistics for each item within each scale.

## Examples

``` r
set.seed(12345)
df <- data.frame(matrix(.5, ncol = 6, nrow = 6))
correlation_martix <- as.matrix(df)
diag(correlation_martix) <- 1
df <- round(generate_correlation_matrix(correlation_martix, nrows = 1000), 0) + 5
key <- list(
  f1 = c("X1", "X2", "X3"),
  f2 = c("X4", "X5", "X6")
)
reverse <- list(
  f1 = c(1, 1, 1),
  f2 = c(1, 1, 1)
)
report_alpha(df = df, key = key, cumulative = TRUE, n.iter = 1)
#>   |                                                                                                                                                                                                      |                                                                                                                                                                                              |   0%  |                                                                                                                                                                                                      |==============================================================================================================================================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n     ase  mean    sd median_r     alpha_criterion
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632 0.01506 15.07 2.520   0.4513 Good and Acceptable
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440 0.01592 14.97 2.439   0.4452 Good and Acceptable
#> 
#> $result_boot
#> data frame with 0 columns and 0 rows
#> 
#> $result_item_statistics
#>   dimension question raw_alpha    n  raw_r  std_r  r_cor r_drop  mean    sd     1     2     3     4     5     6     7     8     9 miss
#> 1        f1       X1    0.7248 1000 0.7837 0.7887 0.6078 0.5192 5.014 1.024 0.001 0.002 0.067 0.225 0.387 0.257 0.057 0.004 0.000    0
#> 2        f1       X2    0.7248 1000 0.8117 0.8108 0.6596 0.5606 5.035 1.049 0.000 0.010 0.053 0.234 0.372 0.262 0.065 0.003 0.001    0
#> 3        f1       X3    0.7248 1000 0.8136 0.8097 0.6570 0.5583 5.025 1.065 0.000 0.004 0.066 0.229 0.393 0.231 0.064 0.013 0.000    0
#> 4        f2       X4    0.7094 1000 0.7820 0.7833 0.5985 0.5053 4.988 1.017    NA 0.006 0.053 0.254 0.389 0.236 0.056 0.006    NA    0
#> 5        f2       X5    0.7094 1000 0.7983 0.7966 0.6300 0.5297 5.018 1.030    NA 0.006 0.052 0.246 0.388 0.235 0.068 0.005    NA    0
#> 6        f2       X6    0.7094 1000 0.8053 0.8058 0.6504 0.5467 4.961 1.020    NA 0.008 0.060 0.243 0.403 0.231 0.048 0.007    NA    0
#> 
#> $result_dropped
#>   dimension question scale_alpha raw_alpha std_alpha g6(smc) average_r   s/n alpha se var_r  med_r
#> 1        f1       X1      0.7248    0.6683    0.6684  0.5019    0.5019 2.015  0.02097    NA 0.5019
#> 2        f1       X2      0.7248    0.6190    0.6194  0.4486    0.4486 1.627  0.02407    NA 0.4486
#> 3        f1       X3      0.7248    0.6218    0.6220  0.4513    0.4513 1.645  0.02391    NA 0.4513
#> 4        f2       X4      0.7094    0.6460    0.6460  0.4771    0.4771 1.825  0.02239    NA 0.4771
#> 5        f2       X5      0.7094    0.6161    0.6161  0.4452    0.4452 1.605  0.02428    NA 0.4452
#> 6        f2       X6      0.7094    0.5949    0.5950  0.4234    0.4234 1.469  0.02562    NA 0.4234
#> 
report_alpha(df = df, key = key, reverse = reverse, check.keys = FALSE, n.iter = 2)
#>   |                                                                                                                                                                                                      |                                                                                                                                                                                              |   0%  |                                                                                                                                                                                                      |==============================================================================================================================================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n     ase  mean     sd median_r boot_ci_2_5% boot_ci_50% boot_ci_97_5%     alpha_criterion
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632 0.01506 5.025 0.8402   0.4513       0.7272      0.7288        0.7303 Good and Acceptable
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440 0.01592 4.989 0.8131   0.4452       0.7177      0.7209        0.7240 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n    ase unidim goodfit     var_r median_r
#> 1        f1     3                1    0.7304    0.7304  0.6448    0.4746 2.710 0.2178 0.8090  0.9979 0.0008785   0.4812
#> 2        f1     3                1    0.7272    0.7271  0.6400    0.4704 2.665 0.2233 0.8055  0.9997 0.0001128   0.4644
#> 3        f2     3                1    0.7175    0.7181  0.6310    0.4591 2.547 0.2399 0.7933  0.9970 0.0013056   0.4504
#> 4        f2     3                1    0.7242    0.7245  0.6380    0.4672 2.630 0.2283 0.8016  0.9977 0.0009626   0.4502
#> 
#> $result_item_statistics
#>   dimension question raw_alpha    n  raw_r  std_r  r_cor r_drop  mean    sd     1     2     3     4     5     6     7     8     9 miss
#> 1        f1       X1    0.7248 1000 0.7837 0.7887 0.6078 0.5192 5.014 1.024 0.001 0.002 0.067 0.225 0.387 0.257 0.057 0.004 0.000    0
#> 2        f1       X2    0.7248 1000 0.8117 0.8108 0.6596 0.5606 5.035 1.049 0.000 0.010 0.053 0.234 0.372 0.262 0.065 0.003 0.001    0
#> 3        f1       X3    0.7248 1000 0.8136 0.8097 0.6570 0.5583 5.025 1.065 0.000 0.004 0.066 0.229 0.393 0.231 0.064 0.013 0.000    0
#> 4        f2       X4    0.7094 1000 0.7820 0.7833 0.5985 0.5053 4.988 1.017    NA 0.006 0.053 0.254 0.389 0.236 0.056 0.006    NA    0
#> 5        f2       X5    0.7094 1000 0.7983 0.7966 0.6300 0.5297 5.018 1.030    NA 0.006 0.052 0.246 0.388 0.235 0.068 0.005    NA    0
#> 6        f2       X6    0.7094 1000 0.8053 0.8058 0.6504 0.5467 4.961 1.020    NA 0.008 0.060 0.243 0.403 0.231 0.048 0.007    NA    0
#> 
#> $result_dropped
#>   dimension question scale_alpha raw_alpha std_alpha g6(smc) average_r   s/n alpha se var_r  med_r
#> 1        f1       X1      0.7248    0.6683    0.6684  0.5019    0.5019 2.015  0.02097    NA 0.5019
#> 2        f1       X2      0.7248    0.6190    0.6194  0.4486    0.4486 1.627  0.02407    NA 0.4486
#> 3        f1       X3      0.7248    0.6218    0.6220  0.4513    0.4513 1.645  0.02391    NA 0.4513
#> 4        f2       X4      0.7094    0.6460    0.6460  0.4771    0.4771 1.825  0.02239    NA 0.4771
#> 5        f2       X5      0.7094    0.6161    0.6161  0.4452    0.4452 1.605  0.02428    NA 0.4452
#> 6        f2       X6      0.7094    0.5949    0.5950  0.4234    0.4234 1.469  0.02562    NA 0.4234
#> 
report_alpha(df = df, key = key, check.keys = FALSE, n.iter = 2, file = "alpha")
#>   |                                                                                                                                                                                                      |                                                                                                                                                                                              |   0%  |                                                                                                                                                                                                      |==============================================================================================================================================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n     ase  mean     sd median_r boot_ci_2_5% boot_ci_50% boot_ci_97_5%     alpha_criterion
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632 0.01506 5.025 0.8402   0.4513       0.7251      0.7270        0.7289 Good and Acceptable
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440 0.01592 4.989 0.8131   0.4452       0.7111      0.7131        0.7152 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n    ase unidim goodfit     var_r median_r
#> 1        f1     3                1    0.7290    0.7289  0.6421    0.4727 2.689 0.2204 0.8077  0.9997 0.0001303   0.4724
#> 2        f1     3                1    0.7250    0.7251  0.6377    0.4679 2.638 0.2262 0.8030  0.9997 0.0001336   0.4616
#> 3        f2     3                1    0.7153    0.7153  0.6276    0.4558 2.513 0.2434 0.7900  0.9975 0.0010986   0.4458
#> 4        f2     3                1    0.7110    0.7112  0.6250    0.4508 2.463 0.2500 0.7835  0.9940 0.0027208   0.4602
#> 
#> $result_item_statistics
#>   dimension question raw_alpha    n  raw_r  std_r  r_cor r_drop  mean    sd     1     2     3     4     5     6     7     8     9 miss
#> 1        f1       X1    0.7248 1000 0.7837 0.7887 0.6078 0.5192 5.014 1.024 0.001 0.002 0.067 0.225 0.387 0.257 0.057 0.004 0.000    0
#> 2        f1       X2    0.7248 1000 0.8117 0.8108 0.6596 0.5606 5.035 1.049 0.000 0.010 0.053 0.234 0.372 0.262 0.065 0.003 0.001    0
#> 3        f1       X3    0.7248 1000 0.8136 0.8097 0.6570 0.5583 5.025 1.065 0.000 0.004 0.066 0.229 0.393 0.231 0.064 0.013 0.000    0
#> 4        f2       X4    0.7094 1000 0.7820 0.7833 0.5985 0.5053 4.988 1.017    NA 0.006 0.053 0.254 0.389 0.236 0.056 0.006    NA    0
#> 5        f2       X5    0.7094 1000 0.7983 0.7966 0.6300 0.5297 5.018 1.030    NA 0.006 0.052 0.246 0.388 0.235 0.068 0.005    NA    0
#> 6        f2       X6    0.7094 1000 0.8053 0.8058 0.6504 0.5467 4.961 1.020    NA 0.008 0.060 0.243 0.403 0.231 0.048 0.007    NA    0
#> 
#> $result_dropped
#>   dimension question scale_alpha raw_alpha std_alpha g6(smc) average_r   s/n alpha se var_r  med_r
#> 1        f1       X1      0.7248    0.6683    0.6684  0.5019    0.5019 2.015  0.02097    NA 0.5019
#> 2        f1       X2      0.7248    0.6190    0.6194  0.4486    0.4486 1.627  0.02407    NA 0.4486
#> 3        f1       X3      0.7248    0.6218    0.6220  0.4513    0.4513 1.645  0.02391    NA 0.4513
#> 4        f2       X4      0.7094    0.6460    0.6460  0.4771    0.4771 1.825  0.02239    NA 0.4771
#> 5        f2       X5      0.7094    0.6161    0.6161  0.4452    0.4452 1.605  0.02428    NA 0.4452
#> 6        f2       X6      0.7094    0.5949    0.5950  0.4234    0.4234 1.469  0.02562    NA 0.4234
#> 
```
