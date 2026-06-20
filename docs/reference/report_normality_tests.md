# Battery of normality tests

Runs eight normality tests on each numeric column of `df`: Shapiro-Wilk,
Anderson-Darling, Cramér-von Mises, Shapiro-Francia, Jarque-Bera,
Kolmogorov-Smirnov, Lilliefors, and Pearson chi-squared. Each column is
z-standardised before testing. Columns with fewer than 8 or more than
4999 non-missing observations are skipped with a console message.
Results are printed to the console; when `file` is supplied they are
also written to a `.log` file and a colour-coded `.xlsx` file with
significant p-values (\\p \le 0.05\\) highlighted.

## Usage

``` r
report_normality_tests(df, file = NULL)
```

## Arguments

- df:

  Data frame or numeric vector.

- file:

  Character string naming the output files (without extension). When
  supplied, a `.log` and an `.xlsx` file are written. When `NULL`
  (default) no files are written.

## Value

Invisibly returns `NULL`. Called for its side effects of printing
results and optionally writing output files.

## Examples

``` r
vector <- generate_missing(rnorm(1000), missing = 10)
df <- generate_missing(mtcars[, 1:2], missing = 10)
report_normality_tests(df = df)
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> [1] "####################################################################################################"
#> [1] "NORMALITY TESTS"
#> [1] "####################################################################################################"
#> [1] ""
#> [1] "##################################################"
#>    variable  n         statistic   df                       p                                         method                               method1 alternative n.classes
#> 1       mpg 22 0.907671371467255 <NA>      0.0424444224272723                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       mpg 22               Inf <NA>    0.000027272727264549       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       mpg 22 0.128757857642527 <NA>      0.0417684795385981                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       mpg 22 0.903713354926944 <NA>      0.0364168554522193                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       mpg 22  7.01542555265127    2      0.0299653736283138                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       mpg 22 0.157810675388589 <NA>       0.164530584661579 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       mpg 22 0.157810675388589 <NA>       0.643686079119771  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       mpg 22  4.90909090909091    5       0.427075407590626              Pearson chi-square normality test                                  <NA>        <NA>         8
#> 9       cyl 22 0.761764951040477 <NA>    0.000133662862340822                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 10      cyl 22               Inf <NA>    0.000027272727264549       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 11      cyl 22  0.29650206155579 <NA>    0.000305792535894916                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 12      cyl 22 0.788076490086437 <NA>    0.000655612595921643                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 13      cyl 22  1.56101050728319    2       0.458174458500057                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 14      cyl 22 0.265050034934503 <NA>      0.0003056791731641 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 15      cyl 22 0.265050034934503 <NA>      0.0908983294265368  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 16      cyl 22  39.8181818181818    5 0.000000162489408865354              Pearson chi-square normality test                                  <NA>        <NA>         8
#>                                                                                                            instruction
#> 1                                                      Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 2                                                  Anderson-Darling Composite null hypothesis: any normal distribution
#> 3                                                  Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 4                                                   Shapiro-Francia Composite null hypothesis: any normal distribution
#> 5                                                       Jarque-Bera Composite null hypothesis: any normal distribution
#> 6                                                        Lilliefors Composite null hypothesis: any normal distribution
#> 7                                        Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 8  Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
#> 9                                                      Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 10                                                 Anderson-Darling Composite null hypothesis: any normal distribution
#> 11                                                 Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 12                                                  Shapiro-Francia Composite null hypothesis: any normal distribution
#> 13                                                      Jarque-Bera Composite null hypothesis: any normal distribution
#> 14                                                       Lilliefors Composite null hypothesis: any normal distribution
#> 15                                       Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 16 Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
#>    variable  n         statistic   df                       p                                         method                               method1 alternative n.classes
#> 1       mpg 22 0.907671371467255 <NA>      0.0424444224272723                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       mpg 22               Inf <NA>    0.000027272727264549       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       mpg 22 0.128757857642527 <NA>      0.0417684795385981                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       mpg 22 0.903713354926944 <NA>      0.0364168554522193                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       mpg 22  7.01542555265127    2      0.0299653736283138                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       mpg 22 0.157810675388589 <NA>       0.164530584661579 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       mpg 22 0.157810675388589 <NA>       0.643686079119771  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       mpg 22  4.90909090909091    5       0.427075407590626              Pearson chi-square normality test                                  <NA>        <NA>         8
#> 9       cyl 22 0.761764951040477 <NA>    0.000133662862340822                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 10      cyl 22               Inf <NA>    0.000027272727264549       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 11      cyl 22  0.29650206155579 <NA>    0.000305792535894916                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 12      cyl 22 0.788076490086437 <NA>    0.000655612595921643                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 13      cyl 22  1.56101050728319    2       0.458174458500057                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 14      cyl 22 0.265050034934503 <NA>      0.0003056791731641 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 15      cyl 22 0.265050034934503 <NA>      0.0908983294265368  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 16      cyl 22  39.8181818181818    5 0.000000162489408865354              Pearson chi-square normality test                                  <NA>        <NA>         8
#>                                                                                                            instruction
#> 1                                                      Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 2                                                  Anderson-Darling Composite null hypothesis: any normal distribution
#> 3                                                  Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 4                                                   Shapiro-Francia Composite null hypothesis: any normal distribution
#> 5                                                       Jarque-Bera Composite null hypothesis: any normal distribution
#> 6                                                        Lilliefors Composite null hypothesis: any normal distribution
#> 7                                        Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 8  Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
#> 9                                                      Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 10                                                 Anderson-Darling Composite null hypothesis: any normal distribution
#> 11                                                 Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 12                                                  Shapiro-Francia Composite null hypothesis: any normal distribution
#> 13                                                      Jarque-Bera Composite null hypothesis: any normal distribution
#> 14                                                       Lilliefors Composite null hypothesis: any normal distribution
#> 15                                       Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 16 Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
report_normality_tests(df = vector, file = "normality_tests")
#> [1] "####################################################################################################"
#> [1] "NORMALITY TESTS"
#> [1] "####################################################################################################"
#> [1] ""
#> [1] "##################################################"
#>   variable   n          statistic   df                       p                                         method                               method1 alternative n.classes
#> 1       df 990  0.998586932458926 <NA>       0.620872217580073                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       df 990                Inf <NA> 0.000000606060605923275       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       df 990 0.0837307227619041 <NA>       0.185179739187944                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       df 990  0.998781456778591 <NA>       0.686682424892527                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       df 990  0.263513721522177    2       0.876554093867233                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       df 990  0.022569608470269 <NA>       0.255716583067038 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       df 990  0.022569608470269 <NA>       0.694301979540724  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       df 990   30.2505050505051   29       0.401577134664025              Pearson chi-square normality test                                  <NA>        <NA>        32
#>                                                                                                           instruction
#> 1                                                     Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 2                                                 Anderson-Darling Composite null hypothesis: any normal distribution
#> 3                                                 Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 4                                                  Shapiro-Francia Composite null hypothesis: any normal distribution
#> 5                                                      Jarque-Bera Composite null hypothesis: any normal distribution
#> 6                                                       Lilliefors Composite null hypothesis: any normal distribution
#> 7                                       Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 8 Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
#>   variable   n          statistic   df                       p                                         method                               method1 alternative n.classes
#> 1       df 990  0.998586932458926 <NA>       0.620872217580073                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       df 990                Inf <NA> 0.000000606060605923275       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       df 990 0.0837307227619041 <NA>       0.185179739187944                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       df 990  0.998781456778591 <NA>       0.686682424892527                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       df 990  0.263513721522177    2       0.876554093867233                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       df 990  0.022569608470269 <NA>       0.255716583067038 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       df 990  0.022569608470269 <NA>       0.694301979540724  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       df 990   30.2505050505051   29       0.401577134664025              Pearson chi-square normality test                                  <NA>        <NA>        32
#>                                                                                                           instruction
#> 1                                                     Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 2                                                 Anderson-Darling Composite null hypothesis: any normal distribution
#> 3                                                 Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 4                                                  Shapiro-Francia Composite null hypothesis: any normal distribution
#> 5                                                      Jarque-Bera Composite null hypothesis: any normal distribution
#> 6                                                       Lilliefors Composite null hypothesis: any normal distribution
#> 7                                       Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 8 Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
```
