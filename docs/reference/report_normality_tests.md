# Normality tests

Shapiro-Wilk Anderson-Darling Cramer-von-Mises Shapiro-Francia  
Jarque-Bera Kolmogorov-Smirnov Lilliefors Pearson X2

## Usage

``` r
report_normality_tests(df, file = NULL)
```

## Arguments

- df:

  dataframe with continous or ordinal data

- file:

  output filename

## Details

returns xlsx file

## Examples

``` r
vector<-generate_missing(rnorm(1000))
df<-generate_missing(mtcars[,1:2])
report_normality_tests(df=df)
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> [1] "####################################################################################################"
#> [1] "NORMALITY TESTS"
#> [1] "####################################################################################################"
#> [1] ""
#> [1] "##################################################"
#>    variable  n         statistic   df                          p                                         method                               method1 alternative n.classes
#> 1       mpg 27 0.921254116647303 <NA>         0.0423276071025444                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       mpg 27               Inf <NA>      0.0000222222222155954       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       mpg 27 0.123704628950893 <NA>         0.0498054806570441                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       mpg 27 0.925424285337011 <NA>         0.0527225007678811                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       mpg 27  3.24471465167586    2          0.197432736795221                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       mpg 27 0.120773747737586 <NA>          0.396735084164702 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       mpg 27 0.120773747737587 <NA>           0.82582843391655  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       mpg 27  9.44444444444444    5         0.0925968303202636              Pearson chi-square normality test                                  <NA>        <NA>         8
#> 9       cyl 27 0.740438718622917 <NA>      0.0000149363153135164                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 10      cyl 27               Inf <NA>      0.0000222222222155954       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 11      cyl 27 0.420087046355702 <NA>      0.0000137998576701099                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 12      cyl 27 0.763846463424178 <NA>       0.000100615983672709                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 13      cyl 27  2.17368845362549    2          0.337279192639141                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 14      cyl 27 0.288195362641006 <NA>     0.00000370499714288543 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 15      cyl 27 0.288195362641006 <NA>         0.0225525108384136  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 16      cyl 27  52.7037037037037    5 0.000000000386896848823515              Pearson chi-square normality test                                  <NA>        <NA>         8
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
#>    variable  n         statistic   df                          p                                         method                               method1 alternative n.classes
#> 1       mpg 27 0.921254116647303 <NA>         0.0423276071025444                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       mpg 27               Inf <NA>      0.0000222222222155954       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       mpg 27 0.123704628950893 <NA>         0.0498054806570441                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       mpg 27 0.925424285337011 <NA>         0.0527225007678811                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       mpg 27  3.24471465167586    2          0.197432736795221                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       mpg 27 0.120773747737586 <NA>          0.396735084164702 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       mpg 27 0.120773747737587 <NA>           0.82582843391655  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       mpg 27  9.44444444444444    5         0.0925968303202636              Pearson chi-square normality test                                  <NA>        <NA>         8
#> 9       cyl 27 0.740438718622917 <NA>      0.0000149363153135164                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 10      cyl 27               Inf <NA>      0.0000222222222155954       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 11      cyl 27 0.420087046355702 <NA>      0.0000137998576701099                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 12      cyl 27 0.763846463424178 <NA>       0.000100615983672709                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 13      cyl 27  2.17368845362549    2          0.337279192639141                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 14      cyl 27 0.288195362641006 <NA>     0.00000370499714288543 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 15      cyl 27 0.288195362641006 <NA>         0.0225525108384136  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 16      cyl 27  52.7037037037037    5 0.000000000386896848823515              Pearson chi-square normality test                                  <NA>        <NA>         8
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
report_normality_tests(df=vector,file="normality_tests")
#> [1] "####################################################################################################"
#> [1] "NORMALITY TESTS"
#> [1] "####################################################################################################"
#> [1] ""
#> [1] "##################################################"
#>   variable   n          statistic   df                      p                                         method                               method1 alternative n.classes
#> 1       df 995   0.99861790998656 <NA>      0.637339661815536                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       df 995                Inf <NA> 0.00000060301507520677       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       df 995 0.0800830354726192 <NA>      0.206481976102033                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       df 995  0.998806942916717 <NA>      0.700096841540123                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       df 995  0.307630503812954    2      0.857430415014014                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       df 995 0.0218047373207491 <NA>      0.300364227779563 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       df 995 0.0218047373207491 <NA>      0.731436778757185  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       df 995   27.5527638190955   29      0.541908468426035              Pearson chi-square normality test                                  <NA>        <NA>        32
#>                                                                                                           instruction
#> 1                                                     Shapiro-Wilk Composite null hypothesis: any normal distribution
#> 2                                                 Anderson-Darling Composite null hypothesis: any normal distribution
#> 3                                                 Cramer-von-Mises Composite null hypothesis: any normal distribution
#> 4                                                  Shapiro-Francia Composite null hypothesis: any normal distribution
#> 5                                                      Jarque-Bera Composite null hypothesis: any normal distribution
#> 6                                                       Lilliefors Composite null hypothesis: any normal distribution
#> 7                                       Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution
#> 8 Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals
#>   variable   n          statistic   df                      p                                         method                               method1 alternative n.classes
#> 1       df 995   0.99861790998656 <NA>      0.637339661815536                    Shapiro-Wilk normality test                                  <NA>        <NA>      <NA>
#> 2       df 995                Inf <NA> 0.00000060301507520677       Anderson-Darling test of goodness-of-fit Null hypothesis: uniform distribution        <NA>      <NA>
#> 3       df 995 0.0800830354726192 <NA>      0.206481976102033                Cramer-von Mises normality test                                  <NA>        <NA>      <NA>
#> 4       df 995  0.998806942916717 <NA>      0.700096841540123                 Shapiro-Francia normality test                                  <NA>        <NA>      <NA>
#> 5       df 995  0.307630503812954    2      0.857430415014014                        Robust Jarque Bera Test                                  <NA>        <NA>      <NA>
#> 6       df 995 0.0218047373207491 <NA>      0.300364227779563 Lilliefors (Kolmogorov-Smirnov) normality test                                  <NA>        <NA>      <NA>
#> 7       df 995 0.0218047373207491 <NA>      0.731436778757185  Asymptotic one-sample Kolmogorov-Smirnov test                                  <NA>   two-sided      <NA>
#> 8       df 995   27.5527638190955   29      0.541908468426035              Pearson chi-square normality test                                  <NA>        <NA>        32
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
