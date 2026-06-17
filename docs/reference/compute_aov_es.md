# Compute eta and omega

Computes omega using aov object. Based on
http://stats.stackexchange.com/a/126520

## Usage

``` r
compute_aov_es(model, ss = "I")
```

## Arguments

- model:

  object aov

- ss:

  Character type of sums of squares "I" "II" "III"

## Examples

``` r
form<-formula(uptake~Treatment)
one_way_between<-aov(form,CO2)
factorial_between<-aov(uptake~Treatment*Type,CO2)
compute_aov_es(model=one_way_between,ss="I")
#> Error in eval(model$call$formula): object 'form' not found
sjstats::anova_stats(one_way_between,digits=10)
#> etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f
#> ------------------------------------------------------------------------
#> 0.102 |         0.102 |   0.090 |           0.090 |     0.091 |    0.337
#>       |               |         |                 |           |         
#> 
#> etasq |      term |    sumsq | df |  meansq | statistic | p.value | power
#> -------------------------------------------------------------------------
#> 0.102 | Treatment |  988.114 |  1 | 988.114 |     9.293 |   0.003 | 0.862
#>       | Residuals | 8718.861 | 82 | 106.328 |           |         |      
compute_aov_es(model=one_way_between,ss="II")
#> Error in eval(model$call$formula): object 'form' not found
sjstats::anova_stats(one_way_between,digits=10)
#> etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f
#> ------------------------------------------------------------------------
#> 0.102 |         0.102 |   0.090 |           0.090 |     0.091 |    0.337
#>       |               |         |                 |           |         
#> 
#> etasq |      term |    sumsq | df |  meansq | statistic | p.value | power
#> -------------------------------------------------------------------------
#> 0.102 | Treatment |  988.114 |  1 | 988.114 |     9.293 |   0.003 | 0.862
#>       | Residuals | 8718.861 | 82 | 106.328 |           |         |      
compute_aov_es(model=one_way_between,ss="III")
#> Error in eval(model$call$formula): object 'form' not found
sjstats::anova_stats(one_way_between,digits=10)
#> etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f
#> ------------------------------------------------------------------------
#> 0.102 |         0.102 |   0.090 |           0.090 |     0.091 |    0.337
#>       |               |         |                 |           |         
#> 
#> etasq |      term |    sumsq | df |  meansq | statistic | p.value | power
#> -------------------------------------------------------------------------
#> 0.102 | Treatment |  988.114 |  1 | 988.114 |     9.293 |   0.003 | 0.862
#>       | Residuals | 8718.861 | 82 | 106.328 |           |         |      
compute_aov_es(model=factorial_between,ss="I")
#>                        call ss    comparisons Df    Sum Sq    Mean Sq  F value
#> 1 uptake ~ Treatment * Type  I      Treatment  1  988.1144  988.11440 15.41641
#> 2 uptake ~ Treatment * Type  I           Type  1 3365.5344 3365.53440 52.50856
#> 3 uptake ~ Treatment * Type  I Treatment:Type  1  225.7296  225.72964  3.52180
#> 4 uptake ~ Treatment * Type  I      Residuals 80 5127.5971   64.09496       NA
#>         Pr(>F)      etasq partial_etasq    omegasq partial_omegasq  epsilonsq
#> 1 1.817080e-04 0.10179426    0.16156982 0.09456686      0.14648382 0.09519128
#> 2 2.377680e-10 0.34671298    0.39626543 0.33787899      0.38011297 0.34011000
#> 3 6.421283e-02 0.02325437    0.04216624 0.01654217      0.02914641 0.01665139
#> 4           NA         NA            NA         NA              NA         NA
#>    cohens_f
#> 1 0.4389820
#> 2 0.8101586
#> 3 0.2098154
#> 4        NA
sjstats::anova_stats(factorial_between,digits=10)
#> etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f
#> ------------------------------------------------------------------------
#> 0.102 |         0.162 |   0.095 |           0.146 |     0.095 |    0.439
#> 0.347 |         0.396 |   0.338 |           0.380 |     0.340 |    0.810
#> 0.023 |         0.042 |   0.017 |           0.029 |     0.017 |    0.210
#>       |               |         |                 |           |         
#> 
#> etasq |           term |    sumsq | df |   meansq | statistic | p.value | power
#> -------------------------------------------------------------------------------
#> 0.102 |      Treatment |  988.114 |  1 |  988.114 |    15.416 |  < .001 | 0.975
#> 0.347 |           Type | 3365.534 |  1 | 3365.534 |    52.509 |  < .001 | 1.000
#> 0.023 | Treatment:Type |  225.730 |  1 |  225.730 |     3.522 |   0.064 | 0.467
#>       |      Residuals | 5127.597 | 80 |   64.095 |           |         |      
compute_aov_es(model=factorial_between,ss="II")
#>                        call ss    comparisons Df    Sum Sq    Mean Sq  F value
#> 1 uptake ~ Treatment * Type II      Treatment  1  988.1144  988.11440 15.41641
#> 2 uptake ~ Treatment * Type II           Type  1 3365.5344 3365.53440 52.50856
#> 3 uptake ~ Treatment * Type II Treatment:Type  1  225.7296  225.72964  3.52180
#> 4 uptake ~ Treatment * Type II      Residuals 80 5127.5971   64.09496       NA
#>         Pr(>F)      etasq partial_etasq    omegasq partial_omegasq  epsilonsq
#> 1 1.817080e-04 0.10179426    0.16156982 0.09456686      0.14648382 0.09519128
#> 2 2.377680e-10 0.34671298    0.39626543 0.33787899      0.38011297 0.34011000
#> 3 6.421283e-02 0.02325437    0.04216624 0.01654217      0.02914641 0.01665139
#> 4           NA         NA            NA         NA              NA         NA
#>    cohens_f
#> 1 0.4389820
#> 2 0.8101586
#> 3 0.2098154
#> 4        NA
sjstats::anova_stats(factorial_between,digits=10)
#> etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f
#> ------------------------------------------------------------------------
#> 0.102 |         0.162 |   0.095 |           0.146 |     0.095 |    0.439
#> 0.347 |         0.396 |   0.338 |           0.380 |     0.340 |    0.810
#> 0.023 |         0.042 |   0.017 |           0.029 |     0.017 |    0.210
#>       |               |         |                 |           |         
#> 
#> etasq |           term |    sumsq | df |   meansq | statistic | p.value | power
#> -------------------------------------------------------------------------------
#> 0.102 |      Treatment |  988.114 |  1 |  988.114 |    15.416 |  < .001 | 0.975
#> 0.347 |           Type | 3365.534 |  1 | 3365.534 |    52.509 |  < .001 | 1.000
#> 0.023 | Treatment:Type |  225.730 |  1 |  225.730 |     3.522 |   0.064 | 0.467
#>       |      Residuals | 5127.597 | 80 |   64.095 |           |         |      
compute_aov_es(model=factorial_between,ss="III")
#>                        call  ss    comparisons Df     Sum Sq     Mean Sq
#> 1 uptake ~ Treatment * Type III      Treatment  1   134.6438   134.64381
#> 2 uptake ~ Treatment * Type III           Type  1   924.0238   924.02381
#> 3 uptake ~ Treatment * Type III Treatment:Type  1   225.7296   225.72964
#> 4 uptake ~ Treatment * Type III    (Intercept)  1 26217.3333 26217.33333
#> 5 uptake ~ Treatment * Type III      Residuals 80  5127.5971    64.09496
#>      F value       Pr(>F)      etasq partial_etasq    omegasq partial_omegasq
#> 1   2.100692 1.511413e-01 0.02099874    0.02558678 0.01089374      0.01293400
#> 2  14.416481 2.839087e-04 0.14410864    0.15269030 0.13278520      0.13772290
#> 3   3.521800 6.421283e-02 0.03520428    0.04216624 0.02495869      0.02914641
#> 4 409.038894 3.440811e-33         NA            NA         NA              NA
#> 5         NA           NA         NA            NA         NA              NA
#>    epsilonsq  cohens_f
#> 1 0.01100264 0.1620452
#> 2 0.13411254 0.4245068
#> 3 0.02520818 0.2098154
#> 4         NA        NA
#> 5         NA        NA
sjstats::anova_stats(car::Anova(factorial_between,Type=3),digits=10)
#> etasq | partial.etasq | omegasq | partial.omegasq | epsilonsq | cohens.f
#> ------------------------------------------------------------------------
#> 0.102 |         0.162 |   0.095 |           0.146 |     0.095 |    0.439
#> 0.347 |         0.396 |   0.338 |           0.380 |     0.340 |    0.810
#> 0.023 |         0.042 |   0.017 |           0.029 |     0.017 |    0.210
#>       |               |         |                 |           |         
#> 
#> etasq |           term |    sumsq | df |   meansq | statistic | p.value | power
#> -------------------------------------------------------------------------------
#> 0.102 |      Treatment |  988.114 |  1 |  988.114 |    15.416 |  < .001 | 0.975
#> 0.347 |           Type | 3365.534 |  1 | 3365.534 |    52.509 |  < .001 | 1.000
#> 0.023 | Treatment:Type |  225.730 |  1 |  225.730 |     3.522 |   0.064 | 0.467
#>       |      Residuals | 5127.597 | 80 |   64.095 |           |         |      
```
