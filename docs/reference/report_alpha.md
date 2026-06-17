# Estimate alpha for several dimensions and export results to xlsx

Uses an arbitrary input

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

  dataframe

- key:

  index of trait names and items constituting a trait

- questions:

  trait names and items constituting a trait

- reverse:

  index of trait names and index for reversal

- mini:

  minimum rating in scale if NULL reversal will be performed using the
  empirical minimum

- maxi:

  maximum rating in scale if NULL reversal will be performed using the
  empirical maximum

- file:

  output filename

- ...:

  arguments passed to psych::alpha

## Examples

``` r
set.seed(12345)
df<-data.frame(matrix(.5,ncol=6,nrow=6))
correlation_martix<-as.matrix(df)
diag(correlation_martix)<-1
df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
key<-list(f1=c("X1","X2","X3"),
          f2=c("X4","X5","X6"))
reverse<-list(f1=c(1,1,1),
              f2=c(1,1,1))
report_alpha(df=df,key=key,cumulative=TRUE,n.iter=1)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440
#>       ase  mean    sd median_r     alpha_criterion
#> 1 0.01506 15.07 2.520   0.4513 Good and Acceptable
#> 2 0.01592 14.97 2.439   0.4452 Good and Acceptable
#> 
#> $result_boot
#> data frame with 0 columns and 0 rows
#> 
#> $result_item_statistics
#>   dimension question raw_alpha    n  raw_r  std_r  r_cor r_drop  mean    sd
#> 1        f1       X1    0.7248 1000 0.7837 0.7887 0.6078 0.5192 5.014 1.024
#> 2        f1       X2    0.7248 1000 0.8117 0.8108 0.6596 0.5606 5.035 1.049
#> 3        f1       X3    0.7248 1000 0.8136 0.8097 0.6570 0.5583 5.025 1.065
#> 4        f2       X4    0.7094 1000 0.7820 0.7833 0.5985 0.5053 4.988 1.017
#> 5        f2       X5    0.7094 1000 0.7983 0.7966 0.6300 0.5297 5.018 1.030
#> 6        f2       X6    0.7094 1000 0.8053 0.8058 0.6504 0.5467 4.961 1.020
#>       1     2     3     4     5     6     7     8     9 miss
#> 1 0.001 0.002 0.067 0.225 0.387 0.257 0.057 0.004 0.000    0
#> 2 0.000 0.010 0.053 0.234 0.372 0.262 0.065 0.003 0.001    0
#> 3 0.000 0.004 0.066 0.229 0.393 0.231 0.064 0.013 0.000    0
#> 4    NA 0.006 0.053 0.254 0.389 0.236 0.056 0.006    NA    0
#> 5    NA 0.006 0.052 0.246 0.388 0.235 0.068 0.005    NA    0
#> 6    NA 0.008 0.060 0.243 0.403 0.231 0.048 0.007    NA    0
#> 
#> $result_dropped
#>   dimension question scale_alpha raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1       X1      0.7248    0.6683    0.6684  0.5019    0.5019 2.015
#> 2        f1       X2      0.7248    0.6190    0.6194  0.4486    0.4486 1.627
#> 3        f1       X3      0.7248    0.6218    0.6220  0.4513    0.4513 1.645
#> 4        f2       X4      0.7094    0.6460    0.6460  0.4771    0.4771 1.825
#> 5        f2       X5      0.7094    0.6161    0.6161  0.4452    0.4452 1.605
#> 6        f2       X6      0.7094    0.5949    0.5950  0.4234    0.4234 1.469
#>   alpha se var_r  med_r
#> 1  0.02097    NA 0.5019
#> 2  0.02407    NA 0.4486
#> 3  0.02391    NA 0.4513
#> 4  0.02239    NA 0.4771
#> 5  0.02428    NA 0.4452
#> 6  0.02562    NA 0.4234
#> 
report_alpha(df=df,key=key,reverse=reverse,check.keys=FALSE,n.iter=2)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440
#>       ase  mean     sd median_r boot_ci_2_5% boot_ci_50% boot_ci_97_5%
#> 1 0.01506 5.025 0.8402   0.4513       0.7149      0.7176        0.7203
#> 2 0.01592 4.989 0.8131   0.4452       0.7146      0.7173        0.7199
#>       alpha_criterion
#> 1 Good and Acceptable
#> 2 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7204    0.7202  0.6347    0.4617 2.573
#> 2        f1     3                1    0.7147    0.7145  0.6286    0.4548 2.503
#> 3        f2     3                1    0.7201    0.7202  0.6322    0.4618 2.574
#> 4        f2     3                1    0.7144    0.7144  0.6256    0.4547 2.501
#>      ase unidim goodfit     var_r median_r
#> 1 0.2343 0.7951  0.9946 0.0023770   0.4415
#> 2 0.2421 0.7878  0.9941 0.0026245   0.4430
#> 3 0.2351 0.7968  0.9994 0.0002671   0.4621
#> 4 0.2446 0.7893  0.9991 0.0003957   0.4596
#> 
#> $result_item_statistics
#>   dimension question raw_alpha    n  raw_r  std_r  r_cor r_drop  mean    sd
#> 1        f1       X1    0.7248 1000 0.7837 0.7887 0.6078 0.5192 5.014 1.024
#> 2        f1       X2    0.7248 1000 0.8117 0.8108 0.6596 0.5606 5.035 1.049
#> 3        f1       X3    0.7248 1000 0.8136 0.8097 0.6570 0.5583 5.025 1.065
#> 4        f2       X4    0.7094 1000 0.7820 0.7833 0.5985 0.5053 4.988 1.017
#> 5        f2       X5    0.7094 1000 0.7983 0.7966 0.6300 0.5297 5.018 1.030
#> 6        f2       X6    0.7094 1000 0.8053 0.8058 0.6504 0.5467 4.961 1.020
#>       1     2     3     4     5     6     7     8     9 miss
#> 1 0.001 0.002 0.067 0.225 0.387 0.257 0.057 0.004 0.000    0
#> 2 0.000 0.010 0.053 0.234 0.372 0.262 0.065 0.003 0.001    0
#> 3 0.000 0.004 0.066 0.229 0.393 0.231 0.064 0.013 0.000    0
#> 4    NA 0.006 0.053 0.254 0.389 0.236 0.056 0.006    NA    0
#> 5    NA 0.006 0.052 0.246 0.388 0.235 0.068 0.005    NA    0
#> 6    NA 0.008 0.060 0.243 0.403 0.231 0.048 0.007    NA    0
#> 
#> $result_dropped
#>   dimension question scale_alpha raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1       X1      0.7248    0.6683    0.6684  0.5019    0.5019 2.015
#> 2        f1       X2      0.7248    0.6190    0.6194  0.4486    0.4486 1.627
#> 3        f1       X3      0.7248    0.6218    0.6220  0.4513    0.4513 1.645
#> 4        f2       X4      0.7094    0.6460    0.6460  0.4771    0.4771 1.825
#> 5        f2       X5      0.7094    0.6161    0.6161  0.4452    0.4452 1.605
#> 6        f2       X6      0.7094    0.5949    0.5950  0.4234    0.4234 1.469
#>   alpha se var_r  med_r
#> 1  0.02097    NA 0.5019
#> 2  0.02407    NA 0.4486
#> 3  0.02391    NA 0.4513
#> 4  0.02239    NA 0.4771
#> 5  0.02428    NA 0.4452
#> 6  0.02562    NA 0.4234
#> 
report_alpha(df=df,key=key,check.keys=FALSE,n.iter=2,file="alpha")
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440
#>       ase  mean     sd median_r boot_ci_2_5% boot_ci_50% boot_ci_97_5%
#> 1 0.01506 5.025 0.8402   0.4513       0.7280      0.7307        0.7334
#> 2 0.01592 4.989 0.8131   0.4452       0.6889      0.6986        0.7084
#>       alpha_criterion
#> 1 Good and Acceptable
#> 2 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7335    0.7336  0.6476    0.4786 2.754
#> 2        f1     3                1    0.7278    0.7278  0.6428    0.4713 2.674
#> 3        f2     3                1    0.7089    0.7089  0.6190    0.4480 2.435
#> 4        f2     3                1    0.6884    0.6883  0.5962    0.4240 2.208
#>      ase unidim goodfit     var_r median_r
#> 1 0.2128 0.8134  0.9996 0.0001764   0.4857
#> 2 0.2212 0.8052  0.9960 0.0016791   0.4609
#> 3 0.2540 0.7825  0.9997 0.0001352   0.4427
#> 4 0.2912 0.7556  0.9989 0.0005453   0.4370
#> 
#> $result_item_statistics
#>   dimension question raw_alpha    n  raw_r  std_r  r_cor r_drop  mean    sd
#> 1        f1       X1    0.7248 1000 0.7837 0.7887 0.6078 0.5192 5.014 1.024
#> 2        f1       X2    0.7248 1000 0.8117 0.8108 0.6596 0.5606 5.035 1.049
#> 3        f1       X3    0.7248 1000 0.8136 0.8097 0.6570 0.5583 5.025 1.065
#> 4        f2       X4    0.7094 1000 0.7820 0.7833 0.5985 0.5053 4.988 1.017
#> 5        f2       X5    0.7094 1000 0.7983 0.7966 0.6300 0.5297 5.018 1.030
#> 6        f2       X6    0.7094 1000 0.8053 0.8058 0.6504 0.5467 4.961 1.020
#>       1     2     3     4     5     6     7     8     9 miss
#> 1 0.001 0.002 0.067 0.225 0.387 0.257 0.057 0.004 0.000    0
#> 2 0.000 0.010 0.053 0.234 0.372 0.262 0.065 0.003 0.001    0
#> 3 0.000 0.004 0.066 0.229 0.393 0.231 0.064 0.013 0.000    0
#> 4    NA 0.006 0.053 0.254 0.389 0.236 0.056 0.006    NA    0
#> 5    NA 0.006 0.052 0.246 0.388 0.235 0.068 0.005    NA    0
#> 6    NA 0.008 0.060 0.243 0.403 0.231 0.048 0.007    NA    0
#> 
#> $result_dropped
#>   dimension question scale_alpha raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1       X1      0.7248    0.6683    0.6684  0.5019    0.5019 2.015
#> 2        f1       X2      0.7248    0.6190    0.6194  0.4486    0.4486 1.627
#> 3        f1       X3      0.7248    0.6218    0.6220  0.4513    0.4513 1.645
#> 4        f2       X4      0.7094    0.6460    0.6460  0.4771    0.4771 1.825
#> 5        f2       X5      0.7094    0.6161    0.6161  0.4452    0.4452 1.605
#> 6        f2       X6      0.7094    0.5949    0.5950  0.4234    0.4234 1.469
#>   alpha se var_r  med_r
#> 1  0.02097    NA 0.5019
#> 2  0.02407    NA 0.4486
#> 3  0.02391    NA 0.4513
#> 4  0.02239    NA 0.4771
#> 5  0.02428    NA 0.4452
#> 6  0.02562    NA 0.4234
#> 
```
