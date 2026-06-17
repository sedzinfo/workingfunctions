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
report_alpha(df=df,key=key,reverse=reverse,check.keys=FALSE,n.iter=2)
#>   |                                                                                                                                                                                                      |                                                                                                                                                                                              |   0%  |                                                                                                                                                                                                      |==============================================================================================================================================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n     ase  mean     sd median_r boot_ci_2_5% boot_ci_50% boot_ci_97_5%     alpha_criterion
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632 0.01506 5.025 0.8402   0.4513       0.7025      0.7036        0.7047 Good and Acceptable
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440 0.01592 4.989 0.8131   0.4452       0.7046      0.7046        0.7047 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n    ase unidim goodfit     var_r median_r
#> 1        f1     3                1    0.7024    0.7020  0.6150    0.4399 2.356 0.2653 0.7715  0.9934 0.0031053   0.4512
#> 2        f1     3                1    0.7047    0.7047  0.6162    0.4430 2.386 0.2605 0.7760  0.9964 0.0016985   0.4438
#> 3        f2     3                1    0.7047    0.7055  0.6155    0.4440 2.395 0.2618 0.7779  0.9991 0.0004309   0.4486
#> 4        f2     3                1    0.7046    0.7054  0.6158    0.4438 2.394 0.2624 0.7775  0.9982 0.0008232   0.4483
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
report_alpha(df=df,key=key,check.keys=FALSE,n.iter=2,file="alpha")
#>   |                                                                                                                                                                                                      |                                                                                                                                                                                              |   0%  |                                                                                                                                                                                                      |==============================================================================================================================================================================================| 100%
#> $result_total
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n     ase  mean     sd median_r boot_ci_2_5% boot_ci_50% boot_ci_97_5%     alpha_criterion
#> 1        f1     3                1    0.7248    0.7246  0.6381    0.4673 2.632 0.01506 5.025 0.8402   0.4513       0.7293      0.7425        0.7558 Good and Acceptable
#> 2        f2     3                1    0.7094    0.7093  0.6203    0.4486 2.440 0.01592 4.989 0.8131   0.4452       0.7179      0.7180        0.7180 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n    ase unidim goodfit      var_r median_r
#> 1        f1     3                1    0.7286    0.7282  0.6449    0.4718 2.679 0.2197 0.8046  0.9927 0.00307699   0.4505
#> 2        f1     3                1    0.7565    0.7565  0.6759    0.5087 3.106 0.1782 0.8401  0.9967 0.00119893   0.5071
#> 3        f2     3                1    0.7180    0.7181  0.6294    0.4591 2.547 0.2385 0.7942  0.9998 0.00007322   0.4589
#> 4        f2     3                1    0.7179    0.7176  0.6307    0.4586 2.541 0.2384 0.7926  0.9965 0.00152711   0.4430
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
