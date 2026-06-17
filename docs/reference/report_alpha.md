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
#> 1 0.01506 5.025 0.8402   0.4513       0.7131      0.7217        0.7302
#> 2 0.01592 4.989 0.8131   0.4452       0.6820      0.6959        0.7099
#>       alpha_criterion
#> 1 Good and Acceptable
#> 2 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7306    0.7306  0.6446    0.4748 2.712
#> 2        f1     3                1    0.7127    0.7126  0.6251    0.4525 2.480
#> 3        f2     3                1    0.7106    0.7113  0.6236    0.4509 2.463
#> 4        f2     3                1    0.6812    0.6807  0.5918    0.4154 2.132
#>      ase unidim goodfit     var_r median_r
#> 1 0.2171 0.8095  0.9987 0.0005266   0.4737
#> 2 0.2470 0.7862  0.9965 0.0015639   0.4500
#> 3 0.2505 0.7844  0.9964 0.0016266   0.4428
#> 4 0.3038 0.7430  0.9925 0.0039000   0.4115
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
#> 1 0.01506 5.025 0.8402   0.4513       0.7277      0.7293        0.7309
#> 2 0.01592 4.989 0.8131   0.4452       0.7146      0.7267        0.7388
#>       alpha_criterion
#> 1 Good and Acceptable
#> 2 Good and Acceptable
#> 
#> $result_boot
#>   dimension items kaiser_criterion raw_alpha std_alpha g6(smc) average_r   s/n
#> 1        f1     3                1    0.7310    0.7307  0.6467    0.4750 2.714
#> 2        f1     3                1    0.7276    0.7271  0.6430    0.4704 2.664
#> 3        f2     3                1    0.7140    0.7146  0.6265    0.4549 2.503
#> 4        f2     3                1    0.7394    0.7397  0.6548    0.4865 2.842
#>      ase unidim goodfit     var_r median_r
#> 1 0.2150 0.8085  0.9949 0.0021008   0.4618
#> 2 0.2213 0.8036  0.9938 0.0026073   0.4507
#> 3 0.2460 0.7891  0.9979 0.0009489   0.4506
#> 4 0.2033 0.8209  0.9995 0.0001972   0.4791
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
