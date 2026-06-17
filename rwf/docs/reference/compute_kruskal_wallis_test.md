# Kruskal-Wallis Test with Effect Sizes

Runs a one-way Kruskal-Wallis rank-sum test and returns the test
statistic, p-value, and two effect sizes:

- `etasq`: eta-squared for Kruskal-Wallis (\\\eta_H^2\\)

- `epsilonsq`: epsilon-squared (\\\epsilon^2\\)

In simple terms, this tests whether groups differ in their
distributions, and quantifies how large that group effect is.

## Usage

``` r
compute_kruskal_wallis_test(formula, df)
```

## Arguments

- formula:

  A one-way formula in the form `y ~ group`.

- df:

  A data frame containing the variables in `formula`.

## Value

A one-row data frame with:

- `formula`: model formula used

- `method`: test name

- `etasq`: Kruskal-Wallis eta-squared, \\(H-k+1)/(n-k)\\

- `epsilonsq`: epsilon-squared, \\H/(n-1)\\

- `H`: Kruskal-Wallis chi-squared statistic

- `df`: degrees of freedom (\\k-1\\)

- `p`: p-value

## Details

`etasq` and `epsilonsq` are both in \[0, 1\] in typical use. Multiplying
by 100 gives an approximate percentage-style interpretation of explained
rank variance.

## Examples

``` r
form<-formula(bp_before~agegrp)
kruskal.test(formula=form,data=df_blood_pressure)
#> 
#>  Kruskal-Wallis rank sum test
#> 
#> data:  bp_before by agegrp
#> Kruskal-Wallis chi-squared = 19.564, df = 2, p-value = 5.645e-05
#> 
rcompanion::epsilonSquared(x=df_blood_pressure$bp_before,
                           g=df_blood_pressure$agegrp,
                           group="row",
                           ci=TRUE,
                           conf=0.95,
                           type="perc",
                           R=1000,
                           digits=3)
#>   epsilon.squared lower.ci upper.ci
#> 1           0.164   0.0665    0.305
rstatix::kruskal_effsize(df_blood_pressure,form,ci=TRUE,conf.level=0.95,ci.type="perc",nboot=100)
#> # A tibble: 1 × 7
#>   .y.           n effsize conf.low conf.high method  magnitude
#> * <chr>     <int>   <dbl>    <dbl>     <dbl> <chr>   <ord>    
#> 1 bp_before   120   0.150     0.06       0.3 eta2[H] large    
compute_kruskal_wallis_test(formula=form,df=df_blood_pressure)
#>              formula                       method     etasq epsilonsq        H
#> 1 bp_before ~ agegrp Kruskal-Wallis rank sum test 0.1501232 0.1644069 19.56442
#>   df            p
#> 1  2 5.644699e-05
```
