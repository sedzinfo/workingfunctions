# Plot means with standard error for every level in a dataframe

Plot means with standard error for every level in a dataframe

## Usage

``` r
report_factorial_anova(
  df,
  dv,
  wid,
  within = NULL,
  within_full = NULL,
  between = NULL,
  within_covariates = NULL,
  between_covariates = NULL,
  observed = NULL,
  diff = NULL,
  reverse_diff = FALSE,
  type = 3,
  white.adjust = TRUE,
  detailed = TRUE,
  return_aov = TRUE,
  file = NULL,
  post_hoc_test = TRUE,
  base_size = 15
)
```

## Arguments

- df:

  dataframe

- dv:

  names of dependent variables

- wid:

  names of

- within:

  names of within factors

- within_full:

  names of within factors after data are collapsed to means per
  condition

- between:

  names of between factors

- within_covariates:

  names of within covariates

- between_covariates:

  mames of between covariates

- observed:

  names in data that are already specified in either within or between
  that contain predictor variables that are observed variables (not
  manipulated)

- diff:

  names of variables to collapse in a different score

- reverse_diff:

  If TRUE, triggers reversal of the difference collapse requested by
  diff

- type:

  sum of squares 1 2 3

- white.adjust:

  if TRUE corrects for heteroscedasticity

- detailed:

  if TRUE returns detailed information

- return_aov:

  if TRUE returns aov object

- file:

  output filename

- post_hoc_test:

  if TRUE outputs post hoc in file

- base_size:

  base font size

## Examples

``` r
set.seed(12345)
df<-data.frame(id=rep(seq(1,80),each=81,1),
               IV1=rep(LETTERS[1:3],each=1,2160),
               IV2=rep(LETTERS[4:6],each=3,720),
               IV3=rep(LETTERS[7:9],each=9,240),
               IV4=rep(LETTERS[10:12],each=27,80),
               stringsAsFactors=FALSE)
cdf<-data.frame(matrix(.01,ncol=4,nrow=4))
correlation_martix<-as.matrix(cdf)
diag(correlation_martix)<-1
cdf<-generate_correlation_matrix(correlation_martix,nrows=nrow(df))+10
names(cdf)<-paste0("DV",1:4)
df<-data.frame(df,cdf)
df$DV2<-df$DV2+10
df$DV3<-df$DV3+20
df$DV4<-df$DV4+30
df[df$IV1%in%"A",]$DV1<-df[df$IV1%in%"A",]$DV1+1
df[df$IV1%in%"B",]$DV1<-df[df$IV1%in%"B",]$DV1+2
df[df$IV1%in%"C",]$DV1<-df[df$IV1%in%"C",]$DV1+3
cdf(df)
#> $summary
#>   COLLUMNS ROWS TOTAL EMPTY null NAN na INF   FIN FACTOR
#> 1        9 6480 58320     0    0   0  0   0 32400      0
#> 
#> $check
#>   NAMES EMPTY null na NOT_NA NAN INF  FIN RANGE  MEAN MEDIAN    SD         MIN         MAX      MODE      TYPE     CLASS FACTOR
#> 1    id     0    0  0   6480   0   0 6480    80  40.5   40.5 23.09           1          80   numeric   integer   integer  FALSE
#> 2   IV1     0    0  0   6480   0   0    0     3    NA     NA    NA           A           C character character character  FALSE
#> 3   IV2     0    0  0   6480   0   0    0     3    NA     NA    NA           D           F character character character  FALSE
#> 4   IV3     0    0  0   6480   0   0    0     3    NA     NA    NA           G           I character character character  FALSE
#> 5   IV4     0    0  0   6480   0   0    0     3    NA     NA    NA           J           L character character character  FALSE
#> 6   DV1     0    0  0   6480   0   0 6480  6480 11.98  11.99   1.3 7.512912073 16.26404907   numeric    double   numeric  FALSE
#> 7   DV2     0    0  0   6480   0   0 6480  6480 20.01  20.02     1 16.33788325 23.62325263   numeric    double   numeric  FALSE
#> 8   DV3     0    0  0   6480   0   0 6480  6480 30.01     30  0.98 26.12652032 33.14197274   numeric    double   numeric  FALSE
#> 9   DV4     0    0  0   6480   0   0 6480  6480    40  39.99  1.01 36.48240015 43.74431445   numeric    double   numeric  FALSE
#> 
r1<-report_factorial_anova(df=df,wid="id",dv=c("DV1","DV2"),
                           within=c("IV1","IV2"),within_full=c("IV1","IV2"),
                           between=NULL,
                           within_covariates=NULL,between_covariates=NULL,
                           file="anova_within",
                           post_hoc=TRUE)
#> Warning: Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> Warning: Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
r2<-report_factorial_anova(df=df,wid="id",dv=c("DV1","DV2"),
                           within=NULL,within_full=NULL,
                           between=c("IV1","IV2"),
                           within_covariates=NULL,between_covariates=NULL,
                           file="anova_between",
                           post_hoc=TRUE)
#> Warning: The column supplied as the wid variable contains non-unique values across levels of the supplied between-Ss variables. Automatically fixing this by generating unique wid labels.
#> Coefficient covariances computed by hccm()
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> Warning: The column supplied as the wid variable contains non-unique values across levels of the supplied between-Ss variables. Automatically fixing this by generating unique wid labels.
#> Coefficient covariances computed by hccm()
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
r3<-report_factorial_anova(df=df,wid="id",dv=c("DV1","DV2"),
                           within=c("IV3","IV4"),within_full=c("IV3","IV4"),
                           between=c("IV1","IV2"),
                           within_covariates=NULL,between_covariates=NULL,
                           file="anova_mixed",
                           post_hoc=FALSE)
#> Warning: The column supplied as the wid variable contains non-unique values across levels of the supplied between-Ss variables. Automatically fixing this by generating unique wid labels.
#> Warning: Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> Warning: The column supplied as the wid variable contains non-unique values across levels of the supplied between-Ss variables. Automatically fixing this by generating unique wid labels.
#> Warning: Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
r4<-report_factorial_anova(df=df,wid="id",dv=c("DV1","DV2"),
                           within=c("IV1","IV2"),within_full=c("IV1","IV2"),
                           between=NULL,
                           within_covariates=c("DV3","DV4"),between_covariates=NULL,
                           file="anova_within_cov",
                           post_hoc=TRUE)
#> Warning: Implementation of ANCOVA in this version of ez is experimental and not yet fully validated. Also, note that ANCOVA is intended purely as a tool to increase statistical power; ANCOVA can not eliminate confounds in the data. Specifically, covariates should: (1) be uncorrelated with other predictors and (2) should have effects on the DV that are independent of other predictors. Failure to meet these conditions may dramatically increase the rate of false-positives.
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#> Warning: Implementation of ANCOVA in this version of ez is experimental and not yet fully validated. Also, note that ANCOVA is intended purely as a tool to increase statistical power; ANCOVA can not eliminate confounds in the data. Specifically, covariates should: (1) be uncorrelated with other predictors and (2) should have effects on the DV that are independent of other predictors. Failure to meet these conditions may dramatically increase the rate of false-positives.
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: contrasts dropped from factor ezCov due to missing levels
#> Warning: Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
```
