# Score Multiple Thurstonian IRT Response Patterns (MAP / EBM)

Scores many respondents at once using Thurstonian IRT parameters and
returns MAP (empirical Bayes modal) latent trait estimates for each row
in patterns.

In simple terms: this function applies score_tirt_pattern to every
respondent, after first checking and aligning item columns so they match
lambda row order.

## Usage

``` r
score_tirt(patterns, lambda, theta_diag, tau, Psi, nu = NULL)
```

## Arguments

- patterns:

  A matrix or data.frame of response patterns (rows = respondents,
  columns = pair/items).

- lambda:

  Loading matrix (rows = pair/items, columns = latent traits).

- theta_diag:

  Numeric vector of residual variances aligned to rows of lambda.

- tau:

  Numeric vector of thresholds aligned to rows of lambda.

- Psi:

  Latent covariance matrix (traits x traits).

- nu:

  Optional numeric vector of indicator intercepts aligned to rows of
  lambda. If NULL, zeros are used in score_tirt_pattern.

## Value

A numeric matrix of latent scores: rows correspond to respondents in
patterns, columns correspond to traits in colnames(lambda).

## Details

Name alignment is the key safeguard: if both rownames(lambda) and
colnames(patterns) are present, patterns is reordered to match lambda
row order before scoring.

If required lambda names are missing from patterns, the function stops
with an informative error. If names are unavailable, positional
alignment is assumed and a warning is issued.

## Examples

``` r
library(thurstonianIRT)
#> Error in library(thurstonianIRT): there is no package called 'thurstonianIRT'
data("triplets")
#> Warning: data set 'triplets' not found
# define the blocks of items
blocks <-
  set_block(c("i1", "i2", "i3"), traits = c("t1", "t2", "t3"),
            signs = c(1, 1, 1)) +
  set_block(c("i4", "i5", "i6"), traits = c("t1", "t2", "t3"),
            signs = c(-1, 1, 1)) +
  set_block(c("i7", "i8", "i9"), traits = c("t1", "t2", "t3"),
            signs = c(1, 1, -1)) +
  set_block(c("i10", "i11", "i12"), traits = c("t1", "t2", "t3"),
            signs = c(1, -1, 1))
#> Error in set_block(c("i1", "i2", "i3"), traits = c("t1", "t2", "t3"),     signs = c(1, 1, 1)): could not find function "set_block"
# generate the data to be understood by 'thurstonianIRT'
triplets_long <- make_TIRT_data(
  data = triplets, blocks = blocks, direction = "larger",
  format = "pairwise", family = "bernoulli", range = c(0, 1)
)
#> Error in make_TIRT_data(data = triplets, blocks = blocks, direction = "larger",     format = "pairwise", family = "bernoulli", range = c(0, 1)): could not find function "make_TIRT_data"
# fit the data using lavaan
fit <- fit_TIRT_lavaan(triplets_long)
#> Error in fit_TIRT_lavaan(triplets_long): could not find function "fit_TIRT_lavaan"
pars <- extract_tirt_params(fit)
#> Error: object 'fit' not found
patterns<-as.matrix(triplets)
#> Error: object 'triplets' not found
score_tirt(patterns,lambda=pars$lambda,theta_diag=pars$theta_diag,
           tau=pars$tau,Psi=pars$Psi,nu=NULL)
#> Error: object 'patterns' not found
# Check same scores from thurstonianIRT package
triplets_long <- make_TIRT_data(data = triplets,
                                blocks = blocks, 
                                direction = "larger",
                                format = "pairwise", 
                                family = "bernoulli", 
                                range = c(0, 1))
#> Error in make_TIRT_data(data = triplets, blocks = blocks, direction = "larger",     format = "pairwise", family = "bernoulli", range = c(0, 1)): could not find function "make_TIRT_data"
scores_thurstonianIRT<-predict(fit)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'object' in selecting a method for function 'predict': object 'fit' not found
scores<-score_tirt(patterns,lambda=pars$lambda,theta_diag=pars$theta_diag,
        tau=pars$tau,Psi=pars$Psi,nu=NULL)
#> Error: object 'patterns' not found
head(reshape2::recast(scores_thurstonianIRT,formula=id~trait,id.var=1:2))
#> Error: object 'scores_thurstonianIRT' not found
head(scores)
#> Error: object 'scores' not found
```
