# Shrout-Fleiss reliability coefficients

Computes five reliability coefficients from the Shrout and Fleiss (1979)
framework using variance components extracted from a person × item ×
time mixed model (see
[`extract_components`](https://sedzinfo.github.io/rwf/reference/extract_components.md)).
The coefficients cover between-person and within-person reliability
under both fixed and random time designs, and are appropriate for
longitudinal or repeated-measures measurement studies.

## Usage

``` r
shrout(sperson, spersonitem, stime, spersontime, serror, m, k)
```

## Arguments

- sperson:

  Variance component of the person main effect.

- spersonitem:

  Variance component of the person × item interaction.

- stime:

  Variance component of the time main effect.

- spersontime:

  Variance component of the person × time interaction.

- serror:

  Variance component of residual error.

- m:

  Number of items (reports) averaged over.

- k:

  Number of time points averaged over.

## Value

A data frame with one row per reliability coefficient containing columns
`measure`, `result`, and `description`:

- r1f:

  Between-person reliability of a single measure at fixed time points.

- r1r:

  Between-person reliability of a single measure at random time points
  (different people, different days).

- rkf:

  Between-person reliability of scores averaged over `m` items and `k`
  fixed time points.

- rkr:

  Between-person reliability of scores averaged over `k` random time
  points.

- rc:

  Within-person reliability of change across time points.

## References

Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
assessing rater reliability. *Psychological Bulletin, 86*(2), 420–428.

## Examples

``` r
design<-expand.grid(time=1:3,item=1:2,person=1:10)
design<-change_data_type(design,type="factor")
design$response<-rnorm(30,0,0.1)
model<-mixlm::lm(response~r(time)*r(person)+r(item)*r(person),data=design)
result<-extract_components(model)
vc<-result$components
shrout(sperson=vc[2,3],spersonitem=vc[5,3],stime=vc[1,3],
       spersontime=vc[4,3],serror=vc[6,3],3,3)
#>   measure result                                                                                         description
#> 1     r1f 0.3233                            Reliability (between persons) of measures taken on the same fixed k time
#> 2     r1r 0.2841                           Reliability (between persons) of measures taken on the same random k time
#> 3      rc 0.1654                                                              Reliability (within persons) of change
#> 4     rkf 0.5891        Reliability (between persons) of average measures taken over fixed m items and fixed k times
#> 5     rkr 0.5435 Reliability (between persons) of different random time with same number of points k between periods
```
