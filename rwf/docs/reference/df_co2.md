# Carbon Dioxide Uptake in Grass Plants

The CO2 data frame has 84 rows and 5 columns of data from an experiment
on the cold tolerance of the grass species Echinochloa crus-galli.

## Usage

``` r
df_co2
```

## Format

A data frame with 84 rows and 5 variables:

- Plant:

  an ordered factor with levels Qn1 \< Qn2 \< Qn3 \< ... \< Mc1 giving a
  unique identifier for each plant). Used as a grouping factor.

- Type:

  a factor with levels Quebec Mississippi giving the origin of the plant

- Treatment:

  a factor with levels nonchilled chilled

- conc:

  a numeric vector of ambient carbon dioxide concentrations (mL/L)

- uptake:

  a numeric vector of carbon dioxide uptake rates (in \\\mu\\mol/m²/sec)

## Source

Potvin, C., Lechowicz, M. J. and Tardif, S. (1990) “The statistical
analysis of ecophysiological response curves obtained from experiments
involving repeated measures”, Ecology, 71, 1389–1400. Pinheiro, J. C.
and Bates, D. M. (2000) Mixed-effects Models in S and S-PLUS, Springer.

## Details

Grouped formulas like `uptake ~ conc | Plant` are useful in lattice
graphics and mixed-effect models. The vertical bar (\`\|\`) separates
the grouping variable. This allows modeling or plotting the response
(`uptake`) versus the predictor (`conc`) within each level of `Plant`.

## Examples

``` r
data(df_co2)
head(df_co2)
#>   Plant   Type  Treatment conc uptake
#> 1   Qn1 Quebec nonchilled   95   16.0
#> 2   Qn1 Quebec nonchilled  175   30.4
#> 3   Qn1 Quebec nonchilled  250   34.8
#> 4   Qn1 Quebec nonchilled  350   37.2
#> 5   Qn1 Quebec nonchilled  500   35.3
#> 6   Qn1 Quebec nonchilled  675   39.2
```
