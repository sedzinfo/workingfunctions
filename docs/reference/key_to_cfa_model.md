# Convert a key list to a lavaan CFA model string

Converts the named list key format used by
[`report_alpha`](https://sedzinfo.github.io/rwf/reference/report_alpha.md)
into a lavaan model syntax string suitable for passing directly to
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). Each list
element becomes one factor definition line of the form
`factorname =~ item1+item2+...`.

## Usage

``` r
key_to_cfa_model(key)
```

## Arguments

- key:

  A named list where each name is a factor (trait) label and each
  element is a character vector of item column names belonging to that
  factor, e.g. `list(f1 = c("x1","x2","x3"), f2 = c("x4","x5","x6"))`.

## Value

A single character string containing the full lavaan model specification
with one factor definition per line.

## Examples

``` r
population_model <- "t1=~x1+.5*x2+.5*x3
                     t2=~x4+.5*x5+.5*x6
                     t3=~x7+.5*x8+.5*x9"
model_data <- lavaan::simulateData(population_model, sample.nobs = 1000)
key <- list(f1 = paste0("x", 1:3), f2 = paste0("x", 4:6), f3 = paste0("x", 7:9))
model <- key_to_cfa_model(key)
fit <- lavaan::cfa(model, model_data)
```
