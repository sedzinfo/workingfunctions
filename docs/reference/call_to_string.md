# Convert a model call to a compact string

Extracts the call from a model object and returns it as a single
whitespace-free string. Tries `model$call` first, falling back to
`model$Call` if the first is `NULL`.

## Usage

``` r
call_to_string(model)
```

## Arguments

- model:

  A model object with a `call` or `Call` element (e.g. from `lm`, `glm`,
  `coxph`).

## Value

A character scalar with the model call, whitespace removed.

## Examples

``` r
df <- generate_correlation_matrix()
model <- lm(df$X1 ~ df$X2)
call_to_string(model)
#> [1] "lm(formula=df$X1~df$X2)"
```
