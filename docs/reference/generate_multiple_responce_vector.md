# Generate multiple responce vector

Generate multiple responce vector

## Usage

``` r
generate_multiple_responce_vector(
  responces = 1:4,
  responded = 1:4,
  length = 10
)
```

## Arguments

- responces:

  unique categories allowed

- responded:

  number of categories observed in iteration

- length:

  length of returned vector

## Examples

``` r
generate_multiple_responce_vector(responces=1:4,responded=1:4,length=10)
#>  [1] "3"          "3, 4, 2, 1" "1, 2"       "1"          "3, 2, 4, 1" "2"          "1, 4"       "2, 4, 1"    "3, 4, 2, 1" "2, 1"      
```
