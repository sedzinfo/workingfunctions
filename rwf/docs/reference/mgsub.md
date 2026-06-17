# Sub for multiple patterns

Sub for multiple patterns

## Usage

``` r
mgsub(mydata, pattern, replacement, ...)
```

## Arguments

- mydata:

  Character

- pattern:

  Character to search for

- replacement:

  Replacement character

- ...:

  arguments passed to gsub

## Examples

``` r
mgsub(mydata="#$%^&*_+",pattern=c("%","*"),"REPLACE",fixed=TRUE)
#> [1] "#$REPLACE^&REPLACE_+"
```
