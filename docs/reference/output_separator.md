# Output separator

Heading, main output, and instructions for output for the console
environment

## Usage

``` r
output_separator(
  string,
  output = NULL,
  instruction = NULL,
  length = getOption("width")/2
)
```

## Arguments

- string:

  Title of output

- output:

  object to print

- instruction:

  Character provided instructions regarding the output

- length:

  Numeric Length of separator measured in number of characters

## Examples

``` r
output_separator(string="TEST",output="TEST",instruction="TEST",length=100)
#> [1] "####################################################################################################"
#> [1] "TEST"
#> [1] "####################################################################################################"
#> [1] "TEST"
#> [1] "##################################################"
#> [1] "TEST"
output_separator(string="TEST",instruction="TEST",length=100)
#> [1] "####################################################################################################"
#> [1] "TEST"
#> [1] "####################################################################################################"
#> [1] "TEST"
#> [1] "##################################################"
output_separator(string="TEST",output="TEST",length=100)
#> [1] "####################################################################################################"
#> [1] "TEST"
#> [1] "####################################################################################################"
#> [1] "TEST"
output_separator(string="TEST")
#> [1] "####################################################################################################"
#> [1] "TEST"
#> [1] "####################################################################################################"
```
