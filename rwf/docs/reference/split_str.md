# Split string to dataframe

Split string to dataframe

## Usage

``` r
split_str(vector, split = "/", include_original = FALSE)
```

## Arguments

- vector:

  String

- split:

  Separation character

- include_original:

  if TRUE it will return the input on a separate collumn

## Examples

``` r
string<-paste0(1:10,"/",
               generate_string(nchar=2,vector_length=10),"/",
               generate_string(nchar=2,vector_length=10),"/",
               generate_string(nchar=2,vector_length=10))
split_str(string,split="/")
#>    X1 X2 X3 X4
#> 1   1 Ij Mj tk
#> 2   2 Oz NA yQ
#> 3   3 YZ 7d zy
#> 4   4 5k fL nE
#> 5   5 8a qH jK
#> 6   6 Rk aa Ic
#> 7   7 Tx jr 5M
#> 8   8 Ur XH re
#> 9   9 KK fC 5M
#> 10 10 cM FO 2e
```
