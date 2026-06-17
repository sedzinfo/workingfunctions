# Install and load multiple packages

Install and load multiple packages. If packages exist,they are loaded,if
packages don't exist,they are downloaded installed and loaded

## Usage

``` r
install_load(package)
```

## Arguments

- package:

  Vector Package names

## Author

Steven Worthington

## Examples

``` r
install_load("car")
#> Loading required package: car
#> Loading required package: carData
#>  car 
#> TRUE 
install_load(c("car","ggplot2"))
#>     car ggplot2 
#>    TRUE    TRUE 
```
