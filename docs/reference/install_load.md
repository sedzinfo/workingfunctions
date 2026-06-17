# Install and load multiple packages

Checks whether each package in `package` is already installed. Missing
packages are downloaded from CRAN with `dependencies = TRUE` and then
loaded. Packages that are already installed are loaded directly without
reinstalling.

## Usage

``` r
install_load(package)
```

## Arguments

- package:

  Character vector of package names to install (if needed) and load.

## Value

A named logical vector (one element per package) indicating whether each
package was successfully attached: `TRUE` if loaded, `FALSE` if loading
failed.

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
