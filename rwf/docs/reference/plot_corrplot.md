# Correlation matrix plots

Correlation matrix plots

## Usage

``` r
plot_corrplot(mydata, title = "", base_size = 10, fill_limits = c(-1, 0, 1))
```

## Arguments

- mydata:

  correlation matrix

- title:

  plot title

- base_size:

  base font size

- fill_limits:

  lower and upper limit for fill

## Examples

``` r
plot_corrplot(stats::cor(mtcars),title="Correlation")

plot_corrplot(stats::cor(mtcars),base_size=20)
```
