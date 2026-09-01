# Generate a data frame of random numbers

Creates a data frame populated with either normally or uniformly
distributed random values, useful for testing and simulation.

Creates a data frame populated with either normally or uniformly
distributed random values, useful for testing and simulation.

## Usage

``` r
generate_data(
  nrows = 10,
  ncols = 5,
  mean = 0,
  sd = 1,
  min = 1,
  max = 5,
  type = "normal"
)

generate_data(
  nrows = 10,
  ncols = 5,
  mean = 0,
  sd = 1,
  min = 1,
  max = 5,
  type = "normal"
)
```

## Arguments

- nrows:

  Integer. Number of rows to generate. Default is `10`.

- ncols:

  Integer. Number of columns to generate. Default is `5`.

- mean:

  Numeric. Mean of the normal distribution. Only used when
  `type = "normal"`. Default is `0`.

- sd:

  Numeric. Standard deviation of the normal distribution. Only used when
  `type = "normal"`. Default is `1`.

- min:

  Integer. Minimum value of the uniform distribution. Only used when
  `type = "uniform"`. Default is `1`.

- max:

  Integer. Maximum value of the uniform distribution. Only used when
  `type = "uniform"`. Default is `5`.

- type:

  Character. Distribution to sample from. One of `"normal"` or
  `"uniform"`. Default is `"normal"`.

## Value

A data frame with `nrows` rows and `ncols` columns of randomly generated
numeric values.

A data frame with `nrows` rows and `ncols` columns of randomly generated
numeric values.

## Examples

``` r
generate_data(nrows = 10, ncols = 5, mean = 0, sd = 1, type = "normal")
#>         X1      X2       X3       X4      X5
#> 1  -1.3665  1.2884  0.69182  0.85255  1.1515
#> 2  -0.3570  0.1400 -0.45729 -1.04514  0.9101
#> 3  -0.6981 -1.6281  1.13787  0.02722  1.7592
#> 4  -0.8304  1.6001  2.68642  1.53388  0.1239
#> 5  -0.7960  0.3068 -1.18675 -1.25198  0.1248
#> 6   2.2377 -0.2909  0.46368 -0.41974 -0.9420
#> 7   1.9493  1.0607  0.03259 -0.17063 -0.9759
#> 8  -0.7456  0.7209  0.72049  0.48061 -0.3787
#> 9   1.3433  2.7310 -1.23805 -0.94414  2.2673
#> 10 -0.9560  1.3839  0.10214 -1.36628 -0.1218
generate_data(nrows = 10, ncols = 5, min = 1, max = 5, type = "uniform")
#>    X1 X2 X3 X4 X5
#> 1   1  1  3  3  3
#> 2   5  3  1  5  1
#> 3   5  4  2  5  4
#> 4   5  4  1  5  3
#> 5   1  1  5  4  2
#> 6   5  4  1  4  3
#> 7   3  2  5  3  2
#> 8   3  2  4  2  2
#> 9   3  2  2  5  1
#> 10  4  3  5  2  2
generate_data(nrows = 10, ncols = 5, mean = 0, sd = 1, type = "normal")
#>         X1       X2      X3      X4      X5
#> 1  -0.3758  0.52006 -0.4257 -1.6507 -0.5532
#> 2  -0.2199 -0.10664 -0.7266 -0.5644 -0.2681
#> 3  -0.2040  1.32223  0.1539  0.8252 -1.8148
#> 4   0.1152  0.59758  0.4579  1.1517 -1.0327
#> 5  -1.4629  0.17504 -0.5990 -2.0526  1.0227
#> 6  -2.0034 -1.61863  1.2428  0.3260 -0.3709
#> 7   0.5972  1.59131 -1.0819  0.2984 -0.9079
#> 8   1.6038  0.03383 -0.9024  1.3690  1.2096
#> 9  -0.1801  0.35264 -0.8946 -0.7522  2.8539
#> 10  1.6866  0.02569 -1.0721  0.2913  1.7613
generate_data(nrows = 10, ncols = 5, min = 1, max = 5, type = "uniform")
#>    X1 X2 X3 X4 X5
#> 1   5  1  5  4  3
#> 2   4  4  5  2  4
#> 3   5  4  1  2  3
#> 4   2  3  3  5  1
#> 5   2  3  3  5  5
#> 6   2  1  3  4  3
#> 7   2  4  2  3  4
#> 8   3  3  2  1  4
#> 9   1  1  3  2  3
#> 10  5  4  2  2  1
```
