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
#>          X1      X2       X3       X4       X5
#> 1   0.60323  0.4974 -1.65618  0.12813  1.31782
#> 2   1.31574  0.9144  0.04342 -0.35905  0.06451
#> 3   0.68472  0.2075 -0.92541 -0.04561 -0.70391
#> 4  -0.61247 -0.2546 -0.25186  1.08405  0.67851
#> 5  -1.38907 -1.8324 -1.51694  1.16788 -0.30463
#> 6  -0.24243  0.7359 -0.50498  1.10878 -1.77363
#> 7  -0.20802 -0.3318 -0.18359 -1.36490 -0.40189
#> 8   0.37117  0.8524  0.71797  1.82302 -0.30290
#> 9   0.09241 -0.5918  0.63670  0.12676 -0.18661
#> 10  2.12522 -1.1416 -0.55386 -1.00118  0.87192
generate_data(nrows = 10, ncols = 5, min = 1, max = 5, type = "uniform")
#>    X1 X2 X3 X4 X5
#> 1   5  1  5  5  1
#> 2   3  1  3  2  5
#> 3   2  5  1  4  4
#> 4   5  2  2  1  4
#> 5   2  1  3  2  3
#> 6   2  5  3  2  3
#> 7   2  4  3  1  1
#> 8   2  4  1  2  3
#> 9   5  4  1  4  5
#> 10  1  5  3  1  4
generate_data(nrows = 10, ncols = 5, mean = 0, sd = 1, type = "normal")
#>          X1      X2       X3      X4        X5
#> 1  -1.63315  0.5505 -0.16215  0.6101 -0.242534
#> 2   0.04037 -0.4649  0.63739  1.7577 -0.876454
#> 3   1.69609 -2.1686 -0.12470  0.9945  0.844466
#> 4  -0.71669 -0.7662  0.31947  0.5154  0.795040
#> 5   1.36476  0.4301 -0.02466  1.5829  0.008519
#> 6  -0.84653  1.0715 -0.75010 -0.5658  0.939763
#> 7  -0.56355 -1.1797  1.40135  0.3101 -0.490114
#> 8  -0.42507  0.7687  0.38016 -0.3917  0.475741
#> 9  -0.94468  0.4975 -1.36661  0.3042  0.627421
#> 10  0.52944 -1.0144 -1.08956  1.2782  0.712198
generate_data(nrows = 10, ncols = 5, min = 1, max = 5, type = "uniform")
#>    X1 X2 X3 X4 X5
#> 1   1  1  2  5  1
#> 2   1  5  1  4  4
#> 3   1  2  3  2  3
#> 4   2  2  4  2  1
#> 5   2  4  2  5  5
#> 6   3  2  1  3  3
#> 7   4  2  2  4  4
#> 8   5  5  2  4  4
#> 9   3  4  4  2  1
#> 10  2  1  2  4  1
```
