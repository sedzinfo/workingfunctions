# Plot two-way interaction graphs for all IV pair and DV combinations

For every unique pair of independent variables (IV1 x IV2) and every
dependent variable (DV), produces a line-and-point interaction plot with
group means on the y-axis. IV1 levels appear on the x-axis (flipped) and
IV2 levels are represented by colour and line group. Optional error bars
and per-group sample size annotations are included.

When the number of combinations exceeds four times the available CPU
cores the plots are produced in parallel via `future.apply`, otherwise
sequentially.

## Usage

``` r
plot_interaction(
  df,
  dv,
  iv,
  base_size = 20,
  type = "se",
  order_factor = TRUE,
  title = "",
  note = ""
)
```

## Arguments

- df:

  A data frame containing both the independent and dependent variables.

- dv:

  Integer vector of column indices for the continuous dependent
  variables.

- iv:

  Integer vector of column indices for the categorical independent
  variables. Columns are coerced to factors automatically.

- base_size:

  Base font size in pt passed to `theme_bw`. Default `20`.

- type:

  Type of error bar to display. One of `"se"` (standard error), `"ci"`
  (95% confidence interval), `"sd"` (standard deviation), or `""` (no
  error bars). Default `"se"`.

- order_factor:

  Logical. If `TRUE` factor levels on the x-axis are sorted by the group
  mean of the DV (descending). Default `TRUE`.

- title:

  Character. Plot title applied to every panel. Default `""`.

- note:

  Character. Caption / footnote appended to every panel. Default `""`.

## Value

A named list with three elements:

- `plot_data` — named list of summary data frames (one per IV1-IV2-DV
  combination) as returned by
  [`Rmisc::summarySE`](https://rdrr.io/pkg/Rmisc/man/summarySE.html).

- `plot_data_df` — single data frame combining all summary data frames
  row-wise.

- `plots` — named list of ggplot objects (one per combination).

All list elements are named `"iv1_iv2_dv"`.

## Examples

``` r
# Single DV, two IVs
plot_interaction(df = mtcars, dv = 2, iv = 8:9, base_size = 20, type = "se")
#> $plot_data
#> $plot_data$am_vs_cyl
#>   am vs  N   cyl    sd     se     ci
#> 1  0  0 12 8.000 0.000 0.0000 0.0000
#> 2  0  1  7 5.143 1.069 0.4041 0.9887
#> 3  1  0  6 6.333 1.506 0.6146 1.5800
#> 4  1  1  7 4.000 0.000 0.0000 0.0000
#> 
#> $plot_data$vs_am_cyl
#>   vs am  N   cyl    sd     se     ci
#> 1  0  0 12 8.000 0.000 0.0000 0.0000
#> 2  0  1  6 6.333 1.506 0.6146 1.5800
#> 3  1  0  7 5.143 1.069 0.4041 0.9887
#> 4  1  1  7 4.000 0.000 0.0000 0.0000
#> 
#> 
#> $plot_data_df
#>   vs am   cyl  N    sd     se     ci
#> 1  0  0 8.000 12 0.000 0.0000 0.0000
#> 2  1  0 5.143  7 1.069 0.4041 0.9887
#> 3  0  1 6.333  6 1.506 0.6146 1.5800
#> 4  1  1 4.000  7 0.000 0.0000 0.0000
#> 5  0  0 8.000 12 0.000 0.0000 0.0000
#> 6  0  1 6.333  6 1.506 0.6146 1.5800
#> 7  1  0 5.143  7 1.069 0.4041 0.9887
#> 8  1  1 4.000  7 0.000 0.0000 0.0000
#> 
#> $plots
#> $plots$am_vs_cyl

#> 
#> $plots$vs_am_cyl

#> 
#> 

# Multiple DVs, two IVs
plot_interaction(df = mtcars, dv = 2:3, iv = 8:9, base_size = 20, type = "se")
#> $plot_data
#> $plot_data$am_vs_cyl
#>   am vs  N   cyl    sd     se     ci
#> 1  0  0 12 8.000 0.000 0.0000 0.0000
#> 2  0  1  7 5.143 1.069 0.4041 0.9887
#> 3  1  0  6 6.333 1.506 0.6146 1.5800
#> 4  1  1  7 4.000 0.000 0.0000 0.0000
#> 
#> $plot_data$vs_am_cyl
#>   vs am  N   cyl    sd     se     ci
#> 1  0  0 12 8.000 0.000 0.0000 0.0000
#> 2  0  1  6 6.333 1.506 0.6146 1.5800
#> 3  1  0  7 5.143 1.069 0.4041 0.9887
#> 4  1  1  7 4.000 0.000 0.0000 0.0000
#> 
#> $plot_data$am_vs_disp
#>   am vs  N  disp    sd     se    ci
#> 1  0  0 12 357.6 71.82 20.734 45.63
#> 2  0  1  7 175.1 49.13 18.570 45.44
#> 3  1  0  6 206.2 95.23 38.879 99.94
#> 4  1  1  7  89.8 18.80  7.107 17.39
#> 
#> $plot_data$vs_am_disp
#>   vs am  N  disp    sd     se    ci
#> 1  0  0 12 357.6 71.82 20.734 45.63
#> 2  0  1  6 206.2 95.23 38.879 99.94
#> 3  1  0  7 175.1 49.13 18.570 45.44
#> 4  1  1  7  89.8 18.80  7.107 17.39
#> 
#> 
#> $plot_data_df
#>    vs am   cyl  disp  N     sd      se      ci
#> 1   0  0 8.000    NA 12  0.000  0.0000  0.0000
#> 2   1  0 5.143    NA  7  1.069  0.4041  0.9887
#> 3   0  1 6.333    NA  6  1.506  0.6146  1.5800
#> 4   1  1 4.000    NA  7  0.000  0.0000  0.0000
#> 5   0  0 8.000    NA 12  0.000  0.0000  0.0000
#> 6   0  1 6.333    NA  6  1.506  0.6146  1.5800
#> 7   1  0 5.143    NA  7  1.069  0.4041  0.9887
#> 8   1  1 4.000    NA  7  0.000  0.0000  0.0000
#> 9   0  0    NA 357.6 12 71.823 20.7337 45.6345
#> 10  1  0    NA 175.1  7 49.131 18.5697 45.4383
#> 11  0  1    NA 206.2  6 95.234 38.8790 99.9416
#> 12  1  1    NA  89.8  7 18.802  7.1065 17.3891
#> 13  0  0    NA 357.6 12 71.823 20.7337 45.6345
#> 14  0  1    NA 206.2  6 95.234 38.8790 99.9416
#> 15  1  0    NA 175.1  7 49.131 18.5697 45.4383
#> 16  1  1    NA  89.8  7 18.802  7.1065 17.3891
#> 
#> $plots
#> $plots$am_vs_cyl

#> 
#> $plots$vs_am_cyl

#> 
#> $plots$am_vs_disp

#> 
#> $plots$vs_am_disp

#> 
#> 
plot_interaction(df = mtcars, dv = 2:3, iv = 8:9, base_size = 20, type = "ci")
#> $plot_data
#> $plot_data$am_vs_cyl
#>   am vs  N   cyl    sd     se     ci
#> 1  0  0 12 8.000 0.000 0.0000 0.0000
#> 2  0  1  7 5.143 1.069 0.4041 0.9887
#> 3  1  0  6 6.333 1.506 0.6146 1.5800
#> 4  1  1  7 4.000 0.000 0.0000 0.0000
#> 
#> $plot_data$vs_am_cyl
#>   vs am  N   cyl    sd     se     ci
#> 1  0  0 12 8.000 0.000 0.0000 0.0000
#> 2  0  1  6 6.333 1.506 0.6146 1.5800
#> 3  1  0  7 5.143 1.069 0.4041 0.9887
#> 4  1  1  7 4.000 0.000 0.0000 0.0000
#> 
#> $plot_data$am_vs_disp
#>   am vs  N  disp    sd     se    ci
#> 1  0  0 12 357.6 71.82 20.734 45.63
#> 2  0  1  7 175.1 49.13 18.570 45.44
#> 3  1  0  6 206.2 95.23 38.879 99.94
#> 4  1  1  7  89.8 18.80  7.107 17.39
#> 
#> $plot_data$vs_am_disp
#>   vs am  N  disp    sd     se    ci
#> 1  0  0 12 357.6 71.82 20.734 45.63
#> 2  0  1  6 206.2 95.23 38.879 99.94
#> 3  1  0  7 175.1 49.13 18.570 45.44
#> 4  1  1  7  89.8 18.80  7.107 17.39
#> 
#> 
#> $plot_data_df
#>    vs am   cyl  disp  N     sd      se      ci
#> 1   0  0 8.000    NA 12  0.000  0.0000  0.0000
#> 2   1  0 5.143    NA  7  1.069  0.4041  0.9887
#> 3   0  1 6.333    NA  6  1.506  0.6146  1.5800
#> 4   1  1 4.000    NA  7  0.000  0.0000  0.0000
#> 5   0  0 8.000    NA 12  0.000  0.0000  0.0000
#> 6   0  1 6.333    NA  6  1.506  0.6146  1.5800
#> 7   1  0 5.143    NA  7  1.069  0.4041  0.9887
#> 8   1  1 4.000    NA  7  0.000  0.0000  0.0000
#> 9   0  0    NA 357.6 12 71.823 20.7337 45.6345
#> 10  1  0    NA 175.1  7 49.131 18.5697 45.4383
#> 11  0  1    NA 206.2  6 95.234 38.8790 99.9416
#> 12  1  1    NA  89.8  7 18.802  7.1065 17.3891
#> 13  0  0    NA 357.6 12 71.823 20.7337 45.6345
#> 14  0  1    NA 206.2  6 95.234 38.8790 99.9416
#> 15  1  0    NA 175.1  7 49.131 18.5697 45.4383
#> 16  1  1    NA  89.8  7 18.802  7.1065 17.3891
#> 
#> $plots
#> $plots$am_vs_cyl

#> 
#> $plots$vs_am_cyl

#> 
#> $plots$am_vs_disp

#> 
#> $plots$vs_am_disp

#> 
#> 
plot_interaction(df = mtcars, dv = 2:3, iv = 9:10, base_size = 20, type = "sd")
#> $plot_data
#> $plot_data$gear_am_cyl
#>   gear am  N   cyl     sd     se     ci
#> 1    3  0 15 7.467 1.1872 0.3065 0.6575
#> 2    4  0  4 5.000 1.1547 0.5774 1.8374
#> 3    4  1  8 4.500 0.9258 0.3273 0.7740
#> 4    5  1  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_gear_cyl
#>   am gear  N   cyl     sd     se     ci
#> 1  0    3 15 7.467 1.1872 0.3065 0.6575
#> 2  0    4  4 5.000 1.1547 0.5774 1.8374
#> 3  1    4  8 4.500 0.9258 0.3273 0.7740
#> 4  1    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$gear_am_disp
#>   gear am  N  disp     sd     se     ci
#> 1    3  0 15 326.3  94.85 24.491  52.53
#> 2    4  0  4 155.7  13.98  6.989  22.24
#> 3    4  1  8 106.7  37.16 13.139  31.07
#> 4    5  1  5 202.5 115.49 51.649 143.40
#> 
#> $plot_data$am_gear_disp
#>   am gear  N  disp     sd     se     ci
#> 1  0    3 15 326.3  94.85 24.491  52.53
#> 2  0    4  4 155.7  13.98  6.989  22.24
#> 3  1    4  8 106.7  37.16 13.139  31.07
#> 4  1    5  5 202.5 115.49 51.649 143.40
#> 
#> 
#> $plot_data_df
#>    am gear   cyl  disp  N       sd      se       ci
#> 1   0    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 2   0    4 5.000    NA  4   1.1547  0.5774   1.8374
#> 3   1    4 4.500    NA  8   0.9258  0.3273   0.7740
#> 4   1    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 5   0    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 6   0    4 5.000    NA  4   1.1547  0.5774   1.8374
#> 7   1    4 4.500    NA  8   0.9258  0.3273   0.7740
#> 8   1    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 9   0    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 10  0    4    NA 155.7  4  13.9789  6.9894  22.2435
#> 11  1    4    NA 106.7  8  37.1630 13.1391  31.0690
#> 12  1    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 13  0    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 14  0    4    NA 155.7  4  13.9789  6.9894  22.2435
#> 15  1    4    NA 106.7  8  37.1630 13.1391  31.0690
#> 16  1    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$gear_am_cyl

#> 
#> $plots$am_gear_cyl

#> 
#> $plots$gear_am_disp

#> 
#> $plots$am_gear_disp

#> 
#> 

# No error bars, unordered factor axis
plot_interaction(df = mtcars, dv = 2, iv = 9:10, base_size = 20, type = "", order_factor = FALSE)
#> $plot_data
#> $plot_data$gear_am_cyl
#>   gear am  N   cyl     sd     se     ci
#> 1    3  0 15 7.467 1.1872 0.3065 0.6575
#> 2    4  0  4 5.000 1.1547 0.5774 1.8374
#> 3    4  1  8 4.500 0.9258 0.3273 0.7740
#> 4    5  1  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_gear_cyl
#>   am gear  N   cyl     sd     se     ci
#> 1  0    3 15 7.467 1.1872 0.3065 0.6575
#> 2  0    4  4 5.000 1.1547 0.5774 1.8374
#> 3  1    4  8 4.500 0.9258 0.3273 0.7740
#> 4  1    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> 
#> $plot_data_df
#>   am gear   cyl  N     sd     se     ci
#> 1  0    3 7.467 15 1.1872 0.3065 0.6575
#> 2  0    4 5.000  4 1.1547 0.5774 1.8374
#> 3  1    4 4.500  8 0.9258 0.3273 0.7740
#> 4  1    5 6.000  5 2.0000 0.8944 2.4833
#> 5  0    3 7.467 15 1.1872 0.3065 0.6575
#> 6  0    4 5.000  4 1.1547 0.5774 1.8374
#> 7  1    4 4.500  8 0.9258 0.3273 0.7740
#> 8  1    5 6.000  5 2.0000 0.8944 2.4833
#> 
#> $plots
#> $plots$gear_am_cyl

#> 
#> $plots$am_gear_cyl

#> 
#> 
```
