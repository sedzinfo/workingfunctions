# Plot group means with error bars for all IV-DV combinations

For every combination of independent variable (IV) and dependent
variable (DV) supplied, produces a horizontal dot plot of group means
with optional error bars (standard error, confidence interval, or
standard deviation). Sample size per group is annotated on each panel.

When the number of IV-DV combinations exceeds four times the available
CPU cores the plots are produced in parallel via `future.apply`,
otherwise sequentially.

## Usage

``` r
plot_oneway(
  df,
  dv,
  iv,
  base_size = 20,
  type = "se",
  order_factor = TRUE,
  title = "",
  note = "",
  width = 60
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

- width:

  Integer. Character width at which long axis labels are wrapped.
  Default `60`.

## Value

A named list with three elements:

- `plot_data` — named list of summary data frames (one per IV-DV pair)
  as returned by
  [`Rmisc::summarySE`](https://rdrr.io/pkg/Rmisc/man/summarySE.html).

- `plot_data_df` — single data frame combining all summary data frames
  row-wise.

- `plots` — named list of ggplot objects (one per IV-DV pair).

All list elements are named `"iv_dv"`.

## Examples

``` r
nrows <- 1000
df <- data.frame(generate_factor(vector=LETTERS[1:5], nrows=nrows, ncols=10, type="random"),
                 generate_data(nrows=nrows, ncols=5, type="normal"))
result <- plot_oneway(df=df, dv=11:15, iv=1:10)

# Single IV, single DV
plot_oneway(df=mtcars, dv=2, iv=9)
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> 
#> $plot_data_df
#>   am   cyl  N    sd     se     ci
#> 1  0 6.947 19 1.545 0.3544 0.7445
#> 2  1 5.077 13 1.553 0.4306 0.9382
#> 
#> $plots
#> $plots$am_cyl

#> 
#> 

# Multiple IVs and DVs
plot_oneway(df=mtcars, dv=2:3, iv=9:10)
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 

# Error bar types
plot_oneway(df=mtcars, dv=2:3, iv=9:10, type="se")
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 
plot_oneway(df=mtcars, dv=2:3, iv=9:10, type="ci")
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 
plot_oneway(df=mtcars, dv=2:3, iv=9:10, type="sd")
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 
plot_oneway(df=mtcars, dv=2:3, iv=9:10, type="")
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 

# Factor ordering
plot_oneway(df=mtcars, dv=2:3, iv=9:10, type="", order_factor=FALSE)
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 
plot_oneway(df=mtcars, dv=2:3, iv=9:10, type="", order_factor=TRUE)
#> $plot_data
#> $plot_data$am_cyl
#>   am  N   cyl    sd     se     ci
#> 1  0 19 6.947 1.545 0.3544 0.7445
#> 2  1 13 5.077 1.553 0.4306 0.9382
#> 
#> $plot_data$gear_cyl
#>   gear  N   cyl     sd     se     ci
#> 1    3 15 7.467 1.1872 0.3065 0.6575
#> 2    4 12 4.667 0.9847 0.2843 0.6257
#> 3    5  5 6.000 2.0000 0.8944 2.4833
#> 
#> $plot_data$am_disp
#>   am  N  disp    sd    se   ci
#> 1  0 19 290.4 110.2 25.28 53.1
#> 2  1 13 143.5  87.2 24.19 52.7
#> 
#> $plot_data$gear_disp
#>   gear  N  disp     sd    se     ci
#> 1    3 15 326.3  94.85 24.49  52.53
#> 2    4 12 123.0  38.91 11.23  24.72
#> 3    5  5 202.5 115.49 51.65 143.40
#> 
#> 
#> $plot_data_df
#>      am gear   cyl  disp  N       sd      se       ci
#> 1     0 <NA> 6.947    NA 19   1.5447  0.3544   0.7445
#> 2     1 <NA> 5.077    NA 13   1.5525  0.4306   0.9382
#> 3  <NA>    3 7.467    NA 15   1.1872  0.3065   0.6575
#> 4  <NA>    4 4.667    NA 12   0.9847  0.2843   0.6257
#> 5  <NA>    5 6.000    NA  5   2.0000  0.8944   2.4833
#> 6     0 <NA>    NA 290.4 19 110.1716 25.2751  53.1010
#> 7     1 <NA>    NA 143.5 13  87.2040 24.1860  52.6968
#> 8  <NA>    3    NA 326.3 15  94.8527 24.4909  52.5277
#> 9  <NA>    4    NA 123.0 12  38.9093 11.2321  24.7218
#> 10 <NA>    5    NA 202.5  5 115.4906 51.6490 143.4006
#> 
#> $plots
#> $plots$am_cyl

#> 
#> $plots$gear_cyl

#> 
#> $plots$am_disp

#> 
#> $plots$gear_disp

#> 
#> 
```
