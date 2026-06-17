# dataframe data type transformations

dataframe data type transformations

## Usage

``` r
change_data_type(df, type)
```

## Arguments

- df:

  dataframe

- type:

  "character" "numeric" "factor" "factor_character" "character_factor"  
  For "factor_character" if factors are found, are converted to
  characters  
  For "character_factor" if characters are found, are converted to
  factors

## Examples

``` r
cdf(df=change_data_type(df=mtcars,"character"))
#> $summary
#>   COLLUMNS ROWS TOTAL EMPTY null NAN na INF FIN FACTOR
#> 1       11   32   352     0    0   0  0   0   0      0
#> 
#> $check
#>    NAMES EMPTY null na NOT_NA NAN INF FIN RANGE MEAN MEDIAN SD  MIN  MAX
#> 1    mpg     0    0  0     32   0   0   0    25   NA     NA NA 10.4 33.9
#> 2    cyl     0    0  0     32   0   0   0     3   NA     NA NA    4    8
#> 3   disp     0    0  0     32   0   0   0    27   NA     NA NA 71.1  472
#> 4     hp     0    0  0     32   0   0   0    22   NA     NA NA   52  335
#> 5   drat     0    0  0     32   0   0   0    22   NA     NA NA 2.76 4.93
#> 6     wt     0    0  0     32   0   0   0    29   NA     NA NA 1.51 5.42
#> 7   qsec     0    0  0     32   0   0   0    30   NA     NA NA 14.5 22.9
#> 8     vs     0    0  0     32   0   0   0     2   NA     NA NA    0    1
#> 9     am     0    0  0     32   0   0   0     2   NA     NA NA    0    1
#> 10  gear     0    0  0     32   0   0   0     3   NA     NA NA    3    5
#> 11  carb     0    0  0     32   0   0   0     6   NA     NA NA    1    8
#>         MODE      TYPE     CLASS FACTOR
#> 1  character character character  FALSE
#> 2  character character character  FALSE
#> 3  character character character  FALSE
#> 4  character character character  FALSE
#> 5  character character character  FALSE
#> 6  character character character  FALSE
#> 7  character character character  FALSE
#> 8  character character character  FALSE
#> 9  character character character  FALSE
#> 10 character character character  FALSE
#> 11 character character character  FALSE
#> 
cdf(df=change_data_type(df=mtcars,"numeric"))
#> $summary
#>   COLLUMNS ROWS TOTAL EMPTY null NAN na INF FIN FACTOR
#> 1       11   32   352     0    0   0  0   0 352      0
#> 
#> $check
#>    NAMES EMPTY null na NOT_NA NAN INF FIN RANGE   MEAN MEDIAN     SD  MIN  MAX
#> 1    mpg     0    0  0     32   0   0  32    25  20.09   19.2   6.03 10.4 33.9
#> 2    cyl     0    0  0     32   0   0  32     3   6.19      6   1.79    4    8
#> 3   disp     0    0  0     32   0   0  32    27 230.72  196.3 123.94 71.1  472
#> 4     hp     0    0  0     32   0   0  32    22 146.69    123  68.56   52  335
#> 5   drat     0    0  0     32   0   0  32    22    3.6    3.7   0.53 2.76 4.93
#> 6     wt     0    0  0     32   0   0  32    29   3.22   3.33   0.98 1.51 5.42
#> 7   qsec     0    0  0     32   0   0  32    30  17.85  17.71   1.79 14.5 22.9
#> 8     vs     0    0  0     32   0   0  32     2   0.44      0    0.5    0    1
#> 9     am     0    0  0     32   0   0  32     2   0.41      0    0.5    0    1
#> 10  gear     0    0  0     32   0   0  32     3   3.69      4   0.74    3    5
#> 11  carb     0    0  0     32   0   0  32     6   2.81      2   1.62    1    8
#>       MODE   TYPE   CLASS FACTOR
#> 1  numeric double numeric  FALSE
#> 2  numeric double numeric  FALSE
#> 3  numeric double numeric  FALSE
#> 4  numeric double numeric  FALSE
#> 5  numeric double numeric  FALSE
#> 6  numeric double numeric  FALSE
#> 7  numeric double numeric  FALSE
#> 8  numeric double numeric  FALSE
#> 9  numeric double numeric  FALSE
#> 10 numeric double numeric  FALSE
#> 11 numeric double numeric  FALSE
#> 
cdf(df=change_data_type(df=mtcars,"factor"))
#> $summary
#>   COLLUMNS ROWS TOTAL EMPTY null NAN na INF FIN FACTOR
#> 1       11   32   352     0    0   0  0   0 352     11
#> 
#> $check
#>    NAMES EMPTY null na NOT_NA NAN INF FIN RANGE MEAN MEDIAN SD  MIN  MAX
#> 1    mpg     0    0  0     32   0   0  32    25   NA     NA NA 10.4 33.9
#> 2    cyl     0    0  0     32   0   0  32     3   NA     NA NA    4    8
#> 3   disp     0    0  0     32   0   0  32    27   NA     NA NA 71.1  472
#> 4     hp     0    0  0     32   0   0  32    22   NA     NA NA   52  335
#> 5   drat     0    0  0     32   0   0  32    22   NA     NA NA 2.76 4.93
#> 6     wt     0    0  0     32   0   0  32    29   NA     NA NA 1.51 5.42
#> 7   qsec     0    0  0     32   0   0  32    30   NA     NA NA 14.5 22.9
#> 8     vs     0    0  0     32   0   0  32     2   NA     NA NA    0    1
#> 9     am     0    0  0     32   0   0  32     2   NA     NA NA    0    1
#> 10  gear     0    0  0     32   0   0  32     3   NA     NA NA    3    5
#> 11  carb     0    0  0     32   0   0  32     6   NA     NA NA    1    8
#>       MODE    TYPE  CLASS FACTOR
#> 1  numeric integer factor   TRUE
#> 2  numeric integer factor   TRUE
#> 3  numeric integer factor   TRUE
#> 4  numeric integer factor   TRUE
#> 5  numeric integer factor   TRUE
#> 6  numeric integer factor   TRUE
#> 7  numeric integer factor   TRUE
#> 8  numeric integer factor   TRUE
#> 9  numeric integer factor   TRUE
#> 10 numeric integer factor   TRUE
#> 11 numeric integer factor   TRUE
#> 
df<-change_data_type(df=mtcars,"factor")
cdf(df=change_data_type(df=df,"factor_character"))
#> $summary
#>   COLLUMNS ROWS TOTAL EMPTY null NAN na INF FIN FACTOR
#> 1       11   32   352     0    0   0  0   0   0      0
#> 
#> $check
#>    NAMES EMPTY null na NOT_NA NAN INF FIN RANGE MEAN MEDIAN SD  MIN  MAX
#> 1    mpg     0    0  0     32   0   0   0    25   NA     NA NA 10.4 33.9
#> 2    cyl     0    0  0     32   0   0   0     3   NA     NA NA    4    8
#> 3   disp     0    0  0     32   0   0   0    27   NA     NA NA 71.1  472
#> 4     hp     0    0  0     32   0   0   0    22   NA     NA NA   52  335
#> 5   drat     0    0  0     32   0   0   0    22   NA     NA NA 2.76 4.93
#> 6     wt     0    0  0     32   0   0   0    29   NA     NA NA 1.51 5.42
#> 7   qsec     0    0  0     32   0   0   0    30   NA     NA NA 14.5 22.9
#> 8     vs     0    0  0     32   0   0   0     2   NA     NA NA    0    1
#> 9     am     0    0  0     32   0   0   0     2   NA     NA NA    0    1
#> 10  gear     0    0  0     32   0   0   0     3   NA     NA NA    3    5
#> 11  carb     0    0  0     32   0   0   0     6   NA     NA NA    1    8
#>         MODE      TYPE     CLASS FACTOR
#> 1  character character character  FALSE
#> 2  character character character  FALSE
#> 3  character character character  FALSE
#> 4  character character character  FALSE
#> 5  character character character  FALSE
#> 6  character character character  FALSE
#> 7  character character character  FALSE
#> 8  character character character  FALSE
#> 9  character character character  FALSE
#> 10 character character character  FALSE
#> 11 character character character  FALSE
#> 
```
