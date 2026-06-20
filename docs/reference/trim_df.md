# Trim whitespace from all character cells in a data frame

Applies [`strwrap`](https://rdrr.io/r/base/strwrap.html) to every
character cell in a data frame, removing leading and trailing
whitespace.

## Usage

``` r
trim_df(df)
```

## Arguments

- df:

  A data frame containing one or more character columns.

## Value

A data frame of the same dimensions with whitespace trimmed from all
character cells. Non-character cells are unchanged.

## Examples

``` r
string<-data.frame(str1=rep(paste0(sample(c(LETTERS,rep(" ",10))),collapse=""),10),
                   str2=rep(paste0(sample(c(LETTERS,rep(" ",10))),collapse=""),10),
                   num1=rnorm(10),
                   stringsAsFactors=FALSE)
trim_df(string)
#>                                str1                                str2    num1
#> 1  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF -1.2283
#> 2  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF -1.1729
#> 3  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF  0.7905
#> 4  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF  0.6196
#> 5  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF  1.0901
#> 6  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF  0.2183
#> 7  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF -2.3666
#> 8  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF -0.9299
#> 9  P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF  0.4009
#> 10 P RB Y S IDQTEHXCKJ LU FVZONMWGA H X DCB WV IOA QT PMZ RSUJKYN G LEF -0.7260
```
