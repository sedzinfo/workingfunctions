# Trim whitespace in dataframe

Trim whitespace in dataframe

## Usage

``` r
trim_df(df)
```

## Arguments

- df:

  dataframe

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
