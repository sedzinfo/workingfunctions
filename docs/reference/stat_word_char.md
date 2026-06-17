# Text similarity measures

Text similarity measures

## Usage

``` r
stat_word_char(text)
```

## Arguments

- text:

  character vector

## Examples

``` r
text<-"There are many variations of passages of Lorem Ipsum available,
but the majority have suffered alteration in some form, by injected humour,
or randomised words which don't look even slightly believable."
stat_word_char(text)
#>   words mean_char sd_char max_char min_char spell_error
#> 1    32     5.219   2.802       10        1           4
```
