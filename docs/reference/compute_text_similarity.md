# Text similarity measures

Text similarity measures

## Usage

``` r
compute_text_similarity(text1, text2)
```

## Arguments

- text1:

  character vector

- text2:

  character vector

## Examples

``` r
text1<-"word_one word_two word_three"
text2<-"word_three word_four word_six"
text3<-"All the Lorem Ipsum generators on the Internet tend to repeat predefined 
chunks as necessary, making this the first true generator on the Internet."
text4<-"It uses a dictionary of over 200 Latin words, combined with a handful of 
model sentence structures, to generate Lorem Ipsum which looks reasonable."
text5<-"The generated Lorem Ipsum is therefore always free from repetition, 
injected humour, or non-characteristic words etc."
text<-c(text1,text2,text3,text4,text5)
text<-unlist(strsplit(text,split=" "))
text1<-unlist(strsplit(text1,split=" "))
text2<-unlist(strsplit(text2,split=" "))
text3<-unlist(strsplit(text3,split=" "))
text4<-unlist(strsplit(text4,split=" "))
text5<-unlist(strsplit(text5,split=" "))
compute_text_similarity(text1,text1)
#>   tversky intersect intersect_weight setdiff1 setdiff2 lengtht1 lengtht2
#> 1       1         3                3        0        0        3        3
compute_text_similarity(text1,text2)
#>     tversky intersect intersect_weight setdiff1 setdiff2 lengtht1 lengtht2
#> 1 0.3333333         1                1        2        2        3        3
compute_text_similarity(text1,text3)
#>   tversky intersect intersect_weight setdiff1 setdiff2 lengtht1 lengtht2
#> 1       0         0                0        3       20        3       24
compute_text_similarity(text1,text4)
#>   tversky intersect intersect_weight setdiff1 setdiff2 lengtht1 lengtht2
#> 1       0         0                0        3       22        3       24
```
