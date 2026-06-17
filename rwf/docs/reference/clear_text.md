# Clear text

Clear text

## Usage

``` r
clear_text(text)
```

## Arguments

- text:

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
clear_text(text)
#> [1] "word one word two word three"                                                                                                                     
#> [2] "word three word four word six"                                                                                                                    
#> [3] "all the lorem ipsum generators on the internet tend to repeat predefined chunks as necessary making this the first true generator on the internet"
#> [4] "it uses a dictionary of over latin words combined with a handful of model sentence structures to generate lorem ipsum which looks reasonable"     
#> [5] "the generated lorem ipsum is therefore always free from repetition injected humour or non characteristic words etc"                               
```
