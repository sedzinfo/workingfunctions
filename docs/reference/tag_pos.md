# Part of speech tagging

Part of speech tagging

## Usage

``` r
tag_pos(text)
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
tag_pos(text)
#> $POStagged
#> [1] "word_one/NN word_two/VBD word_three/CD word_three/CD word_four/NN word_six/NN All/DT the/DT Lorem/NNP Ipsum/NNP generators/NNS on/IN the/DT Internet/NNP tend/VB to/TO repeat/VB predefined/VBN chunks/NNS as/IN necessary/JJ ,/, making/VBG this/DT the/DT first/JJ true/JJ generator/NN on/IN the/DT Internet/NNP ./. It/PRP uses/VBZ a/DT dictionary/NN of/IN over/IN 200/CD Latin/JJ words/NNS ,/, combined/VBN with/IN a/DT handful/NN of/IN model/NN sentence/NN structures/NNS ,/, to/TO generate/VB Lorem/NNP Ipsum/NNP which/WDT looks/VBZ reasonable/JJ ./. The/DT generated/VBD Lorem/NNP Ipsum/NNP is/VBZ therefore/RB always/RB free/JJ from/IN repetition/NN ,/, injected/VBD humour/NN ,/, or/CC non-characteristic/JJ words/NNS etc/FW ./."
#> 
#> $POStags
#>  [1] "NN"  "VBD" "CD"  "CD"  "NN"  "NN"  "DT"  "DT"  "NNP" "NNP" "NNS" "IN"  "DT"  "NNP" "VB"  "TO"  "VB"  "VBN" "NNS" "IN"  "JJ"  ","   "VBG" "DT"  "DT"  "JJ"  "JJ"  "NN"  "IN"  "DT"  "NNP" "."  
#> [33] "PRP" "VBZ" "DT"  "NN"  "IN"  "IN"  "CD"  "JJ"  "NNS" ","   "VBN" "IN"  "DT"  "NN"  "IN"  "NN"  "NN"  "NNS" ","   "TO"  "VB"  "NNP" "NNP" "WDT" "VBZ" "JJ"  "."   "DT"  "VBD" "NNP" "NNP" "VBZ"
#> [65] "RB"  "RB"  "JJ"  "IN"  "NN"  ","   "VBD" "NN"  ","   "CC"  "JJ"  "NNS" "FW"  "."  
#> 
```
