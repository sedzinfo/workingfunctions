# Adjust string aesthetics

Treats spesific characters such as ".", as separating characters and
separates strings with space. Trims leading and trailing spaces and
capitalizes the first letter of the string and lowers the rest.

## Usage

``` r
string_aes(
  vector,
  characterlist = c(".", "_", "-", ",", "$", "<p>", "</p>", "<br>", "<br/>", "<B>",
    "</B>", "<BR/>", "|", "/", "&nbsp"),
  proper = TRUE
)
```

## Arguments

- vector:

  Vector

- characterlist:

  List the list of characters to treat as separating characters

- proper:

  Logical TRUE capitalizes the first letter in sentense format

## Examples

``` r
vector<-c("TES.T","TES<p>T","TES&nbspT")
string_aes(vector=vector)
#> [1] "Tes t" "Tes t" "Tes t"
string_aes(vector=vector,proper=FALSE)
#> [1] "TES T" "TES T" "TES T"
string_aes(vector=vector,proper=TRUE)
#> [1] "Tes t" "Tes t" "Tes t"
```
