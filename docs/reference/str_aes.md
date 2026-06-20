# Clean and format string aesthetics

Replaces a list of separator characters (e.g. `"."`, `"_"`, HTML tags)
with spaces, trims leading and trailing whitespace, collapses internal
whitespace, and optionally applies proper case.

## Usage

``` r
str_aes(
  vector,
  characterlist = c(".", "_", "-", ",", "$", "<p>", "</p>", "<br>", "<br/>", "<B>",
    "</B>", "<BR/>", "|", "/", "&nbsp"),
  proper = TRUE
)
```

## Arguments

- vector:

  Character vector to clean.

- characterlist:

  Character vector of strings to treat as separators, each replaced by a
  single space. Defaults to common punctuation and HTML tags including
  `"."`, `"_"`, `"-"`, `"<p>"`, `"<br>"`, `"&nbsp"`, and others.

- proper:

  Logical. If `TRUE`, capitalises the first letter and lowercases the
  rest of each string. Default is `TRUE`.

## Value

A character vector of the same length as `vector` with separators
replaced, whitespace normalised, and optional proper casing.

## Examples

``` r
vector <- c("TES.T", "TES<p>T", "TES&nbspT")
str_aes(vector = vector)
#> [1] "Tes t" "Tes t" "Tes t"
str_aes(vector = vector, proper = FALSE)
#> [1] "TES T" "TES T" "TES T"
str_aes(vector = vector, proper = TRUE)
#> [1] "Tes t" "Tes t" "Tes t"
```
