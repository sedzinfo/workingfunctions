# Convert a string to proper case

Capitalises the first character and lowercases the rest of each element.

## Usage

``` r
proper(x)
```

## Arguments

- x:

  Character vector.

## Value

A character vector of the same length as `x`.

## Examples

``` r
x<-generate_string(nchar=10,vector=LETTERS,vector_length=10)
proper(x)
#>  [1] "Sdsrwkyngk" "Ywqzjfbuzw" "Nbdqyrxysu" "Uuolytxnkt" "Ofqkvtvjjq" "Ugwanhlmam" "Mgmijnvyaj" "Wllsxfhwzc" "Yqvzoymzkb" "Jykjgmvwoz"
```
