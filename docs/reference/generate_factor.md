# Generate dataframe of factors

Generate dataframe of factors

## Usage

``` r
generate_factor(vector = LETTERS[1:5], nrows = 2, ncols = 10, type = "random")
```

## Arguments

- vector:

  factor pool

- nrows:

  number of rows to generate

- ncols:

  number of collumns to generate

- type:

  "balanced" or "random" "balanced" generates balanced factor vectrors,
  "random" generates random factor vectors

## Examples

``` r
generate_factor(vector=LETTERS[1:5],ncols=5,nrows=10,type="random")
#>    X1 X2 X3 X4 X5
#> 1   E  B  A  B  A
#> 2   C  E  C  E  D
#> 3   C  C  D  A  D
#> 4   B  E  D  D  C
#> 5   B  B  A  E  D
#> 6   B  A  E  E  C
#> 7   A  C  A  E  D
#> 8   E  D  D  D  B
#> 9   C  E  A  E  C
#> 10  D  B  D  E  A
generate_factor(vector=LETTERS[1:5],ncols=5,nrows=10,type="balanced")
#>    X1 X2 X3 X4 X5
#> 1   A  A  A  A  A
#> 2   A  A  A  A  A
#> 3   B  B  B  B  B
#> 4   B  B  B  B  B
#> 5   C  C  C  C  C
#> 6   C  C  C  C  C
#> 7   D  D  D  D  D
#> 8   D  D  D  D  D
#> 9   E  E  E  E  E
#> 10  E  E  E  E  E
generate_factor(vector=LETTERS[1:5],ncols=1,nrows=10,type="balanced")
#>  [1] A A B B C C D D E E
#> Levels: A B C D E
generate_factor(vector=LETTERS[1:5],ncols=1,nrows=10,type="random")
#>  [1] B A B B E B B C B A
#> Levels: A B C D E
```
