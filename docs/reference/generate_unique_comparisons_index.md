# Generate index for unique comparisons

Generate index for unique comparisons

## Usage

``` r
generate_unique_comparisons_index(items)
```

## Arguments

- items:

  number of items

## Examples

``` r
generate_unique_comparisons_index(1)
#>      i1 i2
generate_unique_comparisons_index(2)
#>   i1 i2
#> 2  1  2
generate_unique_comparisons_index(3)
#>   i1 i2
#> 2  1  2
#> 3  1  3
#> 6  2  3
generate_unique_comparisons_index(4)
#>    i1 i2
#> 2   1  2
#> 3   1  3
#> 4   1  4
#> 7   2  3
#> 8   2  4
#> 12  3  4
generate_unique_comparisons_index(5)
#>    i1 i2
#> 2   1  2
#> 3   1  3
#> 4   1  4
#> 5   1  5
#> 8   2  3
#> 9   2  4
#> 10  2  5
#> 14  3  4
#> 15  3  5
#> 20  4  5
generate_unique_comparisons_index(6)
#>    i1 i2
#> 2   1  2
#> 3   1  3
#> 4   1  4
#> 5   1  5
#> 6   1  6
#> 9   2  3
#> 10  2  4
#> 11  2  5
#> 12  2  6
#> 16  3  4
#> 17  3  5
#> 18  3  6
#> 23  4  5
#> 24  4  6
#> 30  5  6
```
