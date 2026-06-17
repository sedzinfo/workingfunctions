# Question dimension table

Return a dataframe with the order of the questions, their respective
dimensions, and the description of the dimensions

## Usage

``` r
questions_dimensions_dataframe(
  key,
  dimensions,
  elaborate_dimensions,
  questions
)
```

## Arguments

- key:

  a vector indicating the dimension of each question. The order of the
  elements in the key represents the order of the questions, the numeric
  values represent the dimension the question belongs to

- dimensions:

  dimension names

- elaborate_dimensions:

  full dimension names

- questions:

  question names

## Examples

``` r
key<-c(1,2,3,4,5,1,2,3,4,5)
dimensions<-paste0("Dimension",1:10)
elaborate_dimensions<-paste0("Elaborated_Dimension",1:10)
questions<-paste0("Question",1:65)
questions_dimensions_dataframe(key,dimensions,elaborate_dimensions,questions)
#>    ORDER  DIMENSION   ELABORATE DIMENSION   QUESTION
#> 1      1 Dimension1 Elaborated_Dimension1  Question1
#> 2      6 Dimension1 Elaborated_Dimension1  Question6
#> 3      2 Dimension2 Elaborated_Dimension2  Question2
#> 4      7 Dimension2 Elaborated_Dimension2  Question7
#> 5      3 Dimension3 Elaborated_Dimension3  Question3
#> 6      8 Dimension3 Elaborated_Dimension3  Question8
#> 7      4 Dimension4 Elaborated_Dimension4  Question4
#> 8      9 Dimension4 Elaborated_Dimension4  Question9
#> 9      5 Dimension5 Elaborated_Dimension5  Question5
#> 10    10 Dimension5 Elaborated_Dimension5 Question10
```
