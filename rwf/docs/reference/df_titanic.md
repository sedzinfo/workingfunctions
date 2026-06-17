# Titanic Dataset

A dataset containing information about passengers on the Titanic.

## Usage

``` r
df_titanic
```

## Format

A data frame with the following variables:

- PassengerId:

  Unique identifier for each passenger

- survived:

  Survival status (0=No, 1=Yes)

- pclass:

  Passenger class (1=1st, 2=2nd, 3=3rd)

- name:

  Name of the passenger

- sex:

  Gender of the passenger

- age:

  Age of the passenger

- sibsp:

  Number of siblings/spouses aboard the Titanic

- parch:

  Number of parents/children aboard the Titanic

- ticket:

  Ticket number

- fare:

  Passenger fare

- cabin:

  Cabin number

- embarked:

  Port of embarkation (C=Cherbourg; Q=Queenstown; S=Southampton)

- boat:

  Lifeboat number

- body:

  Body number

- home.dest:

  Home destination

## Examples

``` r
data(df_titanic)
head(df_titanic)
#>   PassengerId survived pclass                                            name
#> 1           1        1      1                   Allen, Miss. Elisabeth Walton
#> 2           2        1      1                  Allison, Master. Hudson Trevor
#> 3           3        0      1                    Allison, Miss. Helen Loraine
#> 4           4        0      1            Allison, Mr. Hudson Joshua Creighton
#> 5           5        0      1 Allison, Mrs. Hudson J C (Bessie Waldo Daniels)
#> 6           6        1      1                             Anderson, Mr. Harry
#>      sex     age sibsp parch ticket     fare   cabin embarked boat body
#> 1 female 29.0000     0     0  24160 211.3375      B5        S    2   NA
#> 2   male  0.9167     1     2 113781 151.5500 C22 C26        S   11   NA
#> 3 female  2.0000     1     2 113781 151.5500 C22 C26        S        NA
#> 4   male 30.0000     1     2 113781 151.5500 C22 C26        S       135
#> 5 female 25.0000     1     2 113781 151.5500 C22 C26        S        NA
#> 6   male 48.0000     0     0  19952  26.5500     E12        S    3   NA
#>                         home.dest
#> 1                    St Louis, MO
#> 2 Montreal, PQ / Chesterville, ON
#> 3 Montreal, PQ / Chesterville, ON
#> 4 Montreal, PQ / Chesterville, ON
#> 5 Montreal, PQ / Chesterville, ON
#> 6                    New York, NY
```
