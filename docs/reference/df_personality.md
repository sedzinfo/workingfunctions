# Big Five Inventory (BFI-44) Personality Dataset

Responses from 433 participants to the Big Five Inventory (BFI-44), a
widely used measure of the five major dimensions of personality:
Extraversion (E), Agreeableness (A), Conscientiousness (C), Neuroticism
(N), and Openness to Experience (O). Items are rated on a 1-6 Likert
scale. Reverse-scored items are marked with (R).

## Usage

``` r
df_personality
```

## Format

A data frame with 433 rows and 44 variables:

- pers01:

  \[E\] Is talkative

- pers02:

  \[A-R\] Tends to find fault with others

- pers03:

  \[C\] Does a thorough job

- pers04:

  \[N\] Is depressed, blue

- pers05:

  \[O\] Is original, comes up with new ideas

- pers06:

  \[E-R\] Is reserved

- pers07:

  \[A\] Is helpful and unselfish with others

- pers08:

  \[C-R\] Can be somewhat careless

- pers09:

  \[N-R\] Is relaxed, handles stress well

- pers10:

  \[O\] Is curious about many different things

- pers11:

  \[E\] Is full of energy

- pers12:

  \[A-R\] Starts quarrels with others

- pers13:

  \[C\] Is a reliable worker

- pers14:

  \[N\] Can be tense

- pers15:

  \[O\] Is ingenious, a deep thinker

- pers16:

  \[E\] Generates a lot of enthusiasm

- pers17:

  \[A\] Has a forgiving nature

- pers18:

  \[C-R\] Tends to be disorganized

- pers19:

  \[N\] Worries a lot

- pers20:

  \[O\] Has an active imagination

- pers21:

  \[E-R\] Tends to be quiet

- pers22:

  \[A\] Is generally trusting

- pers23:

  \[C-R\] Tends to be lazy

- pers24:

  \[N-R\] Is emotionally stable, not easily upset

- pers25:

  \[O\] Is inventive

- pers26:

  \[E\] Has an assertive personality

- pers27:

  \[A-R\] Can be cold and aloof

- pers28:

  \[C\] Perseveres until the task is finished

- pers29:

  \[N\] Can be moody

- pers30:

  \[O\] Values artistic, aesthetic experiences

- pers31:

  \[E-R\] Is sometimes shy, inhibited

- pers32:

  \[A\] Is considerate and kind to almost everyone

- pers33:

  \[C\] Does things efficiently

- pers34:

  \[N-R\] Remains calm in tense situations

- pers35:

  \[O-R\] Prefers work that is routine

- pers36:

  \[E\] Is outgoing, sociable

- pers37:

  \[A-R\] Is sometimes rude to others

- pers38:

  \[C\] Makes plans and follows through with them

- pers39:

  \[N\] Gets nervous easily

- pers40:

  \[O\] Likes to reflect, play with ideas

- pers41:

  \[O-R\] Has few artistic interests

- pers42:

  \[A\] Likes to cooperate with others

- pers43:

  \[C-R\] Is easily distracted

- pers44:

  \[O\] Is sophisticated in art, music, or literature

## References

John, O. P., & Srivastava, S. (1999). The Big Five trait taxonomy:
History, measurement, and theoretical perspectives. In L. A. Pervin & O.
P. John (Eds.), *Handbook of personality: Theory and research* (2nd ed.,
pp. 102-138). Guilford Press.

## Examples

``` r
data(df_personality)
head(df_personality)
#>   pers01 pers02 pers03 pers04 pers05 pers06 pers07 pers08 pers09 pers10 pers11 pers12 pers13 pers14 pers15 pers16 pers17 pers18 pers19 pers20 pers21 pers22 pers23 pers24 pers25 pers26 pers27 pers28
#> 1      5      4      5      1      4      3      3      1      2      3      2      4      5      4      4      3      5      1      3      4      2      4      2      3      2      5      4      5
#> 2      1      1      5      2      1      2      5      1      5      1      5      3      5      4      1      2      1      3      5      1      5      5      1      3      1      2      3      5
#> 3      4      1      5      3      3      4      5      3      1      4      2      1      5      4      3      2      5      1      5      4      4      4      3      2      4      1      3      4
#> 4      4      2      5      1      4      3      4      4      4      5      4      1      4      5      3      3      4      2      5      4      4      4      3      2      4      2      1      5
#> 5      2      3      5      1      2      4      5      2      3      3      4      2      5      3      3      3      5      1      4      2      5      5      1      3      2      2      4      4
#> 6      1      1      5      4      3      4      4      2      1      4      3      3      5      5      3      2      4      1      5      3      5      4      1      3      3      1      2      5
#>   pers29 pers30 pers31 pers32 pers33 pers34 pers35 pers36 pers37 pers38 pers39 pers40 pers41 pers42 pers43 pers44
#> 1      4      2      5      4      5      3      1      3      4      4      4      3      3      3      5      4
#> 2      3      3      5      5      5      5      5      3      1      1      2      3      3      5      2      1
#> 3      5      4      3      3      5      5      4      2      3      1      5      4      2      3      5      3
#> 4      3      4      4      4      4      4      3      3      3      4      5      5      3      5      2      4
#> 5      4      3      4      4      5      4      4      2      5      5      4      3      2      4      3      2
#> 6      5      4      5      3      4      2      3      1      1      3      5      4      2      4      3      5
```
