# Young People Survey Responses

Survey responses from 1010 young people (15-30 years old) on music
preferences, movie preferences, hobbies, phobias, health habits,
personality traits, spending habits, and demographics.

## Usage

``` r
df_responses
```

## Format

A data frame with 1010 rows and 151 variables. All preference and
personality items are rated on a 1-5 Likert scale (1=strongly disagree /
not interested, 5=strongly agree / very interested) unless otherwise
noted.

- **Participant Number**: Unique participant identifier

- **Music**: Interest in music generally

- **Slow songs or fast songs**: Preference for slow vs fast songs

- **Dance**: Interest in dance music

- **Folk**: Interest in folk music

- **Country**: Interest in country music

- **Classical music**: Interest in classical music

- **Musical**: Interest in musicals

- **Pop**: Interest in pop music

- **Rock**: Interest in rock music

- **Metal or Hardrock**: Interest in metal/hardrock

- **Punk**: Interest in punk music

- **Hiphop, Rap**: Interest in hiphop/rap

- **Reggae, Ska**: Interest in reggae/ska

- **Swing, Jazz**: Interest in swing/jazz

- **Rock n roll**: Interest in rock n roll

- **Alternative**: Interest in alternative music

- **Latino**: Interest in Latino music

- **Techno, Trance**: Interest in techno/trance

- **Opera**: Interest in opera

- **Movies**: Interest in movies generally

- **Horror**: Interest in horror films

- **Thriller**: Interest in thrillers

- **Comedy**: Interest in comedy films

- **Romantic**: Interest in romantic films

- **Sci-fi**: Interest in sci-fi films

- **War**: Interest in war films

- **Fantasy/Fairy tales**: Interest in fantasy films

- **Animated**: Interest in animated films

- **Documentary**: Interest in documentaries

- **Western**: Interest in westerns

- **Action**: Interest in action films

- **History**: Interest in history

- **Psychology**: Interest in psychology

- **Politics**: Interest in politics

- **Mathematics**: Interest in mathematics

- **Physics**: Interest in physics

- **Internet**: Interest in internet

- **PC**: Interest in computers

- **Economy Management**: Interest in economy/management

- **Biology**: Interest in biology

- **Chemistry**: Interest in chemistry

- **Reading**: Interest in reading

- **Geography**: Interest in geography

- **Foreign languages**: Interest in foreign languages

- **Medicine**: Interest in medicine

- **Law**: Interest in law

- **Cars**: Interest in cars

- **Art exhibitions**: Interest in art exhibitions

- **Religion**: Interest in religion

- **Countryside, outdoors**: Interest in countryside/outdoors

- **Dancing**: Interest in dancing

- **Musical instruments**: Interest in playing musical instruments

- **Writing**: Interest in writing

- **Passive sport**: Interest in watching sports

- **Active sport**: Interest in playing sports

- **Gardening**: Interest in gardening

- **Celebrities**: Interest in celebrities

- **Shopping**: Interest in shopping

- **Science and technology**: Interest in science and technology

- **Theatre**: Interest in theatre

- **Fun with friends**: Enjoyment of socialising with friends

- **Adrenaline sports**: Interest in adrenaline sports

- **Pets**: Interest in pets

- **Flying**: Fear of flying

- **Storm**: Fear of storms

- **Darkness**: Fear of darkness

- **Heights**: Fear of heights

- **Spiders**: Fear of spiders

- **Snakes**: Fear of snakes

- **Rats**: Fear of rats

- **Ageing**: Fear of ageing

- **Dangerous dogs**: Fear of dangerous dogs

- **Fear of public speaking**: Fear of public speaking

- **Smoking**: Smoking behaviour/attitude

- **Alcohol**: Alcohol consumption/attitude

- **Healthy eating**: Attitude toward healthy eating

- **Daily events**: Attitude toward planning daily events

- **Prioritising workload**: Ability to prioritise workload

- **Writing notes**: Habit of writing notes

- **Workaholism**: Tendency toward workaholism

- **Thinking ahead**: Tendency to think ahead

- **Final judgement**: Tendency to make final judgements

- **Reliability**: Self-rated reliability

- **Keeping promises**: Tendency to keep promises

- **Loss of interest**: Tendency to lose interest

- **Friends versus money**: Attitude toward friends vs money

- **Funniness**: Self-rated funniness

- **Fake**: Tendency to be fake

- **Criminal damage**: Attitude toward criminal damage

- **Decision making**: Decision-making style

- **Elections**: Attitude toward elections

- **Self-criticism**: Tendency toward self-criticism

- **Judgment calls**: Tendency to make judgment calls

- **Hypochondria**: Tendency toward hypochondria

- **Empathy**: Level of empathy

- **Eating to survive**: Attitude toward eating

- **Giving**: Tendency to give to others

- **Compassion to animals**: Compassion toward animals

- **Borrowed stuff**: Attitude toward returning borrowed items

- **Loneliness**: Tendency to feel lonely

- **Cheating in school**: Attitude toward cheating

- **Health**: Self-rated health

- **Changing the past**: Desire to change the past

- **God**: Belief in God

- **Dreams**: Belief in dreams

- **Charity**: Attitude toward charity

- **Number of friends**: Number of friends

- **Punctuality**: Self-rated punctuality

- **Lying**: Tendency to lie

- **Waiting**: Attitude toward waiting

- **New environment**: Comfort in new environments

- **Mood swings**: Tendency to have mood swings

- **Appearence and gestures**: Attention to appearance and gestures

- **Socializing**: Enjoyment of socializing

- **Achievements**: Drive for achievements

- **Responding to a serious letter**: Tendency to respond to serious
  letters

- **Children**: Attitude toward children

- **Assertiveness**: Level of assertiveness

- **Getting angry**: Tendency to get angry

- **Knowing the right people**: Value placed on knowing the right people

- **Public speaking**: Comfort with public speaking

- **Unpopularity**: Attitude toward unpopularity

- **Life struggles**: Attitude toward life struggles

- **Happiness in life**: Self-rated happiness

- **Energy levels**: Self-rated energy levels

- **Small - big dogs**: Preference for small vs big dogs

- **Personality**: Self-rated personality

- **Finding lost valuables**: Tendency to return lost valuables

- **Getting up**: Ease of getting up in the morning

- **Interests or hobbies**: Number of interests or hobbies

- **Parents' advice**: Attitude toward parents' advice

- **Questionnaires or polls**: Attitude toward questionnaires

- **Internet usage**: Hours of internet usage

- **Finances**: Self-rated financial management

- **Shopping centres**: Attitude toward shopping centres

- **Branded clothing**: Attitude toward branded clothing

- **Entertainment spending**: Spending on entertainment

- **Spending on looks**: Spending on looks

- **Spending on gadgets**: Spending on gadgets

- **Spending on healthy eating**: Spending on healthy eating

- **Age**: Age in years

- **Height**: Height in cm

- **Weight**: Weight in kg

- **Number of siblings**: Number of siblings

- **Gender**: 1=Female, 2=Male

- **Left - right handed**: 1=Right, 2=Left

- **Education**: 1=Primary, 2=Secondary, 3=College/University,
  4=Masters, 5=Doctorate

- **Only child**: 1=Yes, 2=No

- **Village - town**: 1=Village, 2=City

- **House - block of flats**: 1=House, 2=Block of flats

## Source

Collected from students of Statistics at FSEV UK. Available on Kaggle.

## Examples

``` r
data(df_responses)
head(df_responses)
#>   Participant Number Music Slow songs or fast songs Dance Folk Country
#> 1                  1     5                        3     2    1       2
#> 2                  2     4                        4     2    1       1
#> 3                  3     5                        5     2    2       3
#> 4                  4     5                        3     2    1       1
#> 5                  5     5                        3     4    3       2
#> 6                  6     5                        3     2    3       2
#>   Classical music Musical Pop Rock Metal or Hardrock Punk Hiphop, Rap
#> 1               2       1   5    5                 1    1           1
#> 2               1       2   3    5                 4    4           1
#> 3               4       5   3    5                 3    4           1
#> 4               1       1   2    2                 1    4           2
#> 5               4       3   5    3                 1    2           5
#> 6               3       3   2    5                 5    3           4
#>   Reggae, Ska Swing, Jazz Rock n roll Alternative Latino Techno, Trance Opera
#> 1           1           1           3           1      1              1     1
#> 2           3           1           4           4      2              1     1
#> 3           4           3           5           5      5              1     3
#> 4           2           1           2           5      1              2     1
#> 5           3           2           1           2      4              2     2
#> 6           3           4           4           5      3              1     3
#>   Movies Horror Thriller Comedy Romantic Sci-fi War Fantasy/Fairy tales
#> 1      5      4        2      5        4      4   1                   5
#> 2      5      2        2      4        3      4   1                   3
#> 3      5      3        4      4        2      4   2                   5
#> 4      5      4        4      3        3      4   3                   1
#> 5      5      4        4      5        2      3   3                   4
#> 6      5      5        5      5        2      3   3                   4
#>   Animated Documentary Western Action History Psychology Politics Mathematics
#> 1        5           3       1      2       1          5        1           3
#> 2        5           4       1      4       1          3        4           5
#> 3        5           2       2      1       1          2        1           5
#> 4        2           5       1      2       4          4        5           4
#> 5        4           3       1      4       3          2        3           2
#> 6        3           3       2      4       5          3        4           2
#>   Physics Internet PC Economy Management Biology Chemistry Reading Geography
#> 1       3        5  3                  5       3         3       3         3
#> 2       2        4  4                  5       1         1       4         4
#> 3       2        4  2                  4       1         1       5         2
#> 4       1        3  1                  2       3         3       5         4
#> 5       2        2  2                  2       3         3       5         2
#> 6       3        4  4                  1       4         4       3         3
#>   Foreign languages Medicine Law Cars Art exhibitions Religion
#> 1                 5        3   1    1               1        1
#> 2                 5        1   2    2               2        1
#> 3                 5        2   3    1               5        5
#> 4                 4        2   5    1               5        4
#> 5                 3        3   2    3               1        4
#> 6                 4        4   3    5               2        2
#>   Countryside, outdoors Dancing Musical instruments Writing Passive sport
#> 1                     5       3                   3       2             1
#> 2                     1       1                   1       1             1
#> 3                     5       5                   5       5             5
#> 4                     1       1                   1       3             1
#> 5                     4       1                   3       1             3
#> 6                     5       1                   5       1             5
#>   Active sport Gardening Celebrities Shopping Science and technology Theatre
#> 1            5         5           1        4                      4       2
#> 2            1         1           2        3                      3       2
#> 3            2         1           1        4                      2       5
#> 4            1         1           2        4                      3       1
#> 5            1         4           3        3                      3       2
#> 6            4         2           1        2                      3       1
#>   Fun with friends Adrenaline sports Pets Flying Storm Darkness Heights Spiders
#> 1                5                 4    4      1     1        1       1       1
#> 2                4                 2    5      1     1        1       2       1
#> 3                5                 5    5      1     1        1       1       1
#> 4                2                 1    1      2     1        1       3       5
#> 5                4                 2    1      1     2        1       1       1
#> 6                3                 3    2      3     2        2       2       1
#>   Snakes Rats Ageing Dangerous dogs Fear of public speaking       Smoking
#> 1      5    3      1              3                       2  never smoked
#> 2      1    1      3              1                       4  never smoked
#> 3      1    1      1              1                       2 tried smoking
#> 4      5    5      4              5                       5 former smoker
#> 5      1    2      2              4                       3 tried smoking
#> 6      2    2      1              1                       3  never smoked
#>          Alcohol Healthy eating Daily events Prioritising workload
#> 1    drink a lot              4            2                     2
#> 2    drink a lot              3            3                     2
#> 3    drink a lot              3            1                     2
#> 4    drink a lot              3            4                     4
#> 5 social drinker              4            3                     1
#> 6          never              2            2                     2
#>   Writing notes Workaholism Thinking ahead Final judgement Reliability
#> 1             5           4              2               5           4
#> 2             4           5              4               1           4
#> 3             5           3              5               3           4
#> 4             4           5              3               1           3
#> 5             2           3              5               5           5
#> 6             3           3              3               1           3
#>   Keeping promises Loss of interest Friends versus money Funniness Fake
#> 1                4                1                    3         5    1
#> 2                4                3                    4         3    2
#> 3                5                1                    5         2    4
#> 4                4                5                    2         1    1
#> 5                4                2                    3         3    2
#> 6                4                3                    2         3    1
#>   Criminal damage Decision making Elections Self-criticism Judgment calls
#> 1               1               3         4              1              3
#> 2               1               2         5              4              4
#> 3               1               3         5              4              4
#> 4               5               5         5              5              4
#> 5               1               3         5              5              5
#> 6               4               2         5              4              4
#>   Hypochondria Empathy Eating to survive Giving Compassion to animals
#> 1            1       3                 1      4                     5
#> 2            1       2                 1      2                     4
#> 3            1       5                 5      5                     4
#> 4            3       3                 1      1                     2
#> 5            1       3                 1      3                     3
#> 6            1       4                 2      3                     5
#>   Borrowed stuff Loneliness Cheating in school Health Changing the past God
#> 1              4          3                  2      1                 1   1
#> 2              3          2                  4      4                 4   1
#> 3              2          5                  3      2                 5   5
#> 4              5          5                  5      1                 5   4
#> 5              4          3                  5      3                 4   5
#> 6              5          2                  4      3                 3   3
#>   Dreams Charity Number of friends             Punctuality
#> 1      4       2                 3     i am always on time
#> 2      3       1                 3        i am often early
#> 3      1       3                 3 i am often running late
#> 4      3       3                 1        i am often early
#> 5      3       3                 3     i am always on time
#> 6      3       2                 3        i am often early
#>                           Lying Waiting New environment Mood swings
#> 1                         never       3               4           3
#> 2                     sometimes       3               4           4
#> 3                     sometimes       2               3           4
#> 4 only to avoid hurting someone       1               1           5
#> 5         everytime it suits me       3               4           2
#> 6 only to avoid hurting someone       3               4           3
#>   Appearence and gestures Socializing Achievements
#> 1                       4           3            4
#> 2                       4           4            2
#> 3                       3           5            3
#> 4                       3           1            3
#> 5                       3           3            3
#> 6                       3           4            2
#>   Responding to a serious letter Children Assertiveness Getting angry
#> 1                              3        5             1             1
#> 2                              4        2             2             5
#> 3                              4        4             3             4
#> 4                              3        2             5             5
#> 5                              3        5             4             2
#> 6                              2        3             4             3
#>   Knowing the right people Public speaking Unpopularity Life struggles
#> 1                        3               5            5              1
#> 2                        4               4            4              1
#> 3                        3               2            4              4
#> 4                        4               5            3              3
#> 5                        3               5            5              2
#> 6                        4               4            4              3
#>   Happiness in life Energy levels Small - big dogs Personality
#> 1                 4             5                1           4
#> 2                 4             3                5           3
#> 3                 4             4                3           3
#> 4                 2             2                1           2
#> 5                 3             5                3           3
#> 6                 3             4                4           3
#>   Finding lost valuables Getting up Interests or hobbies Parents' advice
#> 1                      3          2                    3               4
#> 2                      4          5                    3               2
#> 3                      3          4                    5               3
#> 4                      1          1                   NA               2
#> 5                      2          4                    3               3
#> 6                      3          3                    5               3
#>   Questionnaires or polls  Internet usage Finances Shopping centres
#> 1                       3 few hours a day        3                4
#> 2                       3 few hours a day        3                4
#> 3                       1 few hours a day        2                4
#> 4                       4 most of the day        2                4
#> 5                       3 few hours a day        4                3
#> 6                       4 few hours a day        2                3
#>   Branded clothing Entertainment spending Spending on looks Spending on gadgets
#> 1                5                      3                 3                   1
#> 2                1                      4                 2                   5
#> 3                1                      4                 3                   4
#> 4                3                      3                 4                   4
#> 5                4                      3                 3                   2
#> 6                3                      3                 1                   4
#>   Spending on healthy eating Age Height Weight Number of siblings Gender
#> 1                          3  20    163     48                  1 female
#> 2                          2  19    163     58                  2 female
#> 3                          2  20    176     67                  2 female
#> 4                          1  22    172     59                  1 female
#> 5                          4  20    170     59                  1 female
#> 6                          4  20    186     77                  1   male
#>   Left - right handed               Education Only child Village - town
#> 1        right handed college/bachelor degree         no        village
#> 2        right handed college/bachelor degree         no           city
#> 3        right handed        secondary school         no           city
#> 4        right handed college/bachelor degree        yes           city
#> 5        right handed        secondary school         no        village
#> 6        right handed        secondary school         no           city
#>   House - block of flats
#> 1         block of flats
#> 2         block of flats
#> 3         block of flats
#> 4         house/bungalow
#> 5         house/bungalow
#> 6         block of flats
```
