# Compute correlation matrix

Compute correlation matrix

## Usage

``` r
compute_power_r_matrix(m, ...)
```

## Arguments

- m:

  correlation matrix

- ...:

  arguments passed to compute_power_r

## Examples

``` r
compute_power_r_matrix(m=stats::cor(mtcars,use="pairwise.complete.obs"),n=100)

#> $plot
#> $plot[[1]]
#> 
#> 
#> $power_table
#>       n          r    p      power alternative
#> 1    10 0.05753435 0.05 0.05177153   two.sided
#> 2    11 0.05753435 0.05 0.05218683   two.sided
#> 3    12 0.05753435 0.05 0.05261579   two.sided
#> 4    13 0.05753435 0.05 0.05304916   two.sided
#> 5    14 0.05753435 0.05 0.05348263   two.sided
#> 6    15 0.05753435 0.05 0.05391422   two.sided
#> 7    16 0.05753435 0.05 0.05434310   two.sided
#> 8    17 0.05753435 0.05 0.05476899   two.sided
#> 9    18 0.05753435 0.05 0.05519192   two.sided
#> 10   19 0.05753435 0.05 0.05561201   two.sided
#> 11   20 0.05753435 0.05 0.05602949   two.sided
#> 12   21 0.05753435 0.05 0.05644457   two.sided
#> 13   22 0.05753435 0.05 0.05685748   two.sided
#> 14   23 0.05753435 0.05 0.05726842   two.sided
#> 15   24 0.05753435 0.05 0.05767760   two.sided
#> 16   25 0.05753435 0.05 0.05808519   two.sided
#> 17   26 0.05753435 0.05 0.05849134   two.sided
#> 18   27 0.05753435 0.05 0.05889622   two.sided
#> 19   28 0.05753435 0.05 0.05929993   two.sided
#> 20   29 0.05753435 0.05 0.05970260   two.sided
#> 21   30 0.05753435 0.05 0.06010434   two.sided
#> 22   31 0.05753435 0.05 0.06050523   two.sided
#> 23   32 0.05753435 0.05 0.06090536   two.sided
#> 24   33 0.05753435 0.05 0.06130480   two.sided
#> 25   34 0.05753435 0.05 0.06170362   two.sided
#> 26   35 0.05753435 0.05 0.06210188   two.sided
#> 27   36 0.05753435 0.05 0.06249963   two.sided
#> 28   37 0.05753435 0.05 0.06289693   two.sided
#> 29   38 0.05753435 0.05 0.06329381   two.sided
#> 30   39 0.05753435 0.05 0.06369031   two.sided
#> 31   40 0.05753435 0.05 0.06408648   two.sided
#> 32   41 0.05753435 0.05 0.06448234   two.sided
#> 33   42 0.05753435 0.05 0.06487793   two.sided
#> 34   43 0.05753435 0.05 0.06527327   two.sided
#> 35   44 0.05753435 0.05 0.06566839   two.sided
#> 36   45 0.05753435 0.05 0.06606332   two.sided
#> 37   46 0.05753435 0.05 0.06645806   two.sided
#> 38   47 0.05753435 0.05 0.06685265   two.sided
#> 39   48 0.05753435 0.05 0.06724709   two.sided
#> 40   49 0.05753435 0.05 0.06764142   two.sided
#> 41   50 0.05753435 0.05 0.06803563   two.sided
#> 42   51 0.05753435 0.05 0.06842975   two.sided
#> 43   52 0.05753435 0.05 0.06882379   two.sided
#> 44   53 0.05753435 0.05 0.06921776   two.sided
#> 45   54 0.05753435 0.05 0.06961167   two.sided
#> 46   55 0.05753435 0.05 0.07000553   two.sided
#> 47   56 0.05753435 0.05 0.07039935   two.sided
#> 48   57 0.05753435 0.05 0.07079314   two.sided
#> 49   58 0.05753435 0.05 0.07118691   two.sided
#> 50   59 0.05753435 0.05 0.07158066   two.sided
#> 51   60 0.05753435 0.05 0.07197441   two.sided
#> 52   61 0.05753435 0.05 0.07236816   two.sided
#> 53   62 0.05753435 0.05 0.07276191   two.sided
#> 54   63 0.05753435 0.05 0.07315568   two.sided
#> 55   64 0.05753435 0.05 0.07354946   two.sided
#> 56   65 0.05753435 0.05 0.07394326   two.sided
#> 57   66 0.05753435 0.05 0.07433709   two.sided
#> 58   67 0.05753435 0.05 0.07473095   two.sided
#> 59   68 0.05753435 0.05 0.07512485   two.sided
#> 60   69 0.05753435 0.05 0.07551878   two.sided
#> 61   70 0.05753435 0.05 0.07591276   two.sided
#> 62   71 0.05753435 0.05 0.07630678   two.sided
#> 63   72 0.05753435 0.05 0.07670085   two.sided
#> 64   73 0.05753435 0.05 0.07709498   two.sided
#> 65   74 0.05753435 0.05 0.07748915   two.sided
#> 66   75 0.05753435 0.05 0.07788339   two.sided
#> 67   76 0.05753435 0.05 0.07827769   two.sided
#> 68   77 0.05753435 0.05 0.07867205   two.sided
#> 69   78 0.05753435 0.05 0.07906647   two.sided
#> 70   79 0.05753435 0.05 0.07946096   two.sided
#> 71   80 0.05753435 0.05 0.07985552   two.sided
#> 72   81 0.05753435 0.05 0.08025014   two.sided
#> 73   82 0.05753435 0.05 0.08064484   two.sided
#> 74   83 0.05753435 0.05 0.08103962   two.sided
#> 75   84 0.05753435 0.05 0.08143446   two.sided
#> 76   85 0.05753435 0.05 0.08182939   two.sided
#> 77   86 0.05753435 0.05 0.08222439   two.sided
#> 78   87 0.05753435 0.05 0.08261947   two.sided
#> 79   88 0.05753435 0.05 0.08301463   two.sided
#> 80   89 0.05753435 0.05 0.08340987   two.sided
#> 81   90 0.05753435 0.05 0.08380519   two.sided
#> 82   91 0.05753435 0.05 0.08420060   two.sided
#> 83   92 0.05753435 0.05 0.08459609   two.sided
#> 84   93 0.05753435 0.05 0.08499166   two.sided
#> 85   94 0.05753435 0.05 0.08538732   two.sided
#> 86   95 0.05753435 0.05 0.08578306   two.sided
#> 87   96 0.05753435 0.05 0.08617890   two.sided
#> 88   97 0.05753435 0.05 0.08657481   two.sided
#> 89   98 0.05753435 0.05 0.08697082   two.sided
#> 90   99 0.05753435 0.05 0.08736691   two.sided
#> 91  100 0.05753435 0.05 0.08776310   two.sided
#> 92   10 0.90203287 0.05 0.98152683   two.sided
#> 93   11 0.90203287 0.05 0.99067648   two.sided
#> 94   12 0.90203287 0.05 0.99537648   two.sided
#> 95   13 0.90203287 0.05 0.99774233   two.sided
#> 96   14 0.90203287 0.05 0.99891259   two.sided
#> 97   15 0.90203287 0.05 0.99948264   two.sided
#> 98   16 0.90203287 0.05 0.99975658   two.sided
#> 99   17 0.90203287 0.05 0.99988662   two.sided
#> 100  18 0.90203287 0.05 0.99994767   two.sided
#> 101  19 0.90203287 0.05 0.99997605   two.sided
#> 102  20 0.90203287 0.05 0.99998913   two.sided
#> 103  21 0.90203287 0.05 0.99999510   two.sided
#> 104  22 0.90203287 0.05 0.99999781   two.sided
#> 105  23 0.90203287 0.05 0.99999902   two.sided
#> 106  24 0.90203287 0.05 0.99999957   two.sided
#> 107  25 0.90203287 0.05 0.99999981   two.sided
#> 108  26 0.90203287 0.05 0.99999992   two.sided
#> 109  27 0.90203287 0.05 0.99999996   two.sided
#> 110  28 0.90203287 0.05 0.99999998   two.sided
#> 111  29 0.90203287 0.05 0.99999999   two.sided
#> 112  30 0.90203287 0.05 1.00000000   two.sided
#> 113  31 0.90203287 0.05 1.00000000   two.sided
#> 114  32 0.90203287 0.05 1.00000000   two.sided
#> 115  33 0.90203287 0.05 1.00000000   two.sided
#> 116  34 0.90203287 0.05 1.00000000   two.sided
#> 117  35 0.90203287 0.05 1.00000000   two.sided
#> 118  36 0.90203287 0.05 1.00000000   two.sided
#> 119  37 0.90203287 0.05 1.00000000   two.sided
#> 120  38 0.90203287 0.05 1.00000000   two.sided
#> 121  39 0.90203287 0.05 1.00000000   two.sided
#> 122  40 0.90203287 0.05 1.00000000   two.sided
#> 123  41 0.90203287 0.05 1.00000000   two.sided
#> 124  42 0.90203287 0.05 1.00000000   two.sided
#> 125  43 0.90203287 0.05 1.00000000   two.sided
#> 126  44 0.90203287 0.05 1.00000000   two.sided
#> 127  45 0.90203287 0.05 1.00000000   two.sided
#> 128  46 0.90203287 0.05 1.00000000   two.sided
#> 129  47 0.90203287 0.05 1.00000000   two.sided
#> 130  48 0.90203287 0.05 1.00000000   two.sided
#> 131  49 0.90203287 0.05 1.00000000   two.sided
#> 132  50 0.90203287 0.05 1.00000000   two.sided
#> 133  51 0.90203287 0.05 1.00000000   two.sided
#> 134  52 0.90203287 0.05 1.00000000   two.sided
#> 135  53 0.90203287 0.05 1.00000000   two.sided
#> 136  54 0.90203287 0.05 1.00000000   two.sided
#> 137  55 0.90203287 0.05 1.00000000   two.sided
#> 138  56 0.90203287 0.05 1.00000000   two.sided
#> 139  57 0.90203287 0.05 1.00000000   two.sided
#> 140  58 0.90203287 0.05 1.00000000   two.sided
#> 141  59 0.90203287 0.05 1.00000000   two.sided
#> 142  60 0.90203287 0.05 1.00000000   two.sided
#> 143  61 0.90203287 0.05 1.00000000   two.sided
#> 144  62 0.90203287 0.05 1.00000000   two.sided
#> 145  63 0.90203287 0.05 1.00000000   two.sided
#> 146  64 0.90203287 0.05 1.00000000   two.sided
#> 147  65 0.90203287 0.05 1.00000000   two.sided
#> 148  66 0.90203287 0.05 1.00000000   two.sided
#> 149  67 0.90203287 0.05 1.00000000   two.sided
#> 150  68 0.90203287 0.05 1.00000000   two.sided
#> 151  69 0.90203287 0.05 1.00000000   two.sided
#> 152  70 0.90203287 0.05 1.00000000   two.sided
#> 153  71 0.90203287 0.05 1.00000000   two.sided
#> 154  72 0.90203287 0.05 1.00000000   two.sided
#> 155  73 0.90203287 0.05 1.00000000   two.sided
#> 156  74 0.90203287 0.05 1.00000000   two.sided
#> 157  75 0.90203287 0.05 1.00000000   two.sided
#> 158  76 0.90203287 0.05 1.00000000   two.sided
#> 159  77 0.90203287 0.05 1.00000000   two.sided
#> 160  78 0.90203287 0.05 1.00000000   two.sided
#> 161  79 0.90203287 0.05 1.00000000   two.sided
#> 162  80 0.90203287 0.05 1.00000000   two.sided
#> 163  81 0.90203287 0.05 1.00000000   two.sided
#> 164  82 0.90203287 0.05 1.00000000   two.sided
#> 165  83 0.90203287 0.05 1.00000000   two.sided
#> 166  84 0.90203287 0.05 1.00000000   two.sided
#> 167  85 0.90203287 0.05 1.00000000   two.sided
#> 168  86 0.90203287 0.05 1.00000000   two.sided
#> 169  87 0.90203287 0.05 1.00000000   two.sided
#> 170  88 0.90203287 0.05 1.00000000   two.sided
#> 171  89 0.90203287 0.05 1.00000000   two.sided
#> 172  90 0.90203287 0.05 1.00000000   two.sided
#> 173  91 0.90203287 0.05 1.00000000   two.sided
#> 174  92 0.90203287 0.05 1.00000000   two.sided
#> 175  93 0.90203287 0.05 1.00000000   two.sided
#> 176  94 0.90203287 0.05 1.00000000   two.sided
#> 177  95 0.90203287 0.05 1.00000000   two.sided
#> 178  96 0.90203287 0.05 1.00000000   two.sided
#> 179  97 0.90203287 0.05 1.00000000   two.sided
#> 180  98 0.90203287 0.05 1.00000000   two.sided
#> 181  99 0.90203287 0.05 1.00000000   two.sided
#> 182 100 0.90203287 0.05 1.00000000   two.sided
#> 183  10 0.55864236 0.05 0.41352700   two.sided
#> 184  11 0.55864236 0.05 0.45772015   two.sided
#> 185  12 0.55864236 0.05 0.49982546   two.sided
#> 186  13 0.55864236 0.05 0.53973112   two.sided
#> 187  14 0.55864236 0.05 0.57737671   two.sided
#> 188  15 0.55864236 0.05 0.61274225   two.sided
#> 189  16 0.55864236 0.05 0.64584000   two.sided
#> 190  17 0.55864236 0.05 0.67670794   two.sided
#> 191  18 0.55864236 0.05 0.70540428   two.sided
#> 192  19 0.55864236 0.05 0.73200283   two.sided
#> 193  20 0.55864236 0.05 0.75658911   two.sided
#> 194  21 0.55864236 0.05 0.77925697   two.sided
#> 195  22 0.55864236 0.05 0.80010586   two.sided
#> 196  23 0.55864236 0.05 0.81923846   two.sided
#> 197  24 0.55864236 0.05 0.83675874   two.sided
#> 198  25 0.55864236 0.05 0.85277040   two.sided
#> 199  26 0.55864236 0.05 0.86737560   two.sided
#> 200  27 0.55864236 0.05 0.88067392   two.sided
#> 201  28 0.55864236 0.05 0.89276166   two.sided
#> 202  29 0.55864236 0.05 0.90373119   two.sided
#> 203  30 0.55864236 0.05 0.91367054   two.sided
#> 204  31 0.55864236 0.05 0.92266320   two.sided
#> 205  32 0.55864236 0.05 0.93078784   two.sided
#> 206  33 0.55864236 0.05 0.93811836   two.sided
#> 207  34 0.55864236 0.05 0.94472383   two.sided
#> 208  35 0.55864236 0.05 0.95066858   two.sided
#> 209  36 0.55864236 0.05 0.95601233   two.sided
#> 210  37 0.55864236 0.05 0.96081033   two.sided
#> 211  38 0.55864236 0.05 0.96511359   two.sided
#> 212  39 0.55864236 0.05 0.96896901   two.sided
#> 213  40 0.55864236 0.05 0.97241967   two.sided
#> 214  41 0.55864236 0.05 0.97550502   two.sided
#> 215  42 0.55864236 0.05 0.97826109   two.sided
#> 216  43 0.55864236 0.05 0.98072075   two.sided
#> 217  44 0.55864236 0.05 0.98291393   two.sided
#> 218  45 0.55864236 0.05 0.98486780   two.sided
#> 219  46 0.55864236 0.05 0.98660702   two.sided
#> 220  47 0.55864236 0.05 0.98815392   two.sided
#> 221  48 0.55864236 0.05 0.98952868   two.sided
#> 222  49 0.55864236 0.05 0.99074952   two.sided
#> 223  50 0.55864236 0.05 0.99183287   two.sided
#> 224  51 0.55864236 0.05 0.99279352   two.sided
#> 225  52 0.55864236 0.05 0.99364478   two.sided
#> 226  53 0.55864236 0.05 0.99439857   two.sided
#> 227  54 0.55864236 0.05 0.99506562   two.sided
#> 228  55 0.55864236 0.05 0.99565554   two.sided
#> 229  56 0.55864236 0.05 0.99617690   two.sided
#> 230  57 0.55864236 0.05 0.99663739   two.sided
#> 231  58 0.55864236 0.05 0.99704388   two.sided
#> 232  59 0.55864236 0.05 0.99740249   two.sided
#> 233  60 0.55864236 0.05 0.99771868   two.sided
#> 234  61 0.55864236 0.05 0.99799731   two.sided
#> 235  62 0.55864236 0.05 0.99824270   two.sided
#> 236  63 0.55864236 0.05 0.99845872   two.sided
#> 237  64 0.55864236 0.05 0.99864877   two.sided
#> 238  65 0.55864236 0.05 0.99881590   two.sided
#> 239  66 0.55864236 0.05 0.99896279   two.sided
#> 240  67 0.55864236 0.05 0.99909183   two.sided
#> 241  68 0.55864236 0.05 0.99920514   two.sided
#> 242  69 0.55864236 0.05 0.99930459   two.sided
#> 243  70 0.55864236 0.05 0.99939184   two.sided
#> 244  71 0.55864236 0.05 0.99946834   two.sided
#> 245  72 0.55864236 0.05 0.99953539   two.sided
#> 246  73 0.55864236 0.05 0.99959414   two.sided
#> 247  74 0.55864236 0.05 0.99964558   two.sided
#> 248  75 0.55864236 0.05 0.99969062   two.sided
#> 249  76 0.55864236 0.05 0.99973003   two.sided
#> 250  77 0.55864236 0.05 0.99976450   two.sided
#> 251  78 0.55864236 0.05 0.99979464   two.sided
#> 252  79 0.55864236 0.05 0.99982098   two.sided
#> 253  80 0.55864236 0.05 0.99984399   two.sided
#> 254  81 0.55864236 0.05 0.99986409   two.sided
#> 255  82 0.55864236 0.05 0.99988164   two.sided
#> 256  83 0.55864236 0.05 0.99989695   two.sided
#> 257  84 0.55864236 0.05 0.99991031   two.sided
#> 258  85 0.55864236 0.05 0.99992196   two.sided
#> 259  86 0.55864236 0.05 0.99993212   two.sided
#> 260  87 0.55864236 0.05 0.99994098   two.sided
#> 261  88 0.55864236 0.05 0.99994869   two.sided
#> 262  89 0.55864236 0.05 0.99995541   two.sided
#> 263  90 0.55864236 0.05 0.99996126   two.sided
#> 264  91 0.55864236 0.05 0.99996635   two.sided
#> 265  92 0.55864236 0.05 0.99997078   two.sided
#> 266  93 0.55864236 0.05 0.99997463   two.sided
#> 267  94 0.55864236 0.05 0.99997798   two.sided
#> 268  95 0.55864236 0.05 0.99998090   two.sided
#> 269  96 0.55864236 0.05 0.99998343   two.sided
#> 270  97 0.55864236 0.05 0.99998563   two.sided
#> 271  98 0.55864236 0.05 0.99998754   two.sided
#> 272  99 0.55864236 0.05 0.99998920   two.sided
#> 273 100 0.55864236 0.05 0.99999064   two.sided
#> 274  10 0.59124207 0.05 0.46616725   two.sided
#> 275  11 0.59124207 0.05 0.51441850   two.sided
#> 276  12 0.59124207 0.05 0.55966036   two.sided
#> 277  13 0.59124207 0.05 0.60182458   two.sided
#> 278  14 0.59124207 0.05 0.64091124   two.sided
#> 279  15 0.59124207 0.05 0.67697193   two.sided
#> 280  16 0.59124207 0.05 0.71009699   two.sided
#> 281  17 0.59124207 0.05 0.74040516   two.sided
#> 282  18 0.59124207 0.05 0.76803533   two.sided
#> 283  19 0.59124207 0.05 0.79313967   two.sided
#> 284  20 0.59124207 0.05 0.81587813   two.sided
#> 285  21 0.59124207 0.05 0.83641404   two.sided
#> 286  22 0.59124207 0.05 0.85491056   two.sided
#> 287  23 0.59124207 0.05 0.87152796   two.sided
#> 288  24 0.59124207 0.05 0.88642158   two.sided
#> 289  25 0.59124207 0.05 0.89974024   two.sided
#> 290  26 0.59124207 0.05 0.91162523   two.sided
#> 291  27 0.59124207 0.05 0.92220958   two.sided
#> 292  28 0.59124207 0.05 0.93161769   two.sided
#> 293  29 0.59124207 0.05 0.93996514   two.sided
#> 294  30 0.59124207 0.05 0.94735875   two.sided
#> 295  31 0.59124207 0.05 0.95389678   two.sided
#> 296  32 0.59124207 0.05 0.95966916   two.sided
#> 297  33 0.59124207 0.05 0.96475793   two.sided
#> 298  34 0.59124207 0.05 0.96923761   two.sided
#> 299  35 0.59124207 0.05 0.97317567   two.sided
#> 300  36 0.59124207 0.05 0.97663304   two.sided
#> 301  37 0.59124207 0.05 0.97966453   two.sided
#> 302  38 0.59124207 0.05 0.98231937   two.sided
#> 303  39 0.59124207 0.05 0.98464162   two.sided
#> 304  40 0.59124207 0.05 0.98667065   two.sided
#> 305  41 0.59124207 0.05 0.98844155   two.sided
#> 306  42 0.59124207 0.05 0.98998552   two.sided
#> 307  43 0.59124207 0.05 0.99133027   two.sided
#> 308  44 0.59124207 0.05 0.99250037   two.sided
#> 309  45 0.59124207 0.05 0.99351751   two.sided
#> 310  46 0.59124207 0.05 0.99440088   two.sided
#> 311  47 0.59124207 0.05 0.99516738   two.sided
#> 312  48 0.59124207 0.05 0.99583190   two.sided
#> 313  49 0.59124207 0.05 0.99640752   two.sided
#> 314  50 0.59124207 0.05 0.99690573   two.sided
#> 315  51 0.59124207 0.05 0.99733660   two.sided
#> 316  52 0.59124207 0.05 0.99770893   two.sided
#> 317  53 0.59124207 0.05 0.99803045   two.sided
#> 318  54 0.59124207 0.05 0.99830788   two.sided
#> 319  55 0.59124207 0.05 0.99854709   two.sided
#> 320  56 0.59124207 0.05 0.99875322   two.sided
#> 321  57 0.59124207 0.05 0.99893071   two.sided
#> 322  58 0.59124207 0.05 0.99908345   two.sided
#> 323  59 0.59124207 0.05 0.99921479   two.sided
#> 324  60 0.59124207 0.05 0.99932767   two.sided
#> 325  61 0.59124207 0.05 0.99942463   two.sided
#> 326  62 0.59124207 0.05 0.99950785   two.sided
#> 327  63 0.59124207 0.05 0.99957925   two.sided
#> 328  64 0.59124207 0.05 0.99964047   two.sided
#> 329  65 0.59124207 0.05 0.99969293   two.sided
#> 330  66 0.59124207 0.05 0.99973785   two.sided
#> 331  67 0.59124207 0.05 0.99977631   two.sided
#> 332  68 0.59124207 0.05 0.99980921   two.sided
#> 333  69 0.59124207 0.05 0.99983735   two.sided
#> 334  70 0.59124207 0.05 0.99986139   two.sided
#> 335  71 0.59124207 0.05 0.99988194   two.sided
#> 336  72 0.59124207 0.05 0.99989948   two.sided
#> 337  73 0.59124207 0.05 0.99991445   two.sided
#> 338  74 0.59124207 0.05 0.99992722   two.sided
#> 339  75 0.59124207 0.05 0.99993810   two.sided
#> 340  76 0.59124207 0.05 0.99994738   two.sided
#> 341  77 0.59124207 0.05 0.99995529   two.sided
#> 342  78 0.59124207 0.05 0.99996202   two.sided
#> 343  79 0.59124207 0.05 0.99996775   two.sided
#> 344  80 0.59124207 0.05 0.99997263   two.sided
#> 345  81 0.59124207 0.05 0.99997678   two.sided
#> 346  82 0.59124207 0.05 0.99998030   two.sided
#> 347  83 0.59124207 0.05 0.99998330   two.sided
#> 348  84 0.59124207 0.05 0.99998584   two.sided
#> 349  85 0.59124207 0.05 0.99998801   two.sided
#> 350  86 0.59124207 0.05 0.99998984   two.sided
#> 351  87 0.59124207 0.05 0.99999140   two.sided
#> 352  88 0.59124207 0.05 0.99999272   two.sided
#> 353  89 0.59124207 0.05 0.99999384   two.sided
#> 354  90 0.59124207 0.05 0.99999479   two.sided
#> 355  91 0.59124207 0.05 0.99999559   two.sided
#> 356  92 0.59124207 0.05 0.99999627   two.sided
#> 357  93 0.59124207 0.05 0.99999685   two.sided
#> 358  94 0.59124207 0.05 0.99999734   two.sided
#> 359  95 0.59124207 0.05 0.99999775   two.sided
#> 360  96 0.59124207 0.05 0.99999810   two.sided
#> 361  97 0.59124207 0.05 0.99999840   two.sided
#> 362  98 0.59124207 0.05 0.99999865   two.sided
#> 363  99 0.59124207 0.05 0.99999886   two.sided
#> 364 100 0.59124207 0.05 0.99999904   two.sided
#>                                                                  method
#> 1   approximate correlation power calculation (arctangh transformation)
#> 2   approximate correlation power calculation (arctangh transformation)
#> 3   approximate correlation power calculation (arctangh transformation)
#> 4   approximate correlation power calculation (arctangh transformation)
#> 5   approximate correlation power calculation (arctangh transformation)
#> 6   approximate correlation power calculation (arctangh transformation)
#> 7   approximate correlation power calculation (arctangh transformation)
#> 8   approximate correlation power calculation (arctangh transformation)
#> 9   approximate correlation power calculation (arctangh transformation)
#> 10  approximate correlation power calculation (arctangh transformation)
#> 11  approximate correlation power calculation (arctangh transformation)
#> 12  approximate correlation power calculation (arctangh transformation)
#> 13  approximate correlation power calculation (arctangh transformation)
#> 14  approximate correlation power calculation (arctangh transformation)
#> 15  approximate correlation power calculation (arctangh transformation)
#> 16  approximate correlation power calculation (arctangh transformation)
#> 17  approximate correlation power calculation (arctangh transformation)
#> 18  approximate correlation power calculation (arctangh transformation)
#> 19  approximate correlation power calculation (arctangh transformation)
#> 20  approximate correlation power calculation (arctangh transformation)
#> 21  approximate correlation power calculation (arctangh transformation)
#> 22  approximate correlation power calculation (arctangh transformation)
#> 23  approximate correlation power calculation (arctangh transformation)
#> 24  approximate correlation power calculation (arctangh transformation)
#> 25  approximate correlation power calculation (arctangh transformation)
#> 26  approximate correlation power calculation (arctangh transformation)
#> 27  approximate correlation power calculation (arctangh transformation)
#> 28  approximate correlation power calculation (arctangh transformation)
#> 29  approximate correlation power calculation (arctangh transformation)
#> 30  approximate correlation power calculation (arctangh transformation)
#> 31  approximate correlation power calculation (arctangh transformation)
#> 32  approximate correlation power calculation (arctangh transformation)
#> 33  approximate correlation power calculation (arctangh transformation)
#> 34  approximate correlation power calculation (arctangh transformation)
#> 35  approximate correlation power calculation (arctangh transformation)
#> 36  approximate correlation power calculation (arctangh transformation)
#> 37  approximate correlation power calculation (arctangh transformation)
#> 38  approximate correlation power calculation (arctangh transformation)
#> 39  approximate correlation power calculation (arctangh transformation)
#> 40  approximate correlation power calculation (arctangh transformation)
#> 41  approximate correlation power calculation (arctangh transformation)
#> 42  approximate correlation power calculation (arctangh transformation)
#> 43  approximate correlation power calculation (arctangh transformation)
#> 44  approximate correlation power calculation (arctangh transformation)
#> 45  approximate correlation power calculation (arctangh transformation)
#> 46  approximate correlation power calculation (arctangh transformation)
#> 47  approximate correlation power calculation (arctangh transformation)
#> 48  approximate correlation power calculation (arctangh transformation)
#> 49  approximate correlation power calculation (arctangh transformation)
#> 50  approximate correlation power calculation (arctangh transformation)
#> 51  approximate correlation power calculation (arctangh transformation)
#> 52  approximate correlation power calculation (arctangh transformation)
#> 53  approximate correlation power calculation (arctangh transformation)
#> 54  approximate correlation power calculation (arctangh transformation)
#> 55  approximate correlation power calculation (arctangh transformation)
#> 56  approximate correlation power calculation (arctangh transformation)
#> 57  approximate correlation power calculation (arctangh transformation)
#> 58  approximate correlation power calculation (arctangh transformation)
#> 59  approximate correlation power calculation (arctangh transformation)
#> 60  approximate correlation power calculation (arctangh transformation)
#> 61  approximate correlation power calculation (arctangh transformation)
#> 62  approximate correlation power calculation (arctangh transformation)
#> 63  approximate correlation power calculation (arctangh transformation)
#> 64  approximate correlation power calculation (arctangh transformation)
#> 65  approximate correlation power calculation (arctangh transformation)
#> 66  approximate correlation power calculation (arctangh transformation)
#> 67  approximate correlation power calculation (arctangh transformation)
#> 68  approximate correlation power calculation (arctangh transformation)
#> 69  approximate correlation power calculation (arctangh transformation)
#> 70  approximate correlation power calculation (arctangh transformation)
#> 71  approximate correlation power calculation (arctangh transformation)
#> 72  approximate correlation power calculation (arctangh transformation)
#> 73  approximate correlation power calculation (arctangh transformation)
#> 74  approximate correlation power calculation (arctangh transformation)
#> 75  approximate correlation power calculation (arctangh transformation)
#> 76  approximate correlation power calculation (arctangh transformation)
#> 77  approximate correlation power calculation (arctangh transformation)
#> 78  approximate correlation power calculation (arctangh transformation)
#> 79  approximate correlation power calculation (arctangh transformation)
#> 80  approximate correlation power calculation (arctangh transformation)
#> 81  approximate correlation power calculation (arctangh transformation)
#> 82  approximate correlation power calculation (arctangh transformation)
#> 83  approximate correlation power calculation (arctangh transformation)
#> 84  approximate correlation power calculation (arctangh transformation)
#> 85  approximate correlation power calculation (arctangh transformation)
#> 86  approximate correlation power calculation (arctangh transformation)
#> 87  approximate correlation power calculation (arctangh transformation)
#> 88  approximate correlation power calculation (arctangh transformation)
#> 89  approximate correlation power calculation (arctangh transformation)
#> 90  approximate correlation power calculation (arctangh transformation)
#> 91  approximate correlation power calculation (arctangh transformation)
#> 92  approximate correlation power calculation (arctangh transformation)
#> 93  approximate correlation power calculation (arctangh transformation)
#> 94  approximate correlation power calculation (arctangh transformation)
#> 95  approximate correlation power calculation (arctangh transformation)
#> 96  approximate correlation power calculation (arctangh transformation)
#> 97  approximate correlation power calculation (arctangh transformation)
#> 98  approximate correlation power calculation (arctangh transformation)
#> 99  approximate correlation power calculation (arctangh transformation)
#> 100 approximate correlation power calculation (arctangh transformation)
#> 101 approximate correlation power calculation (arctangh transformation)
#> 102 approximate correlation power calculation (arctangh transformation)
#> 103 approximate correlation power calculation (arctangh transformation)
#> 104 approximate correlation power calculation (arctangh transformation)
#> 105 approximate correlation power calculation (arctangh transformation)
#> 106 approximate correlation power calculation (arctangh transformation)
#> 107 approximate correlation power calculation (arctangh transformation)
#> 108 approximate correlation power calculation (arctangh transformation)
#> 109 approximate correlation power calculation (arctangh transformation)
#> 110 approximate correlation power calculation (arctangh transformation)
#> 111 approximate correlation power calculation (arctangh transformation)
#> 112 approximate correlation power calculation (arctangh transformation)
#> 113 approximate correlation power calculation (arctangh transformation)
#> 114 approximate correlation power calculation (arctangh transformation)
#> 115 approximate correlation power calculation (arctangh transformation)
#> 116 approximate correlation power calculation (arctangh transformation)
#> 117 approximate correlation power calculation (arctangh transformation)
#> 118 approximate correlation power calculation (arctangh transformation)
#> 119 approximate correlation power calculation (arctangh transformation)
#> 120 approximate correlation power calculation (arctangh transformation)
#> 121 approximate correlation power calculation (arctangh transformation)
#> 122 approximate correlation power calculation (arctangh transformation)
#> 123 approximate correlation power calculation (arctangh transformation)
#> 124 approximate correlation power calculation (arctangh transformation)
#> 125 approximate correlation power calculation (arctangh transformation)
#> 126 approximate correlation power calculation (arctangh transformation)
#> 127 approximate correlation power calculation (arctangh transformation)
#> 128 approximate correlation power calculation (arctangh transformation)
#> 129 approximate correlation power calculation (arctangh transformation)
#> 130 approximate correlation power calculation (arctangh transformation)
#> 131 approximate correlation power calculation (arctangh transformation)
#> 132 approximate correlation power calculation (arctangh transformation)
#> 133 approximate correlation power calculation (arctangh transformation)
#> 134 approximate correlation power calculation (arctangh transformation)
#> 135 approximate correlation power calculation (arctangh transformation)
#> 136 approximate correlation power calculation (arctangh transformation)
#> 137 approximate correlation power calculation (arctangh transformation)
#> 138 approximate correlation power calculation (arctangh transformation)
#> 139 approximate correlation power calculation (arctangh transformation)
#> 140 approximate correlation power calculation (arctangh transformation)
#> 141 approximate correlation power calculation (arctangh transformation)
#> 142 approximate correlation power calculation (arctangh transformation)
#> 143 approximate correlation power calculation (arctangh transformation)
#> 144 approximate correlation power calculation (arctangh transformation)
#> 145 approximate correlation power calculation (arctangh transformation)
#> 146 approximate correlation power calculation (arctangh transformation)
#> 147 approximate correlation power calculation (arctangh transformation)
#> 148 approximate correlation power calculation (arctangh transformation)
#> 149 approximate correlation power calculation (arctangh transformation)
#> 150 approximate correlation power calculation (arctangh transformation)
#> 151 approximate correlation power calculation (arctangh transformation)
#> 152 approximate correlation power calculation (arctangh transformation)
#> 153 approximate correlation power calculation (arctangh transformation)
#> 154 approximate correlation power calculation (arctangh transformation)
#> 155 approximate correlation power calculation (arctangh transformation)
#> 156 approximate correlation power calculation (arctangh transformation)
#> 157 approximate correlation power calculation (arctangh transformation)
#> 158 approximate correlation power calculation (arctangh transformation)
#> 159 approximate correlation power calculation (arctangh transformation)
#> 160 approximate correlation power calculation (arctangh transformation)
#> 161 approximate correlation power calculation (arctangh transformation)
#> 162 approximate correlation power calculation (arctangh transformation)
#> 163 approximate correlation power calculation (arctangh transformation)
#> 164 approximate correlation power calculation (arctangh transformation)
#> 165 approximate correlation power calculation (arctangh transformation)
#> 166 approximate correlation power calculation (arctangh transformation)
#> 167 approximate correlation power calculation (arctangh transformation)
#> 168 approximate correlation power calculation (arctangh transformation)
#> 169 approximate correlation power calculation (arctangh transformation)
#> 170 approximate correlation power calculation (arctangh transformation)
#> 171 approximate correlation power calculation (arctangh transformation)
#> 172 approximate correlation power calculation (arctangh transformation)
#> 173 approximate correlation power calculation (arctangh transformation)
#> 174 approximate correlation power calculation (arctangh transformation)
#> 175 approximate correlation power calculation (arctangh transformation)
#> 176 approximate correlation power calculation (arctangh transformation)
#> 177 approximate correlation power calculation (arctangh transformation)
#> 178 approximate correlation power calculation (arctangh transformation)
#> 179 approximate correlation power calculation (arctangh transformation)
#> 180 approximate correlation power calculation (arctangh transformation)
#> 181 approximate correlation power calculation (arctangh transformation)
#> 182 approximate correlation power calculation (arctangh transformation)
#> 183 approximate correlation power calculation (arctangh transformation)
#> 184 approximate correlation power calculation (arctangh transformation)
#> 185 approximate correlation power calculation (arctangh transformation)
#> 186 approximate correlation power calculation (arctangh transformation)
#> 187 approximate correlation power calculation (arctangh transformation)
#> 188 approximate correlation power calculation (arctangh transformation)
#> 189 approximate correlation power calculation (arctangh transformation)
#> 190 approximate correlation power calculation (arctangh transformation)
#> 191 approximate correlation power calculation (arctangh transformation)
#> 192 approximate correlation power calculation (arctangh transformation)
#> 193 approximate correlation power calculation (arctangh transformation)
#> 194 approximate correlation power calculation (arctangh transformation)
#> 195 approximate correlation power calculation (arctangh transformation)
#> 196 approximate correlation power calculation (arctangh transformation)
#> 197 approximate correlation power calculation (arctangh transformation)
#> 198 approximate correlation power calculation (arctangh transformation)
#> 199 approximate correlation power calculation (arctangh transformation)
#> 200 approximate correlation power calculation (arctangh transformation)
#> 201 approximate correlation power calculation (arctangh transformation)
#> 202 approximate correlation power calculation (arctangh transformation)
#> 203 approximate correlation power calculation (arctangh transformation)
#> 204 approximate correlation power calculation (arctangh transformation)
#> 205 approximate correlation power calculation (arctangh transformation)
#> 206 approximate correlation power calculation (arctangh transformation)
#> 207 approximate correlation power calculation (arctangh transformation)
#> 208 approximate correlation power calculation (arctangh transformation)
#> 209 approximate correlation power calculation (arctangh transformation)
#> 210 approximate correlation power calculation (arctangh transformation)
#> 211 approximate correlation power calculation (arctangh transformation)
#> 212 approximate correlation power calculation (arctangh transformation)
#> 213 approximate correlation power calculation (arctangh transformation)
#> 214 approximate correlation power calculation (arctangh transformation)
#> 215 approximate correlation power calculation (arctangh transformation)
#> 216 approximate correlation power calculation (arctangh transformation)
#> 217 approximate correlation power calculation (arctangh transformation)
#> 218 approximate correlation power calculation (arctangh transformation)
#> 219 approximate correlation power calculation (arctangh transformation)
#> 220 approximate correlation power calculation (arctangh transformation)
#> 221 approximate correlation power calculation (arctangh transformation)
#> 222 approximate correlation power calculation (arctangh transformation)
#> 223 approximate correlation power calculation (arctangh transformation)
#> 224 approximate correlation power calculation (arctangh transformation)
#> 225 approximate correlation power calculation (arctangh transformation)
#> 226 approximate correlation power calculation (arctangh transformation)
#> 227 approximate correlation power calculation (arctangh transformation)
#> 228 approximate correlation power calculation (arctangh transformation)
#> 229 approximate correlation power calculation (arctangh transformation)
#> 230 approximate correlation power calculation (arctangh transformation)
#> 231 approximate correlation power calculation (arctangh transformation)
#> 232 approximate correlation power calculation (arctangh transformation)
#> 233 approximate correlation power calculation (arctangh transformation)
#> 234 approximate correlation power calculation (arctangh transformation)
#> 235 approximate correlation power calculation (arctangh transformation)
#> 236 approximate correlation power calculation (arctangh transformation)
#> 237 approximate correlation power calculation (arctangh transformation)
#> 238 approximate correlation power calculation (arctangh transformation)
#> 239 approximate correlation power calculation (arctangh transformation)
#> 240 approximate correlation power calculation (arctangh transformation)
#> 241 approximate correlation power calculation (arctangh transformation)
#> 242 approximate correlation power calculation (arctangh transformation)
#> 243 approximate correlation power calculation (arctangh transformation)
#> 244 approximate correlation power calculation (arctangh transformation)
#> 245 approximate correlation power calculation (arctangh transformation)
#> 246 approximate correlation power calculation (arctangh transformation)
#> 247 approximate correlation power calculation (arctangh transformation)
#> 248 approximate correlation power calculation (arctangh transformation)
#> 249 approximate correlation power calculation (arctangh transformation)
#> 250 approximate correlation power calculation (arctangh transformation)
#> 251 approximate correlation power calculation (arctangh transformation)
#> 252 approximate correlation power calculation (arctangh transformation)
#> 253 approximate correlation power calculation (arctangh transformation)
#> 254 approximate correlation power calculation (arctangh transformation)
#> 255 approximate correlation power calculation (arctangh transformation)
#> 256 approximate correlation power calculation (arctangh transformation)
#> 257 approximate correlation power calculation (arctangh transformation)
#> 258 approximate correlation power calculation (arctangh transformation)
#> 259 approximate correlation power calculation (arctangh transformation)
#> 260 approximate correlation power calculation (arctangh transformation)
#> 261 approximate correlation power calculation (arctangh transformation)
#> 262 approximate correlation power calculation (arctangh transformation)
#> 263 approximate correlation power calculation (arctangh transformation)
#> 264 approximate correlation power calculation (arctangh transformation)
#> 265 approximate correlation power calculation (arctangh transformation)
#> 266 approximate correlation power calculation (arctangh transformation)
#> 267 approximate correlation power calculation (arctangh transformation)
#> 268 approximate correlation power calculation (arctangh transformation)
#> 269 approximate correlation power calculation (arctangh transformation)
#> 270 approximate correlation power calculation (arctangh transformation)
#> 271 approximate correlation power calculation (arctangh transformation)
#> 272 approximate correlation power calculation (arctangh transformation)
#> 273 approximate correlation power calculation (arctangh transformation)
#> 274 approximate correlation power calculation (arctangh transformation)
#> 275 approximate correlation power calculation (arctangh transformation)
#> 276 approximate correlation power calculation (arctangh transformation)
#> 277 approximate correlation power calculation (arctangh transformation)
#> 278 approximate correlation power calculation (arctangh transformation)
#> 279 approximate correlation power calculation (arctangh transformation)
#> 280 approximate correlation power calculation (arctangh transformation)
#> 281 approximate correlation power calculation (arctangh transformation)
#> 282 approximate correlation power calculation (arctangh transformation)
#> 283 approximate correlation power calculation (arctangh transformation)
#> 284 approximate correlation power calculation (arctangh transformation)
#> 285 approximate correlation power calculation (arctangh transformation)
#> 286 approximate correlation power calculation (arctangh transformation)
#> 287 approximate correlation power calculation (arctangh transformation)
#> 288 approximate correlation power calculation (arctangh transformation)
#> 289 approximate correlation power calculation (arctangh transformation)
#> 290 approximate correlation power calculation (arctangh transformation)
#> 291 approximate correlation power calculation (arctangh transformation)
#> 292 approximate correlation power calculation (arctangh transformation)
#> 293 approximate correlation power calculation (arctangh transformation)
#> 294 approximate correlation power calculation (arctangh transformation)
#> 295 approximate correlation power calculation (arctangh transformation)
#> 296 approximate correlation power calculation (arctangh transformation)
#> 297 approximate correlation power calculation (arctangh transformation)
#> 298 approximate correlation power calculation (arctangh transformation)
#> 299 approximate correlation power calculation (arctangh transformation)
#> 300 approximate correlation power calculation (arctangh transformation)
#> 301 approximate correlation power calculation (arctangh transformation)
#> 302 approximate correlation power calculation (arctangh transformation)
#> 303 approximate correlation power calculation (arctangh transformation)
#> 304 approximate correlation power calculation (arctangh transformation)
#> 305 approximate correlation power calculation (arctangh transformation)
#> 306 approximate correlation power calculation (arctangh transformation)
#> 307 approximate correlation power calculation (arctangh transformation)
#> 308 approximate correlation power calculation (arctangh transformation)
#> 309 approximate correlation power calculation (arctangh transformation)
#> 310 approximate correlation power calculation (arctangh transformation)
#> 311 approximate correlation power calculation (arctangh transformation)
#> 312 approximate correlation power calculation (arctangh transformation)
#> 313 approximate correlation power calculation (arctangh transformation)
#> 314 approximate correlation power calculation (arctangh transformation)
#> 315 approximate correlation power calculation (arctangh transformation)
#> 316 approximate correlation power calculation (arctangh transformation)
#> 317 approximate correlation power calculation (arctangh transformation)
#> 318 approximate correlation power calculation (arctangh transformation)
#> 319 approximate correlation power calculation (arctangh transformation)
#> 320 approximate correlation power calculation (arctangh transformation)
#> 321 approximate correlation power calculation (arctangh transformation)
#> 322 approximate correlation power calculation (arctangh transformation)
#> 323 approximate correlation power calculation (arctangh transformation)
#> 324 approximate correlation power calculation (arctangh transformation)
#> 325 approximate correlation power calculation (arctangh transformation)
#> 326 approximate correlation power calculation (arctangh transformation)
#> 327 approximate correlation power calculation (arctangh transformation)
#> 328 approximate correlation power calculation (arctangh transformation)
#> 329 approximate correlation power calculation (arctangh transformation)
#> 330 approximate correlation power calculation (arctangh transformation)
#> 331 approximate correlation power calculation (arctangh transformation)
#> 332 approximate correlation power calculation (arctangh transformation)
#> 333 approximate correlation power calculation (arctangh transformation)
#> 334 approximate correlation power calculation (arctangh transformation)
#> 335 approximate correlation power calculation (arctangh transformation)
#> 336 approximate correlation power calculation (arctangh transformation)
#> 337 approximate correlation power calculation (arctangh transformation)
#> 338 approximate correlation power calculation (arctangh transformation)
#> 339 approximate correlation power calculation (arctangh transformation)
#> 340 approximate correlation power calculation (arctangh transformation)
#> 341 approximate correlation power calculation (arctangh transformation)
#> 342 approximate correlation power calculation (arctangh transformation)
#> 343 approximate correlation power calculation (arctangh transformation)
#> 344 approximate correlation power calculation (arctangh transformation)
#> 345 approximate correlation power calculation (arctangh transformation)
#> 346 approximate correlation power calculation (arctangh transformation)
#> 347 approximate correlation power calculation (arctangh transformation)
#> 348 approximate correlation power calculation (arctangh transformation)
#> 349 approximate correlation power calculation (arctangh transformation)
#> 350 approximate correlation power calculation (arctangh transformation)
#> 351 approximate correlation power calculation (arctangh transformation)
#> 352 approximate correlation power calculation (arctangh transformation)
#> 353 approximate correlation power calculation (arctangh transformation)
#> 354 approximate correlation power calculation (arctangh transformation)
#> 355 approximate correlation power calculation (arctangh transformation)
#> 356 approximate correlation power calculation (arctangh transformation)
#> 357 approximate correlation power calculation (arctangh transformation)
#> 358 approximate correlation power calculation (arctangh transformation)
#> 359 approximate correlation power calculation (arctangh transformation)
#> 360 approximate correlation power calculation (arctangh transformation)
#> 361 approximate correlation power calculation (arctangh transformation)
#> 362 approximate correlation power calculation (arctangh transformation)
#> 363 approximate correlation power calculation (arctangh transformation)
#> 364 approximate correlation power calculation (arctangh transformation)
#> 
```
