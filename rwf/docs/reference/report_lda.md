# Report for MASS::lda

Report for MASS::lda

## Usage

``` r
report_lda(model, file = NULL, w = 10, h = 10, base_size = 10, title = "")
```

## Arguments

- model:

  object from MASS::lda

- file:

  output filename

- w:

  width of pdf file

- h:

  height of pdf file

- base_size:

  base font size

- title:

  plot title

## Examples

``` r
model<-MASS::lda(case~.,data=infert)
result<-report_lda(model=model)
result<-report_lda(model=model,file="lda")
model<-MASS::lda(Species~.,data=iris)
result<-report_lda(model=model,file="lda")
```
