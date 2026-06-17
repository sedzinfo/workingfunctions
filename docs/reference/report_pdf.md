# Report pdf

Report pdf

## Usage

``` r
report_pdf(
  ...,
  plotlist = NULL,
  file = NULL,
  title = NULL,
  w = 10,
  h = 10,
  print_plot = TRUE
)
```

## Arguments

- ...:

  plot objects

- plotlist:

  list of plot objects

- file:

  output filename

- title:

  output filename

- w:

  width of pdf file

- h:

  height of pdf file

- print_plot:

  if TRUE it prints plot on graphics device

## Examples

``` r
p1<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet,group=Chick))+
           geom_line()+
           ggtitle("Growth curve for individual chicks")+
           theme_bw()
p2<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet))+
           geom_point(alpha=.3)+
           geom_smooth(alpha=.2,size=1,method="loess",formula="y~x")+
           ggtitle("Fitted growth curve per diet")+theme_bw()
cars_plot_multiplot<-plot_multiplot(plotlist=plot_histogram(mtcars[,1:4]),cols=2)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

cars_plot_base<-plot_normality_diagnostics(mtcars)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======                                                                |   9%

#>   |                                                                              |=============                                                         |  18%

#>   |                                                                              |===================                                                   |  27%

#>   |                                                                              |=========================                                             |  36%

#>   |                                                                              |================================                                      |  45%

#>   |                                                                              |======================================                                |  55%

#>   |                                                                              |=============================================                         |  64%

#>   |                                                                              |===================================================                   |  73%

#>   |                                                                              |=========================================================             |  82%

#>   |                                                                              |================================================================      |  91%

#>   |                                                                              |======================================================================| 100%

#> 
report_pdf(p1,p2,print_plot=TRUE)


report_pdf(p1,p2,file="report",print_plot=FALSE)
report_pdf(plotlist=cars_plot_multiplot,print_plot=TRUE)

report_pdf(plotlist=cars_plot_multiplot,file="report",print_plot=FALSE)
report_pdf(plotlist=cars_plot_base,print_plot=TRUE)

report_pdf(plotlist=cars_plot_base,file="report",print_plot=FALSE)
```
