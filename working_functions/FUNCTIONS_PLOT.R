##########################################################################################
# MULTIPLOT
##########################################################################################
#' @title Multiple ggplot plots in one graph
#' @param ... plot objects
#' @param plotlist a list of plots
#' @param cols number of columns in layout 
#' @param layout a matrix specifying the layout. If present,'cols' is ignored
#' @importFrom grid grid.newpage pushViewport viewport
#' @importFrom grDevices recordPlot
#' @keywords functions plot 
#' @export
#' @examples
#' p1<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet,group=Chick))+
#'            geom_line()+
#'            ggtitle("Growth curve for individual chicks")+
#'            theme_bw()
#' p2<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet))+
#'            geom_point(alpha=.3)+
#'            geom_smooth(alpha=.2,size=1,method="loess",formula="y~x")+
#'            ggtitle("Fitted growth curve per diet")+
#'            theme_bw()
#' p3<-ggplot(subset(ChickWeight,Time==21),aes(x=weight,colour=Diet))+
#'            geom_density()+
#'            ggtitle("Final weight, by diet")+theme_bw()
#' p4<-ggplot(subset(ChickWeight,Time==21),aes(x=weight,fill=Diet))+
#'            geom_histogram(colour="black",binwidth=50)+facet_grid(Diet~.)+
#'            ggtitle("Final weight, by diet")+theme_bw()
#' cars_plot<-plot_histogram(mtcars)
#' plot_multiplot(p1,p2,p3,p4,cols=2)
#' plot_multiplot(plotlist=plot_histogram(mtcars[,1:4]),cols=2)
#' plot_multiplot(plotlist=plot_histogram(mtcars),layout=matrix(1:4,ncol=2,byrow=TRUE))
#' plot_multiplot(plotlist=plot_scatterplot(mtcars[,1:4]),cols=2)
#' plot_multiplot(plotlist=cars_plot,layout=matrix(1:4,ncol=2,byrow=TRUE))
#' plot_multiplot(plotlist=cars_plot,cols=3)
plot_multiplot<-function(...,plotlist=NULL,cols=2,layout=NULL) {
  p<-list()
  plots<-c(list(...),plotlist)
  nplots=length(plots)
  if (is.null(layout))
    layout<-matrix(seq(1,cols*ceiling(nplots/cols)),
                   ncol=cols,
                   nrow=ceiling(nplots/cols),byrow=TRUE)
  if (nplots==1) {
    return(plots[[1]])
  } else {
    pages<-ceiling(nplots/max(layout))
    plots_per_page<-max(layout)
    counter<-1
    for(page in 1:pages){
      grid::grid.newpage()
      grid::pushViewport(viewport(layout=grid.layout(nrow(layout),ncol(layout))))
      for (i in 1:plots_per_page) {
        position<-as.data.frame(which(layout==i,arr.ind=TRUE))
        if(counter<=nplots)
          print(plots[[counter]],vp=grid::viewport(layout.pos.row=position$row,layout.pos.col=position$col))
        counter<-counter+1
      }
      p[[page]]<-grDevices::recordPlot()
    }
  }
  return(p)
}
##########################################################################################
# WRAPPER
##########################################################################################
#' @title Wrap string
#' @param x title
#' @param ... arguments passed to strwrap
#' @keywords functions plot 
#' @export
#' @examples
#' wrapper(rep("sting",50),30)
wrapper<-function(x,...) {
  paste(strwrap(x,...),collapse="\n")
}
##########################################################################################
# DUPLICATE Y AXIS
##########################################################################################
#' @title Invert title for duplicating y axis
#' @param grob grob object
#' @import grid gtable 
#' @keywords functions plot 
hinvert_title_grob<-function(grob) {
  widths<-grob$widths
  grob$widths[1]<-widths[3]
  grob$widths[3]<-widths[1]
  grob$vp[[1]]$layout$widths[1]<-widths[3]
  grob$vp[[1]]$layout$widths[3]<-widths[1]
  grob$children[[1]]$hjust<-1-grob$children[[1]]$hjust 
  grob$children[[1]]$vjust<-1-grob$children[[1]]$vjust 
  grob$children[[1]]$x<-unit(1,"npc")-grob$children[[1]]$x
  grob
}
#' @title Duplicate y axis
#' @param p1 Plot 1
#' @param p2 Plot 2
#' @importFrom ggplot2 ggplotGrob 
#' @importFrom gtable gtable_add_cols gtable_add_grob
#' @importFrom grid unit.c grid.newpage grid.draw
#' @keywords functions plot 
#' @export
#' @examples
#' p1<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet,group=Chick))+
#'            geom_line()+
#'            ggtitle("Growth curve for individual chicks")
#' duplicate_y_axis(p1,p1)
duplicate_y_axis<-function(p1,p2) {
  name<-r<-NULL
  g1<-ggplotGrob(p1)
  g2<-ggplotGrob(p2)
  pp<-c(subset(g1$layout,name=="panel",se=t:r))
  index<-which(g2$layout$name=="ylab-l")
  ylab<-g2$grobs[[index]]
  ylab<-hinvert_title_grob(ylab)
  g1<-gtable::gtable_add_cols(g1,g2$widths[g2$layout[index,]$l],pp$r)
  g1<-gtable::gtable_add_grob(g1,ylab,pp$t,pp$r+1,pp$b,pp$r+1,clip="off",name="ylab-r")
  index<-which(g2$layout$name == "axis-l")
  yaxis<-g2$grobs[[index]]
  yaxis$children[[1]]$x<-grid::unit.c(unit(0,"npc"),unit(0,"npc"))
  ticks<-yaxis$children[[2]]
  ticks$widths<-rev(ticks$widths)
  ticks$grobs<-rev(ticks$grobs)
  ticks$grobs[[1]]$x<-ticks$grobs[[1]]$x-unit(1,"npc")+unit(3,"pt")
  ticks$grobs[[2]]<-hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]]<-ticks
  g1<-gtable_add_cols(g1,g2$widths[g2$layout[index,]$l],pp$r)
  g1<-gtable_add_grob(g1,yaxis,pp$t,pp$r+1,pp$b,pp$r+1,clip="off",name="axis-r")
  grid::grid.newpage()
  grid::grid.draw(g1)
}
##########################################################################################
# REPORT PDF
##########################################################################################
#' @title Report pdf
#' @param ... plot objects
#' @param plotlist list of plot objects
#' @param file output filename
#' @param title output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @param print_plot if TRUE it prints plot on graphics device
#' @importFrom grDevices cairo_pdf dev.off pdf
#' @importFrom purrr walk
#' @keywords functions plot
#' @export
#' @examples
#' p1<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet,group=Chick))+
#'            geom_line()+
#'            ggtitle("Growth curve for individual chicks")+
#'            theme_bw()
#' p2<-ggplot(ChickWeight,aes(x=Time,y=weight,colour=Diet))+
#'            geom_point(alpha=.3)+
#'            geom_smooth(alpha=.2,size=1,method="loess",formula="y~x")+
#'            ggtitle("Fitted growth curve per diet")+theme_bw()
#' cars_plot_multiplot<-plot_multiplot(plotlist=plot_histogram(mtcars[,1:4]),cols=2)
#' cars_plot_base<-plot_normality_diagnostics(mtcars)
#' report_pdf(p1,p2,print_plot=TRUE)
#' report_pdf(p1,p2,file="report",print_plot=FALSE)
#' report_pdf(plotlist=cars_plot_multiplot,print_plot=TRUE)
#' report_pdf(plotlist=cars_plot_multiplot,file="report",print_plot=FALSE)
#' report_pdf(plotlist=cars_plot_base,print_plot=TRUE)
#' report_pdf(plotlist=cars_plot_base,file="report",print_plot=FALSE)
report_pdf<-function(...,plotlist=NULL,file=NULL,title=NULL,w=10,h=10,print_plot=TRUE) {
  plotlist<-c(list(...),plotlist)
  if(!is.null(title))
    title<-paste0("_",title)
  if(!is.null(file)) {
    cairo_pdf(invisible(paste0(file,title,".pdf")),onefile=TRUE,width=w,height=h)
    purrr::walk(plotlist,function(p) { print(p) })
    grDevices::dev.off()
  }
  if(print_plot) {
    purrr::walk(plotlist,function(p) { print(p) })
  }
}



