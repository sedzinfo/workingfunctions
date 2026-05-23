#########################################################################################
SCATTERPLOT
#########################################################################################
#' @title Plot plot_scatterplot
#' @param df dataframe if dataframe consists of 2 collumns the second collumn is the outcome and the first collumn is the predictor
#' @param method smoothing method, "auto", "lm", "glm", "gam", "loess" or a function,
#' e.g. MASS::rlm or mgcv::gam, stats::lm, or stats::loess
#' @param formula formula used in smoothing function for geom_smooth
#' @param base_size base font size
#' @param coord_equal if TRUE axes maintain equal scale
#' @param all_orders if TRUE the order of combination is considered
#' @param combinations dataframe if not NULL user can provide a dataframe for variable combinations for x and y axis . First column represents x and second column represents y
#' @param string_aes if TRUE string_aes function is used for names
#' @param title Plot title
#' @import ggplot2 foreach doSNOW
#' @importFrom parallel makeCluster stopCluster
#' @importFrom ggpubr as_ggplot
#' @importFrom ggExtra ggMarginal
#' @keywords regression
#' @export
#' @examples
#' result<-plot_scatterplot(df=mtcars,title="",coord_equal=TRUE,base_size=10)
#' plot_multiplot(plotlist=result[1:12],cols=4)
#' plot_scatterplot(df=mtcars[,1:2],base_size=10,coord_equal=TRUE,all_orders=FALSE)
#' plot_scatterplot(df=mtcars[,1:2],base_size=10,coord_equal=FALSE,all_orders=FALSE)
#' plot_scatterplot(df=mtcars,base_size=10,coord_equal=TRUE,all_orders=FALSE,
#'                  combinations=data.frame(x=c("mpg","mpg","mpg"),
#'                                          y=c("cyl","hp","mpg")))
#' plot_scatterplot(df=mtcars,base_size=10,coord_equal=TRUE,all_orders=TRUE,
#'                  combinations=data.frame(x=c("mpg"),y=c("cyl")))
#' x<-rnorm(1000)
#' y<-x+rnorm(x,sd=.1)
#' plot_scatterplot(df=data.frame(x,y),title="Random Simulation",coord_equal=TRUE)
#' df<-data.frame(matrix(-.999,ncol=2,nrow=2))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-generate_correlation_matrix(correlation_martix,nrows=1000)
#' plot_scatterplot(df,title="Simulation of -.999 Correlation",coord_equal=TRUE,base_size=20)
plot_scatterplot<-function(df,method=lm,formula=y~x,base_size=10,coord_equal=FALSE,all_orders=FALSE,title="",combinations=NULL,string_aes=TRUE) {
  output_plot<-function(i) {
    tempdata<-data.frame(df[,combinations[i,1]],df[,combinations[i,2]])
    names(tempdata)<-c(combinations[i,1],combinations[i,2])
    tempdata<-tempdata[complete.cases(tempdata),]
    if(nrow(tempdata)>=2) {
      if(string_aes)
        names(tempdata)<-string_aes(names(tempdata))
      pearsonr<-stats::cor(tempdata[,1],tempdata[,2],use="pairwise")
      model<-lm(tempdata[,2]~tempdata[,1])
      model_coef<-coef(model)
      slope<-model_coef[[2]]
      degrees<-rad2deg(atan(slope))
      if(degrees>180)
        degrees<-360-degrees
      if(as.character(toString(formula))=="~, y, x") {
        note<-paste0("Pairwise n = ",nrow(tempdata[complete.cases(tempdata),]),
                     "\nPearson r = ",round(pearsonr,4),
                     "\nExplained Variance = ",round(pearsonr^2,4)*100,"%",
                     "\ny = ",round(model_coef[[2]],4),
                     "x + ",round(model_coef[[1]],4),
                     "\nAngle = ",round(degrees,2))
      }
      else {
        note<-paste0("Pairwise n = ",nrow(tempdata[complete.cases(tempdata),]))
      }
      scatter<-ggplot(tempdata,aes(x=tempdata[,1],y=tempdata[,2]))+
        # geom_point(alpha=.1)+
        geom_count(alpha=.1)+
        geom_smooth(method=method,se=TRUE,na.rm=TRUE,formula=formula)+
        labs(x=names(tempdata)[1],y=names(tempdata)[2],title=title,caption=note)+
        geom_rug(size=0.1,alpha=.1)+
        theme_bw(base_size=base_size)+
        theme(legend.position="left",
              legend.justification=c(0,1),
              legend.margin=margin(t=-10,r=0,b=0,l=0,unit="pt"))
      if(min(tempdata[,1],na.rm=TRUE)<=0&max(tempdata[,1],na.rm=TRUE)>=0)
        scatter<-scatter+geom_vline(xintercept=0,alpha=.5,color="gray25")
      if(min(tempdata[,1],na.rm=TRUE)<=0&max(tempdata[,1],na.rm=TRUE)>=0)
        scatter<-scatter+geom_hline(yintercept=0,alpha=.5,color="gray25")
      if(coord_equal) {
        maximum_xy<-max(c(tempdata[,1],tempdata[,2]))
        minimum_xy<-min(c(tempdata[,1],tempdata[,2]))
        scatter<-scatter+coord_equal()
        scatter<-scatter+scale_x_continuous(limits=c(minimum_xy,maximum_xy))+scale_y_continuous(limits=c(minimum_xy,maximum_xy))
      }
      ggpubr::as_ggplot(ggExtra::ggMarginal(scatter,type="histogram",fill="gray25",color="gray50"))
    }
  }

  if(is.null(combinations)) {
    combinations<-comparison_combinations(df,all_orders=all_orders)
    names(combinations)<-c("x","y")
  }
  combinations<-change_data_type(combinations,type="character")
  combinations<-combinations[!combinations[,1]==combinations[,2],]
  row.names(combinations)<-paste0(combinations[,1],"_",combinations[,2])

  n_rows<-nrow(combinations)
  n_cores<-parallel::detectCores()
  if(n_cores*4<n_rows) {
    print(paste("parralel process with",n_cores,"workers for",n_rows,"tasks"))
    parralel=TRUE
  } else {
    parralel=FALSE
  }

  pb<-txtProgressBar(min=0,max=nrow(combinations),style=3)
  if(parralel) {
    cl<-parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    progress<-function(n) setTxtProgressBar(pb,n)
    opts<-list(progress=progress)
    scatterplots<-foreach(i=1:nrow(combinations),.final=function(x) setNames(x,row.names(combinations)),.packages=c("rwf"),.options.snow=opts) %dopar% {
      pdf(NULL)
      output_plot(i)
    }
    close(pb)
    parallel::stopCluster(cl)
    gc(full=TRUE)
  } else {
    scatterplots<-list()
    for(i in 1:nrow(combinations)) {
      setTxtProgressBar(pb,i)
      scatterplots[[row.names(combinations)[i]]]<-output_plot(i)
    }
    close(pb)
  }
  return(scatterplots)
}
