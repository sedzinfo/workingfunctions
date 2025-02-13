##########################################################################################
# PLOT NORMALITY ASSUMPTIONS BASE PLOT
##########################################################################################
#' @title Normality plots
#' @description plot histogram density boxplot qq plot
#' @details uses plot base
#' @param df dataframe or vector with continous or ordinal data
#' @param breaks number of bars to display
#' @param title plot title
#' @param file output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @importFrom graphics plot par hist boxplot title
#' @importFrom stats qqnorm qqline na.omit density
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom grDevices recordPlot
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' plot_normality_diagnostics(df=vector,title="",file="rnorm",breaks=30)
#' plot_normality_diagnostics(df=vector,title="")
#' plot_normality_diagnostics(df=df,title="mtcars")
#' plot_normality_diagnostics(df=df,title="mtcars",file="rnorm")
plot_normality_diagnostics<-function(df,breaks=NULL,title="",file=NULL,w=10,h=10) {
  # default_par<-par(no.readonly=TRUE)
  par(mfrow=c(2,2),adj=.01)
  if(is.null(breaks))
    breaks<-"Sturges"
  plot<-list()
  data_name<-deparse(substitute(df))
  df<-data.frame(df)
  df<-data.frame(df[,sapply(df,is.numeric)])
  if(length(df)==1)
    names(df)<-data_name
  pb<-txtProgressBar(min=0,max=length(names(df)),style=3)
  counter<-0
  for (i in names(df)) {
    counter<-counter+1
    setTxtProgressBar(pb,counter)
    vector<-stats::na.omit(df[,i])
    if (length(vector)>2&var(vector)!=0) {
      hist(vector,main="Histogram",xlab="",warn.unused=FALSE,breaks=breaks)
      plot(stats::density(vector,na.rm=TRUE),main="Density Function")
      boxplot(vector,main="Boxplot",xlab="")
      stats::qqnorm(vector)
      stats::qqline(vector)
      title(main=title,
            sub=paste0("\nVariable=",string_aes(i),
                       "\nObservations=",length(vector),
                       "\nMean=",round(mean(vector),2),
                       "\nSD=",round(stats::sd(vector),2),
                       "\nMedian=",round(stats::median(vector),2)),
            outer=TRUE,line=-1)
      plot[[i]]<-recordPlot()
    } else {
      cat("Graph not produced for",i,"due to sample size\n")
    }
  }
  close(pb)
  # on.exit(par(default_par))
  par(mfrow=c(1,1),adj=0.5)
  report_pdf(plotlist=plot,file=file,title=title,w=w,h=h)
}
##########################################################################################
# PLOT OUTLIER
##########################################################################################
#' @title Outlier graph using mean median and boxplot algorythms
#' @param df dataframe or vector with continous or ordinal data
#' @param method "mean" "median" "boxplot"
#' @param title plot title
#' @param base_size base font size
#' @import ggplot2
#' @importFrom stats median sd quantile na.omit
#' @importFrom ggrepel geom_text_repel
#' @keywords assumptions
#' @author unknown
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' plot_outlier(df=vector,method="mean",title="random vector")
#' plot_outlier(df=vector,method="median")
#' plot_outlier(df=vector,method="boxplot")
#' plot_outlier(df=df,method="mean",title="random vector")
#' plot_outlier(df=df,method="median")
#' plot_outlier(df=df,method="boxplot")
#' plot_multiplot(plotlist=plot_outlier(df=mtcars[,2:5],method="mean"),cols=2)
plot_outlier<-function(df,method="mean",title="",base_size=10) {
  obs<-Outlier<-NULL
  plot<-list()
  data_name<-deparse(substitute(df))
  df<-data.frame(df)
  df<-data.frame(df[,sapply(df,is.numeric)])
  id<-row.names(df)
  if(length(names(df))==1)
    names(df)<-data_name
  pb<-txtProgressBar(min=0,max=length(df),style=3)
  for (i in 1:(length(df))) {
    setTxtProgressBar(pb,i)
    vector<-df[,i]
    midp<-median_point<-stats::median(vector,na.rm=TRUE)
    mean_point<-mean(vector,na.rm=TRUE)
    std<-stats::sd(vector,na.rm=TRUE)
    mad<-stats::median(abs(median_point-vector),na.rm=TRUE)
    if (method=="mean") {
      df_outlier<-data.frame(id=id,obs=vector,Outlier=abs(vector-mean_point)>2*std)
      df_outlier<-df_outlier[complete.cases(df_outlier),]
      midp<-mean_point
      lower<-mean_point-2*std
      upper<-mean_point+2*std
      outliern<-length(which(df_outlier=="TRUE"))
    }
    if (method=="median") {
      df_outlier<-data.frame(id=id,obs=vector,Outlier=abs(vector-median_point)>2*(mad/0.6745))
      df_outlier<-df_outlier[complete.cases(df_outlier),]
      lower<-median_point-2*(mad/0.6745)
      upper<-median_point+2*(mad/0.6745)
      outliern<-length(which(df_outlier=="TRUE"))
    }
    if (method=="boxplot") {
      Q1<-stats::quantile(vector,0.25,na.rm=TRUE)
      Q3<-stats::quantile(vector,0.75,na.rm=TRUE)
      IntQ<-Q3-Q1
      df_outlier<-data.frame(id=id,obs=vector,Outlier=vector<Q1-1.5*IntQ | vector>Q3+1.5*IntQ)
      df_outlier<-df_outlier[complete.cases(df_outlier),]
      lower<-Q1-1.5*IntQ
      upper<-Q3+1.5*IntQ
      outliern<-length(which(df_outlier=="TRUE"))
    }
    plot[[names(df)[i]]]<-ggplot(df_outlier,aes(x=id,y=obs,label=id))+geom_point(aes(colour=Outlier))+
      ggrepel::geom_text_repel(data=subset(df_outlier,Outlier=="TRUE"),aes(label=id),size=2.7,colour="black",box.padding=unit(0.35,"lines"),point.padding=unit(0.3,"lines"))+
      labs(x=paste("Observation ID \n Outliers:",outliern),
           y=names(df)[i],
           title=paste(title),
           caption=paste0("\nMethod=",method,
                          "\nObservations=",nrow(df_outlier),
                          "\nMean=",round(mean(stats::na.omit(df_outlier$obs)),2),
                          "\nSD=",round(stats::sd(stats::na.omit(df_outlier$obs)),2),
                          "\nMedian=",round(stats::median(stats::na.omit(df_outlier$obs)),2)))+
      theme_bw(base_size=base_size)+
      theme(legend.position="none")+
      geom_hline(yintercept=midp,colour="black",linetype="longdash")+
      geom_hline(yintercept=lower,colour="black",linetype="longdash")+
      geom_hline(yintercept=upper,colour="black",linetype="longdash")+
      coord_flip()
  }
  close(pb)
  return(plot)
}
##########################################################################################
#  PLOT HISTOGRAM
##########################################################################################
#' @title Histograms with density function
#' @description Histograms with density function
#' @details uses ggplot
#' @param df dataframe or vector with continous or ordinal data
#' @param bins number of bars to display
#' @param xlims x axis limits
#' @param title plot title
#' @param ylab y label
#' @param base_size numeric base font size
#' @param fill color of bar
#' @param color color of bar outline
#' @import ggplot2
#' @importFrom stats sd median na.omit
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' plot_histogram(df=vector)
#' plot_histogram(df=df,xlims=c(0,50))
#' plot_histogram(df=df)
#' plot_multiplot(plotlist=plot_histogram(df=mtcars),cols=4)
plot_histogram<-function(df,bins=30,title="",base_size=10,xlims=NULL,fill="gray25",color="gray50",ylab="Count") {
  data<-NULL
  plot<-list()
  df<-data.frame(df,id=1:nrow(data.frame(df)),check.names=FALSE)
  df<-df[,sapply(df,is.numeric)]
  if(length(names(df))==2) {
    names(df)[1]<-""
  }
  pb<-txtProgressBar(min=0,max=length(df)-1,style=3)
  for (i in 1:(length(df)-1)) {
    setTxtProgressBar(pb,i)
    vector<-data.frame(data=df[,i])
    plot[[names(df)[i]]]<-ggplot(vector,aes(x=data))+
      geom_histogram(bins=bins,fill=fill,color=color,na.rm=TRUE)+
      theme_bw(base_size=base_size)+
      labs(title=title,
           # x=string_aes(names(df)[i],proper=TRUE),
           x=names(df)[i],
           y=ylab,
           caption=paste0("\nObservations=",length(vector[complete.cases(vector),]),
                          "\nMean=",round(mean(vector[,1],na.rm=TRUE),2),
                          "\nSD=",round(stats::sd(vector[,1],na.rm=TRUE),2),
                          "\nMedian=",round(stats::median(vector[,1],na.rm=TRUE),2)))+
      if(!is.null(xlims)) {
        lims(x=xlims)
      }
  }
  close(pb)
  return(plot)
}
##########################################################################################
# PLOT QQ
##########################################################################################
#' @title qq plots
#' @description qq plots
#' @details uses ggplot
#' @param df dataframe or vector with continous or ordinal data
#' @param title plot title
#' @param base_size numeric base font size
#' @import ggplot2 
#' @importFrom stats quantile qnorm
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' plot_qq(df=vector)
#' plot_qq(df=df)
#' plot_multiplot(plotlist=plot_qq(df=mtcars),cols=4)
plot_qq<-function(df,title="",base_size=10) {
  resids<-NULL
  data_name<-deparse(substitute(df))
  df<-data.frame(df)
  if(length(names(df))==1)
    names(df)<-data_name
  names_df<-names(df)
  pb<-txtProgressBar(min=0,max=length(names(df)),style=3)
  plot<-list()
  for (i in 1:length(df)) {
    setTxtProgressBar(pb,i)
    if(is.numeric(df[,i])) {
      y<-stats::quantile(df[,i][!is.na(df[,i])],c(0.25,0.75))
      x<-stats::qnorm(c(0.25,0.75))
      slope<-diff(y)/diff(x)
      intercept<-y[1L]-slope*x[1L]
      d<-data.frame(resids=df[,i])
      plot[[names(df)[i]]]<-ggplot(d,aes(sample=resids))+
        stat_qq(alpha=.1)+
        geom_abline(slope=slope,intercept=intercept)+
        theme_bw(base_size=base_size)+
        labs(title=title,
             caption=paste0("\nVariable=",names(df)[i],
                            "\nObservations=",nrow(df),
                            "\nMean=",round(mean(df[,i]),2),
                            "\nSD=",round(stats::sd(df[,i]),2),
                            "\nMedian=",round(stats::median(df[,i]),2)))
    } 
  }
  close(pb)
  return(plot)
}
##########################################################################################
# PLOT BOXPLOT
##########################################################################################
#' @title Boxplot
#' @description Boxplot
#' @details uses ggplot
#' @param df dataframe or vector with continous or ordinal data
#' @param title Plot title
#' @param base_size numeric base font size
#' @import ggplot2
#' @importFrom reshape2 melt
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' plot_boxplot(df=vector)
#' plot_boxplot(df=generate_missing(vector))
#' plot_boxplot(df=df)
plot_boxplot<-function(df,title="",base_size=10) {
  variable<-value<-NULL
  data_name<-deparse(substitute(df))
  df<-data.frame(df)
  df<-data.frame(df[,sapply(df,is.numeric)])
  if(length(df)==1)
    names(df)<-data_name
  vector<-reshape2::melt(df,measure.vars=names(df),value.name="value",variable.name="variable")
  plot<-ggplot(vector,aes(x=variable,y=value))+
    geom_boxplot()+
    labs(title=title,y="",x="",caption=paste("Observations=",nrow(df)))+
    theme_bw(base_size=base_size)+
    coord_flip()
  return(plot)
}
##########################################################################################
# NORMALITY TESTS
##########################################################################################
#' @title Normality tests
#' @description Shapiro-Wilk Anderson-Darling Cramer-von-Mises Shapiro-Francia Jarque-Bera Kolmogorov-Smirnov Lilliefors Pearson X2
#' @details returns xlsx file
#' @param df dataframe with continous or ordinal data
#' @param file output filename
#' @importFrom plyr rbind.fill
#' @importFrom DescTools AndersonDarlingTest CramerVonMisesTest ShapiroFranciaTest JarqueBeraTest LillieTest PearsonTest
#' @importFrom stats ks.test na.omit shapiro.test
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' report_normality_tests(df=df)
#' report_normality_tests(df=vector,file="normality_tests")
report_normality_tests<-function(df,file=NULL) {
  df=data.frame(df)
  n<-nrow(df)
  instruction_shapiro<-"Shapiro-Wilk Composite null hypothesis: any normal distribution"
  instruction_anderson<-"Anderson-Darling Composite null hypothesis: any normal distribution"
  instruction_crammer<-"Cramer-von-Mises Composite null hypothesis: any normal distribution"
  instruction_shapiro_francia<-"Shapiro-Francia Composite null hypothesis: any normal distribution"
  instruction_jarque<-"Jarque-Bera Composite null hypothesis: any normal distribution"
  instruction_lilliefors<-"Lilliefors Composite null hypothesis: any normal distribution"
  instruction_kolmogorov<-"Kolmogorov-Smirnov Exact null hypothesis: fully specified normal distribution"
  instruction_pearson<-"Pearson X2 Tests weaker null hypothesis: any distribution with the same probabilities for the given class intervals"
  result_df<-data.frame()
  for (i in names(df)) {
    vector<-compute_standard(stats::na.omit(df[,i]),type="z")
    N<-length(vector)
    if (length(vector)<5000&length(vector)>7&var(vector)!=0) {
      result_shapiro<-data.frame(variable=i,N=N,t(unlist(shapiro.test(vector))),instruction=instruction_shapiro)
      result_anderson<-data.frame(variable=i,N=N,t(unlist(DescTools::AndersonDarlingTest(vector))),instruction=instruction_anderson)
      result_cramer<-data.frame(variable=i,N=N,t(unlist(DescTools::CramerVonMisesTest(vector))),instruction=instruction_crammer)
      result_francia<-data.frame(variable=i,N=N,t(unlist(DescTools::ShapiroFranciaTest(vector))),instruction=instruction_shapiro_francia)
      result_jarque<-data.frame(variable=i,N=N,t(unlist(DescTools::JarqueBeraTest(vector))),instruction=instruction_jarque)
      result_lillie<-data.frame(variable=i,N=N,t(unlist(DescTools::LillieTest(vector))),instruction=instruction_lilliefors)
      result_kolmogorov<-data.frame(variable=i,N=N,t(unlist(stats::ks.test(vector,"pnorm",mean=mean(vector),sd=stats::sd(vector),alternative="two.sided"))),instruction=instruction_kolmogorov)
      result_pearson<-data.frame(variable=i,N=N,t(unlist(DescTools::PearsonTest(vector,n.classes=ceiling(2*(n^(2/5))),adjust=TRUE))),instruction=instruction_pearson)
      
      names(result_shapiro)<-c("variable","n","statistic","p","method","data.name","instruction")
      names(result_anderson)<-c("variable","n","statistic","p","method","method1","data.name","instruction")
      names(result_cramer)<-c("variable","n","statistic","p","method","data.name","instruction")
      names(result_francia)<-c("variable","n","statistic","p","method","data.name","instruction")
      names(result_jarque)<-c("variable","n","statistic","df","p","method","data.name","instruction")
      names(result_lillie)<-c("variable","n","statistic","p","method","data.name","instruction")
      names(result_kolmogorov)<-c("variable","n","statistic","p","alternative", "method","data.name","exact","instruction")
      names(result_pearson)<-c("variable","n","statistic","p","method","data.name","n.classes","df","instruction")
      
      result<-plyr::rbind.fill(result_shapiro,
                               result_anderson,
                               result_cramer,
                               result_francia,
                               result_jarque,
                               result_lillie,
                               result_kolmogorov,
                               result_pearson)
      result_df<-plyr::rbind.fill(result_df,result)
      
      result_df<-result_df[,c("variable","n","statistic","df","p","method","method1","alternative","n.classes","instruction")]
      
    } else { cat("NORMALITY INDICES NOT CALCULATED DUE TO OUT OF BOUNDS SAMPLE SIZE FOR",i,"\n") }
  }
  message<-"Significant values indicate deviation from normality,p values depend on sample size"
  write_txt({
    output_separator("NORMALITY TESTS",output=result_df,instruction="")
  },file=file)
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result_df,workbook=wb,sheet="Normality Tests",critical=list(p="<=0.05"),numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
}
##########################################################################################
# OUTLIERS
##########################################################################################
#' @title Percent of outliers in vector
#' @details returns dataframe
#' @param vector numeric vector
#' @importFrom stats sd na.omit
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' outlier_summary(vector)
#' data.frame(sapply(mtcars,outlier_summary))
outlier_summary<-function(vector) {
  zvariable<-(vector-mean(vector,na.rm=TRUE))/stats::sd(vector,na.rm=TRUE)
  outlier95<-abs(zvariable)>=1.96
  outlier99<-abs(zvariable)>=2.58
  outlier999<-abs(zvariable)>=3.29
  ncases<-length(stats::na.omit(zvariable))
  percent95<-round(100*length(subset(outlier95,outlier95==TRUE))/ncases,2)
  percent99<-round(100*length(subset(outlier99,outlier99==TRUE))/ncases,2)
  percent999<-round(100*length(subset(outlier999,outlier999==TRUE))/ncases,2)
  result<-data.frame(outlier=rbind("|z-score| > 1.96"=paste(percent95,"%"),
                                   "|z-score| > 2.58"=paste(percent99,"%"),
                                   "|z-score| > 3.29"=paste(percent999,"%")))
  result<-data.frame("abs_z_1.96"=paste(percent95,"%"),
                     "abs_z_2.58"=paste(percent99,"%"),
                     abs_z_3.29=paste(percent999,"%"))
  return(result)
}
##########################################################################################
# OUTLIERS
##########################################################################################
#' @title Remove outliers
#' @param vector numeric
#' @param probs numeric vector with lowest and highest quantiles
#' @param na.rm if TRUE removes NA values
#' @param ... arguments passed to quantile
#' @importFrom stats quantile IQR
#' @keywords assumptions
#' @export
#' @examples
#' vector<-generate_missing(rnorm(1000))
#' df<-generate_missing(mtcars[,1:2])
#' remove_outliers(vector)
#' data.frame(sapply(df,remove_outliers))
remove_outliers<-function(vector,probs=c(.25,.75),na.rm=TRUE,...) {
  qnt<-stats::quantile(vector,probs=probs,na.rm=na.rm,...)
  H<-1.5*stats::IQR(vector,na.rm=na.rm)
  y<-vector
  y[vector<(qnt[1]-H)]<-NA
  y[vector>(qnt[2]+H)]<-NA
  return(y)
}
