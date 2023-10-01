##########################################################################################
# DESCRIPTIVES
##########################################################################################
#' @title Descriptive statistics
#' @description uses psych
#' @details returns xlsx
#' @param df dataframe
#' @param dv index of dependent variables
#' @param iv index of independent variables
#' @param file output filename
#' @importFrom plyr rbind.fill
#' @importFrom psych describe
#' @keywords descriptives
#' @export
#' @examples
#' compute_descriptives(df=mtcars,dv=1:5)
#' compute_descriptives(df=mtcars,dv=1:2,iv=9:10)
#' compute_descriptives(df=mtcars,dv=1:2,file="descriptives_no_factor")
#' compute_descriptives(df=mtcars,dv=1:2,iv=9:10,file="descriptives_factor")
compute_descriptives<-function(df,dv,iv=NULL,file=NULL) {
  comment<-list(factor="independent variable",
                levels="levels of independent variable\n\ndiscrete variables observed in independent variable",
                variable="dependent variable\n\ncontinous variable",
                n="sample size",
                mean="measure of central tendency\n\nmean",
                sd="measure of dispersion\nstandard deviation\n\nlow values indicate low dispersion of observations from the mean",
                median="measure of central tendency\n\nmedian\n\nvalue separating lower half from higher half of ordered observations",
                trimmed="measure of central tendency\n\ntrimmed mean\n\nmean after droping .1 of minimum and maximum values in data",
                mad="measure of dispersion\n\nmedian absolute deviation\n\nlow values indicate low dispersion of observations from the median",
                min="minimum value observed",
                max="maximum value observed",
                range="measure of dispersion\n\nrange\n\ndifference between minimum and maximum value",
                skew="skewness\nnegative values indicate a left skewed distribution\n\npositive values indicate a right skewed distribution\n\ndeviations from 0 indicate deviation from normality",
                kurtosis="kurtosis\n\nnegative values generally indicate flat peak\n\npositive values generally indicate sharp peak\n\ndeviations from 0 indicate deviation from normality",
                se="measure of dispersion\n\nstandard error\n\nlow values indicate low dispersion of observations from the mean",
                IQR="measure of dispersion\n\ninterquantile range\n\nthe range around the median where 50% of observations fall",
                Q0.1="percentile\n\n10% of observations fall bellow this value",
                Q0.25="percentile\n\n25% of observations fall bellow this value",
                Q0.5="percentile\n\n50% of observations fall bellow this value",
                Q0.75="percentile\n\n75% of observations fall bellow this value",
                Q0.9="percentile\n\n90% of observations fall bellow this value"
  )
  result_df<-data.frame()
  describe<-function(x) psych::describe(x,skew=TRUE,ranges=TRUE,check=TRUE,fast=FALSE,IQR=TRUE,quant=c(.1,.25,.5,.75,.90),na.rm=TRUE)
  if (missing(iv)){
    for (i in names(df)[dv])
      result_df<-plyr::rbind.fill(result_df,data.frame(variable=i,describe(df[,i])))
  }
  else {
    for (i in names(df)[dv]) {
      for (y in names(df)[iv]) {
        temp<-data.frame(df[,i],df[,y])
        names(temp)<-c(i,y)
        temp<-temp[complete.cases(temp),]
        if(nrow(temp)>1) {
          if(is.factor(temp[,y]))
            temp[,y]<-droplevels(temp[,y])
          result<-tapply(temp[,i],temp[,y],FUN=describe)
          result<-data.frame(levels=names(result),do.call(rbind.data.frame,result))
          result_df<-plyr::rbind.fill(result_df,data.frame(factor=y,variable=i,result,row.names=NULL,check.names=FALSE))
        }
      }
    }
  }
  result_df$vars<-NULL
  report_dataframe(result_df,file=file,sheet="Descriptives",comment=comment)
  return(result_df)
}
##########################################################################################
# DESCRIPTIVES
##########################################################################################
#' @title Descriptive statistics
#' @description uses plyr
#' @details returns xlsx
#' @param df dataframe
#' @param iv index of independent variables
#' @param file output filename
#' @importFrom plyr ddply numcolwise
#' @importFrom stats sd mad
#' @keywords descriptives
#' @export
#' @examples
#' compute_aggregate(df=mtcars,iv=9)
#' compute_aggregate(df=mtcars,iv=9:10)
#' compute_aggregate(df=mtcars,iv=9:11)
#' compute_aggregate(df=mtcars,iv=9:11,file="descriptives")
compute_aggregate<-function(df,iv,file=NULL) {
  result_df_mean<-result_df_sd<-result_df_obs<-list()
  factornames<-names(df)[iv]
  result_mean<-data.frame(statistic="mean",plyr::ddply(df,factornames,plyr::numcolwise(mean,na.rm=TRUE)))
  result_sd<-data.frame(statistic="SD",plyr::ddply(df,factornames,plyr::numcolwise(sd,na.rm=TRUE)))
  result_median<-data.frame(statistic="median",plyr::ddply(df,factornames,plyr::numcolwise(median,na.rm=TRUE)))
  result_mad<-data.frame(statistic="mad",plyr::ddply(df,factornames,plyr::numcolwise(stats::mad,na.rm=TRUE)))
  result_trimmed_mean<-data.frame(statistic="trmmed mean",plyr::ddply(df,factornames,plyr::numcolwise(mean,trim=.5,na.rm=TRUE)))
  result_obs<-data.frame(statistic="N",plyr::ddply(df,factornames,plyr::numcolwise(length)))
  result_min<-data.frame(statistic="min",plyr::ddply(df,factornames,plyr::numcolwise(min,na.rm=TRUE)))
  result_max<-data.frame(statistic="max",plyr::ddply(df,factornames,plyr::numcolwise(max,na.rm=TRUE)))
  result_range<-data.frame(statistic="range",plyr::ddply(df,factornames,plyr::numcolwise(function(x) max(x,na.rm=TRUE)-min(x,na.rm=TRUE))))
  result_skewness<-data.frame(statistic="skewness",plyr::ddply(df,factornames,plyr::numcolwise(compute_skewness)))
  result_kurtosis<-data.frame(statistic="kurtosis",plyr::ddply(df,factornames,plyr::numcolwise(compute_kurtosis)))
  result_iqr<-data.frame(statistic="IQR",plyr::ddply(df,factornames,plyr::numcolwise(IQR,na.rm=TRUE,type=7)))
  result_se<-data.frame(statistic="SE",plyr::ddply(df,factornames,plyr::numcolwise(compute_standard_error)))
  result_df<-plyr::rbind.fill(result_mean,result_sd,result_median,result_mad,result_trimmed_mean,result_obs,
                              result_min,result_max,result_range,result_skewness,result_kurtosis,
                              result_iqr,result_se)
  report_dataframe(result_df,file=file,sheet="Descriptives")
  return(result_df)
}
##########################################################################################
# FREQUENCIES
##########################################################################################
#' @title Frequencies by levels
#' @description returns frequency proportion percent
#' @details returns xlsx
#' @param df dataframe
#' @param file output filename
#' @param ordered if TRUE it will output frequencies in descending order
#' @importFrom plyr rbind.fill
#' @keywords descriptives
#' @export
#' @examples
#' compute_frequencies(df=generate_missing(generate_factor(nrows=10,ncols=10),missing=5))
#' compute_frequencies(df=generate_factor())
#' compute_frequencies(df=generate_factor(),file="descriptives")
compute_frequencies<-function(df,ordered=TRUE,file=NULL) {
  frequency<-data.frame()
  for (i in names(df)) {
    mytable<-table(df[i])
    if(nrow(mytable)>0) {
      proportion.table<-data.frame(prop.table(mytable),stringsAsFactors=FALSE)
      mytable<-data.frame(mytable,stringsAsFactors=FALSE)
      proportion.table<-data.frame(proportion.table,stringsAsFactors=FALSE)
      mytable<-data.frame(rep(names(df[i]),nrow(mytable)),mytable,proportion.table$Freq)
      names(mytable)<-c("variable","Observation","Frequency","Proportion")
      mytable<-mytable[order(-mytable$Frequency),]
      frequency<-plyr::rbind.fill(frequency,mytable)
    }
  }
  percent<-frequency$Proportion*100
  frequency<-data.frame(frequency,"Percent"=percent,stringsAsFactors=FALSE)
  report_dataframe(frequency,file=file,sheet="Frequency")
  return(frequency)
}
##########################################################################################
# RESPONSE FREQUENCIES
##########################################################################################
#' @title Response frequencies
#' @description returns count proportion percent
#' @details returns dataframe
#' @param df dataframe
#' @param max maximum score
#' @param uniqueitems number of unique items
#' @param type "frequency" "proportion" "percent" "all"
#' @param file output filename
#' @keywords descriptives
#' @export
#' @examples
#' response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="frequency")
#' response_frequency(mtcars[,c("gear")],uniqueitems=1:8,type="proportion")
#' response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="percent")
#' response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="all")
#' response_frequency(mtcars[,c("gear","carb")],uniqueitems=1:8,type="all",
#'                    file="descriptives")
response_frequency<-function(df,max=10,uniqueitems=NULL,type="percent",file=NULL) {
  df<-data.frame(df)
  min.item<-min(df,na.rm=TRUE)
  max.item<-max(df,na.rm=TRUE)
  if (is.null(uniqueitems))
    uniqueitems<-unique(as.vector(unlist(df)))
  if ((max.item-min.item>max)||(nlevels(factor(df[,1]))>max)||length(uniqueitems)>max) {
    frequency<-NULL
  } else {
    n_var<-dim(df)[2]
    n_cases<-dim(df)[1]
    dummy<-matrix(rep(uniqueitems,n_var),ncol=n_var)
    colnames(dummy)<-names(df)
    xdum<-rbind(df,dummy)
    frequency<-apply(xdum,2,table)
    frequency<-t(frequency-1)
    responses<-rowSums(frequency)
    if(type=="frequency")
      result<-data.frame(type="Frequency",variable=row.names(frequency),frequency,miss=n_cases-responses,responses,check.names=FALSE)
    if(type=="proportion")
      result<-data.frame(type="Proportion",variable=row.names(frequency),frequency/responses,miss=1-responses/n_cases,responses,check.names=FALSE)
    if(type=="percent")
      result<-data.frame(type="Percent",variable=row.names(frequency),(frequency/responses)*100,miss=100-((responses/n_cases)*100),responses,check.names=FALSE)
    if(type=="all") {
      result<-rbind(data.frame(type="Frequency",variable=row.names(frequency),frequency,miss=n_cases-responses,responses,check.names=FALSE),
                    data.frame(type="Proportion",variable=row.names(frequency),frequency/responses,miss=1-responses/n_cases,responses,check.names=FALSE),
                    data.frame(type="Percent",variable=row.names(frequency),(frequency/responses)*100,miss=100-((responses/n_cases)*100),responses,check.names=FALSE),make.row.names=FALSE)
    }
  }
  row.names(result)<-NULL
  if(!is.null(file))
    report_dataframe(result,file=file,sheet="Frequency")
  return(result)
}
##########################################################################################
# COMPUTE CROSSTABLE
##########################################################################################
#' @title Compute crosstables
#' @param df dataframe
#' @param factor_index index of factors
#' @param combinations index of comparisons
#' @keywords descriptives
#' @export
#' @examples
#' combinations<-data.frame(index1=c("vs","am","gear"),index2=c("cyl","cyl","cyl"))
#' compute_crosstable(df=mtcars,combinations=combinations)
#' combinations<-data.frame(index1=c("vs","am"),index2=c("cyl","cyl"))
#' compute_crosstable(df=mtcars,combinations=combinations)
#' compute_crosstable(df=mtcars,factor_index=8:10)
compute_crosstable<-function(df,factor_index=NULL,combinations=NULL) {
  frequency<-proportion<-data.frame()
  counter=0
  if(is.null(combinations)) {
    combinations<-expand.grid(names(df)[factor_index],names(df)[factor_index],stringsAsFactors=FALSE,KEEP.OUT.ATTRS=FALSE)
    names(combinations)<-c("index1","index2")
    combinations<-combinations[!combinations$index1==combinations$index2,]
    combinations<-combinations[!duplicated(t(apply(combinations,1,sort))),]
  }
  combinations<-change_data_type(combinations,type="character")
  pb<-txtProgressBar(min=0,max=nrow(combinations),style=3)
  for(i in 1:nrow(combinations)) {
    counter=counter+1
    setTxtProgressBar(pb,counter)
    
    f1<-as.character(combinations[i,1])
    f2<-as.character(combinations[i,2])
    
    df_table<-data.frame(f1=f1,f2=f2,table(df[,f1],df[,f2]))
    df_prop_table<-data.frame(f1=f1,f2=f2,prop.table(table(df[,f1],df[,f2]))*100)
    
    names(df_table)<-c("f1","f2","l1","l2","Frequency")
    names(df_prop_table)<-c("f1","f2","l1","l2","Percent")
    if(sum(df_table$Frequency)>0) {
      frequency<-plyr::rbind.fill(frequency,df_table)
      proportion<-plyr::rbind.fill(proportion,df_prop_table)
    }
  }
  close(pb)
  result<-merge(frequency,proportion,by=c("f1","f2","l1","l2"),all=TRUE,sort=FALSE)
  
  return(result)
}
##########################################################################################
# PLOT CROSSTABLE
##########################################################################################
#' @title Plot crosstables
#' @param df dataframe
#' @param factor_index index of factors
#' @param combinations index of comparisons
#' @param shape shape of points
#' @param angle angle of xaxis labels
#' @param base_size base font size
#' @param title plot title
#' @import ggplot2
#' @keywords descriptives
#' @export
#' @examples
#' combinations<-data.frame(index1=c("vs","am","gear"),index2=c("cyl","cyl","cyl"))
#' plot_crosstable(df=mtcars,factor_index=8:9)
#' plot_crosstable(df=mtcars,combinations=combinations)
plot_crosstable<-function(df,factor_index,combinations=NULL,shape=16,angle=0,base_size=10,title="") {
  variable1<-variable2<-Frequency<-NULL
  plot<-list()
  counter=0
  if(is.null(combinations)) {
    combinations<-expand.grid(names(df)[factor_index],names(df)[factor_index],stringsAsFactors=FALSE,KEEP.OUT.ATTRS=FALSE)
    names(combinations)<-c("index1","index2")
    combinations<-combinations[!combinations$index1==combinations$index2,]
    combinations<-combinations[!duplicated(t(apply(combinations,1,sort))),]
  }
  if(!is.null(combinations)) {
    combinations<-change_data_type(combinations,type="character")
  }
  pb<-txtProgressBar(min=0,max=nrow(combinations),style=3)
  for(i in 1:nrow(combinations)) {
    counter=counter+1
    setTxtProgressBar(pb,counter)
    df_table<-data.frame(table(df[,as.character(combinations[i,1])],df[,as.character(combinations[i,2])]))
    names(df_table)<-c("variable1","variable2","Frequency")
    if(sum(df_table$Frequency)>0)
      plot[[paste0(combinations[i,1],"_",combinations[i,2])]]<-ggplot(df_table,aes(x=variable1,y=variable2))+
      geom_point(aes(size=Frequency,color=variable1),shape=shape)+
      scale_size_continuous(range=c(1,30))+
      labs(x=string_aes(combinations[i,1]),y=string_aes(combinations[i,2]))+
      geom_text(aes(label=Frequency))+theme_bw(base_size=base_size)+
      theme(axis.text.x=element_text(angle=angle,hjust=1),legend.position="none")+
      labs(title=title,caption=paste("Observations:",sum(df_table$Frequency)))
  }
  close(pb)
  return(plot)
}
##########################################################################################
# PLOT MOSAIC
##########################################################################################
#' @title Plot mosaic plots
#' @param df dataframe
#' @param factor_index index of factors
#' @param base_size base font size
#' @param title plot title
#' @import ggplot2
#' @importFrom stats complete.cases na.omit
#' @keywords descriptives
#' @export
#' @examples
#' plot_mosaic(df=mtcars,factor_index=8:9)
#' plot_mosaic(df=mtcars,factor_index=9:10)
plot_mosaic<-function(df,factor_index,base_size=10,title="") {
  var1Center<-var2height<-NULL
  plot<-list()
  counter=0
  pb<-txtProgressBar(min=0,max=length(names(df[,factor_index])),style=3)
  for (i in names(df[,factor_index])) {
    counter=counter+1
    setTxtProgressBar(pb,counter)
    for (y in names(df[,factor_index])) {
      if(!i==y) {
        tempdata<-df[complete.cases(df[,c(i,y)]),c(i,y)]
        v1<-factor(stats::na.omit(tempdata[,i]))
        v2<-factor(stats::na.omit(tempdata[,y]))
        levVar1<-length(levels(v1))
        levVar2<-length(levels(v2))
        if(levVar1<2)
          levels(v1)<-c(levels(v1),"Second Level is Not Available")
        if(levVar2<2)
          levels(v2)<-c(levels(v2),"Second Level is Not Available")
        plotData<-data.frame(prop.table(table(v1,v2)))
        plotData$marginVar1<-prop.table(table(v1))
        plotData$var2height<-plotData$Freq/plotData$marginVar1
        plotData$var1Center<-c(0,cumsum(plotData$marginVar1)[1:levVar1-1])+plotData$marginVar1/2
        plot[[paste(i,y)]]<-ggplot(plotData,aes(var1Center,var2height))+
          geom_bar(stat="identity",aes(fill=v2),width=plotData$marginVar1,col="White")+
          #geom_bar(stat="identity",aes(width=marginVar1,fill=v2),col="White")+
          geom_text(aes(label=v1,x=var1Center,y=.5,angle=90))+
          labs(x=paste("Proportion"),
               y=paste("Proportion"),
               title=paste0(title,"\n",string_aes(i)," By ",string_aes(y)),
               fill=i,
               caption=paste("Observations:",nrow(tempdata)))+
          theme_bw(base_size=base_size)+
          theme(legend.title=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank())+
          coord_fixed()
      }
    }
  }
  close(pb)
  return(plot)
}
##########################################################################################
# PLOT RESPONSE FREQUENCY
##########################################################################################
#' @title Plot response frequencies
#' @param df dataframe
#' @param factor_index index of factors
#' @param base_size base font size
#' @param title plot title
#' @param width wrap width for x title
#' @import ggplot2
#' @importFrom stats reorder
#' @keywords descriptives
#' @export
#' @examples
#' plot_response_frequencies(df=mtcars,factor_index=1:10)
plot_response_frequencies<-function(df,factor_index,base_size=10,title="",width=100) {
  Freq<-NULL
  plots<-list()
  for (i in names(df[,factor_index])) {
    tempdata<-df[complete.cases(df[,i]),i]
    tempdata<-data.frame(table(tempdata))
    if(nrow(tempdata)>0) {
      plots[[i]]<-ggplot(data.frame(tempdata),aes(x=stats::reorder(tempdata,Freq),y=Freq))+
        geom_bar(stat="identity")+
        labs(x="",y="Count",title=paste(title,wrapper(i,width=width),collapse="\n"),caption=paste0("Observations:",sum(tempdata$Freq)))+
        coord_flip()+
        theme_bw(base_size=base_size)
    }
  }
  return(plots)
}

