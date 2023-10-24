##########################################################################################
# PLOT ONE WAY ANOVA
##########################################################################################
#' @title Plot means with standard error for every level in a dataframe
#' @param df dataframe
#' @param dv index of continous variables
#' @param iv index of factors
#' @param base_size base font size
#' @param order_factor if TRUE it will sort the categorical axis by the continous variable value
#' @param type error bar type to display (1) "se" for standard error (2) "ci" for confidence interval (3) "sd" for standard deviation (4) "" for no error bar
#' @param title plot title
#' @param note footnote
#' @param width wrap width for x title
#' @import ggplot2 foreach
#' @importFrom doSNOW registerDoSNOW
#' @importFrom Rmisc summarySE
#' @importFrom scales wrap_format
#' @importFrom ggpubr as_ggplot
#' @importFrom gridExtra arrangeGrob
#' @importFrom plyr rbind.fill
#' @importFrom parallel detectCores makeCluster
#' @keywords ANOVA
#' @export
#' @examples
#' nrows=1000
#' df<-data.frame(generate_factor(vector=LETTERS[1:5],nrows=nrows,ncols=10,type="random"),
#'                generate_data(nrows=nrows,ncols=5,type="normal"))
#' result<-plot_oneway(df=df,dv=11:15,iv=1:10)
#' plot_oneway(df=mtcars,dv=2,iv=9)
#' plot_oneway(df=mtcars,dv=2,iv=9:10)
#' plot_oneway(df=mtcars,dv=2:3,iv=10)
#' plot_oneway(df=mtcars,dv=2:3,iv=9:10)
#' plot_oneway(df=mtcars,dv=2:3,iv=9:10,type="se")
#' plot_oneway(df=mtcars,dv=2:3,iv=9:10,type="ci")
#' plot_oneway(df=mtcars,dv=2:3,iv=9:10,type="sd")
#' plot_oneway(df=mtcars,dv=2:3,iv=9:10,type="",order_factor=FALSE)
#' plot_oneway(df=mtcars,dv=2:3,iv=9:10,type="",order_factor=TRUE)
plot_oneway<-function(df,dv,iv,base_size=20,type="se",order_factor=TRUE,title="",note="",width=60) {
  se<-ci<-NULL
  output_data<-function(i) {
    index<-as.character(combinations[i,])
    tempdata<-df[complete.cases(df[,index]),index]
    if(length(unique(tempdata[,combinations$iv[i]]))>1)
      Rmisc::summarySE(tempdata,measurevar=combinations$dv[i],groupvars=combinations$iv[i],na.rm=TRUE)
  }
  output_plot<-function(i) {
    tempdata<-plot_data[[i]]
    if (!is.null(tempdata)){
      if(order_factor)
        means_plot<-ggplot(tempdata,aes(x=reorder(tempdata[,1],-tempdata[,3]),y=tempdata[,3]))
      else
        means_plot<-ggplot(tempdata,aes(x=tempdata[,1],y=tempdata[,3]))
      means_plot<-means_plot+
        geom_point()+
        labs(y=string_aes(names(tempdata)[3]),x=wrapper(string_aes(names(tempdata)[1]),width=width),title=title,caption=note)+
        theme_bw(base_size=base_size)+
        scale_x_discrete(labels=scales::wrap_format(50))+
        coord_flip()
      if(type=="se")
        means_plot<-means_plot+geom_errorbar(aes(ymin=tempdata[,3]-se,ymax=tempdata[,3]+se),width=.1)+
        labs(caption=paste("Bars are standard errors",note))
      if(type=="ci")
        means_plot<-means_plot+geom_errorbar(aes(ymin=tempdata[,3]-ci,ymax=tempdata[,3]+ci),width=.1)+
        labs(caption=paste("Bars are confidence intervals",note))
      if(type=="sd")
        means_plot<-means_plot+geom_errorbar(aes(ymin=tempdata[,3]-sd,ymax=tempdata[,3]+sd),width=.1)+
        labs(caption=paste("Bars are standard deviations",note))
      if(type=="") {}
      minaxis<-ggplot_build(means_plot)$layout$panel_scales_y[[1]]$range$range[[1]]
      if(is.null(minaxis)) {}
      else {
        means_plot<-means_plot+annotate("text",x=tempdata[,1],y=minaxis,label=paste0("N:",tempdata$N),alpha=.5,size=base_size/10*2,hjust=0,vjust=2)
      }
      ggpubr::as_ggplot(gridExtra::arrangeGrob(means_plot))
    }
  }
  df[,iv]<-change_data_type(data.frame(df[,iv]),"factor")
  
  combinations<-expand.grid(names(df)[iv],names(df)[dv])
  names(combinations)<-c("iv","dv")
  row.names(combinations)<-paste0(combinations$iv,"_",combinations$dv)
  combinations<-change_data_type(combinations,type="character")
  
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
    cl<-parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)
    progress<-function(n) setTxtProgressBar(pb,n)
    opts<-list(progress=progress)
    plot_data<-foreach(i=1:nrow(combinations),.final=function(x) setNames(x,row.names(combinations)),.packages=c("workingfunctions"),.options.snow=opts) %dopar% {
      output_data(i)
    }
    plots<-foreach(i=1:length(plot_data),.final=function(x) setNames(x,row.names(combinations)),.packages=c("workingfunctions"),.options.snow=opts) %dopar% {
      output_plot(i)
    }
    close(pb)
    parallel::stopCluster(cl)
  } else {
    plot_data<-plots<-list()
    for(i in 1:nrow(combinations)) {
      setTxtProgressBar(pb,i)
      plot_data[[row.names(combinations)[i]]]<-output_data(i)
    }
    for(i in 1:length(plot_data)) {
      setTxtProgressBar(pb,i)
      plots[[row.names(combinations)[i]]]<-output_plot(i)
    }
    close(pb)
  }
  
  plot_data_df<-Reduce(plyr::rbind.fill,plot_data)
  names_input_missing<-setdiff(names(df)[c(iv,dv)],names(plot_data_df))
  names_input<-names(df)[c(iv,dv)]
  names_input<-names_input[!names_input%in%names_input_missing]
  plot_data_df<-plot_data_df[,c(names_input,"N","sd","se","ci")]
  result<-list(plot_data=plot_data,plot_data_df=plot_data_df,plots=plots)
  gc(full=TRUE)
  return(result)
}
##########################################################################################
# PLOT TWO WAY INTERACTION
##########################################################################################
#' @title Plot two way interaction graphs
#' @inheritParams plot_oneway
#' @import ggplot2 foreach
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores stopCluster
#' @importFrom plyr ddply
#' @importFrom Rmisc summarySE
#' @importFrom plyr numcolwise rbind.fill
#' @importFrom scales wrap_format
#' @importFrom ggpubr as_ggplot
#' @importFrom gridExtra arrangeGrob
#' @importFrom stringr str_wrap
#' @keywords ANOVA
#' @export
#' @examples
#' nrows=1000
#' df<-data.frame(generate_factor(vector=LETTERS[1:5],nrows=nrows,ncols=10,type="random"),
#'                generate_data(nrows=nrows,ncols=5,type="normal"))
#' #result<-plot_interaction(df=df,dv=11:15,iv=1:10)
#' plot_interaction(df=mtcars,dv=2:3,iv=8:9,base_size=20,title="",type="se")
#' plot_interaction(df=mtcars,dv=2,iv=8:9,base_size=20,title="",type="se")
#' plot_interaction(df=mtcars,dv=2:3,iv=8:9,base_size=20,title="",type="ci")
#' plot_interaction(df=mtcars,dv=2:3,iv=9:10,base_size=20,title="",type="ci")
#' plot_interaction(df=mtcars,dv=2:3,iv=9:10,base_size=20,title="",type="sd")
#' plot_interaction(df=mtcars,dv=2,iv=9:10,base_size=20,
#'                  title="",type="",order_factor=FALSE)
plot_interaction<-function(df,dv,iv,base_size=20,type="se",order_factor=TRUE,title="",note="") {
  se<-ci<-NULL
  output_data<-function(i) {
    factors<-c(combinations$iv1[i],combinations$iv2[i])
    cors<-combinations$dv[i]
    tempdata_complete_cases<-df[complete.cases(df[,c(factors,cors)]),c(factors,cors)]
    if(nrow(tempdata_complete_cases)>1)
      Rmisc::summarySE(tempdata_complete_cases,measurevar=cors,groupvars=factors,na.rm=TRUE,.drop=TRUE)
  }
  output_plot<-function(i) {
    tempdata<-plot_data[[i]]
    if(!is.null(tempdata)){
      factors<-c(combinations$iv1[i],combinations$iv2[i])
      cors<-combinations$dv[i]
      tempdata_cases<-plyr::ddply(tempdata,factors[1],plyr::numcolwise(sum,na.rm=TRUE))
      if(order_factor)
        interactions_plot<-ggplot(tempdata,aes(x=reorder(tempdata[,factors[1]],-tempdata[,cors]),
                                               y=tempdata[,cors],
                                               color=stringr::str_wrap(tempdata[,factors[2]],width=25),
                                               group=stringr::str_wrap(tempdata[,factors[2]],width=25)))
      else 
        interactions_plot<-ggplot(tempdata,aes(x=tempdata[,factors[1]],
                                               y=tempdata[,cors],
                                               color=stringr::str_wrap(tempdata[,factors[2]],width=25),
                                               group=stringr::str_wrap(tempdata[,factors[2]],width=25)))
      interactions_plot<-interactions_plot+
        scale_color_discrete(breaks=c(levels(tempdata[,factors[2]])),name=factors[2])+
        geom_line()+
        geom_point()+
        #theme(legend.title=element_blank())+
        theme_bw(base_size=base_size)+
        labs(y=string_aes(cors),
             x=str_wrap(string_aes(factors[1]),width=25),
             title=title,
             caption=note,
             color=str_wrap(string_aes(tempdata[,factors[2]]),width=25))+
        scale_x_discrete(labels=scales::wrap_format(100))+
        coord_flip()
      if(type=="se")
        interactions_plot<-interactions_plot+geom_errorbar(aes(ymin=tempdata[,cors]-se,ymax=tempdata[,cors]+se),width=.1,position=position_dodge(0.1))+
        labs(caption=paste("Bars are standard errors",note))
      if(type=="ci")
        interactions_plot<-interactions_plot+geom_errorbar(aes(ymin=tempdata[,cors]-ci,ymax=tempdata[,cors]+ci),width=.1,position=position_dodge(0.1))+
        labs(caption=paste("Bars are confidence intervals",note))
      if(type=="sd")
        interactions_plot<-interactions_plot+geom_errorbar(aes(ymin=tempdata[,cors]-sd,ymax=tempdata[,cors]+sd),width=.1,position=position_dodge(0.1))+
        labs(caption=paste("Bars are standard deviations",note))
      if(type=="") {}
      minaxis<-ggplot_build(interactions_plot)$layout$panel_scales_y[[1]]$range$range[[1]]
      if(is.null(minaxis)) {}
      else
        interactions_plot<-interactions_plot+annotate("text",x=tempdata_cases[,factors[1]],y=minaxis,label=paste("N:",tempdata_cases$N),alpha=.5,size=base_size/10*2,hjust=0,vjust=2)
      ggpubr::as_ggplot(gridExtra::arrangeGrob(interactions_plot))
    }
  }
  df[,iv]<-change_data_type(df[,iv],type="factor")
  
  combinations<-expand.grid(names(df)[iv],names(df)[iv],names(df)[dv])
  names(combinations)<-c("iv1","iv2","dv")
  row.names(combinations)<-paste0(combinations$iv1,"_",combinations$iv2,"_",combinations$dv)
  combinations<-change_data_type(combinations,type="character")
  combinations<-combinations[-which(combinations$iv1==combinations$iv2),]
  combinations<-combinations[!duplicated(combinations),]
  
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
    cl<-parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)
    progress<-function(n) setTxtProgressBar(pb,n)
    opts<-list(progress=progress)
    plot_data<-foreach(i=1:nrow(combinations),.final=function(x) setNames(x,row.names(combinations)),.packages=c("workingfunctions"),.options.snow=opts) %dopar% {
      output_data(i)
    }
    plots<-foreach(i=1:length(plot_data),.final=function(x) setNames(x,row.names(combinations)),.packages=c("workingfunctions","stringr"),.options.snow=opts) %dopar% {
      output_plot(i)
    }
    close(pb)
    parallel::stopCluster(cl)
    gc(full=TRUE)
  } else {
    plot_data<-plots<-list()
    for(i in 1:nrow(combinations)) {
      setTxtProgressBar(pb,i)
      plot_data[[row.names(combinations)[i]]]<-output_data(i)
    }
    for(i in 1:length(plot_data)) {
      setTxtProgressBar(pb,i)
      plots[[row.names(combinations)[i]]]<-output_plot(i)
    }
    close(pb)
  }
  
  plot_data_df<-Reduce(plyr::rbind.fill,plot_data)
  plot_data_df<-plot_data_df[,c(names(df)[c(iv,dv)],setdiff(c("N","sd","se","ci"),names(df)[c(iv,dv)]))]
  result<-list(plot_data=plot_data,plot_data_df=plot_data_df,plots=plots)
  return(result)
}
##########################################################################################
# PLOT ANOVA DIAGNOSTICS
##########################################################################################
#' @title Plot one way diagnostics
#' @param df dataframe
#' @param dv index of continous variables
#' @param iv index of factors
#' @param base_size base font size
#' @note
#' Residuals vs Fitted should be equally spread horizontally otherwize the assumption of equality of variances is violated \cr
#' Normal QQ should show values in the diagonal otherwise the assumption of normality is violated
#' @import ggplot2 ggfortify foreach
#' @importFrom stats formula lm
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @keywords ANOVA
#' @export
#' @examples
#' nrows=1000
#' df<-data.frame(generate_factor(vector=LETTERS[1:5],nrows=nrows,ncols=10,type="random"),
#'                generate_data(nrows=nrows,ncols=5,type="normal"))
#' result<-plot_oneway_diagnostics(df=df,dv=11:15,iv=1:10)
#' plot_oneway_diagnostics(df=mtcars,dv=1:2,iv=9:10)
plot_oneway_diagnostics<-function(df,dv,iv,base_size=10) {
  output_plot<-function(i) {
    factors<-combinations$iv[i]
    cors<-combinations$dv[i]
    tempdata<-df[complete.cases(df[,c(cors,factors)]),]
    tempdata<-tempdata[tempdata[,factors] %in% names(table(tempdata[,factors]))[table(tempdata[,factors])>1],]
    tempdata[,factors]<-factor(tempdata[,factors])
    if(length(unique(tempdata[,factors]))>1) {
      form<-stats::formula(paste0(cors,"~",factors))
      model<-stats::lm(form,data=tempdata)
      autoplot(model,which=1:6,ncol=2,label.size=3)+
        labs(caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
        theme_bw(base_size=base_size)+
        theme(axis.text.x=element_text(angle=45,hjust=1))
    }
  }
  
  combinations<-expand.grid(names(df)[iv],names(df)[dv])
  names(combinations)<-c("iv","dv")
  row.names(combinations)<-paste0(combinations$iv,"_",combinations$dv)
  combinations<-change_data_type(combinations,type="character")
  
  n_rows<-nrow(combinations)
  n_cores<-parallel::detectCores()
  if(n_cores*4<n_rows) {
    print(paste("parralel process with",n_cores,"workers for",n_rows,"tasks"))
    parralel=TRUE
  } else {
    parralel=FALSE
  }
  
  pb<-txtProgressBar(min=0,max=nrow(combinations),style=3)
  if(parralel){
    cl<-parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)
    progress<-function(n) setTxtProgressBar(pb,n)
    opts<-list(progress=progress)
    plots<-foreach(i=1:nrow(combinations),.final=function(x) setNames(x,row.names(combinations)),.packages=c("workingfunctions","ggplot2","ggfortify"),.options.snow=opts) %dopar% {
      output_plot(i)
    }
    close(pb)
    parallel::stopCluster(cl)
    gc(full=TRUE)
  } else {
    plots<-list()
    for(i in 1:nrow(combinations)) {
      setTxtProgressBar(pb,i)
      plots[[row.names(combinations)[i]]]<-output_plot(i)
    }
    close(pb)
  }
  
  return(plots)
}
# ##########################################################################################
# # PLOT ANOVA DIAGNOSTICS
# ##########################################################################################
# plot_lm_rvsf<-function(model,base_size=20) {
#   plot<-ggplot(model,aes(.fitted,.resid))+
#     geom_point()+
#     stat_smooth(method="lm",na.rm=TRUE)+
#     geom_hline(yintercept=0,col="red",linetype="dashed")+
#     labs(x="Fitted values",y="Residuals",title="Residual vs Fitted",caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
#     theme_bw(base_size=base_size)
#   return(plot)
# }
# plot_lm_sl<-function(model,base_size=20) {
#   plot<-ggplot(model,aes(.fitted,sqrt(abs(.stdresid))))+
#     geom_point(na.rm=TRUE)+
#     stat_smooth(method="lm",na.rm=TRUE)+
#     labs(x="Fitted value",y=expression(sqrt("|Standardized residuals|")),title="Scale-Location",caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
#     theme_bw(base_size=base_size)
#   return(plot)
# }
# plot_lm_cdvsl<-function(model,base_size=20) {
#   plot<-ggplot(model,aes(.hat,.cooksd))+
#     geom_point(na.rm=TRUE)+
#     stat_smooth(method="lm",na.rm=TRUE)+
#     geom_abline(slope=seq(0,3,0.5),color="gray",linetype="dashed")+
#     labs(x="Leverage hii",y="Cook's Distance",title="Cook's dist vs Leverage hii/(1-hii)",caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
#     theme_bw(base_size=base_size)
#   return(plot)
# }
# plot_lm_rvsl<-function(model,base_size=20) {
#   plot<-ggplot(model,aes(.hat,.stdresid))+
#     geom_point(aes(size=.cooksd),na.rm=TRUE)+
#     stat_smooth(method="lm",na.rm=TRUE)+
#     scale_size_continuous("Cook's Distance")+
#     labs(x="Leverage",y="Standardized Residuals",title="Residual vs Leverage Plot",caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
#     theme(legend.position="right")+
#     theme_bw(base_size=base_size)
#   return(plot)
# }
# plot_lm_cd<-function(model,base_size=20) {
#   plot<-ggplot(model,aes(seq_along(.cooksd),.cooksd))+
#     geom_bar(stat="identity",position="identity")+
#     labs(x="Obs. Number",y="Cook's distance",title="Cook's distance",caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
#     theme_bw(base_size=base_size)
#   return(plot)
# }
# plot_lm_qq<-function(model,base_size=20) {
#   plot<-ggplot(model,aes(x=as.numeric(qstats::qnorm(.stdresid,plot.it=FALSE,datax=TRUE)[[2]]),y=.stdresid))+
#     geom_point(na.rm=TRUE)+
#     labs(x="Theoretical Quantiles",y="Standardized Residuals",title="Normal Q-Q",caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
#     theme_bw(base_size=base_size)
#   return(plot)
# }
