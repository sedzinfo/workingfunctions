##########################################################################################
# PLOTS
##########################################################################################
#' @title Plot timeseries
#' @details returns plot
#' @param df ts object
#' @param base_size base font size
#' @param ylab y label
#' @param title plot title
#' @import ggplot2
#' @keywords time series
#' @export
#' @examples
#' ts_data<-ts(UKDriverDeaths,start=1969,end=1984,frequency=12)
#' result<-plot_ts(ts_data,title="UK driver deaths")
#' for(i in 1969:1984)
#'   result<-result+geom_vline(xintercept=i,color="blue",size=1,alpha=.5)
#' result
#' autoplot(stl(ts_data,s.window='periodic'))+
#'   theme_bw(base_size=10)+
#'   labs(title="UK driver deaths")
#' forecast::gglagplot(data.frame(ts_data),do.lines=FALSE,lags=100)+
#'   theme_bw(base_size=10)+labs(title="UK driver deaths",y="count")
plot_ts<-function(df,base_size=10,ylab="Count",title="") {
  value<-NULL
  df<-data.frame(date=matrix(time(df)),value=matrix(df))
  plot<-ggplot(df,aes(y=value,x=date))+
    geom_line(alpha=.5)+
    geom_point(alpha=.1,size=1)+
    theme_bw(base_size=base_size)+
    labs(title=title,x="Time index",y=ylab,caption=paste("Observations",(nrow(df))))+
    geom_smooth(method='lm',formula=y~x)
  return(plot)
}
##########################################################################################
# PLOT ACF
##########################################################################################
#' @title Plot autocorrelation function of correlation covariance and partial correlation
#' @description uses ggplot
#' @details returns plot
#' @param df ts object
#' @param lag.max maximum lags to include
#' @param base_size base font size
#' @param title plot title
#' @import ggplot2
#' @importFrom Rmisc CI
#' @importFrom plyr rbind.fill
#' @importFrom stats acf na.exclude
#' @keywords time series
#' @export
#' @examples
#' ts_data<-ts(UKDriverDeaths,start=1969,end=1984,frequency=12)
#' plot_acf(df=ts_data,base_size=20)
plot_acf<-function(df,lag.max=length(df),base_size=10,title="") {
  value<-index<-type<-ci<-NULL
  # ggfortify::autoplot(stats::acf(df,lag.max=10000,type="correlation",plot=FALSE))+theme_bw()+labs(title="")
  correlation<-stats::acf(df,lag.max=lag.max,plot=FALSE,type="correlation",na.action=stats::na.exclude)
  covariance<-stats::acf(df,lag.max=lag.max,plot=FALSE,type="covariance",na.action=stats::na.exclude)
  partial<-stats::acf(df,lag.max=lag.max,plot=FALSE,type="partial",na.action=stats::na.exclude)
  ci_correlation<-data.frame(type="Correlation",ci=c(Rmisc::CI(correlation$acf,ci=0.95)[[1]],Rmisc::CI(correlation$acf,ci=0.95)[[3]],Rmisc::CI(correlation$acf,ci=0.95)[[2]]))
  ci_covariance<-data.frame(type="Covariance",ci=c(Rmisc::CI(covariance$acf,ci=0.95)[[1]],Rmisc::CI(covariance$acf,ci=0.95)[[3]],Rmisc::CI(covariance$acf,ci=0.95)[[2]]))
  ci_partial<-data.frame(type="Partial.Correlation",ci=c(Rmisc::CI(partial$acf,ci=0.95)[[1]],Rmisc::CI(partial$acf,ci=0.95)[[3]],Rmisc::CI(partial$acf,ci=0.95)[[2]]))
  partial<-stats::acf(df,lag.max=lag.max,plot=FALSE,type="partial",na.action=stats::na.exclude)
  autocorrelation<-plyr::rbind.fill(data.frame(index=1:nrow(correlation$acf),type="Correlation",value=correlation$acf),
                                    data.frame(index=1:nrow(covariance$acf),type="Covariance",value=covariance$acf),
                                    data.frame(index=1:nrow(partial$acf),type="Partial.Correlation",value=partial$acf))
  plot<-ggplot(autocorrelation,aes(y=value,x=index,color=type,fill=type))+
    geom_line()+
    geom_hline(data=ci_correlation,aes(yintercept=ci[1]),alpha=.5,colour="blue",linetype="longdash")+
    geom_hline(data=ci_correlation,aes(yintercept=ci[3]),alpha=.5,colour="black",linetype="longdash")+
    geom_hline(data=ci_correlation,aes(yintercept=ci[2]),alpha=.5,colour="blue",linetype="longdash")+
    geom_hline(data=ci_covariance,aes(yintercept=ci[1]),alpha=.5,colour="blue",linetype="longdash")+
    geom_hline(data=ci_covariance,aes(yintercept=ci[3]),alpha=.5,colour="black",linetype="longdash")+
    geom_hline(data=ci_covariance,aes(yintercept=ci[2]),alpha=.5,colour="blue",linetype="longdash")+
    geom_hline(data=ci_partial,aes(yintercept=ci[1]),alpha=.5,colour="blue",linetype="longdash")+
    geom_hline(data=ci_partial,aes(yintercept=ci[3]),alpha=.5,colour="black",linetype="longdash")+
    geom_hline(data=ci_partial,aes(yintercept=ci[2]),alpha=.5,colour="blue",linetype="longdash")+
    geom_point(alpha=.1)+
    labs(title=title,x="Lag",y="")+
    facet_grid(type~.,scales="free")+
    theme_bw(base_size=base_size)+
    theme(legend.position="none")
  return(plot)
}
##########################################################################################
# SMOOTHING
##########################################################################################
#' @title Smoothing
#' @description smoothing for timeseries. uses base plot
#' @details returns plot
#' @param df ts object
#' @param start start value
#' @param stop stop value
#' @param step step
#' @param title plot title
#' @param type "default" "kernel" "lowess" "friedman" "splines" "polynomial" "linear"
#' @importFrom graphics par plot lines abline
#' @importFrom grDevices rainbow
#' @importFrom stats time supsmu smooth.spline lowess ksmooth filter lm fitted
#' @keywords time series
#' @export
#' @examples
#' ts_data<-ts(UKDriverDeaths,start=1969,end=1984,frequency=12)
#' par(mfrow=c(2,2))
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="default")
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="polynomial")
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="linear")
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="kernel")
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="lowess")
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="friedman")
#' ts_smoothing(ts_data,start=.01,stop=2,step=.01,title="Driver Deaths in UK",type="splines")
ts_smoothing<-function(df,start=.01,stop=2,step=.001,title="",type="kernel") {
  if(length(df)>0 & is.na(df)[1]==FALSE) {
    plot(df,type="l",ylab="",xlab="Time index",main=paste(paste(string_aes(type),"Smoothing"),title),lwd=1)
    smoothing<-seq(start,stop,step)
    stop<-n<-length(smoothing)
    line.colors<-grDevices::rainbow(n,s=1,v=1,start=0,end=max(1,n-1)/n,alpha=1)
    z=1
    for (i in smoothing) {
      z=1+z
      if(type=="default")
        lines(filter(df,filter=rep(1/round(i),round(i))),col=line.colors[z],lwd=1)
      if(type=="kernel")
        lines(ksmooth(time(df),df,"normal",bandwidth=i),col=line.colors[z],lwd=1)
      if(type=="lowess")
        lines(lowess(df,f=i),col=line.colors[z],lwd=1)
      if(type=="friedman" & i>0 & i<1)
        lines(supsmu(time(df),df,span=i),col=line.colors[z],lwd=1)
      if(type=="splines")
        lines(smooth.spline(time(df),df,spar=i),col=line.colors[z],lwd=1)
    }
    if(type=="polynomial") {
      wk=time(df)-mean(time(df))
      wk2=wk^2
      wk3=wk^3
      cs=cos(2*pi*wk)
      sn=sin(2*pi*wk)
      reg1=lm(df~wk+wk2+wk3,na.action=NULL)
      reg2=lm(df~wk+wk2+wk3+cs+sn,na.action=NULL)
      lines(fitted(reg1))
      lines(fitted(reg2))
    }
    if(type=="linear"){
      fit<-lm(df~time(df))
      abline(fit,lwd=1,col="#400c0c")
    }
  }
}
##########################################################################################
# MOVING AVERAGE
##########################################################################################
#' @title Moving Average
#' @description compute moving average
#' @param df dataframe
#' @param w window
#' @keywords time series
#' @export
#' @examples
#' compute_moving_average(df=mtcars,w=5)
compute_moving_average<-function(df,w) {
  df_ma<-df
  max_row<-nrow(df)
  for(collumn_index in names(df)) {
    for (row_index in 1:max_row) {
      index_ma<-seq(from=(row_index-w),to=(row_index+w),by=1)
      index_ma<-index_ma[index_ma>0]
      index_ma<-index_ma[index_ma<max_row]
      df_ma[row_index,collumn_index]<-mean(df[index_ma,collumn_index])
    }
  }
  return(df_ma)
}
##########################################################################################
# NOTES
##########################################################################################
# http://documents.software.dell.com/Statistics/Textbook/time-series-analysis
##########################################################################################
# ARIMA
##########################################################################################
# One autoregressive (p) parameter: ACF - exponential decay; PACF - spike at lag 1,no correlation for other lags.
# Two autoregressive (p) parameter: ACF - a sine-wave shape pattern or a set of exponential decays; PACF - spikes at lags 1 and 2,no correlation for other lags.
# One moving average (q) parameter: ACF - spike at lag 1,no correlation for other lags; PACF - damps out exponentially.
# Two moving average (q) parameter: ACF - spikes at lags 1 and 2,no correlation for other lags; PACF - a sine-wave shape pattern or a set of exponential decays.
# One autoregressive (p) and one moving average (q) parameter: ACF - exponential decay starting at lag 1; PACF - exponential decay starting at lag 1.
# ARIMA (p,d,q) p=Autoregressive Parameters,d=Differencing Passes,q=Moving Average Parameters.
# CHECK FOR MODEL
# Look at the significance of the coefficients. In R, p-values arent given. For each coefficient,calculate z=estimated coeff. / std. error of coeff. If |z| > 1.96,the estimated coefficient is significantly different from 0.
# Look at the ACF of the residuals. For a good model,all autocorrelations for the residual series should be non-significant. If this isnt the case,you need to try a different model.
# Look at Box-Pierce (Ljung) tests for possible residual autocorrelation at various lags (see Lesson 3.2 for a description of this test).
# If non-constant variance is a concern,look at a plot of residuals versus fits and/or a time series plot of the residuals.
##########################################################################################
# SMOOTHING
##########################################################################################
# Check for errors residuals
# Mean error
# Mean absolute error
# Sum of squared error (SSE), Mean squared error
# Percentage error (PE)
# Mean percentage error (MPE)
# Mean absolute percentage error (MAPE)
