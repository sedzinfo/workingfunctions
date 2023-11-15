plot_shap<-function(xgb_model=NULL,shap_contrib=NULL,X_train,top_n=NULL,x_bound=NULL,dilute=FALSE,scientific=FALSE,my_format=NULL) {
  if (!is.null(shap_contrib)) {
    if (paste0(dim(shap_contrib),collapse=" ") != paste0(dim(X_train),collapse=" ")) 
      stop("supply correct shap_contrib,remove BIAS column.\n")
  }
  shap<-if (is.null(shap_contrib)) 
    shap.values(xgb_model,X_train)
  else list(shap_score=shap_contrib,mean_shap_score=colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)),decreasing=T)])
  std1<-function(x)
    return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  if (is.null(top_n)) 
    top_n<-dim(X_train)[2]
  top_n<-as.integer(top_n)
  if (!top_n %in% c(1:dim(X_train)[2])) {
    message("Please supply correct top_n,by default use all features.\n")
    top_n<-dim(X_train)[2]
  }
  shap_score_sub<-setDT(shap$shap_score)[,names(shap$mean_shap_score)[1:top_n],with=F]
  shap_score_long<-melt.data.table(shap_score_sub,measure.vars=colnames(shap_score_sub))
  fv_sub<-as.data.table(X_train)[,names(shap$mean_shap_score)[1:top_n],with=F]
  fv_sub_long<-melt.data.table(fv_sub,measure.vars=colnames(fv_sub))
  fv_sub_long[,`:=`(stdfvalue,std1(value)),by="variable"]
  names(fv_sub_long)<-c("variable","rfvalue","stdfvalue")
  shap_long2<-cbind(shap_score_long,fv_sub_long[,c("rfvalue","stdfvalue")])
  shap_long2[,`:=`(mean_value,mean(abs(value))),by=variable]
  setkey(shap_long2,variable)
  data_long<-shap_long2
  if (scientific) {
    label_format="%.1e"
  }
  else {
    label_format="%.3f"
  }
  if (!is.null(my_format)) 
    label_format<-my_format
  N_features<-setDT(data_long)[,uniqueN(variable)]
  if (is.null(dilute)) 
    dilute=FALSE
  nrow_X<-nrow(data_long)/N_features
  if (dilute != 0) {
    dilute<-ceiling(min(nrow_X/10,abs(as.numeric(dilute))))
    set.seed(1234)
    data_long<-data_long[sample(nrow(data_long),min(nrow(data_long)/dilute,nrow(data_long)/2))]
  }
  x_bound<-if (is.null(x_bound)) 
    max(abs(data_long$value)) * 1.1
  else as.numeric(abs(x_bound))
  plot1<-ggplot(data=data_long)+
    coord_flip(ylim=c(-x_bound,x_bound))+
    ggforce::geom_sina(aes(x=variable,y=value,color=stdfvalue),method="counts",maxwidth=0.7,alpha=0.7)+
    geom_text(data=unique(data_long[,c("variable","mean_value")]),aes(x=variable,y=-Inf,label=sprintf(label_format,mean_value)),size=3,alpha=0.7,hjust=-0.2,fontface="bold")+
    scale_color_gradient(low="#FFCC33",high="#6600CC",breaks=c(0,1),labels=c(" Low","High "),guide=guide_colorbar(barwidth=12,barheight=0.3))+
    theme_bw()+
    theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),legend.position="bottom",legend.title=element_text(size=10),legend.text=element_text(size=8),axis.title.x=element_text(size=10))+
    geom_hline(yintercept=0)+
    scale_x_discrete(limits=rev(levels(data_long$variable)),labels=SHAPforxgboost::label.feature(rev(levels(data_long$variable))))+
    labs(y="SHAP value (impact on model output)",x="",color="Feature value  ")
  return(plot1)
}
##############################################
library(xgboost)
library(caret)
library(dplyr)
library(SHAPforxgboost)
library(xgboost)
library(data.table)
library(ggplot2)
load(url("https://github.com/christophM/interpretable-ml-book/blob/master/data/bike.RData?raw=true"))
bike_2=select(bike,-days_since_2011,-cnt,-yr)
bike_x=predict(dummyVars(" ~ .",data=bike_2,fullRank=T),newdata=bike_2)
model_bike=xgboost(data=bike_x,nround=100,objective="reg:linear",label=bike$cnt)  
shap_result_bike=shap.score.rank(model=model_bike,X_train=bike_x)
shap_long_bike=shap.prep(shap=shap_result_bike,X_train=bike_x,top_n=10)
plot_shap(data_long=shap_long_bike)

##############################################
predictors=as.matrix(iris[,-5])
mod1=xgboost::xgboost(data=predictors,label=iris$Species,gamma=0,eta=1,lambda=0,nrounds=10,verbose=TRUE)
SHAPforxgboost::shap.plot.summary(SHAPforxgboost::shap.prep(xgb_model=mod1,X_train=predictors))
plot_shap(xgb_model=mod1,X_train=predictors)


