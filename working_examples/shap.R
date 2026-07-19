library(xgboost)
library(caret)
library(dplyr)
library(data.table)
library(ggplot2)

##############################################
load(url("https://github.com/christophM/interpretable-ml-book/blob/master/data/bike.RData?raw=true"))
bike_2=select(bike,-days_since_2011,-cnt,-yr)
bike_x=predict(dummyVars(" ~ .",data=bike_2,fullRank=T),newdata=bike_2)
model_bike=xgboost(data=bike_x,nrounds=100,objective="reg:squarederror",label=bike$cnt)
xgb.ggplot.shap.summary(data=bike_x,model=model_bike,top_n=10)

##############################################
predictors=as.matrix(iris[,-5])
mod1=xgboost::xgboost(data=predictors,label=iris$Species,gamma=0,eta=1,lambda=0,nrounds=10,verbose=TRUE)
xgb.ggplot.shap.summary(data=predictors,model=mod1,top_n=4)
