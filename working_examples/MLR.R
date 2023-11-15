##########################################################################################
# LOAD
##########################################################################################
library(psycholatefunctions)
install_load(c("mlr","xgboost","ParamHelpers"))
rm(list=ls())
graphics.off()
load(file="rworking/projects/graduace/data/essay_feat.RD")
load(file="rworking/projects/graduace/data/tsr.RD")
tsr<-plyr::rbind.fill(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9,sr10,sr11,sr12,sr13,sr14,sr15)
tst<-plyr::rbind.fill(srt1,srt2,srt3,srt4,srt5,srt6,srt7,srt8,srt9,srt10,srt11,srt12,srt13,srt14,srt15)
sbj<-plyr::rbind.fill(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15)
df<-data.frame(tsr,tst,sbj)
df[is.na(df)]<-0
predictor<-"score"
##########################################################################################
# REGRESSION
##########################################################################################
feat_formula<-as.formula(c("total~",paste(names(essay_feat)[9:length(essay_feat)],collapse="+")))
ttd<-k_fold(essay_feat,k=10,model_formula=feat_formula)
trainTask<-makeRegrTask(data=ttd$k_folding$train$fold1,target="total")
testTask<-makeRegrTask(data=ttd$k_folding$test$fold1,target="total")
xgb_learner<-makeLearner("regr.xgboost",
                         predict.type="response",
                         par.vals=list(objective="reg:linear",
                                       eval_metric="error",
                                       nrounds=500))
xgb_model<-train(xgb_learner,task=trainTask)
getParamSet("regr.xgboost")
xgb_params<-makeParamSet(makeDiscreteParam("booster",values=c("gbtree","gblinear","dart")),     # gbtree
                         makeNumericParam("eta",lower=0,upper=1),                               # 0.3
                         makeNumericParam("gamma",lower=0,upper=100),                           # 0
                         makeIntegerParam("max_depth",lower=1,upper=100),                       # 6
                         makeIntegerParam("nrounds",lower=1,upper=1000),                        # 1
                         makeDiscreteParam("sample_type",values=c("uniform","weighted")),       # uniform
                         makeDiscreteParam("normalize_type",values=c("tree","forest")),         # tree
                         makeNumericParam("rate_drop",lower=0,upper=1),                         # 0
                         makeNumericParam("skip_drop",lower=0,upper=1))                         # 0
control<-makeTuneControlRandom(maxit=5)
resample_desc<-makeResampleDesc("CV",iters=2)
tuned_params<-tuneParams(learner=xgb_learner,
                         task=trainTask,
                         resampling=resample_desc,
                         par.set=xgb_params,
                         control=control)
xgb_tuned_learner<-setHyperPars(learner=xgb_learner,par.vals=tuned_params$x)
xgb_model<-train(xgb_tuned_learner,trainTask)
pred<-predict(xgb_model,testTask)
pred$data$response<-round(pred$data$response,0)
proportion_accurate(pred$data$truth,pred$data$response)
confusion_matrix_percent(pred$data$truth,pred$data$response)
xgb<-xgboost::xgb.train(params=tuned_params$x,data=ttd$xbg$fold1$train,nthread=8,nrounds=100,watchlist=ttd$xbg$fold1$watchlist)
report_xgboost(xgb_model$learner.model,ttd$xbg$fold1)
##########################################################################################
# CLASSIFICATION
##########################################################################################
titanic_data<-get_all_vars(titanic_formula,titanic)
titanic_data$Survived<-factor(titanic_data$Survived)
combined<-normalizeFeatures(titanic_data,target="Survived",cols=c("Age","Fare","SibSp"))
combined<-createDummyFeatures(combined,target="Survived",cols=c("Sex","Embarked"))
imp<-impute(combined,classes=list(factor=imputeMode(),integer=imputeMean(),numeric=imputeMean()))
ttd<-k_fold(imp$data,k=10,model_formula=as.formula(Survived~.))
train<-ttd$k_folding$train$fold1
test<-ttd$k_folding$test$fold1
trainTask<-makeClassifTask(data=train,target="Survived",positive=1)
testTask<-makeClassifTask(data=test,target="Survived")
xgb_learner<-makeLearner("classif.xgboost",
                         predict.type="response",
                         par.vals=list(objective="binary:logistic",
                                       eval_metric="error",
                                       nrounds=50))
xgb_model<-train(xgb_learner,task=trainTask)
getParamSet("classif.xgboost")
control<-makeTuneControlRandom(maxit=100)
resample_desc<-makeResampleDesc("CV",iters=5)
tuned_params<-tuneParams(learner=xgb_learner,
                         task=trainTask,
                         resampling=resample_desc,
                         par.set=xgb_params,
                         control=control)
xgb_tuned_learner<-setHyperPars(learner=xgb_learner,par.vals=tuned_params$x)
xgb_model<-train(xgb_tuned_learner,trainTask)
pred<-predict(xgb_model,testTask)
confusion_matrix_percent(as.character(pred$data$truth),as.character(pred$data$response))
##########################################################################################
# 
##########################################################################################
getParamSet("regr.xgboost")
getParamSet("classif.xgboost")
model_formula<-as.formula(c(paste0(predictor,"~"),paste(names(df)[2:length(df)],collapse="+")))
kfdata<-k_fold(df,k=10,model_formula=model_formula)
trainTask<-makeRegrTask(data=kfdata$k_folding$train$fold1,target=predictor)
testTask<-makeRegrTask(data=kfdata$k_folding$test$fold1,target=predictor)
xgb_learner<-makeLearner(cl="regr.xgboost",id="regr.xgboost",
                         predict.type="response",
                         par.vals=list(objective="reg:linear",
                                       eval_metric="error",
                                       nrounds=500))
xgb_model<-train(xgb_learner,task=trainTask)
control<-makeTuneControlRandom(same.resampling.instance=TRUE,maxit=5,tune.threshold=FALSE)
resample_desc<-makeResampleDesc("CV",iters=2)
tuned_params<-tuneParams(learner=xgb_learner,
                         task=trainTask,
                         resampling=resample_desc,
                         par.set=xgb_params,
                         control=control)
xgb_tuned_learner<-setHyperPars(learner=xgb_learner,par.vals=tuned_params$x)
xgb_model<-train(xgb_tuned_learner,trainTask)
pred<-predict(xgb_model,testTask)
pred$data$response<-round(pred$data$response,0)
proportion_accurate(pred$data$truth,pred$data$response)
confusion_matrix_percent(pred$data$truth,pred$data$response)
##########################################################################################
# NOTES
##########################################################################################
# xgb_params<-makeParamSet(makeDiscreteParam("booster",values=c("gbtree","gblinear","dart")),     # gbtree
#                          makeNumericParam("eta",lower=0,upper=1),                               # 0.3
#                          makeNumericParam("gamma",lower=0,upper=Inf),                           # 0
#                          makeIntegerParam("max_depth",lower=1,upper=Inf),                       # 6
#                          makeNumericParam("min_child_weight",lower=0,upper=Inf),                # 1
#                          makeNumericParam("subsample",lower=0,upper=1),                         # 1
#                          makeNumericParam("colsample_bytree",lower=0,upper=1),                  # 1
#                          makeNumericParam("colsample_bylevel",lower=0,upper=1),                 # 1
#                          makeIntegerParam("num_parallel_tree",lower=1,upper=Inf),               # 1
#                          makeNumericParam("lambda",lower=0,upper=Inf),                          # 1
#                          makeNumericParam("lambda_bias",lower=0,upper=Inf),                     # 0
#                          makeNumericParam("alpha",lower=0,upper=Inf),                           # 0
#                          makeNumericParam("max_delta_step",lower=0,upper=Inf),                  # 0
#                          makeIntegerVectorParam("monotone_constraints",len=0,lower=-1,upper=1), # 0
#                          makeNumericParam("tweedie_variance_power",lower=1,upper=2),            # 1.5
#                          makeIntegerParam("nrounds",lower=1,upper=Inf),                         # 1
#                          makeDiscreteParam("sample_type",values=c("uniform","weighted")),       # uniform
#                          makeDiscreteParam("normalize_type",values=c("tree","forest")),         # tree
#                          makeNumericParam("rate_drop",lower=0,upper=1),                         # 0
#                          makeNumericParam("skip_drop",lower=0,upper=1))                         # 0
