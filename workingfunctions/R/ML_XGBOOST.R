##########################################################################################
# TREE PLOT
##########################################################################################
# #' @title Plot trees for xgboost::xgb.train
# #' @param model object from xgboost::xgb.train
# #' @param train Train dataset
# #' @param file output filename
# #' @keywords ML
# #' @export
# #' @examples
# #' infert_formula<-as.formula(factor(case)~education+spontaneous+induced)
# #' boston_formula<-as.formula(c("medv~",paste(names(MASS::Boston)[1:13],collapse="+")))
# #' train_test_classification<-psycholatefunctions::k_fold(df=infert,model_formula=infert_formula)
# #' train_test_regression<-psycholatefunctions::k_fold(df=MASS::Boston,model_formula=boston_formula)
# #' xgb_classification<-xgboost::xgb.train(data=train_test_classification$xgb$f1$train,
# #'                                        watchlist=train_test_classification$xgb$f1$watchlist,
# #'                                        eta=.1,
# #'                                        nthread=8,
# #'                                        nround=20,
# #'                                        objective="binary:logistic")
# #' xgb_regression<-xgboost::xgb.train(data=train_test_regression$xgb$f1$train,
# #'                                    watchlist=train_test_regression$xgb$f1$watchlist,
# #'                                    eta=.3,
# #'                                    nthread=8,
# #'                                    nround=20)
# #' xgb.plot.multi.trees(model = xgb_classification, features_keep = 2)
# #' plot_trees_xgboost(xgb_classification,train_test_classification$xgb$f1,file="Classification")
# #' plot_trees_xgboost(xgb_regression,train_test_regression$xbg$f1,file="Regression")
# plot_trees_xgboost<-function(model,train,file="xgboost") {
#   xgboost_trees<-xgboost::xgb.plot.multi.trees(model=model,feature_names=colnames(train),features_keep=10)
#   htmlwidgets::saveWidget(xgboost_trees,invisible(paste0(toString(getwd()),"/",file,".html")),selfcontained=TRUE)
# }
##########################################################################################
# XGBOOST
##########################################################################################
#' @title Report for xgboost::xgb.train
#' @param model object from xgboost::xgb.train
#' @param validation_data validation data
#' @param label outcome variable name
#' @param file output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @param base_size base font size
#' @param title plot title
#' @param fast if TRUE error values are not saved in output
#' @import ggplot2
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom stringr str_replace_all fixed
#' @importFrom xgboost xgb.DMatrix xgb.plot.deepness xgb.importance xgb.ggplot.importance
#' @importFrom reshape2 melt
#' @keywords ML
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+education+spontaneous+induced)
#' boston_formula<-as.formula(c("medv~",paste(names(MASS::Boston)[1:13],collapse="+")))
#' train_test_classification<-workingfunctions::k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-workingfunctions::k_fold(df=MASS::Boston,model_formula=boston_formula)
#' xgb_classification<-xgboost::xgb.train(data=train_test_classification$xgb$f1$train,
#'                                        watchlist=train_test_classification$xgb$f1$watchlist,
#'                                        eta=.1,
#'                                        nthread=8,
#'                                        nround=20,
#'                                        objective="binary:logistic")
#' xgb_regression<-xgboost::xgb.train(data=train_test_regression$xgb$f1$train,
#'                                    watchlist=train_test_regression$xgb$f1$watchlist,
#'                                    eta=.3,
#'                                    nthread=8,
#'                                    nround=20)
#' report_xgboost(model=xgb_classification,
#'                validation_data=train_test_classification$f$test$f1,
#'                label=train_test_classification$outcome,
#'                file="Classification")
#' report_xgboost(model=xgb_regression,
#'                validation_data=train_test_regression$f$test$f1,
#'                label=train_test_regression$outcome,
#'                file="Regression")
report_xgboost<-function(model,validation_data=NULL,label=NULL,file="xgboost",w=10,h=10,base_size=10,title="",fast=FALSE) {
  Depth<-Tree<-Cover<-Weight<-value<-Iteration<-Metric<-Factor<-NULL
  plots<-list()
  if(!is.null(validation_data)) {
    observed<-validation_data[,label]
    predicted<-predict(model,newdata=xgboost::xgb.DMatrix(data=data.matrix(validation_data[,model$feature_names]),label=validation_data[,label]))
    plots[[paste0("performance")]]<-result_confusion_performance(observed=observed,predicted=predicted)
    plots$regression<-plot_scatterplot(data.frame(observed=observed,predicted=predicted))
  }
  ##########################################################################################
  parameters<-data.frame(Hyperparameter=names(model$params),value=c(unlist(model$params)))
  model_call<-data.frame(Parameters="Call",value=stringr::str_replace_all(toString(deparse(model$call)),stringr::fixed(" "),""))
  evaluation_log<-data.frame(model$evaluation_log)
  result<-list(parameters=parameters,evaluation_log=evaluation_log)
  ##########################################################################################
  xgboost_model_depth<-data.frame(xgboost::xgb.plot.deepness(model,which='max.depth',plot=FALSE))
  plots$depth<-ggplot(data=xgboost_model_depth,aes(y=Depth,x=Tree))+geom_point(alpha=.1)+labs(x="Tree",y="Depth",title="")+theme_bw(base_size=base_size)
  plots$cover<-ggplot(data=xgboost_model_depth,aes(y=Cover,x=Tree))+geom_point(alpha=.1)+labs(x="Tree",y="Cover",title="")+theme_bw(base_size=base_size)
  plots$weight<-ggplot(data=xgboost_model_depth,aes(y=Weight,x=Tree))+geom_point(alpha=.1)+labs(x="Tree",y="Weight",title="")+theme_bw(base_size=base_size)
  ##########################################################################################
  error<-data.frame(model$evaluation_log)
  error<-reshape2::melt(error,id.vars="iter",variable.name="Metric")
  names(error)<-c("Iteration","Metric","value")
  error$Metric<-string_aes(error$Metric)
  plots$error<-ggplot(data=error,aes(y=value,x=Iteration,color=Metric))+
    geom_line(size=base_size/15)+
    labs(x="Iteration",y="Proportion",title=paste0("Error:",title))+
    theme_bw(base_size=base_size)
  ##########################################################################################
  importance_data<-xgboost::xgb.importance(model$feature_names,model=model)
  plot<-xgboost::xgb.ggplot.importance(importance_data,rel_to_first=TRUE)+theme_bw(base_size=base_size)
  importance_data<-data.frame(importance_data)
  importance_data$Feature<-factor(importance_data$Feature,levels=importance_data[order(importance_data$Importance),"Feature"])
  importance_data<-reshape2::melt(importance_data,id.vars="Feature")
  names(importance_data)[1:2]<-c("Factor","Metric")
  plots$importance<-ggplot(data=importance_data,aes(y=value,x=Factor,fill=Metric))+
    geom_bar(stat="identity",position=position_dodge(),colour="white")+
    labs(x="Predictor",y="Relative Importance",title=paste("Importance",title))+
    theme_bw(base_size=base_size)+theme(axis.text.x=element_text())+
    coord_flip()
  ##########################################################################################
  report_pdf(plotlist=plots,file=file,title=title,w=w,h=h,print_plot=TRUE)
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_confusion_matrix(plots$performance$confusion_matrix,wb)
    excel_critical_value(importance_data,wb,"Feature Importance",numFmt="#0.00")
    excel_critical_value(result$parameters,wb,"Hyperparameters",numFmt="#0.00")
    if (!fast)
      excel_critical_value(result$evaluation_log,wb,"Evaluation Log",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
}
##########################################################################################
# NOTES
##########################################################################################
# model$handle
# model$raw
# model$callbacks
##########################################################################################
# xgb_classification
##########################################################################################
# xgb_classification$handle
# xgb_classification$raw
# xgb_classification$niter
# xgb_classification$evaluation_log
# xgb_classification$call
# xgb_classification$params
# xgb_classification$callbacks$cb.print.evaluation
# xgb_classification$callbacks$cb.evaluation.log
##########################################################################################
# XGBOOST_REGRESSION
##########################################################################################
# xgb_regression$handle
# xgb_regression$raw
# xgb_regression$niter
# xgb_regression$evaluation_log
# xgb_regression$call
# xgb_regression$params
# xgb_regression$callbacks



