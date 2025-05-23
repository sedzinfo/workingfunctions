##########################################################################################
# DECISION TREE
##########################################################################################
#' @title Report for rpart::rpart
#' @param model objectfromrpart::rpart
#' @param file filenameofoutput
#' @param w width of pdffile
#' @param h height of pdffile
#' @param base_size base font size
#' @param fast if TRUE trainand test raw datasets and error values are not saved in output
#' @keywordsML
#' @export
#' @examples
#' titanic_formula<-formula(survived~pclass+sex+age+sibsp)
#' train_test_classification<-k_fold(df=df_titanic,model_formula=titanic_formula,k=12)
#' rtree_classification<-rpart::rpart(titanic_formula,train_test_classification$f$train$f1,model=TRUE,x=TRUE,y=TRUE)
#' report_rtree(model=rtree_classification,file="Classification")
#' boston_formula<-formula(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat)
#' train_test_regression<-k_fold(df=MASS::Boston,model_formula=boston_formula)
#' rtree_regression<-rpart::rpart(boston_formula,train_test_regression$f$train$f1,model=TRUE,x=TRUE,y=TRUE)
#' report_rtree(model=rtree_regression,file="Regression")
report_rtree<-function(model,file="rpart_rtree",w=10,h=10,base_size=10,fast=TRUE) {
  result<-data.frame(model$cptable)
  result$nsplit<-factor(result$nsplit+1)
  minimun_size<-as.numeric(as.character(result[which.min(result[,"xerror"]),"nsplit"]))
  initial_model<-model
  model<-rpart::prune(model,cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
  ##########################################################################################
  importance<-model$variable.importance
  importance<-data.frame(names=names(importance),importance=importance)
  importance$names<-factor(importance$names,levels=rev(as.character(importance$names)))
  plot_importance<-ggplot(importance,aes(x=names,y=importance))+
    geom_bar(stat='identity')+
    labs(title="ImportancePlot",y="RelativeInfluence",x="Predictor")+
    theme_bw(base_size=base_size)+
    scale_x_discrete(limits=rev(levels(names)))+
    coord_flip()
  ##########################################################################################
  par(mfrow=c(2,2))
  rpart::plotcp(model)
  rpart::rsq.rpart(model)
  par(mfrow=c(1,1))
  error<-data.frame(model$cptable)
  error$nsplit<-factor(error$nsplit+1)
  tree_size<-error[which.min(error[,"xerror"]),"nsplit"]
  error<-reshape2::melt(error,id.vars="nsplit")
  names(error)<-c("Split","Metric","value")
  plot_prune<-ggplot(error,aes(x=Split,y=value,color=Metric))+
    geom_line(aes(group=Metric))+
    geom_point()+
    labs(title=paste("ErrorPlot","SuggestedSize:",tree_size),y="Metricvalue",x="SizeofTree")+
    theme_bw(base_size=base_size)
  ##########################################################################################
  cairo_pdf(invisible(paste0(file,".pdf")),onefile=TRUE,width=w,height=h)
  par(mfrow=c(2,2))
  rpart::plotcp(model)
  rpart::rsq.rpart(model)
  par(mfrow=c(1,1))
  rpart.plot::rpart.plot(model,type=1)
  plot_importance
  plot_prune
  dev.off()
  
  par(mfrow=c(2,2))
  print(rpart::plotcp(model))
  print(rpart::rsq.rpart(model))
  par(mfrow=c(1,1))
  print(rpart.plot::rpart.plot(model,type=1))
  print(plot_importance)
  print(plot_prune)
  
  frame<-data.frame(model$frame)
  cp<-data.frame(model$cptable)
  parameters<-data.frame(parameters=unlist(model$control))
  splits<-data.frame(name=row.names(model$splits),model$splits,row.names=NULL)
  importance<-data.frame(importance=model$variable.importance)
  ordered<-data.frame(ordered=model$ordered)
  data<-data.frame(y=model$y,x=model$x,model=model$model)
  call<-data.frame(call=call_to_string(model))
  result<-list(frame=frame,cp=cp,parameters=parameters,splits=splits,importance=importance,ordered=ordered,call=call)
  
  if(!is.null(file)){
    filename<-paste0(file,".xlsx")
    if(file.exists(filename))file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$frame,wb,"Frame",numFmt="#0")
    excel_critical_value(result$cp,wb,"CP",numFmt="#0")
    excel_critical_value(result$parameters,wb,"Parameters",numFmt="#0.00")
    excel_critical_value(result$splits,wb,"Splits",numFmt="#0.00")
    excel_critical_value(result$importance,wb,"Importance",numFmt="#0.00")
    excel_critical_value(result$ordered,wb,"Ordered",numFmt="#0.00")
    if(!fast){
      excel_critical_value(result$data,wb,"Data",numFmt="#0.00")
      excel_critical_value(result$confusion_matrix$df.train,wb,"Predict",numFmt="#0.00")
    }
    excel_critical_value(result$call,wb,"Call",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  return(result)
}
##########################################################################################
# NOTES
##########################################################################################
# rtree_classification$frame
# rtree_classification$where
# rtree_classification$call
# rtree_classification$terms
# rtree_classification$cptable
# rtree_classification$method
# rtree_classification$parms$prior
# rtree_classification$parms$loss
# rtree_classification$parms$split
# rtree_classification$control
# rtree_classification$functions
# rtree_classification$numresp
# rtree_classification$splits
# rtree_classification$csplit
# rtree_classification$variable.importance
# rtree_classification$y
# rtree_regression$x
# rtree_regression$model
# rtree_classification$ordered
# rtree_regression$frame
# rtree_regression$where
# rtree_regression$call
# rtree_regression$terms
# rtree_regression$cptable
# rtree_regression$method
# rtree_regression$parms
# rtree_regression$control
# rtree_regression$functions
# rtree_regression$numresp
# rtree_regression$splits
# rtree_regression$variable.importance
# rtree_regression$y
# rtree_regression$x
# rtree_regression$model
# rtree_regression$ordered
