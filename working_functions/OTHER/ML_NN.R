##########################################################################################
#PLOTGW
##########################################################################################
#for(iinnn_regression$model.list$variables){
#neuralnet::gwplot(nn_regression,selected.covariate=i)
#}
##########################################################################################
#NN
##########################################################################################
#' @title Reportforneuralnet::neuralnet
#' @param modelobjectfromneuralnet::neuralnet
#' @param validation_datavalidationdata
#' @param filefilenameofoutput
#' @param wwidthofpdffile
#' @param hheightofpdffile
#' @param base_sizebasefontsize
#' @param fast_outputifTRUErawdatasetsarenotsavedinoutput
#' @keywords ML
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+spontaneous+induced)
#' boston_formula<-as.formula(c("medv~",paste(names(MASS::Boston)[1:13],collapse="+")))
#' train_test_classification_nn<-k_fold(df=recode_scale_dummy(infert),model_formula=infert_formula)
#' train_test_regression_nn<-k_fold(df=recode_scale_dummy(MASS::Boston),model_formula=boston_formula)
#' nn_classification<-neuralnet::neuralnet(infert_formula,data=train_test_classification_nn$f$train$f1,linear.output=FALSE)
#' nn_regression<-neuralnet::neuralnet(boston_formula,data=train_test_regression_nn$f$train$f1,linear.output=TRUE)
#' report_nn(model=nn_classification,validation_data=train_test_classification_nn$f$test$f1,file="Classification")
#' report_nn(model=nn_regression,validation_data=train_test_regression_nn$f$test$f1,file="Regression")
report_nn<-function(model,validation_data,file="nn",w=10,h=10,base_size=10,fast_output=TRUE){
  model_variables<-all.vars(formula(model))
  observed<-validation_data[,model_variables[1]]
  predicted<-predict(model,validation_data)
  confusion_matrix<-confusion_matrix_percent(observed=observed,predicted=ifelse(predicted[,1]>.5,1,0))
  model_call<-data.frame(Call=trimws(toString(deparse(model$call))),linear.output=model$linear.output)
  response<-data.frame(model$response)
  covariate<-data.frame(model$covariate)
  data<-data.frame(model$data)
  model_response<-data.frame(response.variable=model$model.list$response)
  model_variables<-data.frame(variables=model$model.list$variables)
  variables<-plyr::rbind.fill(model_variables,model_response)
  net.result<-data.frame(Result=model$net.result[[1]])
  generalized_weights<-data.frame(model$generalized.weights)
  result_matrix<-data.frame(model$result.matrix)
  result<-list(response=response,covariate=covariate,data=data,net.result=net.result,generalized_weights=generalized_weights,result_matrix=result_matrix,
               variables=variables,model_call=model_call,net.result=net.result)
  cairo_pdf(invisible(paste0(file,".pdf")),onefile=TRUE,width=w,height=h)
  plot(model,rep="best")
  dev.off()
  if(!is.null(file)){
    filename<-paste0(file,".xlsx")
    if(file.exists(filename))file.remove(filename)
    wb<-openxlsx::createWorkbook()
    if(!model$linear.output)
      excel_confusion_matrix(confusion_matrix,wb)
    if(!fast_output==TRUE)
      excel_critical_value(result$data,wb,"Data",numFmt="#0.00")
    excel_critical_value(result$response,wb,"Response",numFmt="#0.00")
    excel_critical_value(result$covariate,wb,"Covariate",numFmt="#0.00")
    excel_critical_value(result$net.result,wb,"NetResult",numFmt="#0.00")
    excel_critical_value(result$result_matrix,wb,"Result",numFmt="#0.00")
    excel_critical_value(result$variables,wb,"variables",numFmt="#0.00")
    excel_critical_value(result$model_call,wb,"Call",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
}
##########################################################################################
#NOTES
##########################################################################################
#model$err.fct
#model$act.fct
#model$weights
#model$startweights
##########################################################################################
#REGRESSIONOUTPUT
##########################################################################################
#nn_regression$call
#nn_regression$response
#nn_regression$covariate
#nn_regression$model.list
#nn_regression$model.list$response
#nn_regression$model.list$variables
#nn_regression$err.fct()
#nn_regression$act.fct()
#nn_regression$linear.output
#nn_regression$data
#nn_regression$net.result
#nn_regression$weights
#nn_regression$startweights
#nn_regression$generalized.weights
#nn_regression$result.matrix
