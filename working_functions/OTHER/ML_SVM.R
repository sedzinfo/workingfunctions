##########################################################################################
# SUPPORT VECTOR MACHINE
##########################################################################################
#' @title Report for e1071::svm
#' @param model object from e1071::svm
#' @param validation_data validation data
#' @param file filename of output
#' @param w width of pdf file
#' @param h height of pdf file
#' @param base_size base font size
#' @keywords ML
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+education+spontaneous+induced)
#' boston_formula<-as.formula(c("medv~",paste(names(MASS::Boston)[1:13],collapse="+")))
#' train_test_classification<-psycholatefunctions::k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-psycholatefunctions::k_fold(df=MASS::Boston,model_formula=boston_formula)
#' svm_tune_classification<-e1071::tune(e1071::svm,
#'                                      infert_formula,
#'                                      data=train_test_classification$f$train$f1,
#'                                      ranges=list(cost=c(seq(0.01,1,0.01),1:100)))
#' svm_classification<-e1071::svm(infert_formula,
#'                                data=train_test_classification$f$train$f1)
#' report_svm(model=svm_classification,
#'            validation_data=train_test_classification$f$test$f1,
#'            file="Classification")
svm_classification$
report_svm<-function(model,validation_data,file="support_vector_machine",w=10,h=10,base_size=10) {
  require(e1071)
  
  variable_names<-all.vars(formula(model$call))
  observed<-validation_data[,variable_names[1]]
  predicted<-predict(model,newdata=validation_data)
  model_summary<-summary(model)
  model_call<-data.frame(call=stringr::str_replace_all(toString(deparse(model$call)),stringr::fixed(" "),""))
  terms<-model$terms
  confusion_matrix<-confusion_matrix_percent(observed=observed,predicted=predicted)
  result<-list(model_summary=model_summary,confusion_matrix=confusion_matrix,terms=terms)
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_confusion_matrix(confusion_matrix,wb)
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
}
##########################################################################################
# NOTES
##########################################################################################
# The model has to pass through a tuning process
##########################################################################################
# CLASSIFICATION OUTPUT
##########################################################################################
# svm_classification$call
# svm_classification$type
# svm_classification$kernel
# svm_classification$cost
# svm_classification$degree
# svm_classification$gamma
# svm_classification$coef0
# svm_classification$nu
# svm_classification$epsilon
# svm_classification$sparse
# svm_classification$scaled
# svm_classification$x.scale$`scaled:center`
# svm_classification$x.scale$`scaled:scale`
# svm_classification$nclasses
# svm_classification$levels
# svm_classification$tot.nSV
# svm_classification$nSV
# svm_classification$labels
# svm_classification$SV
# svm_classification$index
# svm_classification$rho
# svm_classification$compprob
# svm_classification$probA
# svm_classification$probB
# svm_classification$sigma
# svm_classification$coefs
# svm_classification$na.action
# svm_classification$fitted
# svm_classification$decision.values
# svm_classification$residuals
# svm_classification$terms
