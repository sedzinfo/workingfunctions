##########################################################################################
# RANDOM FOREST
##########################################################################################
#' @title Report for randomForest::randomForest
#' @param model object from randomForest::randomForest
#' @param validation_data validation data
#' @param file filename of output
#' @param w width of pdf file
#' @param h height of pdf file
#' @param base_size base font size
#' @param fast if TRUE error values are not saved in output
#' @keywords ML
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+education+spontaneous+induced)
#' boston_formula<-as.formula(c("medv~",paste(names(MASS::Boston)[1:13],collapse="+")))
#' train_test_classification<-workingfunctions::k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-workingfunctions::k_fold(df=MASS::Boston,model_formula=boston_formula)
#' rf_classification<-randomForest::randomForest(infert_formula,
#'                                               data=train_test_classification$f$train$f1,
#'                                               xtest=train_test_classification$f$x_test$f1,
#'                                               ytest=factor(train_test_classification$f$y_test$f1),
#'                                               ntree=200,
#'                                               mtry=5,
#'                                               na.action="na.exclude",
#'                                               proximity=TRUE,
#'                                               importance=TRUE,
#'                                               localImp=TRUE,
#'                                               do.trace=TRUE,
#'                                               keep.forest=TRUE,
#'                                               keep.inbag=TRUE)
#' rf_regression<-randomForest::randomForest(boston_formula,
#'                                           data=train_test_regression$f$train$f1,
#'                                           xtest=data.frame(train_test_regression$f$x_test$f1),
#'                                           ytest=train_test_regression$f$y_test$f1,
#'                                           ntree=200,
#'                                           mtry=5,
#'                                           na.action="na.exclude",
#'                                           proximity=TRUE,
#'                                           importance=TRUE,
#'                                           localImp=TRUE,
#'                                           do.trace=TRUE,
#'                                           keep.forest=TRUE,
#'                                           keep.inbag=TRUE)
#' report_random_forest(model=rf_classification,
#'                      validation_data=train_test_classification$f$test$f1,
#'                      file="Classification")
#' report_random_forest(model=rf_regression,
#'                      validation_data=train_test_regression$f$test$f1,
#'                      file="Regression")
report_random_forest<-function(model,validation_data,file=NULL,w=10,h=10,base_size=10,fast=TRUE) {
  require(randomForest)
  margin_plot<-list()
  variable_names<-all.vars(formula(model$call))
  observed<-validation_data[,variable_names[1]]
  predicted<-predict(model,validation_data)
  confusion_matrix<-confusion_matrix_percent(observed=observed,predicted=predict(model,validation_data))
  importance<-data.frame(model$importance,SD=model$importanceSD)
  ##########################################################################################
  importance<-data.frame(model$importance)
  importance_data<-data.frame(variable=row.names(importance),importance)
  lni<-length(names(importance_data))
  names(importance_data)[c((lni-1),lni)]<-c("Mean Decrease Accuracy","Mean Decrease Gini")
  names(importance_data)[1]<-"Factor"
  importance_data$`Mean Decrease Gini`<-importance_data$`Mean Decrease Gini`/100
  importance_data$Factor<-factor(importance_data$Factor,levels=importance_data[order(importance_data$`Mean Decrease Gini`),"Factor"])
  importance_data<-reshape2::melt(importance_data,id.vars="Factor")
  importance_plot<-ggplot(data=importance_data,aes(y=value,x=Factor,fill=variable))+geom_bar(stat="identity",position=position_dodge(),colour="white")+labs(x="Predictor",y="Relative Importance",title="Importance Plot")+theme_bw(base_size=base_size)+coord_flip()
  ##########################################################################################
  if(model$type=="classification") {
    error<-reshape2::melt(data.frame(Tree=factor(1:nrow(model$err.rate)),model$err.rate),id.vars="Tree",variable.name="Measure")
    names(error)[2]<-"Measure"
    error_plot<-ggplot(data=error,aes(y=value,x=as.numeric(Tree),color=Measure))+geom_line(size=1)+labs(x="Trees",y="",title="Classification Error Rate")+theme_bw(base_size=base_size)
    ##########################################################################################
    margin_data<-randomForest::margin(model)
    margin_data<-data.frame(Factor=row.names(as.matrix(margin_data)),value=matrix(margin_data),row.names=NULL)
    margin_data<-data.frame(Names=row.names(margin_data),margin_data,row.names=NULL)
    margin_data$Names<-factor(margin_data$Names,levels=margin_data[order(margin_data$value),"Names"])
    margin_plot<-ggplot(data=margin_data,aes(y=value,x=as.numeric(Names),color=Factor))+geom_point()+labs(x="index",y="",title="Margin Plot")+theme_bw(base_size=base_size)
    ##########################################################################################
    confusion_matrix_model<-plyr::rbind.fill(data.frame(type="Train",Category=row.names(model$confusion),model$confusion,check.names=FALSE),data.frame(type="Test",Category=row.names(model$test$confusion),model$test$confusion,check.names=FALSE))
    importance<-importance[order(importance$MeanDecreaseGini),]
    outliers<-randomForest::outlier(model)
    error<-data.frame(Error.Rate=model$err.rate)
    model_description<-data.frame(Model=t(data.frame(type=model$type,Tree_Number=model$ntree,M_Try=model$mtry,Max_Categories=model$forest$maxcat,Nodes=model$forest$nrnodes,Forest_Tree_Number=model$forest$ntree,Forest_Class_Number=model$forest$nclass,Call=stringr::str_replace_all(toString(deparse(model$call)),stringr::fixed(" "),""),stringsAsFactors=FALSE)))
  }
  if(model$type=="regression") {
    error<-data.frame(Tree=factor(1:length(model$mse)),Mean.Square.Error=model$mse,Pseudo.R.Squared=model$rsq)
    error<-reshape2::melt(error,id.vars="Tree",measure.vars=c("Pseudo.R.Squared","Mean.Square.Error"),variable.name="Measure")
    names(error)[2]<-"Measure"
    error$Measure<-gsub("."," ",error$Measure,fixed=TRUE)
    error_plot<-ggplot(data=error,aes(y=value,x=as.numeric(Tree),group=Measure,color=Measure))+geom_line(size=1)+labs(x="Trees",y="",title="Regression Error Rate")+theme_bw(base_size=base_size)
    ##########################################################################################
    outliers<-confusion_matrix_model<-data.frame()
    error<-data.frame(MSE=model$mse,RSQ=model$rsq)
    model_description<-data.frame(Model=t(data.frame(type=model$type,Tree_Number=model$ntree,M_Try=model$mtry,Nodes=model$forest$nrnodes,Forest_Tree_Number=model$forest$ntree,Call=stringr::str_replace_all(toString(deparse(model$call)),stringr::fixed(" "),""),stringsAsFactors=FALSE)))
  }
  result<-list(model_description=model_description,confusion_matrix=confusion_matrix,confusion_matrix_model=confusion_matrix_model,importance=importance,outliers=outliers,error=error)
  report_pdf(plotlist=list(error_plot,importance_plot,margin_plot),file=file,print_plot=TRUE)
  if (!file=="") {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    if(model$type=="classification")
      excel_confusion_matrix(confusion_matrix,wb)
    if (!fast)
      excel_critical_value(result$error,wb,"ERROR",numFmt="#0.00" )
    excel_critical_value(result$importance,wb,"IMPORTANCE",numFmt="#0.00" )
    excel_critical_value(result$model_description,wb,"MODEL DESCRIPTION",numFmt="#0.00" )
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
}
##########################################################################################
# NOTES
##########################################################################################
# model.tree<-party::ctree(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=mydata[train,])
# c.forest<-party::cforest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=mydata[train,])
# plot(model.tree)
# model<-rf_regression
# model<-rf_classification
# plot(model)
# varImpPlot(model)
# treesize(model)
# varUsed(model)
# getTree(model)
# margin(model)
# plot(margin(model))
# MDSplot(model,train$Survived,pch=as.numeric(train$Survived))
##########################################################################################
# CLASSIFICATION OUTPUT
##########################################################################################
# rf_classification$call
# rf_classification$type
# rf_classification$predicted
# rf_classification$err.rate
# rf_classification$confusion
# rf_classification$votes
# rf_classification$oob.times
# rf_classification$classes
# rf_classification$importance
# rf_classification$importanceSD
# rf_classification$localImportance
# rf_classification$proximity
# rf_classification$ntree
# rf_classification$mtry
# rf_classification$forest$ndbigtree
# rf_classification$forest$nodestatus
# rf_classification$forest$bestvar
# rf_classification$forest$treemap
# rf_classification$forest$nodepred
# rf_classification$forest$xbestsplit
# rf_classification$forest$pid
# rf_classification$forest$cutoff
# rf_classification$forest$ncat
# rf_classification$forest$maxcat
# rf_classification$forest$nrnodes
# rf_classification$forest$ntree
# rf_classification$forest$nclass
# rf_classification$forest$xlevels
# rf_classification$y
# rf_classification$test$predicted
# rf_classification$test$err.rate
# rf_classification$test$confusion
# rf_classification$test$votes
# rf_classification$test$proximity
# rf_classification$inbag
# rf_classification$terms
# rf_classification$na.action
##########################################################################################
# REGRESSION OUTPUT
##########################################################################################
# rf_regression$call
# rf_regression$type
# rf_regression$predicted
# rf_regression$mse
# rf_regression$rsq
# rf_regression$oob.times
# rf_regression$importance
# rf_regression$importanceSD
# rf_regression$localImportance
# rf_regression$proximity
# rf_regression$ntree
# rf_regression$mtry
# rf_regression$forest$ndbigtree
# rf_regression$forest$nodestatus
# rf_regression$forest$leftDaughter
# rf_regression$forest$rightDaughter
# rf_regression$forest$nodepred
# rf_regression$forest$bestvar
# rf_regression$forest$xbestsplit
# rf_regression$forest$ncat
# rf_regression$forest$nrnodes
# rf_regression$forest$ntree
# rf_regression$forest$xlevels
# rf_regression$coefs
# rf_regression$y
# rf_regression$test
# rf_regression$test$predicted
# rf_regression$test$mse
# rf_regression$test$rsq
# rf_regression$test$proximity
# rf_regression$inbag
# rf_regression$terms
