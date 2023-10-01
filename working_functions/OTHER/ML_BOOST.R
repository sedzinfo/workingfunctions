##########################################################################################
# BOOST
##########################################################################################
#' @title Report for gbm::gbm
#' @param model object from gbm::gbm
#' @param validation_data validation data
#' @param file Filename of output
#' @param w width of pdf file
#' @param h height of pdf file
#' @param base_size base font size
#' @param n.trees number of trees
#' @keywords ML boost
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+education+spontaneous+induced)
#' boston_formula<-as.formula(c("medv~",paste(names(MASS::Boston)[1:13],collapse="+")))
#' train_test_classification<-psycholatefunctions::k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-psycholatefunctions::k_fold(df=MASS::Boston,model_formula=boston_formula)
#' boost_classification<-gbm::gbm(infert_formula,
#'                                data=train_test_classification$f$train$f1,
#'                                distribution="gaussian",
#'                                n.trees=5000,
#'                                interaction.depth=10,
#'                                keep.data=TRUE,
#'                                verbose=TRUE)
#' boost_regression<-gbm::gbm(boston_formula,
#'                            data=train_test_regression$f$train$f1,
#'                            distribution="gaussian",
#'                            n.trees=5000,
#'                            interaction.depth=10,
#'                            keep.data=TRUE,
#'                            verbose=TRUE)
#' report_boost(model=boost_classification,
#'              validation_data=train_test_classification,
#'              file="Classification")
#' report_boost(model=boost_regression,
#'              validation_data=train_test_regression,
#'              file="Regression")
report_boost<-function(model,validation_data,file="boost",w=10,h=10,base_size=10,n.trees=5000) {
  require(ggplot2)
  require(gbm)
  variable_names<-all.vars(formula(model$call))
  importance<-data.frame(unclass(summary(model,plot=FALSE)))
  names(importance)<-c("variable","Relative.Influence")
  ##########################################################################################
  error<-data.frame(iteration=1:length(model$train.error),train_error=model$train.error,valid_error=model$valid.error,oobag_improve=model$oobag.improve)
  error<-reshape2::melt(error,id.vars="iteration",variable.name="Metric")
  names(error)[2]<-"Metric"
  error$Metric<-string_aes(error$Metric)
  error_plot<-ggplot(data=error,aes(y=value,x=iteration,color=Metric))+
    geom_line(size=base_size/15)+
    labs(x="Iteration",y="",title="Error")+
    theme_bw(base_size=base_size)
  ##########################################################################################
  importance<-data.frame(summary(model,plot=TRUE))
  importance<-importance[order(importance$rel.inf),]
  importance$var<-factor(importance$var,levels=as.character(importance$var))
  importance_plot<-ggplot(importance,aes(x=var,y=rel.inf))+geom_bar(stat='identity')+
    labs(title="Importance Plot",y="Relative Influence",x="Predictor")+
    theme_bw(base_size=base_size)+
    coord_flip()
  ##########################################################################################
  model_description<-(data.frame(init=model$initF,
                                 bag.fraction=model$bag.fraction,
                                 distribution=model$distribution$name,
                                 interaction.depth=model$interaction.depth,
                                 minimum.observations.node=model$n.minobsinnode,
                                 classes.number=model$num.classes,
                                 tree.number=model$n.trees,
                                 n.train=model$nTrain,
                                 train.fraction=model$train.fraction,
                                 response=model$response.name[1],
                                 shrinkage=model$shrinkage,
                                 cv.folds=model$cv.folds,
                                 verbose=model$verbose,
                                 call=stringr::str_replace_all(toString(deparse(model$call)),stringr::fixed(" "),"")))
  model_description<-data.frame(Name=names(model_description),Description=t(model_description),row.names=NULL)
  model_error<-data.frame(train.error=model$train.error,valid.error=model$valid.error,oob.improve=model$oobag.improve)
  predict_model<-data.frame(prediction=predict(model,n.trees=n.trees))
  tree_example<-gbm::pretty.gbm.tree(model)
  result<-list(importance=importance,model_description=model_description,terms=terms,model_error=model_error,predict_model=predict_model,tree_example=tree_example)
  
  cairo_pdf(invisible(paste0(file,".pdf")),onefile=TRUE,width=w,height=h)
  print(error_plot)
  print(importance_plot)
  
  importance<-data.frame(unclass(summary(model,plot=FALSE)))
  vars<-importance$var
  for (i in vars) {
    print(plot(model,i=i))
  }
  dev.off()
  
  if (!file=="") {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$model_error,wb,"ERROR",numFmt="#0.00" )
    excel_critical_value(result$importance,wb,"IMPOTRANCE",numFmt="#0.00" )
    excel_critical_value(result$model_description,wb,"MODEL DESCRIPTION",numFmt="#0.00" )
    openxlsx::saveWorkbook(wb,invisible(paste0(file,".xlsx")),TRUE)
  }
}
##########################################################################################
# NOTES
##########################################################################################
# model$trees
# model$c.splits
# model$fit
# model$var.levels
# model$var.monotone
# model$var.names
# model$var.type
# model$data
##########################################################################################
# CLASSIFICATION OUTPUT
##########################################################################################
# boost_classification$initF
# boost_classification$fit
# boost_classification$train.error
# boost_classification$valid.error
# boost_classification$oobag.improve
# boost_classification$trees
# boost_classification$c.splits
# boost_classification$bag.fraction
# boost_classification$distribution
# boost_classification$interaction.depth
# boost_classification$n.minobsinnode
# boost_classification$num.classes
# boost_classification$n.trees
# boost_classification$nTrain
# boost_classification$train.fraction
# boost_classification$response.name
# boost_classification$shrinkage
# boost_classification$var.levels
# boost_classification$var.monotone
# boost_classification$var.names
# boost_classification$var.type
# boost_classification$verbose
# boost_classification$data$y
# boost_classification$data$x
# boost_classification$data$x.order
# boost_classification$data$offset
# boost_classification$data$Misc
# boost_classification$data$w
# boost_classification$Terms
# boost_classification$cv.folds
# boost_classification$call
# boost_classification$m
##########################################################################################
# REGRESSION OUTPUT
##########################################################################################
# boost_regression$initF
# boost_regression$fit
# boost_regression$train.error
# boost_regression$valid.error
# boost_regression$oobag.improve
# boost_regression$trees
# boost_regression$c.splits
# boost_regression$bag.fraction
# boost_regression$distribution$name
# boost_regression$interaction.depth
# boost_regression$n.minobsinnode
# boost_regression$num.classes
# boost_regression$n.trees
# boost_regression$nTrain
# boost_regression$train.fraction
# boost_regression$response.name
# boost_regression$shrinkage
# boost_regression$var.levels
# boost_regression$var.monotone
# boost_regression$var.names
# boost_regression$var.type
# boost_regression$verbose
# boost_regression$data
# boost_regression$data$y
# boost_regression$data$x
# boost_regression$data$x.order
# boost_regression$data$offset
# boost_regression$data$Misc
# boost_regression$data$w
# boost_regression$Terms
# boost_regression$cv.folds
# boost_regression$call
# boost_regression$m
