##########################################################################################
# DECISION TREE
##########################################################################################
#' @title Report for tree::tree
#' @param model object from tree::tree
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
#' train_test_classification<-psycholatefunctions::k_fold(df=infert,model_formula=infert_formula)
#' train_test_regression<-psycholatefunctions::k_fold(df=MASS::Boston,model_formula=boston_formula)
#' train_test_classification<-psycholatefunctions::k_fold(df=infert,
#'                                                        model_formula=infert_formula)
#' train_test_regression<-psycholatefunctions::k_fold(df=MASS::Boston,
#'                                                    model_formula=boston_formula)
#' tree_classification<-tree::tree(infert_formula,
#'                                 train_test_classification$f$train$f1,
#'                                 x=TRUE,
#'                                 y=TRUE,
#'                                 wts=TRUE)
#' tree_regression<-tree::tree(boston_formula,
#'                             train_test_regression$f$train$f1,
#'                             x=TRUE,
#'                             y=TRUE,
#'                             wts=TRUE)
#' report_tree_tree(model=tree_classification,
#'                  validation_data=train_test_classification$f$test$f1,
#'                  file="Classification")
#' report_tree_tree(model=tree_regression,
#'                  validation_data=train_test_regression$f$test$f1,
#'                  file="Regression")

iris_formula<-as.formula(factor(Species)~Sepal.Width+Petal.Length)
rtree_iris<-tree::tree(iris_formula,iris,x=TRUE,y=TRUE)
report_rtree(rtree_iris,file="Classification")
report_tree_tree(rtree_iris,iris)

library(rattle)
plot(rtree_iris)
text(rtree_iris)
rattle::fancyRpartPlot(rpart::rpart(iris_formula,iris,model=TRUE,x=TRUE,y=TRUE),main="",sub="",caption="",type=4,box.palette=0)

load(file="/mnt/WDRED_REMOTE/Dropbox (Psycholate)/dimitrios/working/data/examples.rda")
load(file="/mnt/WDRED_REMOTE/Dropbox (Psycholate)/dimitrios/working/data/data.rda")

titanic_formula<-formula(factor(Survived) ~ Pclass + Sex + Age + SibSp)
rtree_titanic<-tree::tree(titanic_formula,kaggle_csv$titanic.csv,x=TRUE,y=TRUE)
rattle::fancyRpartPlot(rpart::rpart(titanic_formula,kaggle_csv$titanic.csv,model=TRUE,x=TRUE,y=TRUE),main="",sub="",caption="",type=4,box.palette=0)



report_tree_tree<-function(model,validation_data,file="tree",w=10,h=10,base_size=10,fast=TRUE) {
  require(tree)
  initial_model<-model
  confusion_matrix<-data.frame()
  if(unclass(summary(model))$type=="\nClassification tree:\n")
    cv_tree<-tree::cv.tree(model,FUN=prune.misclass)
  if(unclass(summary(model))$type=="\nRegression tree:\n")
    cv_tree<-tree::cv.tree(model)
  result<-data.frame(Tree.Size=cv_tree$size,Deviance=cv_tree$dev,K=cv_tree$k)
  result<-remove_nc(result,value=NA)
  minimun_size<-min(result[result$Deviance %in% min(result$Deviance),]$Tree.Size)
  result<-list(result=result,minimun_size=minimun_size)
  if(unclass(summary(model))$type=="\nClassification tree:\n")
    model<-tree::prune.misclass(model,best=result$minimun_size)
  if(unclass(summary(model))$type=="\nRegression tree:\n")
    model<-tree::prune.tree(model,best=result$minimun_size)
  
  variable_names<-all.vars(formula(initial_model$call))
  confusion<-correlation<-residuals<-data.frame()
  plots<-confusion_performance<-list()
  
  tree_description<-data.frame(model$frame)
  model_call<-data.frame(call=stringr::str_replace_all(toString(deparse(model$call)),stringr::fixed(" "),""))
  wcw<-data.frame(Where=model$where,Outcome=model$y,Predictor=data.frame(model$x),Weights=model$weights)
  observed<-validation_data[,variable_names[1]]
  predicted<-predict(model,validation_data)
  
  if(unclass(summary(model))$type=="\nClassification tree:\n") {
    for(i in 1:ncol(predicted)) {
      plots[[paste0("performance_",i)]]<-result_confusion_performance(observed=observed,predicted=predicted[,i])
      plots[[paste0("separability_",i)]]<-plot_separability(observed=observed,predicted=predicted[,i])
      cm<-confusion_matrix_percent(observed=observed,predicted=ifelse(predicted[,i]>mean(plots[[paste0("performance_",i)]]$cut),1,0))
      plots[[paste0("confusion_",i)]]<-plot_confusion(observed=observed,predicted=ifelse(predicted[,i]>mean(plots[[paste0("performance_",i)]]$cut),1,0))
      cm<-data.frame(observed=row.names(cm),cm,check.names=FALSE)
      confusion_matrix<-plyr::rbind.fill(confusion_matrix,cm)
    }
  }
  ##########################################################################################
  result_df<-result$result
  result_k<-result_df[!result_df$K %in% NA,]
  plots$error_size<-ggplot(data=result_k,aes(y=Deviance,x=K))+
    geom_line(size=base_size/10)+
    geom_point(size=base_size/2)+
    labs(title="Error",x="K",y="Deviance")+
    theme_bw(base_size=base_size)
  plots$error_k<-ggplot(data=result_df,aes(y=Deviance,x=Tree.Size))+
    geom_line(size=base_size/10)+
    geom_point(size=base_size/2)+
    labs(title="Error",x="Tree Size",y="Deviance")+
    theme_bw(base_size=base_size)+
    scale_x_continuous(breaks=result$result$Tree.Size)
  ##########################################################################################
  if(unclass(summary(model))$type=="\nRegression tree:\n") {
    residuals<-data.frame(Residuals=unclass(summary(model))$residuals)
  }
  plots$regression<-plot_scatterplot(data.frame(observed,predicted))
  result<-list(model_summary=summary(model),confusion_matrix=confusion_matrix,tree_description=tree_description,wcw=wcw,terms=model$terms,correlation=correlation,residuals=residuals)
  
  cairo_pdf(invisible(paste0(file,".pdf")),onefile=TRUE,width=w,height=h)
  plot(initial_model)
  text(initial_model)
  plot(model)
  text(model)
  print(plots)
  dev.off()
  
  if (!file=="") {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    if(unclass(summary(model))$type=="\nClassification tree:\n")
      excel_critical_value(confusion_matrix,wb,"Confusion matrix")
    if(unclass(summary(model))$type=="\nRegression tree:\n")
      excel_critical_value(residuals,wb,"Residuals",numFmt="#0.00" )
    excel_critical_value(tree_description,wb,"Tree",numFmt="#0.00" )
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
}
##########################################################################################
# NOTES
##########################################################################################
# tree<-rpart::rpart(formula(model$call),mydata,subset=train)
# rpart.plot::prp(tree)
# rpart.plot::rpart.plot(tree)
# plot_tree(model)
# plot_tree(prune_tree)
##########################################################################################
# CLASSIFICATION OUTPUT
##########################################################################################
# tree_classification$frame
# tree_classification$where
# tree_classification$terms
# tree_classification$call
# tree_classification$x
# tree_classification$y
# tree_classification$weights
##########################################################################################
# REGRESSION OUTPUT
##########################################################################################
# tree_regression$frame
# tree_regression$where
# tree_regression$terms
# tree_regression$call
# tree_regression$x
# tree_regression$y
# tree_regression$weights
