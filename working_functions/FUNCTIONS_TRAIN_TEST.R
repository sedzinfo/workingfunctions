##########################################################################################
# PLOT ROC
##########################################################################################
#' @title Plot Receiver Operating Characteristic (ROC) curve 
#' @param observed vector of observed outcomes
#' @param predicted vector of predicted outcome probability
#' @param base_size base font size
#' @param title plot title
#' @importFrom pROC roc
#' @importFrom pROC ggroc
#' @keywords functions
#' @export
#' @examples
#' observed<-round(abs(rnorm(100,m=0,sd=.5)))
#' predicted<-abs(rnorm(100,m=0,sd=.5))
#' plot_roc(observed=observed,predicted=predicted)
#' df1<-data.frame(matrix(.999,ncol=2,nrow=2))
#' correlation_martix<-as.matrix(df1)
#' diag(correlation_martix)<-1
#' df1<-generate_correlation_matrix(correlation_martix,nrows=1000)
#' df1$X1<-ifelse(abs(df1$X1)<1,0,1)
#' df1$X2<-abs(df1$X2)
#' df1$X2<-(df1$X2-min(df1$X2))/(max(df1$X2)-min(df1$X2))
#' plot_roc(observed=round(abs(df1$X1),0),predicted=abs(df1$X2))
plot_roc<-function(observed,predicted,base_size=10,title="") {
  plotlist<-list()
  rco1<-pROC::roc(observed,predicted,ci=TRUE,levels=as.factor(rev(sort(unique(observed)))),quiet=TRUE)
  rco2<-pROC::roc(observed,predicted,ci=TRUE,levels=as.factor(sort(unique(observed))),quiet=TRUE)
  rp<-function(rco) {
    plot<-pROC::ggroc(rco,alpha=0.5)+
      geom_abline(intercept=1,slope=1)+
      labs(title=paste("ROC",title),
           caption=paste0("Observations:",length(observed),
                          "\nAUC:",round(as.numeric(as.character(rco$auc))*100,2),"%",
                          "\nControl Level:",as.character(rco$levels),
                          "\nDirection:",rco$direction))+
      theme_bw(base_size=base_size)+
      coord_fixed()
    return(plot)
  }
  plotlist[[toString(rco1$levels)]]<-rp(rco1)
  plotlist[[toString(rco2$levels)]]<-rp(rco2)
  return(plotlist)
}
##########################################################################################
# PLOT CONFUSION
##########################################################################################
#' @title Plot confusion matrix
#' @param observed vector of observed outcomes
#' @param predicted vector of predicted outcomes
#' @param base_size base font size
#' @param title plot title
#' @importFrom reshape2 melt
#' @keywords functions
#' @export
#' @examples
#' plot_confusion(observed=c(1,2,3,1,2,3),predicted=c(1,2,3,1,2,3))
#' observed<-c(rep("male",10),rep("female",10),"male","male")
#' predicted<-c(rep("male",10),rep("female",10),"female","female")
#' plot_confusion(observed=observed,predicted=predicted)
plot_confusion<-function(observed,predicted,base_size=10,title="") {
  value<-NULL
  cmatrixp<-confusion_matrix_percent(observed=observed,predicted=predicted)
  cmatrix<-confusion(observed=observed,predicted=predicted)
  pa<-proportion_accurate(observed=observed,predicted=predicted)
  observations<-sum(cmatrix)
  cmatrix<-reshape2::melt(cmatrix)
  cmatrix$observed<-factor(cmatrix$observed,levels=rev(sort(unique(cmatrix$observed))))
  cmatrix$predicted<-factor(cmatrix$predicted,levels=sort(unique(cmatrix$predicted)))
  plot_confusion<-ggplot(cmatrix,aes(x=observed,y=predicted,fill=value))+
    geom_tile(color="white")+
    geom_text(aes(x=observed,y=predicted,label=value),color="black",size=base_size/2)+
    theme_bw(base_size=base_size)+
    theme(axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          panel.background=element_blank(),
          legend.position="none")+
    scale_fill_continuous(low="#fafaff")+
    scale_x_discrete(position="top")+
    labs(title=paste("Confusion Matrix",title),
         caption=paste0("Observations:",observations,
                        "\nAccuracy:",round(pa$cm_diagonal,2),
                        "\nAccuracy with off diagonals:",round(pa$cm_off_diagonal,2),
                        "\nKappa unweighted:",round(pa$kappa_unweighted,2),
                        "\nKappa linear:",round(pa$kappa_linear,2),
                        "\nKappa squared:",round(pa$kappa_squared,2)))+
    coord_fixed()
  return(plot_confusion)
}
##########################################################################################
# PLOT SEPARABILITY
##########################################################################################
#' @title Plot separability
#' @param observed vector of observed outcomes
#' @param predicted vector of predicted outcome probability
#' @param base_size base font size
#' @param title plot title
#' @keywords functions
#' @export
#' @examples
#' df1<-data.frame(matrix(.999,ncol=2,nrow=2))
#' correlation_martix<-as.matrix(df1)
#' diag(correlation_martix)<-1
#' df1<-generate_correlation_matrix(correlation_martix,nrows=1000)
#' df1$X1<-ifelse(abs(df1$X1)<1,0,1)
#' df1$X2<-abs(df1$X2)
#' df1$X2<-(df1$X2-min(df1$X2))/(max(df1$X2)-min(df1$X2))
#' plot_separability(observed=round(abs(df1$X1),0),predicted=abs(df1$X2))
plot_separability<-function(observed,predicted,base_size=10,title="") {
  df<-data.frame(observed=as.factor(observed),predicted=predicted)
  plot<-ggplot(df,aes(x=predicted,color=factor(observed)))+
    geom_density(size=1)+
    labs(title=paste("Predicted proportion vs Observed category",title),
         color="observed",
         caption=paste0("Observations:",nrow(df)))+
    theme_bw(base_size=base_size)
  return(plot)
}
##########################################################################################
# PLOT CONFUSION PERFORMANCE
##########################################################################################
#' @title Plot performance of confusion matrix for different cut off points
#' @inheritParams plot_roc
#' @param step stepping for tested cut values
#' @importFrom plyr rbind.fill
#' @importFrom reshape2 melt
#' @keywords functions
#' @export
#' @examples
#' df<-data.frame(matrix(.999,ncol=2,nrow=2))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-generate_correlation_matrix(correlation_martix,nrows=1000)
#' df$X1<-ifelse(abs(df$X1)<1,0,1)
#' df$X2<-abs(df$X2)
#' df$X2<-(df$X2-min(df$X2))/(max(df$X2)-min(df$X2))
#' result_confusion_performance(observed=round(abs(df$X1),0),
#'                              predicted=abs(df$X2),
#'                              step=.01)
#' result_confusion_performance(observed=c(1,2,3,1,2,3),predicted=abs(rnorm(6,0,sd=.1)))
result_confusion_performance<-function(observed,predicted,step=.1,base_size=10,title="") {
  cut_point<-value<-variable<-NULL
  df_cut_performance<-data.frame()
  min_predicted<-min(predicted,na.rm=TRUE)
  max_predicted<-max(predicted,na.rm=TRUE)
  for(i in seq(min_predicted,max_predicted,by=step)) {
    confusion(observed=observed,predicted=ifelse(predicted>i,1,0))
    cmatrix<-confusion_matrix_percent(observed=observed,predicted=ifelse(predicted>i,1,0))
    collumn_proportion<-data.frame(Collumn_Observed=t(as.numeric(cmatrix[1:(nrow(cmatrix)-2),length(cmatrix)])))
    row_proportion<-data.frame(Row_Predicted=t(as.numeric(cmatrix[nrow(cmatrix),1:(length(cmatrix)-2)])))
    overall<-as.numeric(cmatrix[nrow(cmatrix),length(cmatrix)])
    result<-data.frame(cut_point=i,Overall=overall,collumn_proportion,row_proportion)
    df_cut_performance<-plyr::rbind.fill(df_cut_performance,result)
  }
  cp<-reshape2::melt(df_cut_performance,id.vars="cut_point")
  df_cut_performance$Mean_proportion<-rowMeans(df_cut_performance[,3:length(df_cut_performance)],na.rm=TRUE)
  mcp<-df_cut_performance[df_cut_performance$Mean_proportion %in% max(df_cut_performance$Mean_proportion),"cut_point"]
  cmatrix<-confusion(observed=observed,predicted=ifelse(predicted>mean(mcp,na.rm=TRUE),1,0))
  cmatrixp<-confusion_matrix_percent(observed=observed,predicted=ifelse(predicted>mean(mcp,na.rm=TRUE),1,0))
  plot_performance<-ggplot(cp,aes(x=cut_point,y=value,color=variable,linetype=variable))+
    geom_line(size=1)+
    geom_vline(xintercept=mcp,size=1)+
    labs(title=paste("Confusion Matrix Performance",title),
         x="Cut Point",
         y="Proportion Correct",
         caption=paste0("Observations:",nrow(df_cut_performance),"\nCut point:",round(mcp,4)))+
    theme(legend.title=element_blank())+
    scale_color_discrete(name="")+
    scale_linetype(name="")+
    theme_bw(base_size=base_size)
  result<-list(plot_performance=plot_performance,
               cut_performance=df_cut_performance,
               cut=mcp,
               confusion_matrix=cmatrixp)
  return(result)
}
##########################################################################################
# TRAIN TEST DATAFRAME WITH RESPONSES
##########################################################################################
#' @title K-Fold train test sampling
#' @description splits a dataframe in train and test dataframes for model evaluation. Prepared data include data objects for xgboost
#' @param df dataframe
#' @param model_formula model formula
#' @param k k-folds
#' @importFrom xgboost xgb.DMatrix
#' @importFrom stats get_all_vars
#' @keywords functions
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+education+spontaneous+induced)
#' result<-k_fold(infert,k=10,model_formula=infert_formula)
#' model_formula<-as.formula(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb)
#' result<-k_fold(mtcars,k=2,model_formula=model_formula)
k_fold<-function(df,model_formula,k=10) {
  variable_names<-names(stats::get_all_vars(model_formula,data=df))
  index<-sample(cut(1:nrow(df),breaks=k,labels=FALSE))
  rows<-1:nrow(df)
  fold<-xgboost<-list()
  df<-stats::get_all_vars(model_formula,df)
  predictors<-all.vars(model_formula[[3]])
  outcome<-all.vars(model_formula[[2]])
  for (i in 1:k) {
    k_index<-paste0("f",i)
    fold[["index"]][[k_index]]<-iteration_index<-which(index==i)
    fold[["train"]][[k_index]]<-train<-df[rows[-iteration_index],]
    fold[["test"]][[k_index]]<-test<-df[rows[iteration_index],]
    fold[["x_test"]][[k_index]]<-test[,predictors]
    fold[["y_test"]][[k_index]]<-test[,outcome]
    xgbtrain<-xgboost::xgb.DMatrix(data=data.matrix(train[,predictors]),label=train[,outcome])
    xgbtest<-xgboost::xgb.DMatrix(data=data.matrix(test[,predictors]),label=test[,outcome])
    xgboost[[k_index]]<-list(train=xgbtrain,test=xgbtest,
                             watchlist=list(train=xgbtrain,test=xgbtest),
                             ytrain=fold$train[[k_index]][,outcome],
                             ytest=fold$test[[k_index]][,outcome])
    cat("Fold Cases:",i,
        "Train:",nrow(train),
        "Test:",nrow(test),
        "Total:",sum(nrow(train),nrow(test)),
        "Unique Train:",length(unique(row.names(train))),
        "Unique Test",length(unique(row.names(test))),"\n")
  }
  result<-list(f=fold,
               index=index,
               model_formula=model_formula,
               variables=variable_names,
               predictors=predictors,
               outcome=outcome,
               xgb=xgboost)
  return(result)
}
##########################################################################################
# TRAIN TEST DATAFRAME WITH RESPONSES
##########################################################################################
#' @title train test sampling
#' @description splits a dataframe in train and test dataframes for model evaluation. Prepared data include data objects for xgboost
#' @param df dataframe
#' @param model_formula model formula
#' @param k k-folds
#' @importFrom xgboost xgb.DMatrix
#' @importFrom stats get_all_vars
#' @keywords functions
#' @export
#' @examples
#' infert_formula<-as.formula(factor(case)~age+parity+education+spontaneous+induced)
#' result<-k_sample(df=infert,k=10,model_formula=infert_formula)
#' model_formula<-as.formula(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb)
#' result<-k_sample(df=mtcars,k=10,model_formula=model_formula)
k_sample<-function(df,model_formula,k=1) {
  sv<-function(vector)
    split(vector,ceiling(seq_along(vector)/(length(vector)/2)))
  variable_names<-names(stats::get_all_vars(model_formula,data=df))
  if(k>1) {
    index<-sample(cut(1:nrow(df),breaks=k,labels=FALSE))
  } else {
    index<-rep(1,nrow(df))
  }
  rows<-1:nrow(df)
  fold<-xgb<-list()
  df<-stats::get_all_vars(model_formula,df)
  predictors<-all.vars(model_formula[[3]])
  outcome<-all.vars(model_formula[[2]])
  for (i in 1:k) {
    k_index<-paste0("fold",i)
    iteration_index<-sample(which(index==i))
    train_index<-as.numeric(sv(iteration_index)[[1]])
    test_validation_index<-as.numeric(sv(iteration_index)[[2]])
    test_index<-as.numeric(sv(test_validation_index)[[1]])
    validation_index<-as.numeric(sv(test_validation_index)[[2]])
    fold[["index"]][["train"]][[k_index]]<-rows[train_index]
    fold[["index"]][["test"]][[k_index]]<-rows[test_index]
    fold[["index"]][["validation"]][[k_index]]<-rows[validation_index]
    fold[["train"]][[k_index]]<-train<-df[rows[train_index],]
    fold[["test"]][[k_index]]<-test<-df[rows[test_index],]
    fold[["validation"]][[k_index]]<-validation<-df[rows[validation_index],]
    fold[["x_test"]][[k_index]]<-test[,predictors]
    fold[["y_test"]][[k_index]]<-test[,outcome]
    fold[["x_validation"]][[k_index]]<-validation[,predictors]
    fold[["y_validation"]][[k_index]]<-validation[,outcome]
    xgbtrain<-xgboost::xgb.DMatrix(data=data.matrix(train[,predictors]),label=train[,outcome])
    xgbtest<-xgboost::xgb.DMatrix(data=data.matrix(test[,predictors]),label=test[,outcome])
    xgbvalidation<-xgboost::xgb.DMatrix(data=data.matrix(validation[,predictors]),label=validation[,outcome])
    xgb[[k_index]]<-list(train=xgbtrain,
                         test=xgbtest,
                         validation=xgbvalidation,
                         watchlist=list(train=xgbtrain,test=xgbtest),
                         ytrain=fold$train[[k_index]][,outcome],
                         ytest=fold$test[[k_index]][,outcome],
                         yvalidation=fold$test[[k_index]][,outcome])
    cat("Fold Cases:",i,
        "Train:",nrow(train),
        "Test:",nrow(test),
        "Validation:",nrow(test),
        "Total:",sum(nrow(train),nrow(test),nrow(validation)),
        "Unique Train:",length(unique(row.names(train))),
        "Unique Test:",length(unique(row.names(test))),
        "Unique Validation:",length(unique(row.names(validation))),
        "\n")
  }
  result<-list(f=fold,
               index=index,
               model_formula=model_formula,
               variables=variable_names,
               predictors=predictors,
               outcome=outcome,
               xgb=xgb)
  
  return(result)
}
##########################################################################################
# SCALE
##########################################################################################
#' @title Scale and dummy code
#' @description Scales numeric variables between 0 and 1 and creates dummy coding for character and vector variables
#' @param df dataframe
#' @param categories Numeric Number of unique values a vector must have to perform dummy coding
#' @importFrom psych dummy.code
#' @keywords functions
#' @export
#' @examples
#' recode_scale_dummy(infert)
recode_scale_dummy<-function(df,categories=10) {
  index_numeric<-names(df[,sapply(df,is.numeric)])
  if(sum(sapply(df,function(x) is.character(x)|is.factor(x)))>1){
    index<-apply(df[,sapply(df,function(x) is.character(x)|is.factor(x))],2,function(x) length(table(x)))<categories
    index<-names(index)[index]
    dummy<-do.call(data.frame,sapply(df[,index],psych::dummy.code,na.rm=TRUE))
  }
  if(sum(sapply(df,function(x) is.character(x)|is.factor(x)))==1){
    index<-sapply(df,function(x) length(unique(x))<categories&&is.character(x)|is.factor(x))
    index<-names(index)[index]
    dummy<-psych::dummy.code(df[,index],na.rm=TRUE)
  }
  if(sum(sapply(df,function(x) is.character(x)|is.factor(x)))==0)
    dummy<-NULL
  maxs<-apply(df[,index_numeric],2,max,na.rm=TRUE)
  mins<-apply(df[,index_numeric],2,min,na.rm=TRUE)
  scale<-as.data.frame(scale(df[,index_numeric],center=mins,scale=maxs-mins))
  if(!is.null(dummy))
    result<-data.frame(dummy,scale)
  else
    result<-scale
  row.names(result)<-row.names(df)
  return(result)
}
##########################################################################################
# CONFUSION FOR CLASSIFICATION
##########################################################################################
#' @title Create a confusion matrix from observed and expected vectors
#' @param observed vector of observed variables
#' @param predicted vector of predicted variables
#' @importFrom gtools mixedsort
#' @keywords functions
#' @export
#' @examples
#' confusion(observed=c(1,2,3,4,5,10),predicted=c(1,2,3,4,5,11))
#' confusion(observed=c(1,2,2,2,2),predicted=c(1,1,2,2,2))
confusion<-function(observed,predicted) {
  levels<-gtools::mixedsort(as.character(unique(c(observed,predicted))),decreasing=FALSE)
  result<-table(predicted=factor(as.character(predicted),levels=levels),
                observed=factor(as.character(observed),levels=levels))
  return(result)
}
##########################################################################################
# PROPORTION ACCURATE
##########################################################################################
#' @title Proportion overall accuracy of a confusion matrix
#' @inheritParams confusion
#' @importFrom irr kappa2
#' @keywords functions
#' @export
#' @examples
#' proportion_accurate(observed=c(1,2,3,4,5,10),predicted=c(1,2,3,4,5,11))
proportion_accurate<-function(observed,predicted) {
  cmatrix<-confusion(observed=observed,predicted=predicted)
  train_test<-data.frame(observed=observed,predicted=predicted)
  cm_diagonal<-sum(diag(cmatrix))/sum(matrix(cmatrix,ncol=1))
  index<-off_diagonal_index(nrow(cmatrix))
  data<-c()
  for (i in 1:nrow(cmatrix)){
    data<-c(data,cmatrix[index[i,1],index[i,2]])
    if(index[i,3]>0 && index[i,3]<=nrow(cmatrix))
      data<-c(data,cmatrix[index[i,1],index[i,3]])
    if(index[i,4]>0 && index[i,4]<=nrow(cmatrix))
      data<-c(data,cmatrix[index[i,1],index[i,4]])
  }
  cm_off_diagonal<-sum(data)/sum(matrix(cmatrix,ncol=1))
  kappa_squared<-irr::kappa2(train_test,"squared",sort.levels=TRUE)
  kappa_linear<-irr::kappa2(train_test,"equal",sort.levels=TRUE)
  kappa_unweighted<-irr::kappa2(train_test,"unweighted",sort.levels=TRUE)
  result<-data.frame(cm_diagonal=cm_diagonal,
                     cm_off_diagonal=cm_off_diagonal,
                     kappa_unweighted=kappa_unweighted$value,
                     kappa_linear=kappa_linear$value,
                     kappa_squared=kappa_squared$value)
  return(result)
}
##########################################################################################
# CONFUSION MATRIX PERCENT
##########################################################################################
#' @title Confusion matrix with row and column percent
#' @inheritParams confusion
#' @keywords functions
#' @note
#' Total measures - Accuracy: (TP+TN)/total\cr
#' Total measures - Prevalence: (TP+FN)/total\cr
#' Total measures - Proportion Incorrectly Classified: (FN+FP)/total\cr
#' Horizontal measures - True Positive Rate - Sensitivity: TP/(TP+FN)\cr
#' Horizontal measures - True Negative Rate - Specificity: TN/(FP+TN)\cr
#' Horizontal measures - False Negative Rate - Miss Rate: FN/(TP+FN)\cr
#' Horizontal measures - False Positive Rate - Fall-out: FP/(FP+TN)\cr
#' Vertical measures - Positive Predictive value - Precision: TP/(TP+FP)\cr
#' Vertical measures - Negative Predictive value: TN/(FN+TN)\cr
#' Vertical measures - False Omission Rate: FN/(FN+TN)\cr
#' Vertical measures - False Discovery Rate: FP/(TP+FP)\cr
#' @export
#' @examples
#' observed<-factor(round(rnorm(10000,m=10,sd=1)))
#' predicted<-factor(round(rnorm(10000,m=10,sd=1)))
#' confusion_matrix_percent(observed,predicted)
confusion_matrix_percent<-function(observed,predicted) {
  cmatrix<-confusion(observed=observed,predicted=predicted)
  overall_accuracy<-sum(as.numeric(diag(cmatrix)))/sum(as.numeric(matrix(cmatrix,ncol=1)))
  cmatrix<-cbind(cmatrix,sum=(apply(cmatrix[,1:ncol(cmatrix)],1,sum,na.rm=TRUE)))
  cmatrix<-rbind(cmatrix,sum=(apply(cmatrix[1:nrow(cmatrix),],2,sum,na.rm=TRUE)))
  dimensions<-dim(cmatrix)
  recall<-precision<-c()
  for (i in 1:dimensions[1]) {
    precision<-c(precision,cmatrix[i,i]/cmatrix[i,ncol(cmatrix)])
    recall<-c(recall,(cmatrix[i,i]/cmatrix[nrow(cmatrix),i]))
  }
  cmatrix<-rbind(cbind(cmatrix,precision),c(recall,overall_accuracy))
  row.names(cmatrix)[nrow(cmatrix)]<-"p"
  colnames(cmatrix)[dim(cmatrix)[[1]]]<-"p"
  cmatrix[is.nan(cmatrix)]<-0
  result<-data.frame(cmatrix,check.names=FALSE)
  result<-format(round(result,2),nsmall=2)
  return(result)
}
