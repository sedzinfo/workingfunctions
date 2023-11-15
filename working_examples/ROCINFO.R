# [ROCInfo] : 
# Pass in the data that already consists the predicted score and actual outcome.  to obtain the ROC curve 
# @data    : your data.table or data.frame type data that consists the column of the predicted score and actual outcome
# @predict : predicted score's column name
# @actual  : actual results' column name
# @cost.fp : associated cost for a false positive 
# @cost.fn : associated cost for a false negative 
# return   : a list containing  
#	1. plot         : a side by side roc and cost plot,title showing optimal cutoff value title showing optimal cutoff,total cost,and area under the curve (auc)
# 2. cutoff       : optimal cutoff value according to the specified fp/fn cost 
#	3. totalcost    : total cost according to the specified fp/fn cost
#	4. auc          : area under the curve
#	5. sensitivity  : TP / (TP+FN)
#	6. specificity  : TN / (FP+TN)
plot_roc_info<-function(data,predict,actual,cost.fp,cost.fn) {
  # calculate the values using the ROCR library true positive,false postive 
  pred<-prediction(data[[predict]],data[[actual]])
  perf<-performance(pred,"tpr","fpr")
  roc_dt<-data.frame(fpr=perf@x.values[[1]],tpr=perf@y.values[[1]])
  # cost with the specified false positive and false negative cost false postive rate*number of negative instances*false positive cost+false negative rate*number of positive instances*false negative cost
  cost<-perf@x.values[[1]]*cost.fp*sum(data[[actual]]==0)+(1-perf@y.values[[1]])*cost.fn*sum(data[[actual]]==1)
  cost_dt<-data.frame(cutoff=pred@cutoffs[[1]],cost=cost)
  # optimal cutoff value,and the corresponding true positive and false positive rate
  best_index<-which.min(cost)
  best_cost<-cost_dt[best_index,"cost"]
  best_tpr<-roc_dt[best_index,"tpr"]
  best_fpr<-roc_dt[best_index,"fpr"]
  best_cutoff<-pred@cutoffs[[1]][best_index]
  # area under the curve
  auc<-performance(pred,"auc")@y.values[[1]]
  # normalize the cost to assign colors to 1
  normalize<-function(v) (v - min(v)) / diff(range(v))
  # create color from a palette to assign to the 100 generated threshold between 0 ~ 1 then normalize each cost and assign colors to it,the higher the blacker don't times it by 100,there will be 0 in the vector
  col_ramp<-colorRampPalette(c("green","orange","red","black"))(100)   
  col_by_cost<-col_ramp[ceiling(normalize(cost)*99)+1]
  roc_plot<-ggplot(roc_dt,aes(fpr,tpr))+
    geom_line(color=rgb(0,0,1,alpha=0.1))+
    geom_point(color=col_by_cost,size=1,alpha=0.1)+
    geom_segment(aes(x=0,y=0,xend=1,yend=1),alpha=0.5)+
    labs(title="ROC",x="False Postive Rate",y="True Positive Rate")+
    geom_hline(yintercept=best_tpr,alpha=0.8,linetype="dashed",color="steelblue4")+
    geom_vline(xintercept=best_fpr,alpha=0.8,linetype="dashed",color="steelblue4")+
    theme_bw()
  cost_plot<-ggplot(cost_dt,aes(cutoff,cost))+
    geom_line(color="blue",alpha=0.5)+
    geom_point(color=col_by_cost,size=4,alpha=0.5)+
    ggtitle("Cost")+
    scale_y_continuous(labels=comma)+
    geom_vline(xintercept=best_cutoff,alpha=0.8,linetype="dashed",color="steelblue4")	
  # the main title for the two arranged plot
  sub_title<-sprintf("Cutoff at %.2f - Total Cost=%d,AUC=%.3f",best_cutoff,best_cost,auc)
  # arranged into a side by side plot
  plot<-arrangeGrob(roc_plot,cost_plot,ncol=2,top=textGrob(sub_title,gp=gpar(fontsize=16,fontface="bold")))
  return(list(plot=plot,cutoff=best_cutoff,totalcost=best_cost,auc=auc,sensitivity=best_tpr,specificity=1 - best_fpr))
}
# [ConfusionMatrixInfo] : 
# Obtain the confusion matrix plot and data.table for a given
# dataset that already consists the predicted score and actual outcome.
# @data    : your data.table or data.frame type data that consists the column
#            of the predicted score and actual outcome 
# @predict : predicted score's column name
# @actual  : actual results' column name
# @cutoff  : cutoff value for the prediction score 
# return   : 1. data : a data.table consisting of three column  the first two stores the original value of the prediction and actual outcome from
#			 		   the passed in data frame,the third indicates the type,which is after choosing the  cutoff value,will this row be a true/false positive/ negative 
#            2. plot : plot that visualizes the data.table 
compute_cm_info<-function(data,predict,actual,cutoff) {	
  # extract the column ;
  # relevel making 1 appears on the more commonly seen position in a two by two confusion matrix	
  predict<-data[[predict]]
  actual<-relevel(as.factor(data[[actual]]),"1")
  result<-data.table(actual=actual,predict=predict)
  # caculating each pred falls into which category for the confusion matrix
  result[,type := ifelse(predict >= cutoff & actual == 1,"TP",ifelse(predict >= cutoff & actual == 0,"FP",ifelse(predict <  cutoff & actual == 1,"FN","TN"))) %>% as.factor() ]
  # jittering : can spread the points along the x axis 
  plot<-ggplot(result,aes(actual,predict,color=type))+
    geom_violin(fill="white",color=NA)+
    geom_jitter(shape=1)+
    geom_hline(yintercept=cutoff,color="blue",alpha=0.6)+
    scale_y_continuous(limits=c(0,1))+
    scale_color_discrete(breaks=c("TP","FN","FP","TN"))+# ordering of the legend
    guides(col=guide_legend(nrow=2))+# adjust the legend to have two rows
    ggtitle(sprintf("Confusion Matrix with Cutoff at %.2f",cutoff))
  return(list(data=result,plot=plot))
}



