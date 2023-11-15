##########################################################################################
# 
##########################################################################################
library(caret)
library(tidyverse)
library(readr)
library(parallel)
library(doParallel)
library(gridExtra)
library(plyr)
library(pso)
library(GA)
library(DEoptim)
library(GGally)
library(xgboost)
library(broom)
library(knitr)
library(kableExtra)
library(tictoc)
download.file(url="http://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls",destfile="Concrete_Data.xls",method="curl")
concrete_data<-readxl::read_xls(path="Concrete_Data.xls",sheet=1)[1:100,]
colnames(concrete_data)<-c("Cement","Slag","Ash","Water","Superplasticizer","Coarse_Aggregate","Fine_Aggregate","Age","Strength")
# Recalculate composition as proportions ranging from 0 to 1
concrete_data[,1:7]<-t(apply(X=concrete_data[,1:7],MARGIN=1,FUN=function(x) {x/sum(x)}))
# Average the values of compressive strength of replicate experiments
concrete_data<-ddply(.data=concrete_data,.variables=.(Cement,Slag,Ash,Water,Superplasticizer,`Coarse_Aggregate`,`Fine_Aggregate`,Age),.fun=function(x) c(Strength=mean(x$Strength)))
n_cores<-detectCores()
cluster<-makeCluster(detectCores())
registerDoParallel(cluster)
##########################################################################################
# 
##########################################################################################
set.seed(1)
training_index<-createDataPartition(y=concrete_data$Strength,p=0.80)[[1]]
training_set<-concrete_data[training_index,]
test_set<-concrete_data[-training_index,]
CV_folds<-2
CV_repeats<-2
minimum_resampling<-2
##########################################################################################
# 
##########################################################################################
# Create training and test set using stratified partioning
train_control<-caret::trainControl(method="repeatedcv",number=CV_folds,repeats=CV_repeats,verboseIter=TRUE,returnData=FALSE)
adapt_control_grid<-caret::trainControl(method="adaptive_cv",number=CV_folds,repeats=CV_repeats,adaptive=list(min=minimum_resampling,alpha=0.05,method="gls",complete=TRUE),search="grid",verboseIter=TRUE,returnData=FALSE) 
adapt_control_random<-caret::trainControl(method="adaptive_cv",number=CV_folds,repeats=CV_repeats,adaptive=list(min=minimum_resampling,alpha=0.05,method="gls",complete=TRUE),search="random",verboseIter=TRUE,returnData=FALSE) 
##########################################################################################
# GRID SEARCH
##########################################################################################
XGBoost_Linear_grid<-expand.grid(nrounds=c(50,100,250,500),eta=c(0.01,0.1,1),lambda=c(0.1,0.5,1),alpha= c(0.1,0.5,1)) # L2 Regularization (Ridge Regression) # L1 Regularization (Lasso Regression)
GS_XGBoost_Linear_model<-caret::train(Strength~.,data=training_set,method="xgbLinear",trControl=adapt_control_grid,verbose=FALSE,silent=1,tuneGrid=XGBoost_Linear_grid)
saveRDS(object=GS_XGBoost_Linear_model,file="GS_XGBoost_Linear_model.rds")
##########################################################################################
# 
##########################################################################################
RMSE_plot_XGBoost_Linear<-GS_XGBoost_Linear_model$results %>%dplyr::select(nrounds,lambda,alpha,eta,RMSE) %>% gather(key=Parameter,value=Value,-nrounds,-lambda,-alpha,-eta) %>% filter(Parameter == "RMSE") %>% 
  ggplot(mapping=aes(x=nrounds,y=Value,col=factor(alpha),shape=factor(lambda)))+geom_line(size=1)+geom_point(size=2)+facet_wrap(~eta)+theme_bw()+labs(title="Average Cross-validated RMSE of XGBoost Linear Model",subtitle="Horizontal tiles representative of step size shrinkage",x="# Boosting Iterations",y="RMSE",col="alpha",shape="lambda")+scale_x_continuous(breaks=unique(GS_XGBoost_Linear_model$results$nrounds))+scale_color_brewer(type="qual",palette="Set1")
MAE_plot_XGBoost_Linear<-GS_XGBoost_Linear_model$results %>% dplyr::select(nrounds,lambda,alpha,eta,MAE) %>% gather(key=Parameter,value=Value,-nrounds,-lambda,-alpha,-eta) %>% filter(Parameter == "MAE") %>% 
  ggplot(mapping=aes(x=nrounds,y=Value,col=factor(alpha),shape=factor(lambda)))+geom_line(size=1)+geom_point(size=2)+facet_wrap(~eta)+theme_bw()+labs(title="Average Cross-validated MAE of XGBoost Linear Model",subtitle="Horizontal tiles representative of step size shrinkage",x="# Boosting Iterations",y="MAE",col="alpha",shape="lambda")+scale_x_continuous(breaks=unique(GS_XGBoost_Linear_model$results$nrounds))+scale_color_brewer(type="qual",palette="Set1")
Rsquared_plot_XGBoost_Linear<-GS_XGBoost_Linear_model$results %>% dplyr::select(nrounds,lambda,alpha,eta,Rsquared) %>% gather(key=Parameter,value=Value,-nrounds,-lambda,-alpha,-eta) %>% filter(Parameter == "Rsquared") %>% 
  ggplot(mapping=aes(x=nrounds,y=Value,col=factor(alpha),shape=factor(lambda)))+geom_line(size=1)+geom_point(size=2)+facet_wrap(~eta)+labs(title="Average Cross-validated R-squared of XGBoost Linear Model",subtitle="Horizontal tiles representative of step size shrinkage",x="# Boosting Iterations",y="R-squared",col="alpha",shape="lambda")+theme_bw()+scale_x_continuous(breaks=unique(GS_XGBoost_Linear_model$results$nrounds))+scale_color_brewer(type="qual",palette="Set1")
grid.arrange(grobs=list(RMSE_plot_XGBoost_Linear,MAE_plot_XGBoost_Linear,Rsquared_plot_XGBoost_Linear),ncol=1,nrow=3)
##########################################################################################
# RANDOM
##########################################################################################
n_combinations<-nrow(XGBoost_Linear_grid)
set.seed(1)
RS_XGBoost_Linear_model<-caret::train(Strength~.,data=training_set,method="xgbLinear",trControl=adapt_control_random,verbose=FALSE,silent=1,tuneLength=n_combinations)
saveRDS(object=RS_XGBoost_Linear_model,file="RS_XGBoost_Linear_model.rds")
##########################################################################################
# GENETIC
##########################################################################################
# Set parameter settings for search algorithm
max_iter<-10
pop_size<-10
# Create custom function for assessing solutions
eval_function_XGBoost_Linear<-function(x1,x2,x3,x4,data,train_settings) {
  suppressWarnings(XGBoost_Linear_model<-caret::train(Strength ~.,data=data,method="xgbLinear",trControl=train_settings,verbose=FALSE,silent=1,tuneGrid=expand.grid(nrounds=round(x1),eta=10^x2,alpha=10^x3,lambda=10^x4)))
  return(-XGBoost_Linear_model$results$RMSE)
}
nrounds_min_max<-c(10,10^3)
eta_min_max<-c(-5,3)
alpha_min_max<-c(-3,1)
lambda_min_max<-c(-3,1)
set.seed(1)
GA_model_XGBoost_Linear<-GA::ga(type="real-valued",
                                fitness=function(x) eval_function_XGBoost_Linear(x[1],x[2],x[3],x[4],data=training_set,train_settings=train_control),
                                lower=c(nrounds_min_max[1],eta_min_max[1],alpha_min_max[1],lambda_min_max[1]),
                                upper=c(nrounds_min_max[2],eta_min_max[2],alpha_min_max[2],lambda_min_max[2]),
                                popSize=pop_size,maxiter=max_iter,pmutation=0.5,elitism=0.3,parallel=n_cores,optim=F,keepBest=T,seed=1)
GA::plot.ga(GA_model_XGBoost_Linear,main="Genetic Algorithm: RMSE values at each iteration")
GA_XGBoost_Linear_grid<-expand.grid(nrounds=round(GA_model_XGBoost_Linear@solution[1]),eta=10^GA_model_XGBoost_Linear@solution[2],alpha=10^GA_model_XGBoost_Linear@solution[3],lambda=10^GA_model_XGBoost_Linear@solution[4])
GA_XGBoost_Linear_model<-caret::train(Strength ~.,data=training_set,method="xgbLinear",trControl=train_control,verbose=F,metric="RMSE",maximize=FALSE,silent=1,tuneGrid=GA_XGBoost_Linear_grid)
saveRDS(object=GA_XGBoost_Linear_model,file=paste0("GA_XGBoost_Linear_model_",stringr::str_remove_all(Sys.time(),pattern=":"),".rds"))
##########################################################################################
# DIFFERENTIAL EVOLUTION
##########################################################################################
eval_function_XGBoost_Linear<-function(x,data,train_settings) {
  x1<-x[1]; x2<-x[2]; x3<-x[3]; x4<-x[4]
  XGBoost_Linear_model<-caret::train(Strength~.,data=data,method="xgbLinear",trControl=train_settings,verbose=FALSE,silent=1,tuneGrid=expand.grid(nrounds=round(x1),eta=10^x2,alpha=10^x3,lambda=10^x4))
  return(XGBoost_Linear_model$results$RMSE)
}
set.seed(1)
DE_model_XGBoost_Linear<-DEoptim::DEoptim(fn=eval_function_XGBoost_Linear,
                                          lower=c(nrounds_min_max[1],eta_min_max[1],alpha_min_max[1],lambda_min_max[1]),
                                          upper=c(nrounds_min_max[2],eta_min_max[2],alpha_min_max[2],lambda_min_max[2]),
                                          control=DEoptim.control(NP=pop_size,
                                                                  itermax=max_iter,
                                                                  CR=0.5,
                                                                  storepopfreq=1,
                                                                  parallelType=1),data=training_set,train_settings=train_control)
DE_solutions<-DE_model_XGBoost_Linear$optim$bestmem
ggplot(mapping=aes(x=1:length(DE_model_XGBoost_Linear$member$bestvalit),y=DE_model_XGBoost_Linear$member$bestvalit))+
  geom_line(col="grey50")+ geom_point(col="grey50")+theme_bw()+theme(aspect.ratio=0.9)+labs(x="Iteration",y="RMSE",title="Best RMSE value at each iteration",subtitle="Results using Differential Evolution")+
  scale_x_continuous(breaks=1:DE_model_XGBoost_Linear$optim$iter,minor_breaks=NULL)
DE_XGBoost_Linear_grid<-expand.grid(nrounds=round(DE_solutions[1]),eta=10^DE_solutions[2],alpha=10^DE_solutions[3],lambda=10^DE_solutions[4])
set.seed(1)
DE_XGBoost_Linear_model<-caret::train(Strength~.,data=training_set,method="xgbLinear",trControl=train_control,verbose=F,metric="RMSE",maximize=FALSE,silent=1,tuneGrid=DE_XGBoost_Linear_grid)
saveRDS(object=DE_XGBoost_Linear_model,file=paste0("DE_XGBoost_Linear_model_PSO_",stringr::str_remove_all(Sys.time(),pattern=":"),".rds"))
saveRDS(object=DE_XGBoost_Linear_model$finalModel,file=paste0("DE_XGBoost_Linear_model_PSO_",class(DE_XGBoost_Linear_model$finalModel)[1],"_",stringr::str_remove_all(Sys.time(),pattern=":"),".rds"))
##########################################################################################
# PARTICLE SWARM OPTIMIZATION
##########################################################################################
eval_function_XGBoost_Linear<-function(x,data,train_settings) {
  x1<-x[1]; x2<-x[2]; x3<-x[3]; x4<-x[4]
  suppressWarnings(XGBoost_Linear_model<-caret::train(Strength ~.,data=data,method="xgbLinear",trControl=train_settings,verbose=FALSE,silent=1,tuneGrid=expand.grid(nrounds=round(x1),eta=10^x2,alpha=10^x3,lambda=10^x4)))
  return(XGBoost_Linear_model$results$RMSE)
}
set.seed(1)
PSO_model_XGBoost_Linear<-pso::psoptim(par=rep(NA,4),
                                       fn=eval_function_XGBoost_Linear,
                                       lower=c(nrounds_min_max[1],eta_min_max[1],alpha_min_max[1],lambda_min_max[1]),
                                       upper=c(nrounds_min_max[2],eta_min_max[2],alpha_min_max[2],lambda_min_max[2]),
                                       control=list(trace=1,#  produce tracing information on the progress of the optimization
                                                    maxit=max_iter,# maximum number of iterations
                                                    REPORT=1,#  frequency for reports
                                                    trace.stats=T,
                                                    s=pop_size,# Swarm Size,
                                                    maxit.stagnate=round(0.75*max_iter),# maximum number of iterations without improvement
                                                    vectorize=T,
                                                    type="SPSO2011" # method used
                                       ),data=training_set,train_settings=train_control)
PSO_summary<-data.frame(Iteration=PSO_model_XGBoost_Linear$stats$it,Mean=PSO_model_XGBoost_Linear$stats$f %>% sapply(FUN=mean),Median=PSO_model_XGBoost_Linear$stats$f %>% sapply(FUN=median),Best=PSO_model_XGBoost_Linear$stats$error %>% sapply(FUN=min))
PSO_summary %>% gather(key="Parameter",value="Value",- Iteration) %>% 
  ggplot(mapping=aes(x=Iteration,y=Value,col=Parameter))+geom_line()+geom_point()+theme_bw()+theme(aspect.ratio=0.9)+scale_x_continuous(breaks=PSO_model_XGBoost_Linear$stats$it,minor_breaks=NULL)+
  labs(x="Iteration",y="RMSE",title="RMSE values at each iteration",subtitle="Results using Particle Swarm Optimization")+scale_color_brewer(type="qual",palette="Set1")
PSO_XGBoost_Linear_grid<-expand.grid(nrounds=round(PSO_model_XGBoost_Linear$par[1]), # number of boosting iterations
                                     eta=PSO_model_XGBoost_Linear$par[2],# learning rate,low value means model is more robust to overfitting
                                     alpha=PSO_model_XGBoost_Linear$par[3],# L2 Regularization (Ridge Regression)
                                     lambda=PSO_model_XGBoost_Linear$par[4] # L1 Regularization (Lasso Regression)
)
set.seed(1)
PSO_XGBoost_Linear_model<-caret::train(Strength ~.,data=training_set,method="xgbLinear",trControl=train_control,verbose=F,metric="RMSE",maximize=FALSE,silent=1,tuneGrid=PSO_XGBoost_Linear_grid)
saveRDS(object=PSO_XGBoost_Linear_model,file=paste0("PSO_XGBoost_Linear_model_",stringr::str_remove_all(Sys.time(),pattern=":"),".rds"))
##########################################################################################
# 
##########################################################################################
Summary_Table_Training<-bind_rows(GS_XGBoost_Linear_model$results %>% arrange(RMSE) %>% .[1,] %>% select(RMSE,MAE,Rsquared,nrounds,eta,lambda,alpha) %>% round(5),
                                  RS_XGBoost_Linear_model$results %>% arrange(RMSE) %>% .[1,] %>% select(RMSE,MAE,Rsquared,nrounds,eta,lambda,alpha) %>% round(5),
                                  GA_XGBoost_Linear_model$results %>% arrange(RMSE) %>% .[1,] %>% select(RMSE,MAE,Rsquared,nrounds,eta,lambda,alpha) %>% round(5),
                                  DE_XGBoost_Linear_model$results %>% arrange(RMSE) %>% .[1,] %>% select(RMSE,MAE,Rsquared,nrounds,eta,lambda,alpha) %>% round(5),
                                  PSO_XGBoost_Linear_model$results %>% arrange(RMSE) %>% .[1,] %>% select(RMSE,MAE,Rsquared,nrounds,eta,lambda,alpha) %>% round(5))
Summary_Table_Training<-Summary_Table_Training %>% 
  add_column(Method=c("Grid Search","Random Search","Genetic Algorithm","Differential Evolution","Particle Swarm Optimization"),.before=1) %>% 
  add_column(`Processing Time`=round(c(GS_T1-GS_T0,RS_T1-RS_T0,GA_T1-GA_T0,DE_T1-GS_T0,PSO_T1-PSO_T0),0))
Summary_Table_Training %>% 
  kable(align="c",caption="Training Set Statistics and Hyperparameter Values of XGBoost Models.") %>% 
  kableExtra::kable_styling(bootstrap_options=c("striped","hover","condensed","responsive"),full_width=T,position="center") %>%
  kableExtra::footnote(general=paste0("Note:\nSummary statistics obtained using ",CV_folds,"-fold cross validation repeated ",CV_repeats," times.","\nGrid and Random Search  performed with adaptive resampling."),general_title="\n ")
# Custom functions to plot observed,predicted and residual values for each method
library(ggplot2,quietly=T,verbose=F)
predicted_observed_plot<-function(predicted_val,observed_val,residual_val,model_name="",R_squared,...) {
  plot<-ggplot(mapping=aes(x=predicted_val,y=observed_val,col=abs(residual_val)))+geom_point(alpha=0.9,size=2)+geom_abline(intercept=0,slope=1)+theme_bw()+
    labs(title=paste0(model_name,"\nPredicted vs Observed: Test Set"),subtitle=paste0("R-squared: ",R_squared),x="Predicted",y="Observed",col="Absolute Deviation")+
    theme(aspect.ratio=0.9,panel.grid.minor.x=element_blank(),legend.title=element_text(size=10,face="bold"),legend.text=element_text(size=9),plot.title=element_text(size=12,face="bold"),axis.title=element_text(size=10,face="bold"),axis.text.x=element_text(angle=0),legend.position="none")+
    coord_equal()+ scale_color_viridis_c(direction=-1)
  return(plot)
}
residuals_plot<-function(predicted_val,residual_val,model_name="",MAE,RMSE,...) {
  plot<-ggplot(mapping=aes(x=predicted_val,y=residual_val,col=abs(residual_val)))+geom_point(alpha=0.9,size=2)+geom_abline(intercept=0,slope=0)+theme_bw()+
    labs(title=paste0(model_name,"\nResiduals: Test Set"),subtitle=paste0("RMSE: ",RMSE,",MAE: ",round(MAE,3)),x="Predicted",y="Residual",col="Absolute Deviation")+
    theme(aspect.ratio=0.9,panel.grid.minor.x=element_blank(),legend.title=element_text(size=10,face="bold"),legend.text=element_text(size=9),plot.title=element_text(size=12,face="bold"),axis.title=element_text(size=10,face="bold"),axis.text.x=element_text(angle=0),legend.position="none")+
    coord_equal()+ scale_color_viridis_c(direction=-1)
  return(plot)
}
### Grid Search (GS)
test_set$XGBoost_Linear_GS<-predict(GS_XGBoost_Linear_model,test_set) # Make predictions on test set
test_set$XGBoost_Linear_GS_residual<-test_set$Strength - test_set$XGBoost_Linear_GS # Calculate Residuals on test set
R_squared<-round(cor(test_set$XGBoost_Linear_GS,test_set$Strength),4) # Calculate test set R-squared,RMSE,MAE
RMSE<-signif(RMSE(pred=test_set$XGBoost_Linear_GS,obs=test_set$Strength,na.rm=T),6)
MAE<-signif(MAE(pred=test_set$XGBoost_Linear_GS,obs=test_set$Strength),6)
GS_Test_Set_Statistics<-c(RMSE,MAE,R_squared)
# Plot predicted vs observed values and residuals
XGBoost_Linear_GS_pred_obs<-predicted_observed_plot(predicted_val=test_set$XGBoost_Linear_GS,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_GS_residual,R_squared=R_squared,model_name="Grid Search") 
XGBoost_Linear_GS_residuals<-residuals_plot(predicted_val=test_set$XGBoost_Linear_GS,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_GS_residual,MAE=MAE,RMSE=RMSE,model_name="Grid Search")
### Random Search
test_set$XGBoost_Linear_RS<-predict(RS_XGBoost_Linear_model,test_set) # Make predictions on test set
test_set$XGBoost_Linear_RS_residual<-test_set$Strength - test_set$XGBoost_Linear_RS
R_squared<-round(cor(test_set$XGBoost_Linear_RS,test_set$Strength),4)
RMSE<-signif(RMSE(pred=test_set$XGBoost_Linear_RS,obs=test_set$Strength,na.rm=T),6)
MAE<-signif(MAE(pred=test_set$XGBoost_Linear_RS,obs=test_set$Strength),6)
RS_Test_Set_Statistics<-c(RMSE,MAE,R_squared)
XGBoost_Linear_RS_pred_obs<-predicted_observed_plot(predicted_val=test_set$XGBoost_Linear_RS,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_RS_residual,R_squared=R_squared,model_name="Random Seach")
XGBoost_Linear_RS_residuals<-residuals_plot(predicted_val=test_set$XGBoost_Linear_RS,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_RS_residual,MAE=MAE,RMSE=RMSE,model_name="Random Seach")
### Genetic Algorithm (GA)
test_set$XGBoost_Linear_GA<-predict(GA_XGBoost_Linear_model,test_set)
test_set$XGBoost_Linear_GA_residual<-test_set$Strength - test_set$XGBoost_Linear_GA
R_squared<-round(cor(test_set$XGBoost_Linear_GA,test_set$Strength),4)
RMSE<-signif(RMSE(pred=test_set$XGBoost_Linear_GA,obs=test_set$Strength,na.rm=T),6)
MAE<-signif(MAE(pred=test_set$XGBoost_Linear_GA,obs=test_set$Strength),6)
GA_Test_Set_Statistics<-c(RMSE,MAE,R_squared)
XGBoost_Linear_GA_pred_obs<-predicted_observed_plot(predicted_val=test_set$XGBoost_Linear_GA,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_GA_residual,R_squared=R_squared,model_name="Genetic Algorithm")
XGBoost_Linear_GA_residuals<-residuals_plot(predicted_val=test_set$XGBoost_Linear_GA,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_GA_residual,MAE=MAE,RMSE=RMSE,model_name="Genetic Algorithm")
### Differential evolution (DE)
test_set$XGBoost_Linear_DE<-predict(DE_XGBoost_Linear_model,test_set)
test_set$XGBoost_Linear_DE_residual<-test_set$Strength - test_set$XGBoost_Linear_DE
R_squared<-round(cor(test_set$XGBoost_Linear_DE,test_set$Strength),4)
RMSE<-signif(RMSE(pred=test_set$XGBoost_Linear_DE,obs=test_set$Strength,na.rm=T),6)
MAE<-signif(MAE(pred=test_set$XGBoost_Linear_DE,obs=test_set$Strength),6)
DE_Test_Set_Statistics<-c(RMSE,MAE,R_squared)
XGBoost_Linear_DE_pred_obs<-predicted_observed_plot(predicted_val=test_set$XGBoost_Linear_DE,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_DE_residual,R_squared=R_squared,model_name="Differential Evolution")
XGBoost_Linear_DE_residuals<-residuals_plot(predicted_val=test_set$XGBoost_Linear_DE,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_DE_residual,MAE=MAE,RMSE=RMSE,model_name="Differential Evolution")
### Particle Swarm Optimization (PSO)
test_set$XGBoost_Linear_PSO<-predict(PSO_XGBoost_Linear_model,test_set)
test_set$XGBoost_Linear_PSO_residual<-test_set$Strength - test_set$XGBoost_Linear_PSO
R_squared<-round(cor(test_set$XGBoost_Linear_PSO,test_set$Strength),4)
RMSE<-signif(RMSE(pred=test_set$XGBoost_Linear_PSO,obs=test_set$Strength,na.rm=T),6)
MAE<-signif(MAE(pred=test_set$XGBoost_Linear_PSO,obs=test_set$Strength),6)
PSO_Test_Set_Statistics<-c(RMSE,MAE,R_squared)
XGBoost_Linear_PSO_pred_obs<-predicted_observed_plot(predicted_val=test_set$XGBoost_Linear_PSO,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_PSO_residual,R_squared=R_squared,model_name="Particle Swarm Optimisation")
XGBoost_Linear_PSO_residuals<-residuals_plot(predicted_val=test_set$XGBoost_Linear_PSO,observed_val=test_set$Strength,residual_val=test_set$XGBoost_Linear_PSO_residual,MAE=MAE,RMSE=RMSE,model_name="Particle Swarm Optimisation")
Summary_Table_Test<-rbind(GS_Test_Set_Statistics,RS_Test_Set_Statistics,GA_Test_Set_Statistics,DE_Test_Set_Statistics,PSO_Test_Set_Statistics,deparse.level=0) %>% data.frame()
colnames(Summary_Table_Test)<-c("RMSE","MAE","R-squared")
Summary_Table_Test<-Summary_Table_Test %>% add_column(Method=c("Grid Search","Random Search","Genetic Algorithm","Differential Evolution","Particle Swarm Optimization"),.before=1)
Summary_Table_Test %>% 
  kable(align="c",caption="Test Set Statistics of XGBoost Models.") %>% 
  kableExtra::kable_styling(bootstrap_options=c("striped","hover","condensed","responsive"),full_width=T,position="center") %>%
  kableExtra::footnote(general=paste0(""),general_title="\n ")
g<-gridExtra::grid.arrange(XGBoost_Linear_GS_pred_obs,XGBoost_Linear_GS_residuals,XGBoost_Linear_RS_pred_obs,XGBoost_Linear_RS_residuals,XGBoost_Linear_GA_pred_obs,XGBoost_Linear_GA_residuals,XGBoost_Linear_DE_pred_obs,XGBoost_Linear_DE_residuals,XGBoost_Linear_PSO_pred_obs,XGBoost_Linear_PSO_residuals,ncol=2)
