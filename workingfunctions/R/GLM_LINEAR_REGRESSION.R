##########################################################################################
# SCATTERPLOT
##########################################################################################
#' @title Plot plot_scatterplot
#' @param df dataframe if dataframe consists of 2 collumns the second collumn is the outcome and the first collumn is the predictor
#' @param method smoothing method, "auto", "lm", "glm", "gam", "loess" or a function, 
#' e.g. MASS::rlm or mgcv::gam, stats::lm, or stats::loess
#' @param formula formula used in smoothing function for geom_smooth
#' @param base_size base font size
#' @param coord_equal if TRUE axes maintain equal scale
#' @param all_orders if TRUE the order of combination is considered
#' @param combinations dataframe if not NULL user can provide a dataframe for variable combinations for x and y axis . First column represents x and second column represents y
#' @param string_aes if TRUE string_aes function is used for names
#' @param title Plot title
#' @import ggplot2 foreach doSNOW
#' @importFrom parallel makeCluster stopCluster
#' @importFrom ggpubr as_ggplot
#' @importFrom ggExtra ggMarginal
#' @keywords regression
#' @export
#' @examples
#' result<-plot_scatterplot(df=mtcars,title="",coord_equal=TRUE,base_size=10)
#' plot_multiplot(plotlist=result[1:12],cols=4)
#' plot_scatterplot(df=mtcars[,1:2],base_size=10,coord_equal=TRUE,all_orders=FALSE)
#' plot_scatterplot(df=mtcars[,1:2],base_size=10,coord_equal=FALSE,all_orders=FALSE)
#' plot_scatterplot(df=mtcars,base_size=10,coord_equal=TRUE,all_orders=FALSE,
#'                  combinations=data.frame(x=c("mpg","mpg","mpg"),
#'                                          y=c("cyl","hp","mpg")))
#' plot_scatterplot(df=mtcars,base_size=10,coord_equal=TRUE,all_orders=TRUE,
#'                  combinations=data.frame(x=c("mpg"),y=c("cyl")))
#' x<-rnorm(1000)
#' y<-x+rnorm(x,sd=.1)
#' plot_scatterplot(df=data.frame(x,y),title="Random Simulation",coord_equal=TRUE)
#' df<-data.frame(matrix(-.999,ncol=2,nrow=2))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-generate_correlation_matrix(correlation_martix,nrows=1000)
#' plot_scatterplot(df,title="Simulation of -.999 Correlation",coord_equal=TRUE,base_size=20)
plot_scatterplot<-function(df,method=lm,formula=y~x,base_size=10,coord_equal=FALSE,all_orders=FALSE,title="",combinations=NULL,string_aes=TRUE) {
  output_plot<-function(i) {
    tempdata<-data.frame(df[,combinations[i,1]],df[,combinations[i,2]])
    names(tempdata)<-c(combinations[i,1],combinations[i,2])
    tempdata<-tempdata[complete.cases(tempdata),]
    if(nrow(tempdata)>=2) {
      if(string_aes)
        names(tempdata)<-string_aes(names(tempdata))
      pearsonr<-stats::cor(tempdata[,1],tempdata[,2],use="pairwise")
      model<-lm(tempdata[,2]~tempdata[,1])
      model_coef<-coef(model)
      slope<-model_coef[[2]]
      degrees<-rad2deg(atan(slope))
      if(degrees>180)
        degrees<-360-degrees
      if(as.character(toString(formula))=="~, y, x") {
        note<-paste0("Pairwise n = ",nrow(tempdata[complete.cases(tempdata),]),
                     "\nPearson r = ",round(pearsonr,4),
                     "\nExplained Variance = ",round(pearsonr^2,4)*100,"%",
                     "\ny = ",round(model_coef[[2]],4),
                     "x + ",round(model_coef[[1]],4),
                     "\nAngle = ",round(degrees,2))
      }
      else {
        note<-paste0("Pairwise n = ",nrow(tempdata[complete.cases(tempdata),]))
      }
      scatter<-ggplot(tempdata,aes(x=tempdata[,1],y=tempdata[,2]))+
        # geom_point(alpha=.1)+
        geom_count(alpha=.1)+
        geom_smooth(method=method,se=TRUE,na.rm=TRUE,formula=formula)+
        labs(x=names(tempdata)[1],y=names(tempdata)[2],title=title,caption=note)+
        geom_rug(size=0.1,alpha=.1)+
        theme_bw(base_size=base_size)+
        theme(legend.position="left",
              legend.justification=c(0,1),
              legend.margin=margin(t=-10,r=0,b=0,l=0,unit="pt"))
      if(min(tempdata[,1],na.rm=TRUE)<=0&max(tempdata[,1],na.rm=TRUE)>=0)
        scatter<-scatter+geom_vline(xintercept=0,alpha=.5,color="gray25")
      if(min(tempdata[,1],na.rm=TRUE)<=0&max(tempdata[,1],na.rm=TRUE)>=0)
        scatter<-scatter+geom_hline(yintercept=0,alpha=.5,color="gray25")
      if(coord_equal) {
        maximum_xy<-max(c(tempdata[,1],tempdata[,2]))
        minimum_xy<-min(c(tempdata[,1],tempdata[,2]))
        scatter<-scatter+coord_equal()
        scatter<-scatter+scale_x_continuous(limits=c(minimum_xy,maximum_xy))+scale_y_continuous(limits=c(minimum_xy,maximum_xy))
      }
      ggpubr::as_ggplot(ggExtra::ggMarginal(scatter,type="histogram",fill="gray25",color="gray50"))
    }
  }
  
  if(is.null(combinations)) {
    combinations<-comparison_combinations(df,all_orders=all_orders)
    names(combinations)<-c("x","y")
  }
  combinations<-change_data_type(combinations,type="character")
  combinations<-combinations[!combinations[,1]==combinations[,2],]
  row.names(combinations)<-paste0(combinations[,1],"_",combinations[,2])
  
  n_rows<-nrow(combinations)
  n_cores<-parallel::detectCores()
  if(n_cores*4<n_rows) {
    print(paste("parralel process with",n_cores,"workers for",n_rows,"tasks"))
    parralel=TRUE
  } else {
    parralel=FALSE
  }
  
  pb<-txtProgressBar(min=0,max=nrow(combinations),style=3)
  if(parralel) {
    cl<-parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    progress<-function(n) setTxtProgressBar(pb,n)
    opts<-list(progress=progress)
    scatterplots<-foreach(i=1:nrow(combinations),.final=function(x) setNames(x,row.names(combinations)),.packages=c("workingfunctions"),.options.snow=opts) %dopar% {
      pdf(NULL)
      output_plot(i) 
    }
    close(pb)
    parallel::stopCluster(cl)
    gc(full=TRUE)
  } else {
    scatterplots<-list()
    for(i in 1:nrow(combinations)) {
      setTxtProgressBar(pb,i)
      scatterplots[[row.names(combinations)[i]]]<-output_plot(i)
    }
    close(pb)
  }
  return(scatterplots)
}
##########################################################################################
# REGRESSION
##########################################################################################
#' @title Regression
#' @param model object ml
#' @param base_size base font size
#' @param title plot title
#' @param w width of pdf file. Relevant only when file string is not empty
#' @param h height of pdf file. Relevant only when file string is not empty
#' @param plot_diagnostics if TRUE it will output linear model diagnostics plots
#' @param file output filename
#' @note
#' (1) Problematic values for standardized residuals > +-1.96 \cr
#' **Standardized residuals** are residuals divided by an estimated standard deviation and they can be interpreted as z scores in that: \cr
#' - 95.00% of z-scores lie between -1.96 and +1.96 \cr
#' - 99.00% of z-scores lie between -2.58 and +2.58 \cr 
#' - 99.99% of z-scores lie between -3.29 and +3.29 \cr
#' (2) **Studentized residuals** indicate the the ability of the model to predict that case. They follow a t distribution \cr
#' (3) **DFFits** indicate the difference between the adjusted predicted value and the original predicted value. 
#' Adjusted predicted value for a case refers to the predicted value of that case, when that case is excluded from model fit. \cr
#' (4) **Cook's distance** indicates leverage. Problematic values for cook's distance > 1 Cook and Weisberg (1982). \cr
#' (5) **Hat values** indicate leverage. Problematic values for Hat values 2 or 3 times the average (k+1/n) \cr
#' The average leverage value is defined as (k+1)/n, k=number of predictors, n=number of participants. 
#' Leverage values lie between 0 (no influence) and 1 (complete influence over prediction) \cr
#' - Hoaglin and Welsch (1978) recommends investigating cases with values greater than twice the average (2(k+1)/n) \cr
#' - Stevens (2002) recommends investigating cases with values greater than three times the average (3(k+1)/n) \cr
#' **T-tests** test the hypothesis that b's are different from 0 \cr
#' **Multiple R^2**: Variance Explained \cr
#' **Adjusted R^2**: Indicates how much variance in Y would be accounted for if the model is derived from the population from which the sample was taken. 
#' Idealy, R^2 = Adjusted R^2 \cr
#' **F-Statistic** tests the null hypothesis is that the overall model has no effect \cr
#' **Covariance ratios** critical values CVR>1+[3(k+1)/n] CRV<1-[3(k+1)/n]. In general we should obtain small values or we may have to remove cases\cr
#' **ASSUMPTIONS** \cr
#' (1) variable types: All predictors must be quantitative or categorical (with two levels), 
#' and the outcome variable must be quantitative (interval data), 
#' continuous and unbounded (no constraints on the variability of the outcome)
#' (2) Non-zero variance \cr
#' (3) No perfect multicollinearity \cr
#' (4) Predictors are uncorrelated with -external variables- \cr
#' (5) Homoscedasticity: At each level of the predictor variable(s), the variance of the residual terms should be constant. 
#' Residuals at each level of the predictor(s) should have similar variance (homoscedasticity) \cr
#' (6) Independent errors: For any two observations the residual terms should be uncorrelated (or independent)\cr
#' This eventuality is sometimes described as a lack of autocorrelation.
#' This assumption can be tested with the Durbin-Watson test,which tests for serial correlations between errors.
#' Specifically, it tests whether adjacent residuals are correlated
#' The size of the Durbin-Watson statistic depends upon the number of predictors in the model and the number of observations
#' As a very conservative rule of thumb, values less than 1 or greater than 3 are definitely cause for concern; 
#' however,values closer to 2 may still be problematic depending on your sample and model
#' R also provides a p-value of the autocorrelation. 
#' Be very careful with the Durbin-Watson test, though, as it depends on the order of the data: if you reorder your data, you-ll get a different value \cr
#' (7) Normally distributed errors: It is assumed that the residuals in the model are random, normally distributed variables with a mean of 0 \cr
#' (8) Independence: It is assumed that all of the values of the outcome variable are independent 
#' (in other words, each value of the outcome variable comes from a separate entity) \cr
#' (9) Linearity: The mean values of the outcome variable for each increment of the predictor(s) lie along a straight line \cr
#' @import ggfortify
#' @importFrom stats confint deviance
#' @importFrom QuantPsyc lm.beta
#' @importFrom car vif
#' @keywords regression
#' @export
#' @examples
#' form<-formula(mpg~qsec)
#' regressionmodel<-lm(form,data=mtcars)
#' multipleregressionmodel<-lm(mpg~qsec*hp*wt*drat,data=mtcars)
#' res<-report_regression(model=regressionmodel,plot_diagnostics=TRUE)
#' res<-report_regression(model=multipleregressionmodel)
#' res<-report_regression(model=regressionmodel,file="regression")
#' res<-report_regression(model=multipleregressionmodel,
#'                        file="regression",
#'                        plot_diagnostics=TRUE)
report_regression<-function(model,base_size=10,title="",file=NULL,w=10,h=10,plot_diagnostics=TRUE) {
  anova<-NULL
  instruction_coefficients<-c("Unstandardized coefficients (b's) indicate the change in the outcome resulting from a unit change in the predictor",
                              "Standardized coefficients (for more than one predictors), indicate the change in outcome as a result of a unit change by a standard deviation of the predictor",
                              "t-test checks if coefficients are significantly different from 0. Coefficients of 0 indicate no predictor effects",
                              "Significance value for t-test")
  instruction_anova<-c("ANOVA tests for differences between the baseline model (model with no coefficient) and the predictive model (model with coefficient). A significant F shows that the predictor(s) significantly changes model predictability",
                       "Significance value for ANOVA",
                       "Null hypothesis: no variance explained by the predictor")
  instruction_durbin<-c("Test the assumption of independent errors.\nTest values may vary between 0 and 4.\nValues above 3 and bellow 1 are problematic.\nValues of 2 are ideal indicating uncorrelated residuals.
                        \nA value greater than 2 indicates a negative correlation between adjacent residuals.\nA value less than 2 indicates a positive correlation between adjacent residuals.",
                        "Autocorrelation",
                        "Durbin-Watson Statistic",
                        "Significance value for Durbin-Watson Statistic")
  instruction_vif<-c("Variance Inflation Factor VIF indicates whether a predictor has strong linear relationship with other predictors. If the largest VIF is greater than 10 it is problematic Myers(1990).",
                     "Tolerance=1/VIF Tolerance bellow .1 indicates serious problem. Tolerance bellow .2 indicates potential problem Field & Myers(2012).",
                     "Mean VIF if average VIF is greater than 1 the regression may be biased.")
  if(plot_diagnostics) {
    plot<-autoplot(model,which=1:6,ncol=2)+
      labs(title=title,
           caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
      theme_bw(base_size=base_size)+
      theme(axis.text.x=element_text(angle=20,hjust=1))
    report_pdf(plot,file=file,title=title,w=w,h=h,print_plot=TRUE)
  }
  
  outlier_test<-car::outlierTest(model)
  dw<-car::durbinWatsonTest(model)
  vif<-data.frame()
  try({vif_result<-car::vif(model,type="predictor")
  vif<-data.frame(vif=vif_result,
                  tolerance=1/vif_result,
                  mean_vif=mean(vif_result),
                  check.names=FALSE)},
  silent=TRUE)
  diagnostics<-data.frame(simple_residuals=stats::resid(model),
                          standard_residuals=stats::rstandard(model),
                          student_residuals=stats::rstudent(model),
                          fitted=stats::fitted(model),
                          cooks_distance=stats::cooks.distance(model),
                          dffits=stats::dffits(model),
                          hatvalues=stats::hatvalues(model),
                          covariance_ratio=data.frame(covariance_ratio=stats::covratio(model)),
                          dfbeta=stats::dfbeta(model),
                          effects=model$effects,
                          check.names=FALSE)
  model_coefficients<-Reduce(function(x, y) merge(x,y,all=TRUE,sort=FALSE,by="row.names"),
                             list(data.frame(standardized=QuantPsyc::lm.beta(model),check.names=FALSE),
                                  data.frame(summary(model)[[4]],stats::confint(model),check.names=FALSE)))
  result<-list(r=data.frame(r_squared=summary(model)$r.squared,adjusted_r_squared=summary(model)$adj.r.squared),
               coeficients=model_coefficients,
               anova=anova(model),
               deviance=data.frame(deviance=stats::deviance(model)),
               variance_covariance=data.frame(stats::vcov(model),check.names=FALSE),
               outlier_test=data.frame(rstudent=outlier_test$rstudent,
                                       p=outlier_test$p,
                                       bonf.p=outlier_test$bonf.p,
                                       signif=outlier_test$signif,
                                       cutoff=outlier_test$cutoff),
               durbin_watson=data.frame(dw$r,
                                        dw$dw,
                                        dw$p,
                                        dw$alternative),
               vif=vif,
               call=data.frame(call=paste0("lm(",deparse(eval(model$call$formula)),",data=",model$call$data,")")),
               diagnostics=diagnostics)
  
  write_txt({
    output_separator("Summary",output=summary(model))
    output_separator("Coefficients",output=result$coeficients,instruction=instruction_coefficients)
    output_separator("ANOVA",output=result$anova,instruction=instruction_anova)
    output_separator("Deviance",output=result$deviance)
    output_separator("Outliers",output=result$outlier_test)
    output_separator("Durbin Watson",output=result$durbin_watson,instruction=instruction_durbin)
    if (nrow(vif)>0)
      output_separator("MULTICOLINEARITY",output=result$vif,instruction=instruction_vif)
    output_separator("CALL",output=result$call)
  },file=file)
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$r,workbook=wb,sheet="r")
    excel_critical_value(result$coeficients,workbook=wb,sheet="Coefficients",title=NULL,
                         comment=list(Estimate=instruction_coefficients[1],
                                      standardized=instruction_coefficients[2],
                                      "t value"=instruction_coefficients[3],
                                      "Pr(>|t|)"=instruction_coefficients[4],
                                      "Std. Error"="Standard Error of Estimates",
                                      "2.5 %"="Confidence Intervals",
                                      "97.5 %"="Confidence Intervals"),
                         critical=list("Pr(>|t|)"="<0.05"),
                         numFmt="#0.00")
    excel_critical_value(result$anova,workbook=wb,sheet="ANOVA",
                         title=instruction_anova[3],
                         comment=list("F value"=instruction_anova[1],
                                      "Pr(>F)"=instruction_anova[2]),
                         critical=list("Pr(>F)"="<0.05"),
                         numFmt="#0.00")
    # excel_critical_value(deviance,workbook=wb,sheet="Deviance",title="fit index",numFmt="#0.00")
    excel_critical_value(result$variance_covariance,workbook=wb,sheet="Variance Covariance",numFmt="#0.00")
    excel_critical_value(result$outlier_test,workbook=wb,sheet="Outliers",numFmt="#0.00")
    excel_critical_value(result$durbin_watson,workbook=wb,sheet="Durbin Watson",title=toString(instruction_durbin[1]),
                         comment=list(r=instruction_durbin[2],
                                      dw=instruction_durbin[3],
                                      p=instruction_durbin[4]),
                         numFmt="#0.00")
    if (nrow(vif)>0)
      excel_critical_value(result$vif,workbook=wb,sheet="VIF",title=toString(instruction_vif),
                           comment=list(vif=instruction_vif[1],
                                        tolerance=instruction_vif[2],
                                        mean_vif=instruction_vif[3]),
                           numFmt="#0.00")
    excel_critical_value(result$diagnostics,workbook=wb,sheet="Diagnostics",
                         comment=list(standard_residuals="Problematic values for standardized residuals > +-1.96",
                                      student_residuals="Studentized residuals indicate the the ability of the model to predict that case. They follow a t distribution",
                                      cooks_distance="Cook's distance indicates leverage. Problematic values for cook's distance > 1 Cook and Weisberg (1982).",
                                      hatvalues="Hat values indicate leverage. Problematic values are 2 or 3 times the average (k+1/n),
                                      Hoaglin and Welsch (1978) recommend investigating cases with values greater than twice the average (2(k+1)/n), 
                                      Stevens (2002) recommend investigating cases with values greater than three times the average (3(k+1)/n)",
                                      dffits="DFFits indicate the difference between the adjusted predicted value and the original predicted value.
                                      Adjusted predicted value for a case refers to the predicted value of that case, when that case is excluded from model fit."),
                         critical=list(standard_residuals=c("<-1.96",">1.96"),cooks_distance=">1"),
                         numFmt="#0.00")
    excel_critical_value(result$call,workbook=wb,sheet="Call",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  return(result)
}
##########################################################################################
# POLYNOMIAL REGRESSION
##########################################################################################
# predictorlog<-cordata$predictor
# outcomelog<-cordata$outcome^2
# curvilinear=lm(outcomelog~predictorlog+I(1/predictorlog))
# curvilinear3=lm(outcomelog~predictorlog+I(predictorlog^2)+I(predictorlog^3))
# curvilinear2=lm(outcomelog~predictorlog+I(predictorlog^2))
# curvilinear1=lm(outcomelog~predictorlog)
