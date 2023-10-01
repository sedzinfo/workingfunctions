##########################################################################################
# MODEL PLOT
##########################################################################################
#' @title Plot cfa model
#' @param model lavaan object
#' @param ... arguments passed to semPlot::semPaths
#' @importFrom semPlot semPaths
#' @keywords SEM
#' @export
#' @examples
#' model='LATENT1=~X1+X2+X3
#'        LATENT2=~X4+X5+X6'
#' df<-lavaan::simulateData(model=model,model.type="cfa",
#'                              return.type="data.frame",sample.nobs=100)
#' df<-generate_missing(df)
#' fit<-lavaan::cfa(model,data=df,missing="ML")
#' plot_cfa(fit)
#' model='LATENT1=~X1+X2+X3+X4+X5+X6
#'        LATENT2=~X1+X2+X3+X4+X5+X6'
plot_cfa<-function(model,...) {
  plot<-list()
  tryCatch({
    semPlot::semPaths(model,what="est",layout="circle",ask=FALSE,...)
    title("Estimates",line=3)
    plot[["circle_estimates"]]<-recordPlot()
  })
  tryCatch({
    semPlot::semPaths(model,what="std",layout="circle",ask=FALSE,...)
    title("Standardized Estimates",line=3)
    plot[["circle_standard_estimates"]]<-recordPlot()
  })
  tryCatch({
    semPlot::semPaths(model,what="eq",layout="circle",ask=FALSE,...)
    title("Parameters With Equality Constraints",line=3)
    plot[["circle_parameters_wih_equality_constraints"]]<-recordPlot()
  })
  
  tryCatch({
    semPlot::semPaths(model,what="est",layout="tree",ask=FALSE,...)
    title("Estimates",line=3)
    plot[["tree_estimates"]]<-recordPlot()
  })
  tryCatch({
    semPlot::semPaths(model,what="std",layout="tree",ask=FALSE,...)
    title("Standardized Estimates",line=3)
    plot[["tree_standard_estimates"]]<-recordPlot()
  })
  tryCatch({
    semPlot::semPaths(model,what="eq",layout="tree",ask=FALSE,...)
    title("Parameters With Equality Constraints",line=3)
    plot[["tree_parameters_wih_equality_constraints"]]<-recordPlot()
  })
  
  tryCatch({
    semPlot::semPaths(model,what="est",layout="spring",ask=FALSE,...)
    title("Estimates",line=3)
    plot[["spring_estimates"]]<-recordPlot()
  })
  tryCatch({
    semPlot::semPaths(model,what="std",layout="spring",ask=FALSE,...)
    title("Standardized Estimates",line=3)
    plot[["spring_standard_estimates"]]<-recordPlot()
  })
  tryCatch({
    semPlot::semPaths(model,what="eq",layout="spring",ask=FALSE,...)
    title("Parameters With Equality Constraints",line=3)
    plot[["spring_parameters_wih_equality_constraints"]]<-recordPlot()
  })
  return(plot)
}
##########################################################################################
# MODEL
##########################################################################################
#' @title Report
#' @param model lavaan object
#' @param file output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @importFrom stats predict
#' @importFrom lavaan inspect parameterEstimates modificationIndices
#' @importFrom stringr str_replace_all fixed
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @keywords SEM
#' @export
#' @examples
#' model='LATENT=~ITEM1+ITEM2+ITEM3+ITEM4+ITEM5'
#' df<-lavaan::simulateData(model=model,model.type="cfa",
#'                              return.type="data.frame",sample.nobs=100)
#' df<-generate_missing(df)
#' fit<-lavaan::cfa(model,data=df,missing="ML")
#' report_cfa(fit)
#' report_cfa(fit,file="cfa")
report_cfa<-function(model,file=NULL,w=10,h=10) {
  pt<-options(fit=c("GFI","AGFI","RMSEA","NFI","NNFI","CFI","RNI","IFI","SRMR","AIC","AICc","BIC","CAIC"))
  r_squared<-data.frame(r_squared=lavaan::inspect(model,"rsquare",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE))
  fit<-data.frame(fit=lavaan::inspect(model,"fit",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE))
  unstandardized_estimates<-lavaan::inspect(model,"est",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)
  standardized_estimates<-lavaan::inspect(model,"std",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)
  group<-data.frame()
  if(model@Model@ngroups>1) {
    group<-data.frame(data.frame(GROUP_COLLUMN=lavaan::inspect(model,what="group",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)),
                      data.frame(GROUPS=lavaan::inspect(model,what="group.label",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)),
                      data.frame(NGROUPS=lavaan::inspect(model,what="ngroups",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)),
                      data.frame(OBSERVATIONS=lavaan::inspect(model,what="nobs",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)),
                      data.frame(ORIGINAL_OBSERVATIONS=lavaan::inspect(model,what="norig",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)),
                      data.frame(TOTAL=lavaan::inspect(model,what="ntotal",add.labels=TRUE,add.class=TRUE,list.by.group=TRUE,drop.list.single.group=TRUE)))
  }
  parameters<-data.frame(lavaan::parameterEstimates(model,se=TRUE,zstat=TRUE,pvalue=TRUE,ci=TRUE,level=0.95,boot.ci.type="perc",standardized=TRUE,fmi=FALSE,remove.system.eq=TRUE,remove.eq=FALSE,remove.ineq=FALSE,remove.def=FALSE,rsquare=TRUE,add.attributes=TRUE))
  modification_indices<-data.frame(lavaan::modificationIndices(model,standardized=TRUE,cov.std=TRUE,information="expected",power=TRUE,delta=0.1,alpha=0.05,high.power=0.75,sort.=TRUE,minimum.value=0,free.remove=FALSE,na.remove=TRUE,op=NULL))
  sample_covariance<-data.frame(model@SampleStats@cov)
  if(typeof(lavaan::predict(model))=="list")
    predict<-data.frame(do.call(rbind.data.frame,model@Data@X),do.call(rbind.data.frame,lavaan::predict(model)))
  else
    predict<-data.frame(model@Data@X,lavaan::predict(model))
  call<-data.frame(call=stringr::str_replace_all(deparse(model@call),stringr::fixed(" "),""),stringsAsFactors=FALSE)
  result<-list(r_squared=r_squared,fit_indices=fit,parameters=parameters,modification_indices=modification_indices,sample_covariance=sample_covariance,
               unstandardized_estimates=unstandardized_estimates,standardized_estimates=standardized_estimates,group=group,predict=predict,call=call)
  
  plot<-plot_cfa(model)
  report_pdf(plotlist=plot,file=file,title="diagram",w=w,h=h,print_plot=TRUE)
  write_txt({
    output_separator("SUMMARY",output=lavaan::summary(model,standardized=TRUE,fit.measures=TRUE,rsquare=TRUE))
    output_separator("R_SQUARED",output=data.frame(result$r_squared))
    output_separator("FIT INDICES",output=data.frame(result$fit_indices))
    output_separator("PARAMETERS",output=result$parameters)
    output_separator("UNSTANDARDIZED PARAMETERS",output=result$unstandardized_estimates)
    output_separator("STANDARDIZED PARAMETERS",output=result$standardized_estimates)
    output_separator("SAMPLE COVARIANCE",output=result$sample_covariance)
    output_separator("CALL",output=result$call$call)
  },file=file)
  if(!is.null(file)){
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$r_squared,workbook=wb,sheet="R_Squared",numFmt="#0.00")
    excel_critical_value(result$fit_indices,workbook=wb,sheet="Fit_Indices",numFmt="#0.00")
    excel_critical_value(result$parameters,workbook=wb,sheet="Parameters",numFmt="#0.00")
    excel_critical_value(result$modification_indices,workbook=wb,sheet="Modification_Indices",numFmt="#0.00")
    excel_critical_value(result$group,workbook=wb,sheet="Groups",numFmt="#0.00")
    excel_matrix(result$sample_covariance,workbook=wb,sheet="Sample_Covariance",numFmt="#0.00")
    excel_matrix(result$predict,workbook=wb,sheet="Scores",numFmt="#0.00")
    excel_critical_value(result$call,workbook=wb,sheet="Call",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  return(result)
}
##########################################################################################
# SIMULATE CFA FROM FROM COEFFICIENTS
##########################################################################################
#' @title Simulate CFA from coefficients
#' @description Simulates cfa from coefficients
#'              Simulates cfa from correlations of obeserved data
#'              Returns fit indices for predefined set of sample sizes
#' @param model_sim lavaan model spesification with defined coefficients
#' @param model lavaan model spesification with free coefficients
#' @param df dataframe
#' @param minnobs start sample size
#' @param maxnobs end sample size
#' @param stepping stepping
#' @param file output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @importFrom parallel detectCores makeCluster
#' @keywords SEM
#' @export
#' @examples
#' model_sim='LATENT=~1*X1+0.5*X2+1.5*X3+1.5*X4+X5'
#' model='LATENT=~X1+X2+X3+X4+X5'
#' df<-lavaan::simulateData(model=model_sim,model.type="cfa",
#'                          return.type="data.frame",sample.nobs=1000)
#' # simulate_cfa_fit(model_sim=model_sim,model=model,
#' #                  minnobs=50,maxnobs=1000,stepping=100,file="report")
#' # simulate_cfa_fit(model=model,df=df,
#' #                  minnobs=50,maxnobs=1000,stepping=100,file="report")
simulate_cfa_fit<-function(model_sim=NULL,model=NULL,df=NULL,minnobs=50,maxnobs=1000,stepping=10,file=NULL,w=10,h=10) {
  nobs<-NULL
  cl<-parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  sequence<-seq(from=minnobs,to=maxnobs,by=stepping)
  pb<-txtProgressBar(min=0,max=length(sequence),style=3)
  progress<-function(n) setTxtProgressBar(pb,n)
  opts<-list(progress=progress)
  sim_results<-foreach(nobs=sequence,.combine=rbind,.packages=c("psycholatefunctions","lavaan"),.options.snow=opts) %dopar% {
    if(!is.null(model_sim))
      sim<-lavaan::simulateData(model=model_sim,model.type="cfa",return.type="data.frame",sample.nobs=nobs,orthogonal=TRUE)
    if(!is.null(df))
      sim<-psycholatefunctions::simulate_correlation_from_sample(df,nrows=nobs)
    fit<-lavaan::cfa(model,data=sim)
    fit_indices<-data.frame(FIT=lavaan::inspect(fit,"fit"))
    data.frame(data.frame(observations=nobs,data.frame(t(fit_indices))),row.names=NULL)
  }
  close(pb)
  parallel::stopCluster(cl)
  plot_data<-remove_nc(sim_results,remove_rows=TRUE,aggressive=FALSE,remove_cols=TRUE,remove_zero_variance=TRUE)
  combinations<-data.frame(X1=rep("observations",length(names(plot_data))),X2=names(plot_data),stringsAsFactors=FALSE)
  plots<-plot_scatterplot(df=plot_data,combinations=combinations)
  report_dataframe(sim_results,sheet="simulation",file=file)
  report_pdf(plotlist=plots,w=w,h=w,file=file)
  result<-list(sim_results,plots)
  return(result)
}
