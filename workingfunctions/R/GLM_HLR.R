##########################################################################################
# REPORT HLR
##########################################################################################
#' @title Report HLR
#' @param df dataframe
#' @param corlist Numeric outcome index
#' @param factorlist Numeric predictor index
#' @param predictor Character predictor name
#' @param random_effect Character random effect name
#' @param file Character file
#' @param sheet Character sheet
#' @importFrom nlme gls
#' @importFrom nlme lme
#' @importFrom plyr rbind.fill
#' @export
#' @examples
#' report_hlr(df=infert,corlist=8,factorlist=1,
#'            predictor="case",random_effect="case")
report_hlr<-function(df,corlist,factorlist,predictor,random_effect,file=NULL,sheet="report") {
  fbaseline<-fpredictor<-frandom_intercept<-frandom_slope<-anova<-NULL
  anova_comparisons<-data.frame()
  pb<-txtProgressBar(min=0,max=length(corlist),style=3)
  counter=0
  for (i in corlist) {
    temp<-df[,c(i,factorlist,which(predictor==(names(df))))]
    temp<-temp[complete.cases(temp),]
    counter=counter+1
    setTxtProgressBar(pb,counter)
    dv<-names(df)[i]
    fbaseline<-formula(paste(dv,"~1"))
    fpredictor<-formula(paste(dv,"~",predictor))
    frandom_intercept<-formula(paste("~1|",random_effect))
    frandom_slope<-formula(paste("~",predictor,"|",random_effect))
    
    base<-nlme::gls(formula(fbaseline),method="ML",data=temp)
    random_intercept<-nlme::lme(fixed=formula(fbaseline),random=frandom_intercept,data=temp,method="ML")
    random_intercept_predictor<-nlme::lme(fixed=fpredictor,random=frandom_intercept,data=temp,method="ML")
    fixed<-c(deparse(fbaseline),deparse(fbaseline),deparse(fpredictor))
    random<-c(NA,deparse(frandom_intercept),deparse(frandom_intercept))
    anova_intercept_predictor<-aor<-data.frame(fixed=fixed,
                                               random=random,
                                               anova(base,random_intercept,random_intercept_predictor))
    res<-try(nlme::lme(fixed=fpredictor,random=frandom_slope,data=temp,method="ML"),silent = TRUE)
    if(!class(res)=="try-error") {
      random_intercept_slope<-nlme::lme(fixed=fpredictor,random=frandom_slope,data=temp,method="ML")
      anova_random_intercept_slope<-aor<-data.frame(fixed=c(fixed,deparse(fpredictor)),
                                                    random=c(random,deparse(frandom_slope)),
                                                    anova(base,random_intercept,random_intercept_predictor,random_intercept_slope))
    }
    anova_comparisons<-plyr::rbind.fill(anova_comparisons,data.frame(dv=dv,model=row.names(aor),aor))
  }
  report_dataframe(anova_comparisons,sheet=sheet,file=file,type="critical_value",critical=list(p.value="<0.05"))
  return(anova_comparisons)
}
