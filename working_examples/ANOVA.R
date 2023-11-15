##########################################################################################
# EXAMPLES
##########################################################################################
data<-read.table(header=TRUE,text='subject sex age before after
                                       1   F   old    9.5   7.1
                                       2   M   old   10.3  11.0
                                       3   M   old    7.5   5.8
                                       4   F   old   12.4   8.8
                                       5   M   old   10.2   8.6
                                       6   M   old   11.0   8.0
                                       7   M young    9.1   3.0
                                       8   F young    7.9   5.2
                                       9   F   old    6.6   3.4
                                      10   M young    7.7   4.0
                                      11   M young    9.4   5.3
                                      12   M   old   11.6  11.3
                                      13   M young    9.9   4.6
                                      14   F young    8.6   6.4
                                      15   F young   14.3  13.5
                                      16   F   old    9.2   4.7
                                      17   M young    9.8   5.1
                                      18   F   old    9.9   7.3
                                      19   F young   13.0   9.5
                                      20   M young   10.2   5.4
                                      21   M young    9.0   3.7
                                      22   F young    7.9   6.2
                                      23   M   old   10.1  10.0
                                      24   M young    9.0   1.7
                                      25   M young    8.6   2.9
                                      26   M young    9.4   3.2
                                      27   M young    9.7   4.7
                                      28   M young    9.3   4.9
                                      29   F young   10.7   9.8
                                      30   M   old    9.3   9.4')
data$subject<-factor(data$subject)
aov1<-aov(before~sex,data=data)
aov2<-aov(after~sex*age,data=data)
aov3<-aov(after~sex+age+sex:age,data=data)
summary(aov1)
summary(aov2)
summary(aov3)
TukeyHSD(aov1)
TukeyHSD(aov2)
TukeyHSD(aov3)

library(tidyr)
data_long<-gather(data,time,value,before:after)

aov_time<-aov(value~time+Error(subject/time),data=data_long)
summary(aov_time)
model.tables(aov_time,"means")

aov_age_time<-aov(value~age*time+Error(subject/time),data=data_long)
summary(aov_age_time)
model.tables(aov_age_time,"means")

# Two within variables
aov.ww<-aov(y~w1*w2+Error(subject/(w1*w2)),data=data_long)
# One between variable and two within variables
aov.bww<-aov(y~b1*w1*w2+Error(subject/(w1*w2))+b1,data=data_long)
# Two between variables and one within variables
aov.bww<-aov(y~b1*b2*w1+Error(subject/(w1))+b1*b2,data=data_long)
summary(aov_time)
car::Anova(aov_age_time)
##########################################################################################
# EFFECT SIZE
##########################################################################################
#http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
effect_size<-function(condition,ss_effect,ss_error,df_effect,df_error,n) {
  ss_total<-sum(ss_effect)+sum(ss_error)
  ms_effect<-ss_effect/df_effect
  ms_error<-ss_error/df_error
  ms_total<-ms_effect+ms_error
  etasq<-ss_effect/ss_total
  partial.etasq<-ss_effect/(ss_effect+ss_error)
  omegasq<-(df_effect*(ms_effect-ms_error))/(ss_total+ms_error)
  partial.omegasq<-(df_effect*(ms_effect-ms_error))/(df_effect*ms_effect+(n-df_effect)*ms_error)
  cohens_f<-sqrt(etasq/(1-etasq))
  result<-data.frame(condition,ss_effect,ss_error,df_effect,df_error,n,ms_effect,ms_error,etasq,partial.etasq,omegasq,partial.omegasq,cohens_f)
  result[,sapply(result,is.numeric)]<-round(result[,sapply(result,is.numeric)],3)
  return(result)
}
##########################################################################################
# REPORT BETWEEN FACTORIAL
##########################################################################################
#' @title Factorial between subjects design
#' @inheritParams plot_oneway_diagnostics
#' @inheritParams plot_interaction
#' @param method methods for post hoc "hsd" "bonferroni" "lsd" "scheffe" "newmankeuls" "duncan"
#' @param plot_interactions if TRUE it will output 2 way interaction plots
#' @param plot_diagnostics if TRUE it will output ANOVA diagnostics plots
#' @param file output filename
#' @param w width of pdf file
#' @param h height of pdf file
#' @note eta squared is analogous to R^2 in linear regression as variance explained thus as factors increase eta square decreases partial eta squared tries to account for this, however it cannot be interpreted as variance explained
#' @import ggfortify
#' @importFrom car leveneTest Anova
#' @importFrom plyr rbind.fill
#' @importFrom sjstats anova_stats
#' @importFrom DescTools PostHocTest
#' @importFrom stats resid rstandard rstudent cooks.distance dffits hatvalues covratio dfbeta aov as.formula
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @keywords ANOVA
#' @export
#' @examples
#' report_anova_between(df=mtcars,dv=3:6,iv=8:9,
#'                      plot_diagnostics=FALSE,plot_interactions=FALSE)
#' report_anova_between(df=CO2,dv=4:5,iv=2)
#' report_anova_between(df=CO2,dv=4:5,iv=2:3)
#' report_anova_between(df=CO2,dv=4:5,iv=2:3,file="anova_between",
#'                      plot_diagnostics=TRUE,plot_interactions=TRUE)
report_anova_between<-function(df,dv,iv,method="scheffe",file=NULL,plot_diagnostics=FALSE,plot_interactions=FALSE,base_size=10,note="",title="",type="se",w=10,h=10) {
  homogeneity_instruction<-"significant tests show heteroscedasticity violating ANOVA assumptions. Levene test depends on sample size and normality: Non normal distributions may result in false significance"
  df[,iv]<-change_data_type(data.frame(df[,iv]),"factor")
  result<-diagnostics_plot<-list()
  levene<-summary<-model_influence<-post_hoc<-model_call<-descriptives<-data.frame()
  pb<-txtProgressBar(min=0,max=length(dv),style=3)
  counter=0
  for(i in dv) {
    counter=counter+1
    setTxtProgressBar(pb,counter)
    temp<-df[,c(i,iv)]
    temp<-temp[complete.cases(temp),]
    form<-paste(names(df)[i],"~",paste(names(df)[iv],collapse="*"))
    model<-stats::aov(formula(form),data=temp)
    if(plot_diagnostics)
      diagnostics_plot[[i]]<-autoplot(model,which=1:6,ncol=2)+
      labs(title=title,caption=paste0(deparse(model$terms),"\nobservations=",nrow(model$model)))+
      theme_bw(base_size=base_size)+
      theme(axis.text.x=element_text(angle=20,hjust=1))
    levene_test<-data.frame(dv=names(df)[i],car::leveneTest(formula(form),data=temp),check.names=FALSE)
    levene_test<-data.frame(levene_test[-nrow(levene_test),],df_error=levene_test$Df[nrow(levene_test)],check.names=FALSE)
    levene<-plyr::rbind.fill(levene,levene_test)
    
    call<-paste0("aov(",deparse(eval(model$call$formula)),",data=",model$call$data,")")
    ss_1<-try(data.frame(call=call,ss="I",sjstats::anova_stats(model)),silent=TRUE)
    ss_2<-try(data.frame(call=call,ss="II",sjstats::anova_stats(car::Anova(model,type=2))),silent=TRUE)
    ss_3<-try(data.frame(call=call,ss="III",sjstats::anova_stats(car::Anova(model,type=3))),silent=TRUE)
    ss_1<-data.frame(ss_1[-nrow(ss_1),],df_error=ss_1$df[nrow(ss_1)],sumsq_error=ss_1$sumsq[nrow(ss_1)],error_meansq=ss_1$meansq[nrow(ss_1)])
    ss_2<-data.frame(ss_2[-nrow(ss_2),],df_error=ss_2$df[nrow(ss_2)],sumsq_error=ss_2$sumsq[nrow(ss_2)],error_meansq=ss_1$meansq[nrow(ss_2)])
    ss_3<-data.frame(ss_3[-nrow(ss_3),],df_error=ss_3$df[nrow(ss_3)],sumsq_error=ss_3$sumsq[nrow(ss_3)],error_meansq=ss_3$meansq[nrow(ss_3)])
    summary_tests<-data.frame(dv=names(df)[i],plyr::rbind.fill(ss_1,ss_2,ss_3),check.names=FALSE)
    summary<-plyr::rbind.fill(summary,summary_tests)
    
    model_influence<-plyr::rbind.fill(model_influence,data.frame(dv=names(df)[i],
                                                                 residuals=stats::resid(model),
                                                                 standard_residuals=stats::rstandard(model),
                                                                 sqrt_standard_residuals=sqrt(rstandard(model)^2),
                                                                 student_residuals=stats::rstudent(model),
                                                                 fitted=stats::fitted(model),
                                                                 cooks_distance=stats::cooks.distance(model),
                                                                 dffits=stats::dffits(model),
                                                                 hatvalues=stats::hatvalues(model),
                                                                 covariance_ratio=stats::covratio(model),
                                                                 dfbeta=stats::dfbeta(model),
                                                                 effects=model$effects,
                                                                 model$model,
                                                                 #model$qr$qr,
                                                                 check.names=FALSE))
    
    ph<-lapply(DescTools::PostHocTest(model,method=method),function(x) { data.frame(levels=row.names(x),x) })
    post_hoc<-plyr::rbind.fill(post_hoc,data.frame(dv=as.character(names(df)[i]),do.call(rbind,ph)))
  }
  close(pb)
  descriptives<-compute_aggregate(df,iv=iv)
  adjustment<-compute_adjustment(0.05,counter)$bonferroni
  summary$bonferroni_p<-adjustment
  post_hoc$bonferroni_p<-adjustment
  levene$bonferroni_p<-adjustment
  summary$significant<-as.character(summary$p.value<adjustment)
  levene$significant<-as.character(levene$`Pr(>F)`<adjustment)
  post_hoc$significant<-as.character(post_hoc$pval<adjustment)
  result<-list(summary=summary,
               levene=levene,
               model_influence=model_influence,
               post_hoc=post_hoc,
               descriptives=descriptives)
  if(length(iv)>1 & plot_interactions) {
    interaction_means<-plot_interaction(df,dv,iv,base_size=base_size,note=note,title=title,type=type)
    report_pdf(plotlist=interaction_means$plots,file=file,title="two_way_interaction_means",w=w,h=h,print_plot=FALSE)
  }
  if(plot_diagnostics)
    report_pdf(plotlist=diagnostics_plot,file=file,title="diagnostics",w=w,h=h,print_plot=FALSE)
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$summary,workbook=wb,sheet="ANOVA",critical=list(p.value="<0.05"),numFmt="#0.00")
    excel_critical_value(result$post_hoc,workbook=wb,sheet="Post hoc",critical=list(pval="<0.05"),numFmt="#0.00")
    excel_critical_value(result$levene,workbook=wb,sheet="Levene",critical=list("Pr(>F)"="<0.05"),title=homogeneity_instruction,numFmt="#0.00")
    excel_critical_value(result$model_influence,workbook=wb,sheet="Results",numFmt="#0.00")
    excel_critical_value(result$descriptives,workbook=wb,sheet="Descriptives",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  return(result)
}
##########################################################################################
# REPORT WITHIN FACTORIAL
##########################################################################################
#' @title Factorial within subjects design
#' @param df dataframe
#' @param dv index of continous variables
#' @param iv index of within factors
#' @param id index of subject
#' @inheritParams plot_interaction
#' @param plot_interactions if TRUE it will output 2 way interaction plots
#' @param w width of pdf file
#' @param h height of pdf file
#' @param file output filename
#' @import plyr
#' @importFrom sjstats anova_stats
#' @importFrom car Anova
#' @importFrom plyr rbind.fill
#' @importFrom stats aov
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @keywords ANOVA
#' @export
#' @examples
#' df<-data.frame(id=rep(seq(1,80),each=81,1),
#'                IV1=rep(LETTERS[1:3],each=1,2160),
#'                IV2=rep(LETTERS[4:6],each=3,720),
#'                IV3=rep(LETTERS[7:9],each=9,240),
#'                IV4=rep(LETTERS[10:12],each=27,80),
#'                DV1=rnorm(2160),
#'                DV2=rnorm(2160),
#'                stringsAsFactors=FALSE)
#' table(df$IV1,df$id,df$IV2,df$IV3,df$IV4)
#' table(duplicated(df[,1:5]))
#' report_anova_within(df=df,dv=6,iv=2:4,id=1)
#' report_anova_within(df=df,dv=7,iv=3,id=1)
#' report_anova_within(df=df,dv=6:7,iv=3,id=1)
#' report_anova_within(df=df,dv=6,iv=2:4,id=1,
#'                     plot_interactions=TRUE,base_size=20)
#' report_anova_within(df=df,dv=6:7,iv=2:3,id=1,
#'                     plot_interactions=TRUE,base_size=20,file="anova_within")
report_anova_within<-function(df,dv,iv,id,file=NULL,plot_interactions=FALSE,base_size=10,note="",title="",type="se",w=10,h=10) {
  names_iv<-names(df)[iv]
  names_dv<-names(df)[dv]
  names_id<-names(df)[id]
  
  options(contrasts=c("contr.sum","contr.poly"))
  summary_aov<-post_hoc<-data.frame()
  df[,c(names_iv,names_id)]<-change_data_type(df[,c(names_iv,names_id)],type="factor")
  fs<-c(names_iv,names_id)
  counter<-0
  pb<-txtProgressBar(min=0,max=length(dv),style=3)
  
  for(i in dv) {
    counter=counter+1
    setTxtProgressBar(pb,counter)
    call<-model_formula<-paste(names(df)[i],"~",paste(names_iv,collapse="*"),"+Error(",names_id,"/(",paste(names_iv,collapse="*"),"))")
    data<-plyr::ddply(df,fs,plyr::numcolwise(mean,na.rm=TRUE))[,c(names_id,names_iv,names(df)[i])]
    model<-stats::aov(formula(model_formula),data=data)
    # temp_levels<-names(df[,names(df)[c(iv)]])[sapply(df[,names(df)[c(iv)]],function(x) length(table(x)))>=2]
    emmeans_result<-emmeans::emmeans(model,formula(paste("~",paste(names_iv,collapse="*"))))
    post_hoc<-plyr::rbind.fill(post_hoc,data.frame(dv=names(df)[i],graphics::pairs(emmeans_result),check.names=FALSE,row.names=NULL))
    fit_aov_1<-try({data.frame(call=call,ss="I",sjstats::anova_stats(model))},silent=TRUE)
    fit_aov_2<-try({data.frame(call=call,ss="II",sjstats::anova_stats(car::Anova(model,type="II")))},silent=TRUE)
    fit_aov_3<-try({data.frame(call=call,ss="III",sjstats::anova_stats(car::Anova(model,type="III")))},silent=TRUE)
    if(!is.data.frame(fit_aov_1)) fit_aov_1<-data.frame()
    if(!is.data.frame(fit_aov_2)) fit_aov_2<-data.frame()
    if(!is.data.frame(fit_aov_3)) fit_aov_3<-data.frame()
    df_fit_aov<-plyr::rbind.fill(fit_aov_1,fit_aov_2,fit_aov_3)
    summary_aov<-plyr::rbind.fill(summary_aov,df_fit_aov)
  }
  close(pb)
  result<-list(summary_aov=summary_aov,post_hoc=post_hoc)
  
  descriptives<-compute_descriptives(df=df,dv=dv,iv=iv,file=NULL)
  if(length(iv)>1 & plot_interactions) {
    interaction_means<-plot_interaction(df,dv,iv,base_size=base_size,note=note,title=title,type=type)
    report_pdf(plotlist=interaction_means$plots,file=file,title="two_way_interaction_means",w=w,h=h)
  }
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$summary_aov,workbook=wb,sheet="ANOVA",critical=list(p.value="<0.05"),numFmt="#0.00")
    if(nrow(result$post_hoc)>0)
      excel_critical_value(result$post_hoc,workbook=wb,sheet="Post hoc",critical=list("p.value"="<0.05"),numFmt="#0.00")
    excel_critical_value(descriptives,workbook=wb,sheet="Descriptives",numFmt="#0.00")
    saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  return(result)
}
##########################################################################################
# REPORT MIXED FACTORIAL
##########################################################################################
#' @title Factorial mixed subjects design
#' @param df dataframe
#' @param dv index of continous variables
#' @param between dataframe index of between factors
#' @param within dataframe index of within factors
#' @param subject dataframe index of subject collumn
#' @param file output filename
#' @importFrom sjstats anova_stats
#' @importFrom car Anova
#' @importFrom plyr rbind.fill
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @keywords ANOVA
#' @export
#' @examples
#' IV1<-rep(LETTERS[1:4],each=1,160)
#' IV2<-rep(LETTERS[5:8],each=4,40)
#' IV3<-rep(LETTERS[9:12],each=length(IV2)/4)
#' IV3[1:(length(IV3)/2)]<-"M"
#' IV3[(length(IV3)/2):length(IV3)]<-"N"
#' subject<-rep(seq(1,40),each=16)
#' DV1<-rnorm(length(subject))
#' DV2<-rnorm(length(subject))
#' df<-data.frame(subject,IV1,IV2,IV3,DV1,DV2,stringsAsFactors=FALSE)
#' df<-rbind(data.frame(df,IV4="O"),
#'           data.frame(subject=df$subject+40,df[2:length(df)],IV4="P"),
#'           data.frame(subject=df$subject+80,df[2:length(df)],IV4="Q"))
#' table(df$IV1,df$subject,df$IV2)
#' table(duplicated(df[,1:3]))
#' df<-df[,c("subject","IV1","IV2","IV3","IV4","DV1","DV2")]
#' df[df$IV1%in%"A","DV1"]<-df[df$IV1%in%"A","DV1"]+5
#' df[df$IV1%in%"B","DV2"]<-df[df$IV1%in%"A","DV2"]-5
#' df[df$IV2%in%"E","DV1"]<-df[df$IV2%in%"E","DV1"]+5
#' df[df$IV2%in%"F","DV2"]<-df[df$IV2%in%"F","DV2"]-5
#' report_anova_mixed(df=df,dv=6:7,between=5,within=2:3,subject=1)
#' report_anova_mixed(df=df,dv=6:7,between=4:5,within=2:3,subject=1)
#' report_anova_mixed(df=df,dv=6:7,between=4:5,within=2:3,subject=1,
#'                    file="anova_mixed")
report_anova_mixed<-function(df,dv,between,within,subject,file=NULL) {
  options(contrasts=c("contr.sum","contr.poly"))
  summary_aov<-post_hoc<-levene<-data.frame()
  pb<-txtProgressBar(min=0,max=length(dv),style=3)
  counter=0
  for(i in dv) {
    counter=counter+1
    setTxtProgressBar(pb,counter)
    dv_name<-names(df)[i]
    temp<-df[complete.cases(df[,i]),c(subject,between,within,i)]
    tb<-table(temp[,names(df)[subject]])
    balanced_subject_index<-names(tb)[tb==max(tb)]
    temp<-temp[temp[,names(df)[subject]] %in% balanced_subject_index,]
    # mixed_formula_intercept<-formula(paste(names(df)[i],"~",paste(names(df)[c(between,within)],collapse="*"),"+(1|",names(df)[subject],")"))
    # mixed_formula_slope<-formula(paste(names(df)[i],"~",paste(names(df)[c(between,within)],collapse="*"),"+(1+",names(df)[c(within)],"|",names(df)[subject],")"))
    # model_lmer_intercept<-lmerTest::lmer(mixed_formula_intercept,data=temp)
    # model_lmer_slope<-lmerTest::lmer(mixed_formula_slope,data=temp)
    form<-paste(names(df)[i],"~",paste(names(df)[c(between,within)],collapse="*"),"+(",names(df)[subject],"/(",paste(names(df)[within],collapse="*"),"))")
    model_aov<-stats::aov(formula(form),data=temp)
    result_emmeans<-emmeans::emmeans(model_aov,formula(paste("~",paste(names(df)[c(within,between)],collapse="*"))))
    levene_test<-car::leveneTest(formula(paste(names(df)[i],"~",paste(names(df)[c(between)],collapse="*"))),data=temp)
    
    levene_test<-data.frame(dv=names(df)[i],car::leveneTest(formula(paste(names(df)[i],"~",paste(names(df)[c(between)],collapse="*"))),data=temp),check.names=FALSE)
    levene_test<-data.frame(levene_test[-nrow(levene_test),],df_error=levene_test$Df[nrow(levene_test)],check.names=FALSE)
    levene<-plyr::rbind.fill(levene,levene_test)
    
    fit_aov_1<-try({data.frame(call=form,ss="I",sjstats::anova_stats(model_aov))},silent=TRUE)
    fit_aov_2<-try({data.frame(call=form,ss="II",sjstats::anova_stats(car::Anova(model_aov,type="II")))},silent=TRUE)
    fit_aov_3<-try({data.frame(call=form,ss="III",sjstats::anova_stats(car::Anova(model_aov,type="III")))},silent=TRUE)
    fit_aov_1<-try({data.frame(fit_aov_1[-nrow(fit_aov_1),],df_error=fit_aov_1$df[nrow(fit_aov_1)],sumsq_error=fit_aov_1$sumsq[nrow(fit_aov_1)],meansq_error=fit_aov_1$meansq[nrow(fit_aov_1)])},silent=TRUE)
    fit_aov_2<-try({data.frame(fit_aov_2[-nrow(fit_aov_2),],df_error=fit_aov_2$df[nrow(fit_aov_2)],sumsq_error=fit_aov_2$sumsq[nrow(fit_aov_2)],meansq_error=fit_aov_2$meansq[nrow(fit_aov_2)])},silent=TRUE)
    fit_aov_3<-try({data.frame(fit_aov_3[-nrow(fit_aov_3),],df_error=fit_aov_3$df[nrow(fit_aov_3)],sumsq_error=fit_aov_3$sumsq[nrow(fit_aov_3)],meansq_error=fit_aov_3$meansq[nrow(fit_aov_3)])},silent=TRUE)
    
    if(!is.data.frame(fit_aov_3))
      fit_aov_3<-data.frame()
    df_fit_aov<-plyr::rbind.fill(fit_aov_1,fit_aov_2,fit_aov_3)
    summary_aov<-plyr::rbind.fill(summary_aov,df_fit_aov)
    summary_aov<-summary_aov[,c("call","ss",setdiff(names(summary_aov),c("call","ss")))]
    post_hoc<-plyr::rbind.fill(post_hoc,data.frame(dv=names(df)[i],graphics::pairs(result_emmeans),check.names=FALSE,row.names=NULL))
  }
  close(pb)
  descriptives<-compute_aggregate(df,iv=c(between,within))
  adjustment<-compute_adjustment(0.05,counter)$bonferroni
  summary_aov$bonferroni_p<-adjustment
  summary_aov$significant<-as.character(summary_aov$p.value<adjustment)
  post_hoc$bonferroni_p<-adjustment
  post_hoc$significant<-as.character(post_hoc$p.value<adjustment)
  result<-list(summary_aov=summary_aov,descriptives=descriptives,post_hoc=post_hoc)
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result$summary_aov,wb,"ANOVA",critical=list(p.value="<0.05"),numFmt="#0.00")
    excel_critical_value(result$post_hoc,wb,"Post hoc intercept",critical=list("p.value"="<0.05"),numFmt="#0.00")
    excel_critical_value(result$descriptives,wb,"Descriptives",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  return(result)
}
