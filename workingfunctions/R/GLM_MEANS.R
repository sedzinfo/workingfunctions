##########################################################################################
# T TEST
##########################################################################################
#' @title T test
#' @param file output filename
#' @inheritParams plot_oneway_diagnostics
#' @inheritDotParams stats::t.test
#' @importFrom stats t.test formula
#' @keywords ANOVA
#' @export
#' @examples
#' report_ttests(df=mtcars,dv=2,iv=9)
#' report_ttests(df=mtcars,dv=2,iv=9:10)
#' report_ttests(df=mtcars,dv=2:3,iv=9)
#' report_ttests(df=mtcars,dv=2:3,iv=9:10,alternative="two.sided")
#' report_ttests(df=mtcars,dv=2:7,iv=9:10,alternative="less")
#' report_ttests(df=mtcars,dv=2:7,iv=9:10,alternative="greater")
#' report_ttests(df=mtcars,dv=2:7,iv=9:10,paired=FALSE)
#' #report_ttests(df=mtcars,dv=2:7,iv=9:10,paired=TRUE)
#' report_ttests(df=mtcars,dv=1:7,iv=8:10,var.equal=TRUE)
#' report_ttests(df=mtcars,dv=1:7,iv=8:10,var.equal=TRUE,file="ttest")
report_ttests<-function(df,dv,iv,file=NULL,...) {
  
  comment<-list(DV="dependent variable",
                IV="independent variable",
                level1="level 1",
                level2="level 2",
                n1="sample size for level 1",
                n2="sample size for level 2",
                t="t statistic",
                df="degrees of freedom for t statistic",
                p="p value",
                CI_l="confidence interval lower bound",
                CI_u="confidence interval upper bound",
                alternative="alternative hypothesis",
                method="",
                mean1="mean for level 1",
                mean2="mean for level 2",
                sd1="standard deviation for level 1",
                sd2="standard deviation for level 2",
                sd_pooled="pooled standard deviation\n\ncombined standard deviation of both levels",
                d="measure of effect size\n\ncohen's d\n\nVery small\t0.01\t\tSawilowsky (2009)\nSmall\t\t0.20\t\tCohen (1988)\nMedium\t\t0.50\t\tCohen (1988)\nLarge\t\t0.80\t\tCohen (1988)\nVery large\t\t1.20\t\tSawilowsky (2009)\nHuge\t\t12.0\t\t1Sawilowsky (2009)",
                r="measure of effect size\n\ncorrelation effect size",
                "k_squared[bartlett]"="bartlett test for homogeneity of variances\n\n",
                "df[bartlett]"="bartlett test for homogeneity of variances\n\n",
                "p[bartlett]"="bartlett test for homogeneity of variances\n\nsignificant values indicate heteroscedasticity",
                bonferroni_p="bonferroni adjusted critical value for a=0.05",
                significant="if TRUE result is significant after bonferroni adjustment")
  
  critical<-list(p="<0.05","p[bartlett]"="<0.05")
  
  df_ttest<-data.frame()
  combinations<-expand.grid(names(df)[iv],names(df)[dv])
  names(combinations)<-c("iv","dv")
  row.names(combinations)<-paste0(combinations$iv,"_",combinations$dv)
  combinations<-change_data_type(combinations,type="character")
  pb<-txtProgressBar(min=0,max=length(iv)*length(dv),style=3)
  for(i in 1:nrow(combinations)) {
    independent<-combinations$iv[i]
    dependent<-combinations$dv[i]
    setTxtProgressBar(pb,i)
    tempdata<-df[complete.cases(df[,c(dependent,independent)]),]
    tempdata[,independent]<-factor(tempdata[,independent])
    combinations_levels<-data.frame(t(utils::combn(unique(as.character(tempdata[,independent])),2)),stringsAsindependent=FALSE)
    tempdata_all_levels<-df[complete.cases(df[,c(dependent,independent)]),]
    for(l in 1:nrow(combinations_levels)) {
      f1<-as.character(combinations_levels$X1[l])
      f2<-as.character(combinations_levels$X2[l])
      tempdata<-tempdata_all_levels[tempdata_all_levels[,independent] %in% c(f1,f2),]
      form<-stats::formula(paste0(dependent,"~",independent))
      ttest<-stats::t.test(form,data=tempdata,...)
      bartlett.test<-bartlett.test(form,data=tempdata)
      mean1=mean(tempdata[tempdata[,independent] %in% f1,dependent],na.rm=TRUE)
      mean2=mean(tempdata[tempdata[,independent] %in% f2,dependent],na.rm=TRUE)
      sd1=stats::sd(tempdata[tempdata[,independent] %in% f1,dependent],na.rm=TRUE)
      sd2=stats::sd(tempdata[tempdata[,independent] %in% f2,dependent],na.rm=TRUE)
      n1<-length(tempdata[tempdata[,independent] %in% f1,dependent])
      n2<-length(tempdata[tempdata[,independent] %in% f2,dependent])
      sd_pooled=sqrt((sd1^2+sd2^2)/2)
      cohen_d<-abs(mean2-mean1)/sd_pooled
      ttest_r<-data.frame(DV=independent,
                          IV=dependent,
                          level1=f1,
                          level2=f2,
                          n1=n1,
                          n2=n2,
                          t=ttest$statistic[[1]],
                          df=ttest$parameter[[1]],
                          p=ttest$p.value[[1]],
                          CI_l=ttest$conf.int[[1]],
                          CI_u=ttest$conf.int[[2]],
                          alternative=ttest$alternative,
                          method=ttest$method,
                          mean1=mean1,
                          mean2=mean2,
                          sd1=sd1,
                          sd2=sd2,
                          sd_pooled=sd_pooled,
                          d=cohen_d,
                          r=cohen_d/(sqrt(cohen_d^2)+(((n1+n2)^2)/(n1*n2))),
                          "k_squared[bartlett]"=bartlett.test$statistic,
                          "df[bartlett]"=as.numeric(bartlett.test$parameter),
                          "p[bartlett]"=bartlett.test$p.value,
                          stringsAsFactors=FALSE,
                          check.names=FALSE)
      df_ttest<-plyr::rbind.fill(df_ttest,ttest_r)
    }
  }
  close(pb)
  adjustment<-compute_adjustment(0.05,nrow(df_ttest))
  df_ttest$bonferroni_p<-adjustment$bonferroni
  df_ttest$significant<-as.character(adjustment$bonferroni>df_ttest$p)
  report_dataframe(df_ttest,file=file,sheet="t test",comment=comment,critical=critical)
  return(df_ttest)
}
##########################################################################################
# WILCOXON TEST
##########################################################################################
#' @title Wilcoxon test
#' @param file output filename
#' @inheritParams plot_oneway_diagnostics
#' @inheritDotParams stats::wilcox.test
#' @importFrom stats wilcox.test formula sd
#' @keywords ANOVA
#' @export
#' @examples
#' report_wtests(df=mtcars,dv=2,iv=9)
#' report_wtests(df=mtcars,dv=2,iv=9:10)
#' report_wtests(df=mtcars,dv=2:3,iv=9)
#' report_wtests(df=mtcars,dv=2:3,iv=9:10,alternative="two.sided")
#' report_wtests(df=mtcars,dv=2:7,iv=9:10,alternative="less")
#' report_wtests(df=mtcars,dv=2:7,iv=9:10,alternative="greater")
#' report_wtests(df=mtcars,dv=2:7,iv=9:10,paired=FALSE)
#' #report_wtests(df=mtcars,dv=2:7,iv=9:10,paired=TRUE)
#' report_wtests(df=mtcars,dv=1:7,iv=8:10,var.equal=TRUE)
#' report_wtests(df=mtcars,dv=1:7,iv=8:10,var.equal=TRUE,file="wilcoxontest")
report_wtests<-function(df,dv,iv,file=NULL,...) {
  
  comment<-list(DV="dependent variable",
                IV="independent variable",
                level1="level 1",
                level2="level 2",
                n1="sample size for level 1",
                n2="sample size for level 2",
                W="wilcoxon statistic",
                df="degrees of freedom for t statistic",
                p="p value",
                CI_l="confidence interval lower bound",
                CI_u="confidence interval upper bound",
                alternative="alternative hypothesis",
                method="",
                mean1="mean for level 1",
                mean2="mean for level 2",
                sd1="standard deviation for level 1",
                sd2="standard deviation for level 2",
                sd_pooled="pooled standard deviation\n\ncombined standard deviation of both levels",
                d="measure of effect size\n\ncohen's d\n\nVery small\t0.01\t\tSawilowsky (2009)\nSmall\t\t0.20\t\tCohen (1988)\nMedium\t\t0.50\t\tCohen (1988)\nLarge\t\t0.80\t\tCohen (1988)\nVery large\t\t1.20\t\tSawilowsky (2009)\nHuge\t\t12.0\t\t1Sawilowsky (2009)",
                r="measure of effect size\n\ncorrelation effect size",
                "k_squared[bartlett]"="bartlett test for homogeneity of variances\n\n",
                "df[bartlett]"="bartlett test for homogeneity of variances\n\n",
                "p[bartlett]"="bartlett test for homogeneity of variances\n\nsignificant values indicate heteroscedasticity",
                bonferroni_p="bonferroni adjusted critical value for a=0.05",
                significant="if TRUE result is significant after bonferroni adjustment")
  
  critical<-list(p="<0.05","p[bartlett]"="<0.05")
  
  df_wtest<-data.frame()
  combinations<-expand.grid(names(df)[iv],names(df)[dv])
  names(combinations)<-c("iv","dv")
  row.names(combinations)<-paste0(combinations$iv,"_",combinations$dv)
  combinations<-change_data_type(combinations,type="character")
  pb<-txtProgressBar(min=0,max=length(iv)*length(dv),style=3)
  for(i in 1:nrow(combinations)) {
    independent<-combinations$iv[i]
    dependent<-combinations$dv[i]
    setTxtProgressBar(pb,i)
    tempdata<-df[complete.cases(df[,c(dependent,independent)]),]
    tempdata[,independent]<-factor(tempdata[,independent])
    combinations_levels<-data.frame(t(utils::combn(unique(as.character(tempdata[,independent])),2)),stringsAsindependent=FALSE)
    tempdata_all_levels<-df[complete.cases(df[,c(dependent,independent)]),]
    for(l in 1:nrow(combinations_levels)) {
      f1<-as.character(combinations_levels$X1[l])
      f2<-as.character(combinations_levels$X2[l])
      tempdata<-tempdata_all_levels[tempdata_all_levels[,independent] %in% c(f1,f2),]
      form<-stats::formula(paste0(dependent,"~",independent))
      wtest<-stats::wilcox.test(form,data=tempdata,conf.int=TRUE,...)
      bartlett.test<-bartlett.test(form,data=tempdata)
      mean1=mean(tempdata[tempdata[,independent] %in% f1,dependent],na.rm=TRUE)
      mean2=mean(tempdata[tempdata[,independent] %in% f2,dependent],na.rm=TRUE)
      sd1=stats::sd(tempdata[tempdata[,independent] %in% f1,dependent],na.rm=TRUE)
      sd2=stats::sd(tempdata[tempdata[,independent] %in% f2,dependent],na.rm=TRUE)
      n1<-length(tempdata[tempdata[,independent] %in% f1,dependent])
      n2<-length(tempdata[tempdata[,independent] %in% f2,dependent])
      sd_pooled=sqrt((sd1^2+sd2^2)/2)
      cohen_d<-abs(mean2-mean1)/sd_pooled
      wtest_r<-data.frame(DV=independent,
                          IV=dependent,
                          level1=f1,
                          level2=f2,
                          n1=n1,
                          n2=n2,
                          W=wtest$statistic[[1]],
                          p=wtest$p.value[[1]],
                          CI_l=wtest$conf.int[[1]],
                          CI_u=wtest$conf.int[[2]],
                          alternative=wtest$alternative,
                          method=wtest$method,
                          mean1=mean1,
                          mean2=mean2,
                          sd1=sd1,
                          sd2=sd2,
                          sd_pooled=sd_pooled,
                          d=cohen_d,
                          r=as.numeric(rstatix::wilcox_effsize(form,data=tempdata)$effsize),
                          "k_squared[bartlett]"=bartlett.test$statistic,
                          "df[bartlett]"=as.numeric(bartlett.test$parameter),
                          "p[bartlett]"=bartlett.test$p.value,
                          stringsAsFactors=FALSE,
                          check.names=FALSE)
      df_wtest<-plyr::rbind.fill(df_wtest,wtest_r)
    }
  }
  close(pb)
  adjustment<-compute_adjustment(0.05,nrow(df_wtest))
  df_wtest$bonferroni_p<-adjustment$bonferroni
  df_wtest$significant<-as.character(adjustment$bonferroni>df_wtest$p)
  report_dataframe(df_wtest,file=file,sheet="t test",comment=comment,critical=critical)
  return(df_wtest)
}

