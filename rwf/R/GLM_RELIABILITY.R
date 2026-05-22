##########################################################################################
# PLOT MTMM
##########################################################################################
#' @title Plot multitrait multimethod matrix
#' @param df dataframe
#' @param key List index of trait names and items constituring a trait
#' @param method name of dataframe collumn spesifying the method used for the row observed
#' @param subject name of dataframe collumn spesifying subject id
#' @param title plot title
#' @importFrom stats cor
#' @importFrom reshape2 melt
#' @importFrom stringr str_split_fixed str_replace
#' @keywords reliability
#' @export
#' @examples
#' population_model<-'t1=~x1+.9*x2+.9*x3
#'                    t2=~x4+.9*x5+.9*x6
#'                    t3=~x7+.9*x8+.9*x9'
#' model_data<-lavaan::simulateData(population_model,sample.nobs=1000)
#' model_data<-model_data[sample(1:1000,1000,TRUE),]
#' model_data<-rbind(model_data,model_data,model_data)
#' model_data$method<-c(rep("m1",1000),rep("m2",1000),rep("m3",1000))
#' model_data$id<-rep(1:1000,3)
#' key<-list(t1=paste0("x",1:3),t2=paste0("x",4:6),t3=paste0("x",7:9))
#' plot_mtmm(df=model_data,key=key,method="method",subject="id")
plot_mtmm<-function(df,key,method,subject,title="") {
  trait_x<-trait_y<-type<-value<-NULL
  trait<-data.frame(subject=df[,subject])
  alpha_result<-list()
  for (z in unique(df[,method])) {
    for (i in names(key)) {
      trait[,paste0(i,".",z)]<-rowMeans(df[df[,method] %in% z,key[[i]]],na.rm=TRUE)
      alpha_result[[paste0(i,".",z)]]<-raw_alpha(df[df[,method] %in% z,key[[i]]])
    }
  }
  alphas<-data.frame(scale=names(unlist(alpha_result)),rel=unlist(alpha_result))
  cors<-stats::cor(trait[2:length(trait)],use="pairwise")
  corm<-reshape2::melt(cors)
  corm<-corm[corm[,'Var1']!=corm[,'Var2'] ,]
  rel<-alphas
  names(rel)=c('Var1','value')
  rel$Var2<-rel$Var1
  rel<-rel[which(rel$Var1 %in% colnames(cors)),c('Var1','Var2','value')]
  corm<-rbind(corm,rel)
  corm[,c('trait_X','method_X')]<-stringr::str_split_fixed(corm$Var1,"\\.",n=2)
  corm[,c('trait_Y','method_Y')]<-stringr::str_split_fixed(corm$Var2,"\\.",n=2)
  corm[,c('var1.s','var2.s')]<-t(apply(corm[,c('Var1','Var2')],1,sort))
  corm[which(corm[,'trait_X']==corm[,'trait_Y'] & corm[,'method_X']!=corm[,'method_Y']),'type']<-'monotrait-heteromethod (validity)'
  corm[which(corm[,'trait_X']!=corm[,'trait_Y'] & corm[,'method_X']==corm[,'method_Y']),'type']<-'heterotrait-monomethod'
  corm[which(corm[,'trait_X']!=corm[,'trait_Y'] & corm[,'method_X']!=corm[,'method_Y']),'type']<-'heterotrait-heteromethod'
  corm[which(corm[,'trait_X']==corm[,'trait_Y'] & corm[,'method_X']==corm[,'method_Y']),'type']<-'monotrait-monomethod (reliability)'
  corm$trait_X=factor(corm$trait_X)
  corm$method_X=factor(corm$method_X)
  corm$trait_Y=factor(corm$trait_Y,levels=rev(levels(corm$trait_X)))
  corm$method_Y=factor(corm$method_Y,levels=levels(corm$method_X))
  corm=corm[order(corm$method_X,corm$trait_X),]
  corm=corm[!duplicated(corm[,c('var1.s','var2.s')]),]
  names(corm)<-tolower(names(corm))
  ggplot(data=corm)+
    geom_tile(aes(x=trait_x,y=trait_y,fill=type),size=5)+
    geom_text(aes(x=trait_x,y=trait_y,label=stringr::str_replace(round(value,2),"0\\.",".")))+
    facet_grid(method_y~method_x)+
    labs(x="",y="",title=paste("Mulitrait Multimethod Matrix",title))+
    theme_bw(base_size=10)+
    theme(panel.background=element_rect(colour=NA),
          panel.grid.minor=element_blank(),
          axis.line=element_line(),
          strip.background=element_blank(),
          panel.grid=element_blank(),
          axis.text.x=element_text(angle=45,hjust=1),
          legend.title=element_blank())+
    scale_fill_brewer('type')
}
##########################################################################################
# KEY TO CFA MODEL
##########################################################################################
#' @title Converts key to cfa model spesification
#' @description This function uses the key spesification used in report_alpha function and converts the key to a cfa model spesification
#' @param key index of trait names and items constituring a trait
#' @keywords reliability
#' @export
#' @examples 
#' population_model<-'t1=~x1+.5*x2+.5*x3
#'                    t2=~x4+.5*x5+.5*x6
#'                    t3=~x7+.5*x8+.5*x9'
#' model_data<-lavaan::simulateData(population_model,sample.nobs=1000)
#' key<-list(f1=paste0("x",1:3),f2=paste0("x",4:6),f3=paste0("x",7:9))
#' model<-key_to_cfa_model(key)
#' fit<-lavaan::cfa(model,model_data)
key_to_cfa_model<-function(key) {
  model<-c()
  for (i in names(key))
    model<-c(model,c(i,"=~",paste0(key[[i]],collapse="+"),"\n"))
  model<-(gsub(",","",toString(model)))
  # model<-nake.names(stringi::stri_trans_general(model,"latin"))
  return(model)
}
##########################################################################################
# ALPHA
##########################################################################################
#' @title Raw alpha
#' @param df dataframe with one dimension
#' @importFrom stats cov
#' @keywords reliability
#' @export
#' @examples
#' set.seed(12345)
#' df<-data.frame(matrix(.5,ncol=6,nrow=6))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
#' psych::alpha(df)
#' raw_alpha(df=df)
raw_alpha<-function(df) {
  covariance_matrix<-stats::cov(df,use="pairwise.complete.obs")
  # if(sapply(df,max)==1 && sapply(df,min)==0) {
  #   tetrachoric<-psych::tetrachoric(df,smooth=FALSE,global=FALSE,na.rm=TRUE,delete=TRUE)$rho
  #   standard_deviation<-as.numeric(sapply(df,sd,na.rm=TRUE))
  #   standard_deviation_matrix<-standard_deviation %*% t(standard_deviation)
  #   covariance_matrix<-standard_deviation_matrix*tetrachoric
  # }
  diagonal<-diag(covariance_matrix)
  numerator<-sum(as.numeric(diag(covariance_matrix)))
  denominator<-sum(reshape2::melt(covariance_matrix)$value)
  result<-length(df)/((length(df)-1))*(1-(numerator/denominator))
  return(result)
}
##########################################################################################
# ALPHA DIAGNOSTICS
##########################################################################################
#' @title Item total correlation and r drop
#' @inheritParams raw_alpha
#' @importFrom stats cor
#' @keywords reliability
#' @export
#' @examples
#' set.seed(12345)
#' df<-data.frame(matrix(.5,ncol=6,nrow=6))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
#' psych::alpha(df)
#' alpha_diagnostics(df=df)
alpha_diagnostics<-function(df) {
  corlist0<-corlist1<-corlist2<-c()
  for (i in 1:length(df)) {
    corlist0<-c(corlist0,raw_alpha(df[,-i]))
    corlist1[[i]]<-stats::cor(rowSums(df,na.rm=TRUE),df[i],use="pairwise.complete.obs")
    corlist2[[i]]<-stats::cor(rowSums(df[-i],na.rm=TRUE),df[i],use="pairwise.complete.obs")
  }
  result<-data.frame(item=names(df),
                     alpha.if.item.removed=corlist0,
                     item.total.correlation=corlist1,
                     item.total.correlation.r.drop=corlist2,
                     row.names=names(df))
  return(result)
}
##########################################################################################
# MEAN SD
##########################################################################################
#' @title Mean and SD
#' @inheritParams raw_alpha
#' @param divisor number to use for dividing the rowsums
#' @keywords reliability
#' @export
#' @examples
#' set.seed(12345)
#' df<-data.frame(matrix(.5,ncol=6,nrow=6))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
#' mean_sd_alpha(df)
#' mean_sd_alpha(df,divisor=100)
mean_sd_alpha<-function(df,divisor=NULL) {
  if(is.null(divisor))
    result<-data.frame(MEAN=mean(rowMeans(df,na.rm=TRUE),na.rm=TRUE),SD=stats::sd(rowMeans(df,na.rm=TRUE),na.rm=TRUE))
  else
    result<-data.frame(Mean=mean(rowSums(df,na.rm=TRUE)/divisor,na.rm=TRUE),SD=stats::sd(rowSums(df,na.rm=TRUE)/divisor,na.rm=TRUE))
  return(result)
}
##########################################################################################
# ALPHA OUTPUT
##########################################################################################
#' @title Estimate alpha for several dimensions and export results to xlsx
#' @description Uses an arbitrary input
#' @param df dataframe
#' @param key index of trait names and items constituting a trait
#' @param questions trait names and items constituting a trait
#' @param reverse index of trait names and index for reversal
#' @param mini minimum rating in scale if NULL reversal will be performed using the empirical minimum
#' @param maxi maximum rating in scale if NULL reversal will be performed using the empirical maximum
#' @param file output filename
#' @param ... arguments passed to psych::alpha
#' @keywords reliability
#' @export
#' @examples
#' set.seed(12345)
#' df<-data.frame(matrix(.5,ncol=6,nrow=6))
#' correlation_martix<-as.matrix(df)
#' diag(correlation_martix)<-1
#' df<-round(generate_correlation_matrix(correlation_martix,nrows=1000),0)+5
#' key<-list(f1=c("X1","X2","X3"),
#'           f2=c("X4","X5","X6"))
#' reverse<-list(f1=c(1,1,1),
#'               f2=c(1,1,1))
#' report_alpha(df=df,key=key,cumulative=TRUE,n.iter=1)
#' report_alpha(df=df,key=key,reverse=reverse,check.keys=FALSE,n.iter=2)
#' report_alpha(df=df,key=key,check.keys=FALSE,n.iter=2,file="alpha")
report_alpha<-function(df,key=NULL,questions=NULL,reverse=NULL,mini=NULL,maxi=NULL,file=NULL,...) {
  comment<-list(raw_alpha=toString(c("Crombach's alpha",
                                     "\n\nalpha based on covariances",
                                     "\n\nLambda 3=(n)/(n-1)(1-tr(Vx)/(Vx) = (n)/(n-1)(Vx-tr(Vx)/Vx=alpha",
                                     "\n\n0.91-1.00 Excellent (however check for multicolinearity problems)",
                                     "\n\n0.81-0.90 Good",
                                     "\n\n0.71-0.80 Good and Acceptable",
                                     "\n\n0.61-0.70 Acceptable",
                                     "\n\n0.56-0.60 Marginally Acceptable",
                                     "\n\n0.01-0.55 Unacceptable")),
                std_alpha="standarized alpha based on correlations",
                "g6(smc)"=toString(c("Guttman's Lambda 6 reliability",
                                     "\nsquared multiple correlation",
                                     "\n\nconsiders the amount of variance in each item that can be accounted for the linear regression of all of the other items",
                                     "\n\nlambda 6=1-sum(e^2)/Vx = 1-sum(1-r^2(smc))/Vx",
                                     "\n\nif equal item loadings alpha>G6",
                                     "\nif unequal item loadings alpha<G6",
                                     "\nif there is a general factor alpha<G6")),
                average_r="average interitem correlation",
                median_r="median interitem correlation",
                "s/n"=toString(c("signal to noise ratio",
                                 "\n\nindex of the quality of the test that is linear with the number of items and the average correlation",
                                 "\n\n s/n = n r/(1-r)")),
                ase="alpha standard error",
                mean=toString(c("for total statistics:",
                                "\nmean of the scale formed by averaging or summing the items (depending upon the cumulative option)",
                                "\n\nfor item statistics:",
                                "\nmean of each item")),
                sd=toString(c("for total statistics:",
                              "\nstandard deviation of the total score",
                              "\n\nfor item statistics:",
                              "\nstandard deviation of each item")),
                alpha_drop="A data frame with all of the above for the case of each item being removed one by one.",
                n="number of complete cases for the item",
                raw_r="correlation of each item with the total score, not corrected for item overlap",
                std_r="correlation of each item with the total score (not corrected for item overlap) if the items were all standardized",
                r_cor="item whole correlation corrected for item overlap and scale reliability",
                r_drop="item whole correlation for this item against the scale without this item",
                response_freq="the frequency of each item response (if less than 20)",
                scores=toString(c("scores are by default the average response for all items that a participant took",
                                  "\nIf cumulative=TRUE, then these are sum scores.")),
                "boot_ci_2_5%"="bootstrap confidence interval lower bound 2.5%",
                "boot_ci_50%"="bootstrap confidence interval 50%",
                "boot_ci_97_5%"="bootstrap confidence interval lower bound 97.5%",
                miss="proportion of non answered items",
                "alpha se"="alpha standard error",
                med_r="median interitem correlation",
                unidim="index of unidimensionality",
                kaiser_criterion="number of eigenvalues > 1")
  if(is.null(key))
    key<-list(dimension=names(df))
  call_arguments<-match.call()
  call_string<-data.frame(call=gsub(" ","",gsub("\""," ",gsub(", ,",",",toString(unlist(deparse(call_arguments)))))))
  pb<-txtProgressBar(min=0,max=length(key),style=3)
  result_total<-result_boot<-result_item_statistics<-result_dropped<-data.frame()
  counter<-1
  for(sc in names(key)) {
    counter<-counter+1
    setTxtProgressBar(pb,counter)
    if(length(key[[sc]])<2) next
    temp<-df[,key[[sc]]]
    if(!is.null(reverse))
      temp<-data.frame(psych::reverse.code(keys=reverse[[sc]],items=df[,key[[sc]]],mini=rep(mini,length(reverse[[sc]])),maxi=rep(maxi,length(reverse[[sc]]))))
    result<-psych::alpha(temp,...)
    eigenvalues<-NA
    try({eigenvalues<-eigen(cor(temp,use="pairwise.complete.obs"))$values})
    kaiser_criterion<-length(eigenvalues[eigenvalues>1])
    if(!is.null(result$boot.ci)) {
      boot_CI<-data.frame(t(result$boot.ci),check.names=FALSE)
      result_total_df<-data.frame(dimension=sc,
                                  items=result$nvar,
                                  kaiser_criterion=kaiser_criterion,
                                  result$total,
                                  boot_CI=boot_CI,
                                  stringsAsFactors=FALSE,
                                  check.names=FALSE)
      result_boot_df<-data.frame(dimension=sc,
                                 items=result$nvar,
                                 kaiser_criterion=kaiser_criterion,
                                 result$boot,
                                 stringsAsFactors=FALSE,
                                 check.names=FALSE)
    } else {
      result_total_df<-data.frame(dimension=sc,
                                  items=result$nvar,
                                  kaiser_criterion=kaiser_criterion,
                                  result$total,
                                  stringsAsFactors=FALSE,
                                  check.names=FALSE)
    }
    result_total<-plyr::rbind.fill(result_total,result_total_df)
    if(!is.null(result$boot.ci))
      result_boot<-plyr::rbind.fill(result_boot,result_boot_df)
    if(is.null(questions))
      question<-row.names(result$alpha.drop)
    else
      question<-paste(row.names(result$alpha.drop),questions[[sc]])
    result_item_statistics<-plyr::rbind.fill(result_item_statistics,
                                             data.frame(dimension=sc,
                                                        question=question,
                                                        raw_alpha=as.numeric(result$total[1]),
                                                        result$item.stats,
                                                        result$response.freq,
                                                        stringsAsFactors=FALSE,
                                                        check.names=FALSE))
    result_dropped<-plyr::rbind.fill(result_dropped,
                                     data.frame(dimension=sc,
                                                question=question,
                                                scale_alpha=as.numeric(result$total[1]),
                                                result$alpha.drop,
                                                stringsAsFactors=FALSE,
                                                check.names=FALSE))
  }
  close(pb)
  names(result_total)<-gsub(".","_",tolower(names(result_total)),fixed=TRUE)
  if(!is.null(result$boot.ci))
    names(result_boot)<-gsub(".","_",tolower(names(result_boot)),fixed=TRUE)
  names(result_item_statistics)<-gsub(".","_",tolower(names(result_item_statistics)),fixed=TRUE)
  names(result_dropped)<-gsub(".","_",tolower(names(result_dropped)),fixed=TRUE)
  result_total$alpha_criterion<-NA
  result_total[result_total$raw_alpha<.6,"alpha_criterion"]<-"Unacceptable"
  result_total[result_total$raw_alpha<.7&result_total$raw_alpha>.6,"alpha_criterion"]<-"Acceptable"
  result_total[result_total$raw_alpha<.8&result_total$raw_alpha>.7,"alpha_criterion"]<-"Good and Acceptable"
  result_total[result_total$raw_alpha<.9&result_total$raw_alpha>.8,"alpha_criterion"]<-"Good"
  result_total[result_total$raw_alpha>.9,"alpha_criterion"]<-"Excellent"
  
  raw_alpha_critical_value<-">0.60"
  if(!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(result_total,wb,"total statistics",critical=list(raw_alpha=raw_alpha_critical_value),numFmt="#0.00",comment=comment)
    if(!is.null(result$boot.ci))
      excel_critical_value(result_boot,wb,"bootstrap",critical=list(raw_alpha=raw_alpha_critical_value),numFmt="#0.00",comment=comment)
    excel_critical_value(result_item_statistics,wb,"item statistics",critical=list(raw_alpha=raw_alpha_critical_value),numFmt="#0.00",comment=comment)
    excel_critical_value(result_dropped,wb,"if dropped",critical=list(scale_alpha=raw_alpha_critical_value,raw_alpha=raw_alpha_critical_value),numFmt="#0.00",comment=comment)
    excel_critical_value(call_string,wb,"call",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  result<-list(result_total=result_total,result_boot=result_boot,result_item_statistics=result_item_statistics,result_dropped=result_dropped)
  return(result)
}
##########################################################################################
# EXTRACT COMPONENTS
##########################################################################################
#' @title Extract variance components from model
#' @param model model containing variance components
#' @param title plot title
#' @import ggplot2
#' @keywords reliability
#' @export
#' @examples 
#' design<-expand.grid(time=1:3,item=1:3,person=1:10)
#' design<-change_data_type(design,type="factor")
#' design$response<-rowSums(change_data_type(design[,1:2],type="numeric"))+rnorm(90,0,0.1)
#' model<-mixlm::lm(response~r(time)*r(person)+r(item)*r(person),data=design)
#' extract_components(model)
extract_components<-function(model,title="") {
  vc_percent<-NULL
  res<-mixlm::Anova(model,type="III")
  name_components<-row.names(res$anova)
  components<-data.frame(component=name_components,VC=res$var.comps,vc_percent=abs(res$var.comps)/sum(abs(res$var.comps),na.rm=TRUE)*100)
  component<-components
  components$component<-factor(as.character(components$component),levels=as.character(components$component))
  component_plot<-ggplot(components,aes(x=component,y=vc_percent))+
    geom_bar(stat='identity')+
    geom_line(group=1,color="gray25")+
    geom_point(size=5,color="gray25")+
    labs(title=title,x="components",y="Explained variance (%)")+
    coord_flip()+
    theme_bw()
  result<-list(components=component,plot=component_plot)
  return(result)
}
##########################################################################################
# SHROUT RELIABILITY
##########################################################################################
#' @title Shrout reliability
#' @param sperson variance component of participant
#' @param spersonitem variance component of participant by item interaction
#' @param stime variance component of time
#' @param spersontime variance component of participant by time interaction
#' @param serror variance component of error
#' @param m m item reports
#' @param k k time points
#' @keywords reliability
#' @export
#' @examples 
#' design<-expand.grid(time=1:3,item=1:2,person=1:10)
#' design<-change_data_type(design,type="factor")
#' design$response<-rnorm(30,0,0.1)
#' model<-mixlm::lm(response~r(time)*r(person)+r(item)*r(person),data=design)
#' result<-extract_components(model)
#' vc<-result$components
#' shrout(sperson=vc[2,3],spersonitem=vc[5,3],stime=vc[1,3],
#'        spersontime=vc[4,3],serror=vc[6,3],3,3)
shrout<-function(sperson,spersonitem,stime,spersontime,serror,m,k) {
  instruction<-data.frame(measure=c("r1f","r1r","rkf","rkr","rc"),
                          description=c("Reliability (between persons) of measures taken on the same fixed k time",
                                        "Reliability (between persons) of measures taken on the same random k time",
                                        "Reliability (between persons) of average measures taken over fixed m items and fixed k times",
                                        "Reliability (between persons) of different random time with same number of points k between periods",
                                        "Reliability (within persons) of change"))
  km<-m*k
  r1f<-(sperson+spersonitem/m)/(sperson+spersonitem/m+serror/m)
  r1r<-(sperson+spersonitem/m)/(sperson+spersonitem/m+stime+spersontime+serror/m) #Different people different days
  rkf<-(sperson+spersonitem/m)/(sperson+spersonitem/m+serror/km)
  rkr<-(sperson+spersonitem/m)/(sperson+spersonitem/m+stime/k+spersontime/k+serror/km)
  rc<-(spersontime)/(spersontime+(serror/m))
  result<-data.frame(measure=c("r1f","rkf","rkr","r1r","rc"),
                     result=c(r1f,rkf,rkr,r1r,rc=rc))
  result<-merge(result,instruction,all=TRUE)
  return(result)
}
##########################################################################################
# NOTES 
##########################################################################################
# RELIABILITY CROMBACH'S ALPHA
# Accepted values .7 for ability tests .8 for cognitive tests. High alpha can be achieved merely by the number of items Cortina (1993)
# Guttman's lambda 6 (smc squared multiple correlation) is the average inter item correlation
# G6 = Guttmans lambda this can be calculated from the squared multiple correlation (smc)
# If we calculate someones score by taking the average of all of their items (which is the same as adding up the score and dividing by the number of items)
# column raw_alpha = the overall a if that item is not included in the calculation
# r = correlations between each item and the total score from the questionnaire sometimes called item total correlations,the problem of this statistic is that the item is included in the total
# r.drop = the correlation of that item with the scale total if that item is not included in the scale total. Sometimes this is called the item rest correlation or corrected item total correlation
# in a reliable scale items should correlate with the total so we are looking items that do not correlate with the overall score r.drop should not be bellow .3


