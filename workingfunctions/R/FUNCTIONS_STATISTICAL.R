##########################################################################################
# ADJUST
##########################################################################################
#' @title Compute adjustments
#' @param a alpha criterion
#' @param ntests number of tests
#' @keywords functions statistical
#' @export
#' @examples
#' compute_adjustment(0.05,100)
compute_adjustment<-function(a,ntests) {
  sidak<-1-((1-a)^(1/ntests))
  bonferroni<-a/ntests
  result<-list(sidak=sidak,bonferroni=bonferroni)
  return(result)
}
##########################################################################################
# STANDARDIZE
##########################################################################################
#' @title compute standard scores
#' @param vector vector
#' @param mean numeric applicable to "uz"
#' @param sd numeric applicable to "uz"
#' @param type "z" "uz" "sten" "t" "stanine" "center" "center_reversed" "percent" "scale_zero_one" "normal_density" "cumulative_density" "all"
#' @param input "standard" "non_standard" standard inputs are z scores and non standard are raw scores
#' @importFrom stats qnorm pnorm
#' @keywords functions statistical
#' @export
#' @examples
#' vector<-c(rnorm(10),NA,rnorm(10))
#' compute_standard(vector,type="z")
#' compute_standard(vector,mean=0,sd=1,type="uz")
#' compute_standard(vector,type="sten")
#' compute_standard(vector,type="t")
#' compute_standard(vector,type="stanine")
#' compute_standard(vector,type="center")
#' compute_standard(vector,type="center_reversed")
#' compute_standard(vector,type="percent")
#' compute_standard(vector,type="scale_zero_one")
#' ndf<-compute_standard(seq(-6,6,.01),mean=0,sd=1,type="normal_density")
#' plot(ndf)
#' cdf<-compute_standard(ndf,mean=0,sd=1,type="cumulative_density")
#' plot(cdf)
#' compute_standard(vector,type="all")
#' compute_standard(seq(-6,6,.1),type="all",input="standard")
compute_standard<-function(vector,mean=0,sd=1,type="z",input="non_standard") {
  mean_vector<-mean(vector,na.rm=TRUE)
  sd_vector<-stats::sd(vector,na.rm=TRUE)
  length_vector<-length(vector)
  if(input=="non_standard"){
    numerator<-vector-mean_vector
    denominator<-sd_vector
    z<-numerator/denominator
  }
  if(input=="standard")
    z<-vector
  if (type=="z")
    result<-z
  if (type=="uz")
    result<-vector*sd+mean
  if (type=="sten"){
    result<-round((z*2)+5.5,0)
    result[result<1]<-1
    result[result>10]<-10
  }
  if(type=="t")
    result<-(z*10)+50
  if(type=="stanine"){
    result<-(z*2)+5
    result[result<1]<-1
    result[result>9]<-9
    result<-round(result,0)
  }
  if(type=="center")
    result<-vector-mean(vector,na.rm=TRUE)
  if(type=="center_reversed")
    result<-mean(vector,na.rm=TRUE)-vector
  if(type=="percent")
    result<-(vector/max(vector,na.rm=TRUE))*100
  if(type=="scale_zero_one")
    result<-(vector-min(vector,na.rm=TRUE))/(max(vector,na.rm=TRUE)-min(vector,na.rm=TRUE))
  if(type=="normal_density")
    result<-(1/(sqrt(sd*pi)))*exp(-0.5*((vector-mean)/sd)^2)
  if(type=="cumulative_density") {
    result<-c()
    for(i in 1:length(vector))
      result<-c(result,vector[i]+sum(vector[1:i]))
  }
  if(type=="all") {
    mydata<-data.frame(score=vector)
    mydata$z<-compute_standard(mydata$score,type="z",input=input)
    mydata$sten<-compute_standard(mydata$score,type="sten",input=input)
    mydata$t<-compute_standard(mydata$score,type="t",input=input)
    mydata$stanine<-compute_standard(mydata$score,type="stanine",input=input)
    mydata$percent<-compute_standard(mydata$score,type="percent",input=input)
    mydata$scale_0_1<-compute_standard(mydata$score,type="scale_zero_one",input=input)
    result<-data.frame(mydata[order(mydata$z),])
  }
  return(result)
}
##########################################################################################
# COMPUTE DISSATENUATION
##########################################################################################
#' @title Compute dissatenuation
#' @param variable1 vector
#' @param variable2 vector
#' @param error1 vector error measurement for variable1
#' @param error2 vector error measurement for variable2
#' @importFrom stats var cov
#' @keywords functions statistical
#' @export
#' @examples
#' set.seed(1)
#' compute_dissatenuation(rnorm(10),rnorm(10),rnorm(10),rnorm(10))
compute_dissatenuation<-function(variable1,error1,variable2,error2) {
  correlation<-stats::cov(variable1+error1,variable2+error2)/sqrt(stats::var(variable1+error1)*stats::var(variable2+error2))
  Rb<-stats::var(variable1)/(stats::var(variable1)+stats::var(error1))
  Rth<-stats::var(variable2)/(stats::var(variable2)+stats::var(error2))
  p<-correlation/sqrt(Rb*Rth)
  return(p)
}
##########################################################################################
# COMPUTE SKEWNESS
##########################################################################################
#' @title Compute skewness
#' @param vector vector
#' @keywords functions statistical
#' @note b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2). Used in MINITAB and BMDP.
#' @export
#' @examples
#' set.seed(1)
#' vector<-rnorm(1000)
#' compute_skewness(vector)
#' e1071::skewness(vector)
compute_skewness<-function(vector) {
  vector<-na.omit(vector)
  n<-length(vector)
  x<-vector-mean(vector)
  y<-sqrt(n)*sum(x^3)/(sum(x^2)^(3/2))
  y<-y*((1-1/n))^(3/2)
  return(y)
}
##########################################################################################
# COMPUTE KURTOSIS
##########################################################################################
#' @title Compute kurtosis
#' @param vector vector
#' @keywords functions statistical
#' @note b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3. Used in MINITAB and BMDP.
#' @export
#' @examples
#' set.seed(1)
#' vector<-rnorm(1000)
#' compute_kurtosis(vector)
#' e1071::kurtosis(vector)
compute_kurtosis<-function(vector) {
  vector<-na.omit(vector)
  n<-length(vector)
  x<-vector-mean(vector)
  r<-n*sum(x^4)/(sum(x^2)^2)
  y<-r*(1-1/n)^2-3
  return(y)
}
##########################################################################################
# COMPUTE STANDARD ERROR
##########################################################################################
#' @title Compute standard error
#' @param vector vector
#' @keywords functions statistical
#' @export
#' @examples
#' set.seed(1)
#' vector<-rnorm(1000)
#' compute_standard_error(vector)
compute_standard_error<-function(vector) {
  x<-na.omit(vector)
  y<-sqrt(var(x)/length(x))
  return(y)
}
##########################################################################################
# COMPUTE CONFIDENCE INTERVAL
##########################################################################################
#' @title Compute confidence interval
#' @param vector vector
#' @keywords functions statistical
#' @export
#' @examples
#' set.seed(1)
#' vector<-rnorm(1000)
#' compute_confidence_inteval(vector)
compute_confidence_inteval<-function(vector) {
  x<-na.omit(vector)
  n<-length(x)
  s<-sd(x)
  y<-qnorm(0.975)*s/sqrt(n)
  return(y)
}





