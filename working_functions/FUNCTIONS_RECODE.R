##########################################################################################
# FLATTEN LIST
##########################################################################################
#' @title Flatten two dimensional list
#' @param mydata list with two dimensions
#' @importFrom plyr ldply
#' @keywords functions
#' @export
flatten_list<-function(mydata) {
  result<-plyr::ldply(lapply(mydata,function(x) { data.frame(x,stringsAsFactors=FALSE) }))
  return(result)
}
##########################################################################################
# SWAP
##########################################################################################
#' @title Reverse a numeric vector
#' @param vector numeric
#' @keywords functions
#' @export
#' @examples
#' swap(c(1:10,1,2,3))
swap<-function(vector) {
  f<-factor(vector)
  y<-rev(levels(f))[f]
  class(y)<-class(vector)
  return(y)
}
##########################################################################################
# DUMMY ARRANGE
##########################################################################################
#' @title Takes a vector with multiple responses and dummy arranges it in a dataframe
#' @param vector Vector
#' @importFrom stringr str_split_fixed
#' @keywords functions
#' @export
#' @examples
#' vector1<-gsub(" ","",
#'              generate_multiple_responce_vector(responces=c("Agree","Hi","All"),
#'              responded=1:3,length=10),fixed=TRUE)
#' vector2<-gsub(" ","",
#'              generate_multiple_responce_vector(responces=1:4,responded=1:4,length=10),
#'              fixed=TRUE)
#' vector3<-sample(1:4,10,replace=TRUE)
#' vector4<-sample(LETTERS[1:3],10,replace=TRUE)
#' dummy_arrange(vector1)
#' dummy_arrange(vector2)
#' dummy_arrange(vector3)
#' dummy_arrange(vector4)
dummy_arrange<-function(vector) {
  result<-change_data_type(remove_nc(data.frame(stringr::str_split_fixed(vector,",",n=Inf),check.names=FALSE),value=NA),type="character")
  mydata<-change_data_type(data.frame(matrix(nrow=length(vector),ncol=ncol(result))),type="character")
  names(mydata)<-names(result)
  for (r in 1:nrow(result)) {
    for (c in 1:ncol(result)) {
      value<-result[r,c]
      if(!is.na(value))
        mydata[r,value]<-value
    }
  }
  mydata<-remove_nc(mydata,remove_rows=FALSE,aggressive=FALSE,remove_cols=TRUE,remove_zero_variance=FALSE)
  mydata[!is.na(mydata)]<-1
  mydata[is.na(mydata)]<-0
  mydata<-mydata[,sort(names(mydata))]
  return(mydata)
}
##########################################################################################
# DROP LEVELS
##########################################################################################
#' @title Drops unused factor levels
#' @param df dataframe
#' @param factor_index numeric index of factors. If NULL the function uses is.factor() to discriminate factors
#' @param minimum_frequency the minimum frequency each factor will have, levels with frequency bellow or equal to the defined frequency will be renamed "Other"
#' @keywords functions
#' @export
#' @examples
#' factor1<-factor(c(rep("A",10),rep("B",10)),levels=c("A","B","C","D"))
#' factor2<-factor(c(rep("A",10),rep("B",10)),levels=c("A","B","C","D"))
#' numeric1<-c(1:20)
#' df<-data.frame(numeric1,factor1,factor2)
#' df$factor1
#' drop_levels(df=df,minimum_frequency=9)
#' drop_levels(df=df,minimum_frequency=10)
drop_levels<-function(df,factor_index=NULL,minimum_frequency=5) {
  if(is.null(factor_index))
    factornames<-names(df)[sapply(df,is.factor)]
  else
    factornames<-names(df)[factor_index]
  for(i in factornames) {
    unique_names<-names(table(df[,i]))[table(df[,i])<=minimum_frequency]
    df[,i]<-factor(df[,i],levels=unique(c(levels(df[,i]),"Other")))
    df[,i][df[,i]%in%unique_names]<-"Other"
    df[,i]<-droplevels(df[,i])
  }
  return(df)
}
