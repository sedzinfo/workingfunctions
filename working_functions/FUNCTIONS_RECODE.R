##########################################################################################
# CONVERT JSON TO DATAFRAME PAVLOS STABOULIDES FUNCTIONS FROM READJS R
##########################################################################################
# json2DataFrame<-function(mydata) {
#   result<-plyr::ldply(lapply(mydata,function(x) {
#     data.frame(x,stringsAsFactors=FALSE)
#   }))
#   row.names(result)<-result$.id
#   result$.id<-NULL
#   return(result)
# }
##########################################################################################
# CONVERT JSON TO DATAFRAME
##########################################################################################
#' @title Convert a json object to dataframe
#' @param mydata json object
#' @param fast if TRUE uses a faster algorythm
#' @param row_to_collumn if TRUE it will return row in first collumn
#' @importFrom plyr ldply
#' @importFrom plyr rbind.fill
#' @keywords functions
#' @export
#' @examples
#' file<-"/opt/repo/rworking/projects/pcentral/data/aristotle-accountests-6-17.json"
#' pcentral<-rjson::fromJSON(file=file)
#' biodata<-json2dataframe(mydata=pcentral$itemresponses$biodata)
#' biodata<-json2dataframe(mydata=pcentral$itemresponses$biodata,fast=TRUE)
json2dataframe<-function(mydata,fast=FALSE,row_to_collumn=FALSE) {
  if (fast) {
    result<-plyr::ldply(lapply(mydata,function(x) {
      data.frame(x,stringsAsFactors=FALSE)
    }))
    row.names(result)<-result$.id
    result$.id<-NULL
  } else {
    row_names<-c()
    result<-data.frame()
    mydata[sapply(mydata,is.null)]<-NULL
    pb<-txtProgressBar(min=0,max=length(mydata),style=3)
    for(i in 1:length(mydata)) {
      setTxtProgressBar(pb,i)
      if(length(unlist((mydata)[i]))>0) {
        row_names<-c(row_names,names(mydata)[i])
        result<-plyr::rbind.fill(result,
                                 data.frame(t(unlist(mydata[[i]])),
                                            stringsAsFactors=FALSE))
      }
    }
    row.names(result)<-row_names
    close(pb)
  }
  if(row_to_collumn)
    result<-data.frame(names=row.names(result),result,stringsAsFactors=FALSE)
  return(result)
}
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
# GET CODING
##########################################################################################
#' @title Takes question response options from a json object and returns a list of dataframes mapping response option number to response option text
#' @param coding_list Json object containing question options
#' @keywords functions
#' @export
#' @examples
#' file<-"/opt/repo/rworking/projects/pcentral/data/aristotle-accountests-6-17.json"
#' coding_list<-rjson::fromJSON(file=file)$instr_descr_options$biodata
#' get_coding(coding_list)
get_coding<-function(coding_list) {
  coding<-list()
  for (i in 1:length(coding_list)){
    result<-json2dataframe(coding_list[[i]],fast=TRUE)
    coding[[i]]<-data.frame(Question=names(coding_list)[i],Text=row.names(result),y=result,row.names=NULL,stringsAsFactors=FALSE)
    # if(Hmisc::all.is.numeric(coding[[i]][,1])==TRUE)
    #   names(coding[[i]])[1]<-"x"
    # if(Hmisc::all.is.numeric(coding[[i]][,1])==FALSE)
    #   names(coding[[i]])[1]<-"Text"
    # if(Hmisc::all.is.numeric(coding[[i]][,2])==TRUE)
    #   names(coding[[i]])[2]<-"x"
    # if(Hmisc::all.is.numeric(coding[[i]][,2])==FALSE)
    #   names(coding[[i]])[2]<-"Text"
  }
  names(coding)<-names(coding_list)
  return(coding)
}
##########################################################################################
# RECODE DATAFRAME
##########################################################################################
#' @title Takes a dataframe mapping response option number to response option text and recodes a numeric response vector to text response option for the entire dataframe
#' @param df dataframe
#' @param coding_map dataframe containg map between coding and text
#' @keywords functions
#' @export
#' @examples
#' file<-"/opt/repo/rworking/projects/pcentral/data/aristotle-accountests-6-17.json"
#' pcentral<-rjson::fromJSON(file=file)
#' df<-json2dataframe(pcentral$itemresponses$biodata)
#' coding_map<-get_coding(pcentral$instr_descr_options$biodata)
#' names(df)[c(2,11,9,3,5,6,10,12,7)]<-names(coding_map)
#' recode_dataframe(df=df,coding_map=coding_map)
#' file<-"/opt/repo/rworking/projects/pcentral/data/aristotle-accountests-6-17.json"
#' pcentral<-rjson::fromJSON(file=file)
#' coding_map<-get_coding(pcentral$instr_descr_options$biodata)$Education
#' vector<-json2dataframe(pcentral$itemresponses$biodata)$education[1:50]
#' recode_dataframe(df=vector,coding_map=coding_map)
recode_dataframe<-function(df,coding_map) {
  if(length(intersect(names(df),names(coding_map)))==0) {
    coding_map<-list(X1=coding_map)
    df<-data.frame(X1=df,stringsAsFactors=FALSE)
  }
  names_intersect<-intersect(names(df),names(coding_map))
  for(intersect in names_intersect) {
    # df[,intersect]<-recode_vector(df[,intersect],coding_map[[intersect]])
    coding_map[[intersect]]<-sapply(coding_map[[intersect]],as.character)
    df[,intersect]<-trimws(as.character(df[,intersect]))
    for (i in 1:nrow(coding_map[[intersect]])) {
      df[,intersect][df[,intersect]==coding_map[[intersect]][,"x"][i]]<-coding_map[[intersect]][,"Text"][i]
    }
    df[,intersect]<-factor(df[,intersect],levels=unique(coding_map[[intersect]][,"Text"]))
  }
  return(df)
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
