##########################################################################################
# KEYS
##########################################################################################
#' @title Convert key to index list
#' @param key a vector indicating the dimension of each question. The order of the elements in the key represents the order of the questions, the numeric values represent the dimension the question belongs to
#' @keywords functions keys dimensions
#' @export
#' @examples
#' key<-c(1,2,3,4,5,1,2,3,4,5)
#' questions_by_keys(key)
questions_by_keys<-function(key) {
  keys<-list()
  for (i in 1:max(key))
    keys[[i]]<-which(match(key,i) %in% key)
  return(keys)
}
##########################################################################################
# KEYS
##########################################################################################
#' @title Question dimension table
#' @description Return a dataframe with the order of the questions, their respective dimensions, and the description of the dimensions
#' @inheritParams questions_by_keys
#' @param dimensions dimension names
#' @param elaborate_dimensions full dimension names
#' @param questions question names
#' @keywords functions keys dimensions
#' @export
#' @examples
#' key<-c(1,2,3,4,5,1,2,3,4,5)
#' dimensions<-paste0("Dimension",1:10)
#' elaborate_dimensions<-paste0("Elaborated_Dimension",1:10)
#' questions<-paste0("Question",1:65)
#' questions_dimensions_dataframe(key,dimensions,elaborate_dimensions,questions)
questions_dimensions_dataframe<-function(key,dimensions,elaborate_dimensions,questions) {
  key_list<-questions_by_keys(key)
  questions_dataframe<-questions_bind_dataframe<-list()
  for (i in 1:length(key_list)) {
    order<-as.data.frame(key_list[[i]])
    dimension<-as.data.frame(rep(dimensions[[i]],length(key_list[[i]])))
    elaborate_dimension<-as.data.frame(rep(elaborate_dimensions[[i]],length(key_list[[i]])))
    questions_in_dimension<-as.data.frame(questions[key_list[[i]]])
    names(questions_in_dimension)<-"QUESTION"
    names(dimension)<-"DIMENSION"
    names(elaborate_dimension)<-"ELABORATE DIMENSION"
    names(order)<-"ORDER"
    questions_bind_dataframe<-cbind(order,dimension,elaborate_dimension,questions_in_dimension)
    questions_dataframe<-rbind(questions_dataframe,questions_bind_dataframe)
  }
  return(questions_dataframe)
}
##########################################################################################
# QUESTIONNAIRE REFERENCE
##########################################################################################
#' @title Questionnaire reference
#' @param mydata json output
#' @param questionnaire name of questionnaire to extract reference from
#' @param tag_remove character
#' @importFrom plyr rbind.fill
#' @keywords functions keys dimensions
#' @export
#' @examples
#' stud<-rjson::fromJSON(file="/opt/repo/rworking/projects/jacob/stud.json")
#' iboSTUDPULS2<-json2dataframe(stud$`items-by-order`$STUDPULS2)
#' get_questionaire_reference(stud,"STUDPULS2","DTL2-ST")
get_questionaire_reference<-function(mydata,questionnaire,tag_remove="") {
  ibo<-ibt<-im<-data.frame()
  for (i in 1:length(mydata$`items-by-order`[[questionnaire]])) {
    ibo<-plyr::rbind.fill(ibo,data.frame(tag=mydata$`items-by-order`[[questionnaire]][[i]]$instrdtl_tag,option=names(mydata$`items-by-order`[[questionnaire]][[i]]$options),
                                         json2dataframe(mydata$`items-by-order`[[questionnaire]][[i]]$options),stringsAsFactors=FALSE))
    ibt<-plyr::rbind.fill(ibt,data.frame(tag=mydata$`items-by-tag`[[questionnaire]][[i]]$text,option=names(mydata$`items-by-tag`[[questionnaire]][[i]]$options),
                                         json2dataframe(mydata$`items-by-tag`[[questionnaire]][[i]]$options),stringsAsFactors=FALSE))
    im<-plyr::rbind.fill(im,data.frame(tag=mydata$`items-mixed`[[questionnaire]][[i]]$reference,option=names(mydata$`items-mixed`[[questionnaire]][[i]]$options),
                                       json2dataframe(mydata$`items-mixed`[[questionnaire]][[i]]$options),stringsAsFactors=FALSE))
  }
  attributes<-data.frame(names=row.names(json2dataframe(mydata$attributes)),json2dataframe(mydata$attributes),stringsAsFactors=FALSE)
  reference<-data.frame(ibo,ibt,im,check.names=FALSE,stringsAsFactors=FALSE)
  reference<-reference[,unique(names(reference))]
  reference$tag<-gsub(tag_remove,"",reference$tag,fixed=TRUE)
  result<-list(items_by_order=ibo,items_by_tag=ibt,items_mixed=im,attributes=attributes,reference=reference)
  return(result)
}
##########################################################################################
# GET REVERSALS
##########################################################################################
#' @title Questionnaire reference
#' @param df dataframe from get_questionaire_reference
#' @importFrom plyr rbind.fill
#' @keywords functions keys dimensions
#' @export
#' @examples
#' stud<-rjson::fromJSON(file="/opt/repo/rworking/projects/jacob/stud.json")
#' res<-get_questionaire_reference(stud,"STUDPULS2","DTL2-ST")
#' ibo<-res$items_by_order[!res$items_by_order$instrdtl_value %in% "-9999.00",]
#' ibo$instrdtl_value<-as.numeric(ibo$instrdtl_value)
#' get_reversals(ibo)
get_reversals<-function(df) {
  df$instrdtl_value<-as.numeric(trimws(as.character(df$instrdtl_value)))
  key<-data.frame()
  for (i in unique(df$tag)){
    item<-df[df$tag %in% i,]
    if(item$instrdtl_value[1]<item$instrdtl_value[2])
      key<-plyr::rbind.fill(key,data.frame(tag=i,key=1))
    else
      key<-plyr::rbind.fill(key,data.frame(tag=i,key=-1))
  }
  df<-change_data_type(merge(key,df,by="tag",all=TRUE),type="character")
  result<-list(key=key,items=df)
  return(result)
}

