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
