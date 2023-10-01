##########################################################################################
# ROUND DATAFRAME
##########################################################################################
#' @title Round dataframe
#' @description It only processes numeric values in a dataframe
#' @param df dataframe
#' @param digits decimal points to return. It works only with "round" type
#' @param type "round" "ceiling" "floor" "tenth"
#' @keywords functions
#' @export
#' @examples
#' round_dataframe(df=change_data_type(df=mtcars,type="factor"),digits=0)
#' round_dataframe(df=change_data_type(df=mtcars,type="character"),digits=0)
#' round_dataframe(df=mtcars,digits=0)
#' round_dataframe(df=mtcars,digits=0,type="ceiling")
#' round_dataframe(df=mtcars,digits=0,type="floor")
#' round_dataframe(df=mtcars*100,digits=2,type="tenth")
round_dataframe<-function(df,digits=0,type="round") {
  if(type=="round")
    df[,sapply(df,is.numeric)]<-round(df[,sapply(df,is.numeric)],digits=digits)
  if(type=="ceiling")
    df[,sapply(df,is.numeric)]<-ceiling(df[,sapply(df,is.numeric)])
  if(type=="floor")
    df[,sapply(df,is.numeric)]<-floor(df[,sapply(df,is.numeric)])
  if(type=="tenth")
    df[,sapply(df,is.numeric)]<-round(df[,sapply(df,is.numeric)]/10,digits=digits)
  return(df)
}
##########################################################################################
# CHANGE DATA TYPE OF COLLUMNS IN DATA FRAME
##########################################################################################
#' @title dataframe data type transformations
#' @param df dataframe
#' @param type "character" "numeric" "factor" "factor_character" "character_factor" \cr
#'             For "factor_character" if factors are found, are converted to characters \cr
#'             For "character_factor" if characters are found, are converted to factors
#' @keywords functions
#' @export
#' @examples
#' cdf(df=change_data_type(df=mtcars,"character"))
#' cdf(df=change_data_type(df=mtcars,"numeric"))
#' cdf(df=change_data_type(df=mtcars,"factor"))
#' df<-change_data_type(df=mtcars,"factor")
#' cdf(df=change_data_type(df=df,"factor_character"))
change_data_type<-function(df,type) {
  if(type=="character")
    df[]<-lapply(df,function(x) as.character(trimws(x,which="both",whitespace="[\t\r\n]")))
  if(type=="numeric")
    df[]<-lapply(df,function(x) as.numeric(trimws(as.character(x),which="both",whitespace="[\t\r\n]")))
  if(type=="factor")
    df[]<-lapply(df,as.factor)
    if(type=="factor_character")
    df[]<-apply(df,1:2,function(x) {if(is.factor(x)) as.character(x) else x})
  if(type=="character_factor")
    df[]<-apply(df,1:2,function(x) {if(is.character(x)) factor(x) else x})
  return(df)
}
##########################################################################################
# RBIND ALL
##########################################################################################
#' @title rbind dataframes or matrices with different lengths or collumn names
#' @param df1 dataframe or matrix
#' @param df2 dataframe or matrix
#' @keywords functions
#' @export
#' @examples
#' df1<-generate_correlation_matrix(n=10)
#' df2<-generate_correlation_matrix(n=10)
#' names(df2)[4]<-"X11"
#' rbind_all(df1=df1,df2=df2)
#' row.names(df1)<-21:30
#' rbind_all(df1=df1,df2=df2)
rbind_all<-function(df1,df2) {
  df1_diff<-setdiff(colnames(df1),colnames(df2))
  df2_diff<-setdiff(colnames(df2),colnames(df1))
  df1[,c(as.character(df2_diff))]<-NA
  df2[,c(as.character(df1_diff))]<-NA
  row_names<-c(row.names(df1),row.names(df2))
  result<-rbind(df1,df2)
  ndf1<-deparse(substitute(df1))
  ndf2<-deparse(substitute(df2))
  row_names_df1<-row.names(df1)
  row_names_df2<-row.names(df2)
  if(!TRUE%in%duplicated(row_names))
    row.names(result)<-c(row_names_df1,row_names_df2)
  return(result)
}
##########################################################################################
# REMOVE VALUES THAT CANNOT BE CALCULATED
##########################################################################################
#' @title Replace remove non computable values
#' @details Non computable values are NA, NAN, inf and empty cells.
#' @param df dataframe
#' @param value replacement
#' @param remove_rows if TRUE it will remove rows with non computable values
#' @param aggressive if TRUE it will remove entire row if a single non computable value exists \cr
#'                   if FALSE it will remove row if all values are non computable
#' @param remove_cols if TRUE it will remove collumns with non computable values
#' @param remove_zero_variance if TRUE it will remove collumns with no variance
#' @note This function internally replaces non computable values with the value choosen 
#'       the default value is NA. Then it removes rows and collumns with NA values or zero variance
#' @keywords functions
#' @export
#' @examples
#' df<-mtcars
#' df[1,]<-as.numeric(NaN)
#' df[2,]<-as.numeric(Inf)
#' df[3,]<-as.numeric(-Inf)
#' df[4,]<-as.numeric(NA)
#' df[5,]<-""
#' remove_nc(df=df,value=NA)
#' cdf(remove_nc(df=df,value=NA))
#' df<-generate_missing(mtcars,missing=5)
#' remove_nc(df,remove_rows=TRUE,aggressive=FALSE)
#' remove_nc(df,remove_rows=TRUE,aggressive=TRUE)
#' df<-generate_missing(generate_correlation_matrix(nrows=5),missing=2)
#' df$X2<-NA
#' df$X3<-1
#' remove_nc(df,remove_cols=TRUE,remove_zero_variance=FALSE)
#' remove_nc(df,remove_cols=TRUE,remove_zero_variance=TRUE)
remove_nc<-function(df,value=NA,remove_rows=FALSE,aggressive=FALSE,remove_cols=FALSE,remove_zero_variance=FALSE) {
  df[is.na(df)]<-value
  # df[sapply(df,is.nan)]<-value
  # df[sapply(df,is.infinite)]<-value
  df[df==Inf]<-value
  df[df==-Inf]<-value
  df[df==NaN]<-value
  df[df==""]<-value
  if(remove_rows) {
    if(aggressive)
      df<-df[rowSums(is.na(df))==0,]
    else
      df<-df[apply(df,1,function(x) any(!is.na(x))),]
  }
  if(remove_cols) {
    df<-df[,colSums(!is.na(df))>0]
    if(remove_zero_variance) {
      #df<-df[,!0==apply(df,2,function(x) stats::sd(x,na.rm=TRUE))]
      df<-df[,apply(df,2,function(x) length(table(x)))>1]
    }
  }
  return(df)
}
##########################################################################################
# REPLACE NA WITH PREVIOUS CELLS
##########################################################################################
#' @title Replace NA with the previous element in a vector
#' @param vector Vector
#' @keywords functions
#' @export
#' @examples
#' df1<-generate_missing(rnorm(10),missing=5)
#' df2<-generate_missing(rnorm(10),missing=5)
#' df3<-generate_missing(rnorm(10),missing=5)
#' df4<-generate_missing(rnorm(10),missing=5)
#' df5<-generate_missing(rnorm(10),missing=5)
#' df<-data.frame(df1,df2,df3,df4,df5)
#' row.names(df)<-paste0("A",row.names(df))
#' replace_na_with_previous(df1)
#' df[]<-lapply(df,replace_na_with_previous)
replace_na_with_previous<-function(vector) {
  if(is.na(vector[1]))
    vector[1]<-na.omit(vector)[1]
  for(i in 1:length(vector)) {
    if((i-1)>0){
      if(is.na(vector[i]))
        vector[i]<-vector[i-1]
    }
  }
  return(vector)
}
##########################################################################################
# BIND DATAFRAMES OR VECTORS OF UNEQUAL ROW LENGTHS
##########################################################################################
#' @title pad NA's to collumns in dataframe
#' @param df dataframe
#' @param rowsneeded Numeric number of rows needed
#' @param first Boolean
#' @author Ananda Mahto
#' @keywords functions
padNA<-function(df,rowsneeded,first=TRUE) {
  column_names=colnames(df)
  rowsneeded=rowsneeded-nrow(df)
  temp2=setNames(data.frame(matrix(rep(NA,length(column_names)*rowsneeded),ncol=length(column_names))),column_names)
  if (isTRUE(first)) rbind(df,temp2)
  else rbind(temp2,df)
}
#' @title Get the names of objects in the arguments
#' @param ... objects
#' @author Ananda Mahto
#' @keywords functions
dotnames<-function(...) {
  vnames<-as.list(substitute(list(...)))[-1L]
  result<-unlist(lapply(vnames,deparse),FALSE,FALSE)
  return(result)
}
#' @title cbind dataframes with unequal lengths or row lengths
#' @param first Logical
#' @param ... dataframes or vectors to bind
#' @importFrom stats setNames
#' @author Ananda Mahto
#' @keywords functions
#' @export
#' @examples
#' c_bind(rnorm(10),rnorm(11),rnorm(12),rnorm(13))
c_bind<-function(...,first=TRUE) {
  Names<-dotnames(...)
  datalist<-stats::setNames(list(...),Names)
  nrows<-max(sapply(datalist,function(x) 
    ifelse(is.null(dim(x)),length(x),nrow(x))))
  datalist<-lapply(seq_along(datalist),function(x) {
    z<-datalist[[x]]
    if (is.null(dim(z))) {
      z<-setNames(data.frame(z),Names[x])
    } else {
      if (is.null(colnames(z))) {
        colnames(z)<-paste(Names[x],sequence(ncol(z)),sep="_")
      } else {
        colnames(z)<-paste(Names[x],colnames(z),sep="_")
      }
    }
    padNA(z,rowsneeded=nrows,first=first)
  })
  do.call(cbind,datalist)
}
##########################################################################################
# COMBINATIONS
##########################################################################################
#' @title Produce combinations for comparisons from dataframe names
#' @param df dataframe
#' @param all_orders if TRUE the order of combination is considered 
#' i.e. the combination X1 X2 also appears as X2 X1 if FALSE it is assumed that X1 X2 and X2 X1 are the same and only one of them appears
#' @importFrom utils combn
#' @keywords functions
#' @export
#' @examples
#' comparison_combinations(generate_correlation_matrix(n=10)[,1:4])
comparison_combinations<-function(df,all_orders=TRUE) {
  combinations<-data.frame(t(utils::combn(names(df),2)),stringsAsFactors=FALSE)
  names(combinations)<-c("X1","X2")
  if(all_orders) {
    combinations<-rbind(combinations,data.frame(X1=combinations$X2,X2=combinations$X1))
    combinations<-combinations[order(combinations$X1,combinations$X2),]
  }
  return(combinations)
}
##########################################################################################
# MINIMUM MAXIMUM INDEX OF A VECTOR
##########################################################################################
#' @title Return the minimum and maximum index of a vector
#' @param vector Vector
#' @keywords functions
#' @export
#' @examples
#' vector1<-c(1,2,3,4,5,4,3,2,1)
#' vector2<-c(1,2,3,4,5,5,3,2,1)
#' vector3<-c(1,2,3,5,5,4,3,2,1)
#' vector4<-c(1,2,3,4,6,4,3,2,1)
#' vector5<-c(1,6,3,4,6,4,3,2,1)
#' vector<-vector1
#' which(vector==max(vector),arr.ind=TRUE)
#' which(vector==min(vector),arr.ind=TRUE)
#' min_max_index(vector1)
#' min_max_index(vector2)
#' min_max_index(vector3)
#' min_max_index(vector4)
#' min_max_index(vector5)
min_max_index<-function(vector){
  max_index<-which(vector==max(vector),arr.ind=TRUE)
  min_index<-which(vector==min(vector),arr.ind=TRUE)
  result<-list(max_index=max_index,min_index=min_index)
  return(result)
}
