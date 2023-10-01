##########################################################################################
# MULTIPLE GSUB
##########################################################################################
#' @title Sub for multiple patterns
#' @param mydata Character
#' @param pattern Character to search for
#' @param replacement Replacement character
#' @param ... arguments passed to gsub
#' @keywords functions strings
#' @export
#' @examples
#' mgsub(mydata="#$%^&*_+",pattern=c("%","*"),"REPLACE",fixed=TRUE)
mgsub<-function(mydata,pattern,replacement,...) {
  for (i in 1:length(pattern))
    mydata<-gsub(pattern[i],replacement,mydata,...)
  return(mydata)
}
##########################################################################################
# SPLIT STRING
##########################################################################################
#' @title Split string to dataframe
#' @param vector String
#' @param split Separation character
#' @param include_original if TRUE it will return the input on a separate collumn
#' @keywords functions strings
#' @export
#' @examples
#' string<-paste0(1:10,"/",
#'                generate_string(nchar=2,vector_length=10),"/",
#'                generate_string(nchar=2,vector_length=10),"/",
#'                generate_string(nchar=2,vector_length=10))
#' split_str(string,split="/")
split_str<-function(vector,split="/",include_original=FALSE) {
  split_str<-strsplit(vector,split=split,fixed=TRUE)
  result<-data.frame(matrix(unlist(split_str),byrow=TRUE,ncol=length(split_str[[1]])),stringsAsFactors=FALSE)
  if(include_original)
    result<-data.frame(result,vector,stringsAsFactors=FALSE)
  return(result)
}
##########################################################################################
# SPLIT STRING IN DATAFRAME
##########################################################################################
#' @title Split string in dataframe
#' @param df dataframe
#' @param split Separation character
#' @param type "row" "collumn" 
#'              if "row" it will split the string of row names and it will display it on seperate collumns 
#'              if "collumn" it will split the string of a spesified collumn and it will display it on separate collumns
#' @param index Numeric index of collumn to split. This is only relevant if type="collumn"
#' @param ... arguments passed to split_str
#' @keywords functions strings
#' @export
#' @examples
#' df<-generate_correlation_matrix()
#' string<-paste0(1:nrow(df),"/",
#'                generate_string(nchar=2,vector_length=nrow(df)),"/",
#'                generate_string(nchar=2,vector_length=nrow(df)),"/",
#'                generate_string(nchar=2,vector_length=nrow(df)))
#' row.names(df)<-string
#' split_str_df(df,split="/",type="row")
#' df[,1]<-string
#' split_str_df(df,split="/",type="collumn",index=1)
split_str_df<-function(df,split="/",type="row",index,...) {
  if(type=="row") {
    split<-split_str(vector=as.character(row.names(df)),split=split,...)
    result<-data.frame(split,df,stringsAsFactors=FALSE)
  }
  if(type=="collumn") {
    split<-split_str(vector=as.character(df[,index]),split=split,...)
    result<-data.frame(split,df,stringsAsFactors=FALSE)
  }
  return(result)
}
##########################################################################################
# RETURN RIGHT LEFT CHARACTERS
##########################################################################################
#' @title Return n characters from left or right
#' @param x Character
#' @param n Number of characters to return
#' @param type "right" "left"
#' @keywords functions strings
#' @export
#' @examples
#' sub_str("12345",n=2,type="right")
#' sub_str("12345",n=2,type="left")
sub_str<-function(x,n=2,type) {
  if(type=="right")
    result<-substr(x,nchar(x)-n+1,nchar(x))
  if(type=="left")
    result<-substr(x,1,n)
  return(result)
}
##########################################################################################
# PROPER
##########################################################################################
#' @title Capitalize first character and lowercase the rest
#' @param x Character
#' @keywords functions strings
#' @export
#' @examples
#' x<-generate_string(nchar=10,vector=LETTERS,vector_length=10)
#' proper(x)
proper<-function(x) paste0(toupper(substr(x,1,1)),tolower(substring(x,2)))
##########################################################################################
# TRIM DATAFRAME
##########################################################################################
#' @title Trim whitespace in dataframe
#' @param df dataframe
#' @keywords functions strings
#' @export
#' @examples
#' string<-data.frame(str1=rep(paste0(sample(c(LETTERS,rep(" ",10))),collapse=""),10),
#'                    str2=rep(paste0(sample(c(LETTERS,rep(" ",10))),collapse=""),10),
#'                    num1=rnorm(10),
#'                    stringsAsFactors=FALSE)
#' trim_df(string)
trim_df<-function(df) {
  df[]<-apply(df,1:2,function(x) {
    if(mode(x)=="character"){
      x<-strwrap(x)
    }
  })
  return(df)
}
##########################################################################################
# ADJUST STRING AESTHETICS
##########################################################################################
#' @title Adjust string aesthetics
#' @description Treats spesific characters such as ".", as separating characters and separates strings with space. Trims leading and trailing spaces and capitalizes the first letter of the string and lowers the rest.
#' @param vector Vector
#' @param characterlist List the list of characters to treat as separating characters
#' @param proper Logical TRUE capitalizes the first letter in sentense format
#' @importFrom stringr str_squish
#' @keywords functions strings
#' @export
#' @examples
#' vector<-c("TES.T","TES<p>T","TES&nbspT")
#' string_aes(vector=vector)
#' string_aes(vector=vector,proper=FALSE)
#' string_aes(vector=vector,proper=TRUE)
string_aes<-function(vector,characterlist=c(".","_","-",",","$","<p>","</p>","<br>","<br/>","<B>","</B>","<BR/>","|","/","&nbsp"),proper=TRUE) {
  for (i in characterlist)
    vector<-gsub(i," ",vector,fixed=TRUE)
  result<-trimws(vector,which="both")
  if(proper)
    result<-proper(vector)
  result<-stringr::str_squish(result)
  return(result)
}
##########################################################################################
# MODEL CALL TO STRING
##########################################################################################
#' @title Model call to string
#' @description Takes a call object and convert it to string
#' @param model Model object
#' @keywords functions strings
#' @export
#' @examples
#' df<-generate_correlation_matrix()
#' model<-lm(df$X1~df$X2)
#' call_to_string(model)
call_to_string<-function(model) {
  result<-toString(deparse(model$call))
  if (result=="NULL")
    result<-toString(deparse(model$Call))
  result<-gsub(" ","",result,fixed=TRUE)
  return(result)
}
##########################################################################################
# OUTPUT SEPARATOR
##########################################################################################
#' @title Output separator
#' @description Heading, main output, and instructions for output for the console environment
#' @param string Title of output
#' @param output object to print
#' @param instruction Character provided instructions regarding the output
#' @param length Numeric Length of separator measured in number of characters
#' @keywords functions strings
#' @export
#' @examples
#' output_separator(string="TEST",output="TEST",instruction="TEST",length=100)
#' output_separator(string="TEST",instruction="TEST",length=100)
#' output_separator(string="TEST",output="TEST",length=100)
#' output_separator(string="TEST")
output_separator<-function(string,output=NULL,instruction=NULL,length=getOption("width")/2) {
  separator_title<-paste0(rep("#",length),sep="",collapse="")
  separator_subtitle<-paste0(rep("#",length/2),sep="",collapse="")
  print(separator_title)
  print(string)
  print(separator_title)
  if(!is.null(instruction)){
    print(instruction)
    print(separator_subtitle)
  }
  if(!is.null(output))
    print(output)
}
