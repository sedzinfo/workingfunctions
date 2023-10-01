##########################################################################################
# DATAFRAME TO EXCEL GENERIC
##########################################################################################
#' @title Generic function for creating workbooks and worksheets
#' @description This function is used by excel_matrix and excel_critical_value functions
#' @param df dataframe or matrix
#' @param workbook workbook
#' @param sheet sheet
#' @param title title
#' @param comment comment
#' @param numFmt number formatting
#' @import openxlsx
#' @keywords functions
#' @export
#' @examples
#' comment<-list(mpg="Miles/(US) gallon",
#'               cyl="Number of cylinders",
#'               disp="Displacement (cu.in.)",
#'               hp="Gross horsepower",
#'               drat="Rear axle ratio",
#'               wt="Weight (1000 lbs)",
#'               qsec="1/4 mile time",
#'               vs="Engine (0=V-shaped,1=straight)",
#'               am="Transmission (0=automatic,1=manual)",
#'               gear="Number of forward gears",
#'               carb="Number of carburetors",
#'               extra_comment1="test1",
#'               extra_comment2="test2")
#' mtcor<-data.frame(cor(mtcars))
#' filename<-"excel_generic.xlsx"
#' if (file.exists(filename)) file.remove(filename)
#' wb<-openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb,"sheet")
#' openxlsx::addWorksheet(wb,"correlation")
#' openxlsx::writeData(wb,sheet="sheet",x=mtcars,colNames=TRUE,rowNames=TRUE)
#' openxlsx::writeData(wb,sheet="correlation",x=mtcor,colNames=TRUE,rowNames=TRUE)
#' excel_generic_format(df=mtcars,workbook=wb,sheet="sheet",title="test",
#'                      comment=comment,numFmt="#0.00")
#' excel_generic_format(df=mtcor,workbook=wb,sheet="correlation",title="correlation",
#'                      comment=comment,numFmt="#0.00")
#' openxlsx::saveWorkbook(wb,invisible(paste(filename)),TRUE)
excel_generic_format<-function(df,workbook,sheet="output",title=NULL,comment=NULL,numFmt="#0.00") {
  comment<-comment[intersect(names(comment),names(df))]
  if(!is.null(title)) {
    comment_text<-createComment(comment=title,author=Sys.getenv("USER"),style=NULL,visible=FALSE)
    writeComment(workbook,sheet=sheet,col=1,row=1,comment=comment_text)
  }
  if(!is.null(comment)) {
    for(i in names(comment)) {
      comment_text<-createComment(comment=comment[[i]],author=Sys.getenv("USER"),style=NULL,visible=FALSE,width=100)
      writeComment(workbook,sheet=sheet,col=which(i==names(df))+1,row=1,comment=comment_text)
    }
  }
  integer_names<-names(which(sapply(df,function(y) all(if(is.numeric(y)) y==round(y)))==TRUE))
  non_integer_names<-names(which(sapply(df,function(y) !all(if(is.numeric(y)) y==round(y)))==TRUE))
  freezePane(workbook,sheet,firstActiveRow=1,firstActiveCol=1,firstRow=TRUE,firstCol=TRUE)
  modifyBaseFont(workbook,fontSize=10,fontColour="#000000",fontName="Liberation Sans")
  content_style<-createStyle(border="TopBottomLeftRight",borderColour="gray",borderStyle="thin",valign="bottom",wrapText=FALSE,numFmt=numFmt)
  content_style_integer<-createStyle(border="TopBottomLeftRight",borderColour="gray",borderStyle="thin",valign="bottom",wrapText=FALSE,numFmt="#0")
  header_style<-createStyle(border="TopBottomLeftRight",borderColour="black",borderStyle="thin",valign="bottom",wrapText=FALSE,bgFill=NULL,numFmt=numFmt)
  for (i in non_integer_names)
    addStyle(workbook,sheet=sheet,style=content_style,rows=2:(nrow(df)+1),cols=which(names(df)==i)+1,gridExpand=TRUE,stack=TRUE)
  for (i in integer_names)
    addStyle(workbook,sheet=sheet,style=content_style_integer,rows=2:(nrow(df)+1),cols=which(names(df)==i)+1,gridExpand=TRUE,stack=TRUE)
  addStyle(workbook,sheet=sheet,style=header_style,rows=1:(nrow(df)+1),cols=1,gridExpand=TRUE,stack=TRUE)
  addStyle(workbook,sheet=sheet,style=header_style,rows=1,cols=1:(length(df)+1),gridExpand=TRUE,stack=TRUE)
  removeColWidths(workbook,sheet,cols=1:(length(df)+1))
  setColWidths(workbook,sheet=sheet,cols=1:(length(df)+1),widths="auto")
}
##########################################################################################
# DATAFRAME TO EXCEL MATRIX
##########################################################################################
#' @title Write matrix or dataframe to excel sheet
#' @description Usefull for corellation matrices. It uses conditional formatting for matrices,which outlines high and low values using background color
#' @inheritParams excel_generic_format
#' @param conditional_formatting if TRUE it will use conditional formatting
#' @param diagonal if TRUE it will add background fill to diagonal
#' @param diagonal_length length of diagonal for background fill
#' @import openxlsx
#' @keywords functions
#' @export
#' @examples
#' comment<-list(mpg="Miles/(US) gallon",
#'               cyl="Number of cylinders",
#'               disp="Displacement (cu.in.)",
#'               hp="Gross horsepower",
#'               drat="Rear axle ratio",
#'               wt="Weight (1000 lbs)",
#'               qsec="1/4 mile time",
#'               vs="Engine (0=V-shaped,1=straight)",
#'               am="Transmission (0=automatic,1=manual)",
#'               gear="Number of forward gears",
#'               carb="Number of carburetors",
#'               extra_comment1="test1",
#'               extra_comment2="test2")
#' mtcor<-data.frame(cor(mtcars))
#' filename<-"excel_matrix.xlsx"
#' if (file.exists(filename)) file.remove(filename)
#' wb<-openxlsx::createWorkbook()
#' excel_matrix(mtcars,wb,sheet="matrix",comment=comment,
#'              conditional_formatting=TRUE,diagonal=FALSE)
#' excel_matrix(mtcars,wb,sheet="diagonal_non_square",comment=comment,
#'              conditional_formatting=FALSE,diagonal=TRUE)
#' excel_matrix(mtcars[1:10,1:10],wb,sheet="diagonal_square",comment=comment[1:10],
#'              conditional_formatting=FALSE,diagonal=TRUE)
#' excel_matrix(mtcars,wb,sheet="matrix_diagonal_non_square",comment=comment,
#'              conditional_formatting=TRUE,diagonal=TRUE)
#' excel_matrix(mtcars[1:10,1:10],wb,sheet="matrix_diagonal_square",comment=comment[1:10],
#'              conditional_formatting=TRUE,diagonal=TRUE)
#' excel_matrix(mtcor,wb,sheet="r",comment=comment,
#'              conditional_formatting=FALSE,diagonal=FALSE)
#' excel_matrix(mtcor,wb,sheet="conditional_formatting_r",comment=comment,
#'              conditional_formatting=TRUE,diagonal=TRUE)
#' openxlsx::saveWorkbook(wb,invisible(paste(filename)),TRUE)
excel_matrix<-function(df,workbook,sheet="output",title=NULL,comment=NULL,numFmt="#0.00",conditional_formatting=FALSE,diagonal=FALSE,diagonal_length=nrow(df)) {
  df<-data.frame(df,stringsAsFactors=FALSE,check.names=FALSE)
  openxlsx::addWorksheet(workbook,sheet)
  openxlsx::writeData(workbook,sheet,df,rowNames=TRUE)
  excel_generic_format(df=df,workbook=workbook,sheet=sheet,title=title,comment=comment,numFmt=numFmt)
  if(conditional_formatting)
    openxlsx::conditionalFormatting(workbook,sheet=sheet,cols=1:length(df)+1,rows=1:nrow(df)+1,style=c("red","yellow","green"),type="colourScale")
  if(diagonal) {
    diagonal_style<-openxlsx::createStyle(fgFill="red")
    if(nrow(df)==ncol(df)) {
      for (i in 1:diagonal_length+1) {
        openxlsx::addStyle(workbook,sheet=sheet,style=diagonal_style,rows=i,cols=i,gridExpand=TRUE,stack=FALSE)
      }
    }
  }
}
##########################################################################################
# DATAFRAME TO EXCEL CRITICAL VALUE
##########################################################################################
#' @title Write matrix or dataframe to excel sheet
#' @description Usefull for generic data where conditional formating of a spesific collumn is required
#' @inheritParams excel_generic_format
#' @param critical list in the form of (collumn1=critical_value1,collumn2=critical_value2...)
#' @keywords functions
#' @export
#' @examples
#' comment<-list(mpg="Miles/(US) gallon",
#'               cyl="Number of cylinders",
#'               disp="Displacement (cu.in.)",
#'               hp="Gross horsepower",
#'               drat="Rear axle ratio",
#'               wt="Weight (1000 lbs)",
#'               qsec="1/4 mile time",
#'               vs="Engine (0=V-shaped,1=straight)",
#'               am="Transmission (0=automatic,1=manual)",
#'               gear="Number of forward gears",
#'               carb="Number of carburetors",
#'               extra_comment1="test1",
#'               extra_comment2="test2")
#' filename<-"excel_critical_value.xlsx"
#' if (file.exists(filename)) file.remove(filename)
#' wb<-openxlsx::createWorkbook()
#' df<-generate_missing(generate_correlation_matrix())
#' critical<-list(X1="<0.05",X5="<0")
#' excel_critical_value(df=df,workbook=wb,sheet="critical",comment=list(X1="test"),
#'                      numFmt="#0.00",critical=critical)
#' openxlsx::saveWorkbook(wb,invisible(paste(filename)),TRUE)
#' filename<-"excel_critical_value_comment.xlsx"
#' if (file.exists(filename)) file.remove(filename)
#' wb<-openxlsx::createWorkbook()
#' df<-generate_missing(mtcars)
#' critical<-list(mpg=">20",am="=0")
#' excel_critical_value(df=df,workbook=wb,sheet="critical",comment=comment,
#'                      numFmt="#0.00",critical=critical)
#' openxlsx::saveWorkbook(wb,invisible(paste(filename)),TRUE)
#' filename<-"excel_critical_value_comment_min_max.xlsx"
#' if (file.exists(filename)) file.remove(filename)
#' wb<-openxlsx::createWorkbook()
#' df<-generate_missing(mtcars)
#' critical<-list(mpg=c(">20","<11"),am="=0")
#' excel_critical_value(df=df,workbook=wb,sheet="critical",comment=comment,
#'                      numFmt="#0.00",critical=critical)
#' openxlsx::saveWorkbook(wb,invisible(paste(filename)),TRUE)
excel_critical_value<-function(df,workbook,sheet="output",title=NULL,comment=NULL,numFmt="#0.00",critical=NULL) {
  openxlsx::addWorksheet(workbook,sheet)
  openxlsx::writeData(workbook,sheet,df,rowNames=TRUE)
  excel_generic_format(df=df,workbook=workbook,sheet=sheet,title=title,comment=comment,numFmt=numFmt)
  if(!is.null(critical)) {
    for (i in names(critical)) {
      df<-remove_nc(df,value=NA,remove_rows=FALSE,aggressive=FALSE,remove_cols=FALSE,remove_zero_variance=FALSE)
      if (length(critical[[i]])>1) {
        critical_value_min<-createStyle(bgFill="red",numFmt=numFmt)
        critical_value_max<-createStyle(bgFill="purple",numFmt=numFmt)
        rows<-which(!is.na(df[,i]))+1
        for (r in rows) {
          conditionalFormatting(workbook,sheet,cols=which(i==names(df))+1,rows=r,type="expression",rule=critical[[i]][1],style=critical_value_min)
          conditionalFormatting(workbook,sheet,cols=which(i==names(df))+1,rows=r,type="expression",rule=critical[[i]][2],style=critical_value_max)
        }
      } else {
        critical_value<-createStyle(bgFill="red",numFmt=numFmt)
        rows<-which(!is.na(df[,i]))+1
        for (r in rows) {
          conditionalFormatting(workbook,sheet,cols=which(i==names(df))+1,rows=r,type="expression",rule=critical[[i]],style=critical_value)
        }
      }
    }
  }
}
##########################################################################################
# DATAFRAME TO EXCEL CONFUSION MATRIX
##########################################################################################
#' @title Write matrix or dataframe to excel sheet
#' @description Usefull for correlation matrices since it uses conditional formatting for matrices
#' @param df dataframe or matrix
#' @param workbook workbook
#' @param title comment
#' @import openxlsx
#' @keywords functions
#' @export
#' @examples
#' filename<-"excel_confusion_matrix.xlsx"
#' if (file.exists(filename)) file.remove(filename)
#' observed<-factor(round(rnorm(10000,m=10,sd=1)))
#' predicted<-factor(round(rnorm(10000,m=10,sd=1)))
#' confusion(observed,predicted)
#' cm<-confusion_matrix_percent(observed,predicted)
#' wb<-openxlsx::createWorkbook()
#' excel_confusion_matrix(cm,wb)
#' openxlsx::saveWorkbook(wb,invisible(paste(filename)),TRUE)
excel_confusion_matrix<-function(df,workbook,title="Rows: Expected Collumns: Observed") {
  numFmt="#0"
  sheet="Confusion Matrix"
  comment=NULL
  content_style1<-createStyle(border="TopBottomLeftRight",borderColour="gray",borderStyle="thin",valign="bottom",wrapText=FALSE,numFmt=numFmt,fgFill="yellow")
  content_style2<-createStyle(border="TopBottomLeftRight",borderColour="gray",borderStyle="thin",valign="bottom",wrapText=FALSE,numFmt="#0.00",fgFill="yellow")
  df<-change_data_type(df,type="numeric")
  openxlsx::addWorksheet(workbook,sheet=sheet)
  openxlsx::writeData(workbook,sheet=sheet,df,rowNames=TRUE)
  excel_generic_format(df=df,workbook=workbook,sheet=sheet,title=title,comment=comment,numFmt=numFmt)
  conditionalFormatting(workbook,sheet=sheet,cols=1:(length(df)-1),rows=1:(nrow(df)-1),style=c("white","green"),type="colourScale",numFmt=numFmt)
  addStyle(workbook,sheet=sheet,style=content_style1,rows=nrow(df),cols=2:(ncol(df)+1),gridExpand=TRUE,stack=TRUE)
  addStyle(workbook,sheet=sheet,style=content_style1,rows=2:(nrow(df)+1),cols=ncol(df),gridExpand=TRUE,stack=TRUE)
  addStyle(workbook,sheet=sheet,style=content_style2,rows=nrow(df)+1,cols=2:(ncol(df)+1),gridExpand=TRUE,stack=TRUE)
  addStyle(workbook,sheet=sheet,style=content_style2,rows=2:(nrow(df)+1),cols=ncol(df)+1,gridExpand=TRUE,stack=TRUE)
  removeColWidths(workbook,sheet,cols=1:(length(df)+1))
  setColWidths(workbook,sheet=sheet,cols=1:(length(df)+1),widths="auto")
}
##########################################################################################
# DATAFRAME TO EXCEL
##########################################################################################
#' @title Write matrix or dataframe to excel sheet
#' @description Usefull for generic data where conditional formating of a spesific collumn is required
#' @param df dataframe or matrix
#' @param file output filename of excel file
#' @param type "critical_value" "matrix"
#' @param ... arguments passed to excel_critical_value or to excel_matrix
#' @import openxlsx
#' @keywords functions
#' @export
#' @examples
#' comment<-list(mpg="Miles/(US) gallon",
#'               cyl="Number of cylinders",
#'               disp="Displacement (cu.in.)",
#'               hp="Gross horsepower",
#'               drat="Rear axle ratio",
#'               wt="Weight (1000 lbs)",
#'               qsec="1/4 mile time",
#'               vs="Engine (0=V-shaped,1=straight)",
#'               am="Transmission (0=automatic,1=manual)",
#'               gear="Number of forward gears",
#'               carb="Number of carburetors")
#' report_dataframe(mtcars,sheet="report",file="mtcars",comment=comment,numFmt="#0.00",
#'                  critical=list(am="<0.05"))
#' report_dataframe(mtcars,sheet="report",file=NULL,comment=comment,numFmt="#0.00",
#'                  critical=list(am="<0.05"))
report_dataframe<-function(df,file=NULL,type="critical_value",...) {
  filename<-paste0(file,".xlsx")
  if(!is.null(file)) {
    if (file.exists(filename)) file.remove(filename)
    workbook<-openxlsx::createWorkbook()
    if(type=="critical_value")
      excel_critical_value(df=df,workbook=workbook,...)
    if(type=="matrix")
      excel_matrix(df=df,workbook=workbook,...)
    openxlsx::saveWorkbook(wb=workbook,file=filename,overwrite=TRUE,returnValue=FALSE)
  } else {
    return(df)
  }
}
##########################################################################################
# DATAFRAME INDEX
##########################################################################################
#' @title dataframe index
#' @param nrow number of rows
#' @param ncol number of collumns
#' @keywords functions
#' @export
#' @examples
#' data_frame_index(5,5)
data_frame_index<-function(nrow,ncol) {
  m<-matrix(ncol=2,nrow=nrow)
  ri<-ci<-c()
  for (c in 1:ncol) {
    for (r in 1:nrow) {
      ri<-c(ri,r)
      ci<-c(ci,c)
    }
  }
  result<-as.matrix(data.frame(ri,ci))
  return(result)
}
