##########################################################################################
# CHECK DATAFRAME
##########################################################################################
#' @title Check dataframe
#' @description dataframe summary
#' @param df dataframe
#' @param name_length number of characters to be displayed for names
#' @param nuniques number of unique items to display
#' @param digits number of rounding digits
#' @param parralel if TRUE it will run using multiple cores
#' @param file output filename
#' @import future.apply
#' @importFrom future availableCores plan
#' @importFrom gtools mixedsort
#' @importFrom plyr rbind.fill
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom stats median sd
#' @keywords functions check dataframe
#' @export
#' @examples
#' cdf(df=mtcars,parralel=TRUE)
#' cdf(df=change_data_type(mtcars,"factor"),nuniques=3)
#' cdf(df=data.frame(t(mtcars)),file="mtcars",nuniques=10)
#' cdf(df=mtcars)
#' cdf(df=generate_missing(mtcars))
#' cdf(df=infert,nuniques=10)
#' cdf(df=infert)
#' df<-data.frame(infert,
#'                date=seq(as.Date("2010-1-1"),
#'                     as.Date("2020-1-1"),
#'                     length.out=nrow(infert)))
#' cdf(df=df)
cdf<-function(df,name_length=(getOption("width")/3),digits=2,nuniques=0,parralel=FALSE,file=NULL) {
  if(parralel) {
    future::plan(future::multisession,gc=TRUE,.cleanup=TRUE,workers=future::availableCores("mc.cores"))
  } else {
    future::plan(future::sequential)
  }
  check_df<-future.apply::future_sapply(df,function(y) {
    return(list(EMPTY=length(which(as.character(y)=="")),
                null=length(which(is.null(y))),
                na=length(which(is.na(y))),
                NOT_NA=length(which(!is.na(y))),
                NAN=length(which(is.nan(unlist(y)))),
                INF=length(which(is.infinite(unlist(y)))),
                FIN=length(which(is.finite(unlist(y)))),
                RANGE=length(unique(y)),
                MEAN=if(is.numeric(y)) round(mean(y,na.rm=TRUE),2) else NA,
                MEDIAN=if(is.numeric(y)) round(stats::median(y,na.rm=TRUE),2) else NA,
                SD=if(is.numeric(y)) round(stats::sd(y,na.rm=TRUE),2) else NA,
                MIN=if(is.double(y)) min(y,na.rm=TRUE) else gtools::mixedsort(as.character(na.omit(unique(y))))[1],
                MAX=if(is.double(y)) max(y,na.rm=TRUE) else gtools::mixedsort(as.character(na.omit(unique(y))))[length(na.omit(unique(y)))],
                MODE=mode(y),
                TYPE=typeof(y),
                CLASS=class(y),
                FACTOR=is.factor(y)))
  })
  check_df<-data.frame(NAMES=names(df),t(check_df),stringsAsFactors=FALSE,check.names=FALSE)
  summary_dataframe<-data.frame(COLLUMNS=length(df),
                                ROWS=nrow(df),
                                TOTAL=length(df)*nrow(df),
                                EMPTY=sum(as.numeric(check_df$EMPTY),na.rm=TRUE),
                                null=sum(as.numeric(check_df$null),na.rm=TRUE),
                                NAN=sum(as.numeric(check_df$NAN),na.rm=TRUE),
                                na=sum(as.numeric(check_df$na),na.rm=TRUE),
                                INF=sum(as.numeric(check_df$INF),na.rm=TRUE),
                                FIN=sum(as.numeric(check_df$FIN),na.rm=TRUE),
                                FACTOR=sum(future.apply::future_sapply(df,function(y) length(which(is.factor(y))))),
                                row.names=NULL,
                                check.names=FALSE,
                                stringsAsFactors=FALSE)
  if(nuniques>0) {
    uniques<-future.apply::future_apply(df,2,unique)
    level<-future.apply::future_sapply(df,function(y) levels(y))
    uniques_df<-levels_df<-data.frame()
    for (i in 1:length(uniques)){
      if(length(uniques[[i]])>nuniques)
        uniques[[i]]<-paste(length(uniques[[i]]),"Uniques")
      if(length(level[[i]])>nuniques)
        level[[i]]<-paste(length(level[[i]]),"Levels")
      uniques_df<-plyr::rbind.fill(uniques_df,data.frame(UNIQUES=toString(t(sort(uniques[[i]]))),check.names=FALSE))
      levels_df<-plyr::rbind.fill(levels_df,data.frame(LEVELS=toString(level[[i]]),check.names=FALSE))
    }
    if(all(levels_df$LEVELS==""))
      check_df<-data.frame(check_df,uniques_df,check.names=FALSE)
    else
      check_df<-data.frame(check_df,uniques_df,levels_df,check.names=FALSE)
  }
  if (!is.null(file)) {
    filename<-paste0(file,".xlsx")
    if (file.exists(filename)) file.remove(filename)
    wb<-openxlsx::createWorkbook()
    excel_critical_value(df=check_df,workbook=wb,sheet="variables",numFmt="#0.00")
    excel_critical_value(summary_dataframe,workbook=wb,sheet="summary",numFmt="#0.00")
    openxlsx::saveWorkbook(wb=wb,file=filename,overwrite=TRUE)
  }
  check_df$NAMES<-sub_str(check_df$NAMES,name_length,type="left")
  check_df$MIN<-sub_str(check_df$MIN,floor(name_length/6),type="left")
  check_df$MAX<-sub_str(check_df$MAX,floor(name_length/6),type="left")
  
  row.names(check_df)<-NULL
  result<-list(summary=summary_dataframe,check=check_df)
  return(result)
}
