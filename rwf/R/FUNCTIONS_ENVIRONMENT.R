##########################################################################################
# LOAD ENVIRONMENT
##########################################################################################
#' @title Load environment options
#' @keywords functions
#' @export
#' @examples
#' environment_options()
environment_options<-function() {
  options(encoding="UTF8")
  options(digits=4)
  options(scipen=999)
  options(max.print=10000)
  options(warning.length=1000)
  options(nwarnings=10000)
  options(verbose=FALSE)
}
##########################################################################################
# LOAD AND INSTALL MULTIPLE LIBRARIES
##########################################################################################
#' @title Install and load multiple packages
#' @description Install and load multiple packages. If packages exist,they are loaded,if packages don't exist,they are downloaded installed and loaded
#' @param package Vector Package names
#' @importFrom utils installed.packages install.packages
#' @author Steven Worthington
#' @keywords functions
#' @export
#' @examples
#' install_load("car")
#' install_load(c("car","ggplot2"))
install_load<-function(package) {
  new.package<-package[!(package%in%utils::installed.packages()[,"Package"])]
  if(length(new.package))
    utils::install.packages(new.package,dependencies=TRUE)
  sapply(package,require,character.only=TRUE)
}
##########################################################################################
# INSTALL ALL PACKAGES
##########################################################################################
#' @title Install all packages available in CRAN
#' @details Install all packages available in CRAN. Already installed packages are not downloaded or installed
#' @importFrom utils installed.packages available.packages install.packages
#' @keywords functions
#' @export
install_all_packages<-function() {
  installed_packages<-data.frame(utils::installed.packages(),stringsAsFactors=FALSE)
  available_packages<-data.frame(utils::available.packages(),stringsAsFactors=FALSE)
  missing_packages<-sort(setdiff(sort(unique(available_packages$Package)),sort(unique(installed_packages$Package))))
  utils::install.packages(missing_packages)
}
##########################################################################################
# REMOVE USER INSTALLED PACKAGES
##########################################################################################
#' @title Remove all user packages
#' @importFrom utils remove.packages
#' @keywords functions
#' @export
remove_user_packages<-function() {
  installed_packages<-data.frame(installed.packages())
  installed_packages<-subset(installed_packages,!grepl("MRO",installed_packages$LibPath))
  installed_packages<-installed_packages[!(installed_packages[,"Priority"] %in% c("base","recommended")),]
  path.lib<-unique(installed_packages$LibPath)
  remove_packages<-installed_packages[,1]
  sapply(remove_packages,utils::remove.packages,lib=path.lib)
}
##########################################################################################
# UNLOAD LIBRARY
##########################################################################################
#' @title Unload library
#' @param package Package name
#' @keywords functions
#' @export
detach_package<-function(package) {
  search_item<-paste("package",package,sep=":")
  while(search_item %in% search())
    detach(search_item,unload=TRUE,character.only=TRUE)
}
##########################################################################################
# GET WORKING FILE PATH
##########################################################################################
#' @title Get working file path
#' @keywords functions
#' @export
#' @examples
#' #getfwp()
getfwp<-function() {
  # https://stackoverflow.com/a/36777602/6247402
  # http://stackoverflow.com/a/35842176/2292993
  # http://stackoverflow.com/a/32016824/2292993
  command_arguments<-commandArgs(trailingOnly=FALSE)
  needle<-"--file="
  match=grep(needle,command_arguments)
  if (length(match)>0) {
    return(normalizePath(sub(needle,"",command_arguments[match])))
  } else {
    ls_vars=ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        path<-rstudioapi::getActiveDocumentContext()$path
        if (path!='') {
          return(normalizePath(path))
        } else {
          tryCatch({
            path=rstudioapi::getSourceEditorContext()$path
            path=normalizePath(path)
          },error=function(e) { path='' })
          return(path)
        }
      }
    }
  }
}
##########################################################################################
# LOG FILE
##########################################################################################
#' @title Log console in file
#' @description Logs console in file and then displays log in console
#' @param file Filename of log
#' @param input Script to log in log file
#' @keywords functions
#' @export
#' @examples
#' write_txt(mtcars)
#' write_txt(mtcars,file="mtcars")
write_txt<-function(input,file=NULL) {
  if(!is.null(file)) {
    results<-file(invisible(paste0(file,".log")))
    sink(results,append=TRUE)
    sink(results,append=TRUE,type="message")
  }
  print(input)
  if(!is.null(file)) {
    sink()
    sink(type="message")
    cat(readLines(invisible(paste0(file,".log"))),sep="\n")
  }
}
