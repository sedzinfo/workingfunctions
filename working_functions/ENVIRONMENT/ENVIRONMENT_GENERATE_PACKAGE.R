##########################################################################################
# DIRECTORIES
##########################################################################################
# setwd("F:/Dropbox (Psycholate)/dimitrios/working")
# R CMD check workingfunctions
# R CMD Rd2pdf workingfunctions
# R CMD build workingfunctions --resave-data
library(devtools)
library(roxygen2)
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
working_directory<-"/opt/repositories/workingfunctions/"
working_directory<-"C:/Users/User/Documents/GitHub/workingfunctions/"
# source(paste0(working_directory,"ENVIRONMENT/ENVIRONMENT_DATA.R"))
setwd(working_directory)
# usethis::create_package("workingfunctions")
setwd("workingfunctions")
# dir.create(file.path("data"),showWarnings=FALSE)
filestocopy<-setdiff(list.files(paste0(working_directory,"/working_functions"),full.names=TRUE),
                     list.dirs(paste0(working_directory,"/working_functions"),recursive=FALSE))
file.copy(from=filestocopy,to=paste0(working_directory,"/workingfunctions/R"),recursive=TRUE,copy.mode=TRUE)
document()
setwd("..")
# install("workingfunctions")
setwd(working_directory)

