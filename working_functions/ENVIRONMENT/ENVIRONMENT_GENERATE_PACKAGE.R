##########################################################################################
# DIRECTORIES
##########################################################################################
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
directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
working_directory<-gsub("working_functions/ENVIRONMENT/","",directory)
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
remove.packages("workingfunctions")
install("workingfunctions")
library(workingfunctions)

