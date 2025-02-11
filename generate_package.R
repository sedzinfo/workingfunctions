##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check workingfunctions
# R CMD Rd2pdf workingfunctions
# R CMD build workingfunctions --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
setwd(directory)
directory<-gsub("workingfunctions/","",directory)
# usethis::create_package("workingfunctions")
document()
install()
