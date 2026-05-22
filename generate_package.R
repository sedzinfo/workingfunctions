##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check rwf
# R CMD Rd2pdf rwf
# R CMD build rwf --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
setwd(directory)
directory<-gsub("rwf/","",directory)
# usethis::create_package("rwf")
document()
install()
