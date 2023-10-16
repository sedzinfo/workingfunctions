##########################################################################################
# DIRECTORIES
##########################################################################################
# setwd("F:/Dropbox (Psycholate)/dimitrios/working")
# R CMD check workingfunctions
# R CMD Rd2pdf workingfunctions
# R CMD build workingfunctions --resave-data
library(devtools)
library(roxygen2)
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
source("/opt/repositories/workingfunctions/working_functions/ENVIRONMENT/ENVIRONMENT_DATA.R")
setwd("/opt/repositories/workingfunctions/")
setwd("C:\Users\User\Documents\GitHub\workingfunctions\")

# usethis::create_package("workingfunctions")
setwd("workingfunctions")
# dir.create(file.path("data"),showWarnings=FALSE)
filestocopy<-setdiff(list.files("/opt/repositories/workingfunctions/working_functions",full.names=TRUE),list.dirs("/opt/repositories/workingfunctions/working_functions",recursive=FALSE))
file.copy(from=filestocopy,to="/opt/repositories/workingfunctions/workingfunctions/R",recursive=TRUE,copy.mode=TRUE)
document()
setwd("..")
# install("workingfunctions")
setwd("/opt/repositories/workingfunctions/")

