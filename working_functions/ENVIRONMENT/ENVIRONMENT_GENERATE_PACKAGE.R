##########################################################################################
# DIRECTORIES
##########################################################################################
# setwd("F:/Dropbox (Psycholate)/dimitrios/working")
# R CMD check psycholatefunctions
# R CMD Rd2pdf psycholatefunctions
# R CMD build psycholatefunctions --resave-data
library(devtools)
library(roxygen2)
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
source("/opt/repo/working_functions/ENVIRONMENT/ENVIRONMENT_DATA.R")
# usethis::create_package("psycholatefunctions")
setwd("psycholatefunctions")
# dir.create(file.path("data"),showWarnings=FALSE)
# save(titanic,file="data/data.rda")
filestocopy<-setdiff(list.files("/opt/repo/working_functions",full.names=TRUE),list.dirs("/opt/repo/working_functions",recursive=FALSE))
file.copy(from=filestocopy,to="/opt/repo/psycholatefunctions/R",recursive=TRUE,copy.mode=TRUE)
document()
setwd("..")
# install("psycholatefunctions")
setwd("/opt/repo")
