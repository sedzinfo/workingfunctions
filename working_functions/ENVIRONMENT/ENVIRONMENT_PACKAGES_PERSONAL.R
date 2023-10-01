##########################################################################################
# LOAD
##########################################################################################
setwd("/opt/repo")
# cd opt
# sudo ln -s /mnt/WD500/Dropbox\ \(Psycholate\)/dimitrios/working/ rworking
# sudo ln -s /mnt/WD500/repositories/rstatistics/ repo
# sudo ln -s /mnt/WD500/repositories/ipipread/ ipip
# sudo ln -s /mnt/WD500/repositories/pystatistics/ pyrepo
# sudo ln -s /mnt/WDRED_REMOTE/repositories/ipip_crombach/ ipip_crombach
# sudo ln -s /home/dimitrios/Dropbox\ \(Psycholate\)/dimitrios/working/ rworking
# sudo ln -s /mnt/WD6/repositories/ repositories
##########################################################################################
# CREATE REFERENCE MANUAL
##########################################################################################
# R CMD check workingfunctions
# R CMD Rd2pdf workingfunctions
# R CMD build workingfunctions
##########################################################################################
# PACKAGES
##########################################################################################
# devtools::install_git("https://git.psycholate.com/dimitrios/rstatistics.git",subdir="psycholatefunctions",credentials=git2r::cred_user_pass("dimitrios",getPass::getPass()))
# devtools::install_bitbucket(repo="dimitrios_zacharatos/https://dimitrios_zacharatos@bitbucket.org/psycholate/rstatistics.git",ref="master",subdir="psycholatefunctions",auth_user="dimitrios_zacharatos",password=getPass::getPass())
# git clone https://dimitrios_zacharatos@bitbucket.org/psycholate/rstatistics.git
dir.exists(file.path("R/scripts/packages/rstatistics"))
ifelse(!dir.exists(file.path("R/scripts/packages/rstatistics")),dir.create(file.path("R/scripts/packages/rstatistics")),FALSE)
setwd("R/scripts/packages/rstatistics")
# setwd("F:/repositories/rstatistics")
devtools::install("psycholatefunctions")


