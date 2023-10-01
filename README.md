# workingfunctions
Statistical reporting and visualization for common methods

Statistical reporting and visualization for common methodology. Functions to minimize code and automate procedures using common R packages.

# Installation Instructions

install.packages("devtools")

install.packages("usethis")

install.packages("credentials")

install.packages("remotes")

#set config

usethis::use_git_config(user.name="YourName",user.email="your@mail.com")

#Go to github page to generate token

usethis::create_github_token() 

#paste your PAT into pop-up that follows...

credentials::set_github_pat()

#now remotes::install_github() will work

remotes::install_github("sedzinfo/workingfunctions")
