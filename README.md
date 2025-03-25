# workingfunctions

**Version 0.1**

---

## Overview
`workingfunctions` is an R package designed to streamline **statistical reporting** and enhance **visualizations** for commonly used methodologies in psychology. By automating code-heavy tasks using well-known R packages, `workingfunctions` helps researchers and data scientists save time and reduce coding complexity.

## Key Features
- Statistical reporting for ANOVA, regression, t-tests, and more.
- Intuitive visualizations: ROC curves, histograms, scatterplots, and interaction plots.
- Support for advanced modeling: CFA, IRT models, Thurstonian scales.
- Simulated datasets for practice and demonstration.


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

remotes::install_github("sedzinfo/workingfunctions/workingfunctions")

