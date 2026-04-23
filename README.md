# workingfunctions
**Version 0.1**

![Stars](https://img.shields.io/github/stars/sedzinfo/workingfunctions)
![Watchers](https://img.shields.io/github/watchers/sedzinfo/workingfunctions)
![Repo Size](https://img.shields.io/github/repo-size/sedzinfo/workingfunctions)
![Open Issues](https://img.shields.io/github/issues/sedzinfo/workingfunctions)
![Forks](https://img.shields.io/github/forks/sedzinfo/workingfunctions)
![Last Commit](https://img.shields.io/github/last-commit/sedzinfo/workingfunctions)
![Contributors](https://img.shields.io/github/contributors/sedzinfo/workingfunctions)
![License](https://img.shields.io/github/license/sedzinfo/workingfunctions)
![Release](https://img.shields.io/github/v/release/sedzinfo/workingfunctions)
![Workflow Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/workingfunctions/main.yml)

---

## Overview

`workingfunctions` is an R package designed to streamline **statistical reporting** and enhance **visualizations** for commonly used methodologies in psychology. By automating code-heavy tasks using well-known R packages, `workingfunctions` helps researchers and data scientists save time and reduce coding complexity.

## Key Features

- Statistical reporting for ANOVA, regression, t-tests, and more.
- Intuitive visualizations: ROC curves, histograms, scatterplots, and interaction plots.
- Support for advanced modeling: CFA, IRT models, Thurstonian scales.
- Simulated datasets for practice and demonstration.

---

## Installation

### Public Repository

```r
install.packages("remotes")
remotes::install_github("sedzinfo/workingfunctions/workingfunctions")
```

### Private Repository

```r
install.packages("devtools")
install.packages("usethis")
install.packages("credentials")
install.packages("remotes")

# Set git config
usethis::use_git_config(user.name = "YourName", user.email = "your@mail.com")

# Go to GitHub to generate a Personal Access Token
usethis::create_github_token()

# Paste your PAT into the pop-up that follows
credentials::set_github_pat()

remotes::install_github("sedzinfo/workingfunctions/workingfunctions")
```

---

## Git LFS Setup

Large files (e.g. datasets) are stored using [Git LFS](https://git-lfs.com). Follow the steps below to clone the repository and retrieve them.

### 1. Install Git LFS (one-time setup)

**Ubuntu/Debian:**
```bash
sudo apt install git-lfs
git lfs install
```

**Mac:**
```bash
brew install git-lfs
git lfs install
```

**Windows:**

Download and run the installer from https://git-lfs.com, then:
```bash
git lfs install
```

### 2. Clone the repository

```bash
git clone https://github.com/sedzinfo/workingfunctions.git
cd workingfunctions
```

### 3. Pull the LFS files

```bash
git lfs pull
```

> **Note:** If you cloned the repository *before* installing Git LFS and large files appear as small pointer files, run:
> ```bash
> git lfs install
> git lfs pull
> ```

To verify that LFS files downloaded correctly:
```bash
git lfs ls-files
```
