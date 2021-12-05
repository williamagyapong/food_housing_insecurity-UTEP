# File: packages.R
# Authors: William, John & George
# Last Updated: 12/04/2021 
#
#
# Load Packages-----------------------------------------------------------------
# Using the "pacman" package for managing add-on packages. It will install
# packages, if needed, and then load the packages.

# Install the package manager if not already installed.
if(!("pacman" %in% installed.packages())) install.packages("pacman")

## Make a list of add-on packages for the project
# e1071: offers an interface to the C++ implementation of SVM
# dplyr: data manipulation
# ggplot2: Data visualizations
# RColorBrewer: provides color palettes
# patchwork: helps add ggplot2 plots together to compose multiplot layouts
# knitr: dynamic report generation
# kableExtra: used with knitr::kable() to generate advanced tables
# scales: provides the percent function for adding the percent label
# mice: missing data imputation
# caret: contains functions to streamline the model building process 
# cvAUC: calculates cross-validated area under the ROC curve (AUC) estimates.
# verification:
# pROC: for creating ROC curves
# naniar: missing data visualization
# UpSetR: 

packages <- c("e1071","dplyr", "ggplot2", "RColorBrewer","patchwork",
              "knitr", "kableExtra", "scales", "mice", "cvAUC",
              "verification", "caret", "pROC", "rpart", "caretEnsemble", 
              "naniar", "UpSetR", "doParallel")

pacman::p_load(char = packages, install = TRUE)

rm(packages)

# library(e1071)
# library(tidyverse)
# library(kernlab)
# library(cvAUC)
# library(verification)
# library(caret)
# require(pROC)
#library(caretEnsemble)
# library(randomForest)
# suppressPackageStartupMessages(library(mice))
# library(ROSE) # for treating class imbalances

# Generating missing data maps and summaries
# library(naniar) 
# library(UpSetR)
