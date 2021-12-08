# File: modeling.R
# Purpose: Run models and save results for later use
# Authors: William, John & George
# Last Updated: 12/07/2021 
# 
#
# Load Packages, functions, and datasets---------------------------------------
dir_path <-  dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path) # set the current working directory to this path
source("../packages.R")
source("modeling_functions.R")

# Import already cleaned data
load("../FIHI_clean.RData")

#------------- Train Models ------------------------

# Modeling Housing Insecurity Responses
pa_models <- modeling("permanent_address")

sne_models <- modeling("spent_night_elsewhere", 
                c("earth","glm","knn","svmLinear","svmRadial"))

# Modeling Food Insecurity Responses
FI_q26_models <- modeling("FI_q26",)

FI_q27_models <- modeling("FI_q27")

FI_q28_models <- modeling("FI_q28")

# FI_q30_models <- modeling("FI_q30") # produces errors in the ensemble part due to the svmRadial

FI_q31_models <- modeling("FI_q31")


#------------- Save Results ------------------------
# FI_q30_models (currently discontinued)
save(sne_models, pa_models, FI_q26_models, FI_q27_models,
     FI_q28_models, FI_q31_models,
     file = "trained_models.RData"
     )

# load("trained_models.RData")

