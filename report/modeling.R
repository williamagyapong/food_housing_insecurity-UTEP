# File: modeling.R
# Purpose: Run models and save results for later use
# Authors: William, John & George
# Last Updated: 12/07/2021 
# 
#
# Load Packages, functions, and datasets---------------------------------------

source("modeling_functions.R")


#------------- Train Models ------------------------
pa_models <- modeling("permanent_address")

sne_models <- modeling("spent_night_elsewhere", 
                c("earth","glm","knn","svmLinear","svmRadial"))

FI_q26_models <- modeling("FI_q26")


#------------- Save Results ------------------------
save(pa_models, "trained_models.RData")


