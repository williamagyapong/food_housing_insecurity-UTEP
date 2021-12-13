# File: modeling_functions.R
# Purpose: Provides user-defined functions for convenient model building
# and reporting of results.
# Authors: William, John & George
# Last Updated: 12/07/2021 
# 
#
#--------------------------------------------------------------------------

# Load required packages and preprocessed data from the project root directory

# source("../packages.R")

# load("../FIHI_clean.RData")

# A function for training models
# 
modeling <- function(response, models=NULL, kfolds=5) 
{
 
  # Assign data from the global environment 
   data <- FIHI_sub2[, -1] # remove the response id
  
  # Create a vector of all potential response variables
  response_set <- c("permanent_address", "spent_night_elsewhere", "FI_q26",
                 "FI_q27", "FI_q28", "FI_q30", "FI_q31")
  # Exclude the current variable being modeled
  response_set_sub <- response_set[-which(response_set==response)]
  
  ## Prepare data for modeling
  # - all potential responses related to the one being modeled are removed to 
  # prevent issue of "multicollinearity".
  # - remove also any variable not considered a reasonable predictor.
  
  data<- data %>%
  rename(college_school = `college/school`) %>% # the forward slash serves our 
    # EDA well but not for the modeling: it interferes with aspects of data imputation
  dplyr::select(!contains(response_set_sub), -ends_with("_changed")) %>%
  filter(!!as.name(response) != "NA") # use only complete cases for the response
  
  #Missing  value imputation for predictors
  set.seed(123)
  imputed_df <- mice(data[,-which(names(data)==response)], printFlag = F)
  data_imputed <- as.data.frame(complete(imputed_df, 1)) # use the first imputed data
  
  data_imputed[[response]] <- data[[response]]
  rm(imputed_df)
  
  # Since all variables are categorical, convert them to factors 
  data_imputed <- data_imputed %>%
     mutate(across(everything(), as.factor))
  

  # Data partitioning
  set.seed(123)
  intrain <- createDataPartition(y=1:NROW(data_imputed), p= 0.67, list = FALSE)
  training <- data_imputed[intrain,];  testing <- data_imputed[-intrain,]
  
  
  #------ Model building -----------
  
  # register clusters for parallel model fitting with the doparallel package
  registerDoParallel(cores = detectCores())
  getDoParWorkers()
  set.seed(100)
  
  # specifying desired model settings
  trctrl <- trainControl(
    method="cv",
    number=kfolds,
    savePredictions="final",
    classProbs=TRUE,
    sampling = "up",
    index=createResample(training[[response]],kfolds),
    summaryFunction=twoClassSummary 
    ,allowParallel = TRUE
  )
  
  # run the list of models concurrently
  # ROC is used to tune and evaluate models
  formula <- as.formula(paste(response, "~."))
  if(is.null(models)) {
    # list of default models 
    # Multivariate Adaptive Regression Splines (MARS) is named earth
    models <- c("earth","glm", "lda", "knn", "svmLinear", "svmRadial")
    # For whatever reason unknown to us, bagging leads to NA's in ROC values
    # LDA doesn't work for the spent_night_elsewhere response
  }
  
  model_list <- caretList(
      formula, data=training,
      trControl=trctrl,
      metric = "ROC",
      methodList=models,
      continue_on_fail = FALSE
    )
  
  
  #--- Create an Ensemble model from all the models ----
  # This ensemble code sometimes leads to this error for some of the responses:
  # "Error in check_bestpreds_resamples(modelLibrary) : Component models do not 
  # have the same re-sampling strategies".
  # Some investigations revealed svmRadial to be the cause.
  ensemble <- caretEnsemble(model_list,
                            metric = "ROC",
                            trControl = trctrl)
  # ensemble = ""

  return(list(test=testing, model_list=model_list, ensemble= ensemble))
}


#------------
metrics <- function(model_object, response="", test_data) {
  
  
  # if(is.null(test_data)) test_data <- testing # assumes testing data in the global env
  
  # make predictions
  prediction <- predict(model_object, test_data) 
  prediction_A <- predict(model_object, test_data, type="prob") 
  
  target <- test_data[, response]
  
  cmat <- confusionMatrix(prediction, target, mode = "prec_recall")
  ROC <- roc(target, predictor = prediction_A[,2])
  AUC_m<-round(ROC$auc, digits=4) 
  misscal<- round(mean(prediction != target),digits = 2) # misclassification
  
 # Returned outputs
 return(list(
   accuracy = (1-misscal),
   mcr = misscal,
   sens = round(cmat$byClass[1],2),
   spec = round(cmat$byClass[2],2),
   fbeta = round(cmat$byClass[7],2),
   auc = AUC_m
 ))
}


#------- Compute performance metrics for the full models ---------------
eval_table <- function(model_list,response, test_data, resp_label='---') 
{
  log.metric <- metrics(model_list$glm, response, test_data)
  lda.metric <- metrics(model_list$lda, response, test_data)
  knn.metric <- metrics(model_list$knn, response, test_data)
  mars.metric <- metrics(model_list$earth, response, test_data)
  svc.metric <- metrics(model_list$svmLinear, response, test_data)
  svmR.metric <- metrics(model_list$svmRadial, response, test_data)
  
  metrics.summ <- data.frame(rbind(
        c("Logistic", log.metric$mcr, log.metric$accuracy, 
          log.metric$sens, log.metric$spec, log.metric$fbeta, log.metric$auc),
        
        c("LDA",  lda.metric$mcr, lda.metric$accuracy, lda.metric$sens,
          lda.metric$spec, lda.metric$fbeta, lda.metric$auc),
        
        c("KNN", knn.metric$mcr, knn.metric$accuracy, knn.metric$sens,
          knn.metric$spec, knn.metric$fbeta, knn.metric$auc),
        
        c("MARS",mars.metric$mcr,mars.metric$accuracy,mars.metric$sens,
          mars.metric$spec, mars.metric$fbeta, mars.metric$auc),
        
        c("SVM Linear",  svc.metric$mcr, svc.metric$accuracy, svc.metric$sens,
          svc.metric$spec, svc.metric$fbeta, svc.metric$auc),
        
        c("SVM Radial", svmR.metric$mcr, svmR.metric$accuracy, svmR.metric$sens,
          svmR.metric$spec, svmR.metric$fbeta, svmR.metric$auc)
        ))
  
  names(metrics.summ) <- c("Model", "Misclassification Rate", "Accuracy", 
                           "Sensitivity", "Specificity", "fbeta", "AUC")
  
  return(kable(metrics.summ, align = "lcccccc", booktabs=T, linesep="",
        caption = paste("Evaluation metrics for", resp_label, " as a response variable") ) %>%
    kable_paper("hover", full_width = F)%>% 
    kable_styling(font_size = 12, latex_options = c("HOLD_position")))
  
}


# Ranking Predictors
rank_pred <- function(model, first_n="all", mod_label="Logistic Regression") 
{
  first_n <- ifelse(first_n=="all", "n()", first_n)
  varImp(model, scale = F)$importance %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    arrange(Overall) %>%
    top_n(first_n) %>%
    mutate(rowname = forcats::fct_inorder(rowname )) %>%
    ggplot()+
    geom_col(aes(x = rowname, y = Overall), fill="gray") +
    labs(x="Variables", y="Importance",
         title = paste("Variable Importance by the", mod_label, "Model")) +
    coord_flip() +
    theme_classic()
}

## Rank predictors with an ensemble model
rank_pred_by_ensemb <- function(ensemble, first_n="all") 
{
  # ensemble <- dat$ensemble
  first_n <- ifelse(first_n=="all", "n()", first_n)
  varImp(ensemble, scale=F) %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    dplyr::select(rowname, overall) %>%
    arrange((overall)) %>%
    mutate(rowname = forcats::fct_inorder(rowname )) %>%
    top_n(first_n) %>%
    ggplot()+
    geom_col(aes(x = rowname, y = overall), fill="gray") +
    labs(x="Variables", y="Importance",
         title = "Variable Importance by the Ensemble Model") +
    coord_flip() +
    theme_classic()
}


# rank_pred(dat$model_list$glm)
# rank_pred_by_ensemb(dat$ensemble, 20)

# See available algorithms in caret
# modelnames <- paste(names(getModelInfo()), collapse=',  ')








