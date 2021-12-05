
# Load required packages and preprocessed data from the project root directory

source("../packages.R")

load("../FIHI_clean.RData")

##-------------------- Data Preparation and cleaning ---------------------

FIHI_sub2 <- FIHI_sub2 %>%
  mutate( permanent_address = factor(permanent_address, levels = 1:2, 
                      labels = c("Yes", "No")),
          FI_q28= factor(FI_q28, levels = 1:2, 
                      labels = c("Yes", "No")),
          FI_q30= factor(FI_q30, levels = 1:2, 
                      labels = c("Yes", "No")),
          FI_q31= factor(FI_q31, levels = 1:2, 
                      labels = c("Yes", "No")))

# An omnibus function for modeling responses
# 
modeling <- function(response) 
{
  # response <- 'FI_q31'
  
  # Assign data from the global environment 
   data <- FIHI_sub2[, -1] # remove the response id
  
  # Remove respondent id
  
  # Create a vector of all potential response variables
  response_set <- c("permanent_address", "spent_night_elsewhere", "FI_q26",
                 "FI_q27", "FI_q28", "FI_q30", "FI_q31")
  # Exclude the current variable being modeled
  response_set_sub <- response_set[-which(response_set==response)]
  
  ## Prepare data for modeling
  # - all potential responses related to the one being modeled are removed to 
  # prevent issue of multicollinearity.
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
  # model_df_imputed <- cbind(response = model_df[,response], model_df_imputed)
  data_imputed[[response]] <- data[[response]]
  rm(imputed_df)
  
  # Since all variables are categorical, convert them to factors 
  data_imputed_bal <- data_imputed
  data_imputed_bal <- data_imputed_bal %>%
     mutate(across(everything(), as.factor))
  

  # PARTITION DATA
  set.seed(123)
  intrain <- createDataPartition(y=1:NROW(data_imputed_bal), p= 0.67, list = FALSE)
  training <- data_imputed_bal[intrain,];  testing <- data_imputed_bal[-intrain,]
  
  
  #------ Model building -----------
  registerDoParallel(7)
  getDoParWorkers()
  set.seed(100)
  
  trctrl <- trainControl(
  method="cv",
  number=5,
  savePredictions="final",
  classProbs=TRUE,
  sampling = "up",
  index=createResample(training[[response]], 5),
  summaryFunction=twoClassSummary,
  allowParallel = TRUE
  )
  
  formula <- as.formula(paste0(response, "~."))

  model_list <- caretList(
    formula, data=training,
    trControl=trctrl,
    metric = "ROC",
    methodList=c("glm", "lda", "knn", "nnet", "svmLinear", "svmRadial"),
    continue_on_fail = FALSE
    )

  ensemble <- caretEnsemble(model_list, 
                            metric = "ROC", 
                            trControl = trctrl)


  # return(logis) # 
  return(list(train=training, test=testing, model_list=model_list, ensemble= ensemble))
}


#------------
metrics <- function(model_object, response="", test_data=dat$test) {
  # response = "permanent_address"
  # model_object <- log
  # 
  if(is.null(test_data)) test_data <- testing
  
  # make predictions
  prediction <- predict(model_object, test_data) 
  prediction_A <- predict(model_object, test_data, type="prob") 
  
  target <- test_data[, response]
  
  cmat <- confusionMatrix(prediction, target, mode = "prec_recall")
  ROC <- roc(target, predictor = prediction_A[,2])
  AUC_m<-round(ROC$auc, digits=4) 
  misscal<- round(mean(prediction != target),digits = 2)
  
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
eval_table <- function(model_list,response, resp_label='---') 
{
  log.metric <- metrics(model_list$glm, response)
  lda.metric <- metrics(model_list$lda, response)
  knn.metric <- metrics(model_list$knn, response)
  nnet.metric <- metrics(model_list$nnet, response)
  svc.metric <- metrics(model_list$svmLinear, response)
  svmR.metric <- metrics(model_list$svmRadial, response)
  
  mod.sum <- data.frame(rbind(
                            c("Logistic", log.metric$mcr, log.metric$accuracy, log.metric$sens, log.metric$spec, log.metric$fbeta, log.metric$auc),
                            c("LDA",  lda.metric$mcr, lda.metric$accuracy, lda.metric$sens, lda.metric$spec, lda.metric$fbeta, lda.metric$auc),
                            c("KNN", knn.metric$mcr, knn.metric$accuracy, knn.metric$sens, knn.metric$spec, knn.metric$fbeta, knn.metric$auc),
                            c("Neural Network", nnet.metric$mcr, nnet.metric$accuracy, nnet.metric$sens, nnet.metric$spec, nnet.metric$fbeta, nnet.metric$auc),
                            # c("Bagging", bag.metric$mcr, bag.metric$accuracy, bag.metric$sens, bag.metric$spec, bag.metric$fbeta),
                            c("SVM Linear",  svc.metric$mcr, svc.metric$accuracy, svc.metric$sens, svc.metric$spec, svc.metric$fbeta, svc.metric$auc),
                            c("SVM Radial", svmR.metric$mcr, svmR.metric$accuracy, svmR.metric$sens, svmR.metric$spec, svmR.metric$fbeta, svmR.metric$auc)))
  
  names(mod.sum) <- c("Model", "Misclassification Rate", "Accuracy", 
                      "Sensitivity", "Specificity", "fbeta", "AUC")
  
  kable(mod.sum, align = "lcccccc", caption = paste("Evaluation metrics for", resp_label, " as a response variable") ) %>%
    kable_paper("hover", full_width = F)%>% 
         kable_styling(font_size = 12, latex_options = c("HOLD_position"))

}








