#==============================================================================
# Importing the raw survey data
#
#==============================================================================

file_path <- dirname(rstudioapi::getSourceEditorContext()$path)

# Load the required packages 
source(paste0(file_path, '/packages.r'))

FIHI <- read.csv(paste0(file_path, '/datcsv.csv'))


#==============================================================================
# Helper functions
#==============================================================================

na_to_zero <- function(x) 
{
  return(ifelse(is.na(x), 0, x ))
}

na_to_99 <- function(x) 
{
  return(ifelse(is.na(x), 99, x ))
}

# This function helps restore back any prior missingness. Care must be taken in 
# order not to apply it to a variable where 0 has a different meaning.
zero_to_na <- function(x) 
{
  return(ifelse(x == 0, NA, x ))
}

#==============================================================================
#                 Data Cleaning/Preprocessing
#==============================================================================

FIHI_new <- FIHI %>% 
  # change column names to lowercase
  rename_with(tolower) %>%
  # Removing unwanted variables
  select(-externalid, -satellite, -email, -datetime, -carddata,
         -starts_with(c("q5", "q8","q16", "q18", "q21", "q29", "q24", "q36", "q37", "q38", "q39"))
  ) %>%
  # Strip off trailing strings to get clean variable names
  rename_with(~gsub('\\..*', '', .x))  


##------- Consolidating common variables split across levels -----
# We create dummy columns for the number of non-na values within each range for the targeted variable
FIHI_new_ <- FIHI_new %>% 
  mutate(q6Count = rowSums(!is.na(select(., starts_with("q6")))),
         q7Count = rowSums(!is.na(select(., starts_with("q7")))),
         q11Count = rowSums(!is.na(select(., starts_with("q11")))),
         q25Count = rowSums(!is.na(select(., starts_with("q25"))))) %>%
  # change the 0 level for q25_7 to avoid issues with approach for working on these multiple-response variables
  mutate(q25_7 = ifelse(q25_7 == 0, 7, q25_7))

## View these variables to make sure the count was done correctly
# FIHI_new_%>%select(respondentid,starts_with("q6")) %>%kable()
# FIHI_new_%>%select(respondentid,starts_with("q7")) %>%kable()
# FIHI_new_%>%select(respondentid,starts_with("q11")) %>%kable()
# FIHI_new_%>%select(respondentid,starts_with("q25")) %>%kable()


##-------------- Separating multiple responses ---------------------------------
# Ethnicity: 
#    - level 8 is for mixed race
#    - set Hispanic and White/Caucassian to Hispanic [q6_1!=0 & q6_6!=0]
#    - set Hispanic and Black to Black [q6_1!=0 & q6_4!=0]
#
# Gender:
#    - level 7: Mixed gender
#
# Federal Aid (q25): 
#    - Not applicable -> 7
#    - Multiple sources -> 8
#    - work study is almost lost when multiple responses are sorted out, 
#    so we decided to do: work study + other sources => work study
#    - 
#
FIHI_sub <- FIHI_new_ %>% 
  mutate(across(starts_with(c("q6", "q7", "q11", "q25")), na_to_zero)) %>%
  mutate(q6 = ifelse(q6Count>1, ifelse(q6_1!=0 & q6_6!=0, 1,
                                       ifelse(q6_1!=0 & q6_4!=0, 4, 8)
  ),
  rowSums(across(starts_with("q6_")))), 
  
  q7 = ifelse(q7Count>1, 7, rowSums(across(starts_with("q7_")))),
  
  q11 = ifelse(q11Count>1,10, rowSums(across(starts_with("q11_")))),
  
  q25 = ifelse(q25Count>1, ifelse(q25_2!=0, 2, 8), 
               rowSums(across(starts_with("q25_")))),
  
  .after = starts_with("q4"),
  .keep = "unused") %>%
  relocate(q11, .after = starts_with("q10")) %>% # just to keep the order of the variables
  relocate(q25, .after = starts_with("q23")) %>%
  mutate(across(starts_with(c("q6", "q7", "q11", "q25")), zero_to_na))


##---------- Collapsing levels within some of the variables ---------
#
# Ethnicity(q6): maintain Hispanic (1), Black/African American(4), White(6), Asian(3)
# and collapse American Indian(2), Native Hawaiian(5), mixed race(8) and other(7) into other (8).
# We decided not to keep a mixed race level because the total came to 88 after we 
# assigned (Hispanic, white) to Hispanic, and (Hispanic, Black) to Black.
#
# Gender(q7) now will have only 3 levels (female -> 1, male -> 2, non-binary/non-conforming -> 3).
#
# Income(q9): keep first 5 levels and 100000 or more (11)->8, collapse (6,7)->6, (8,9,10)->7
# 
# Academic level (q10): keep first 5, and collapse last 2 -> 6 
#
# College (q11): maintain first 6, put rest into other (level 7)
#
# Commute (q12): keep car(1), Bus/public (4) and other(8), collapse car & carpool (2,3) -> 9,
# collapse bike, trolley, walk (5,6,7) -> 10. Keep Not applicable (0)
#
# Federal Aid(q25): Emergency loan reduced from 316 to 50, so we added those to loans(3)
#
# Q26: Never -> 1, sometimes true -> 2, often true -> 3
##

FIHI_sub2 <- FIHI_sub %>%
  mutate(q6 = ifelse(q6%in%c(2,5,7,8), 8, q6),
         q7 = ifelse(q7 >=4, 3, q7),
         q9 = ifelse(q9%in%c(6,7), 12, ifelse(q9%in%c(8,9,10), 13, q9)),
         q10 = ifelse(q10>5, 6, q10),
         # q11 = ifelse(q11>6, 7, q11),
         q12 = ifelse(q12%in%2:3, 9, ifelse(q12%in%5:7, 10, ifelse(q12==0, 11, q12))),
         q25 = ifelse(q25==4, 3, q25)
  )

##------------------ Assign labels to the various levels-----------------------
FIHI_sub3 <- FIHI_sub2 %>%
  mutate(
    q1 = factor(q1, levels = 1:2, 
                labels = c("Full Time", "Part Time")),
    q2 = factor(q2, levels = 1:3, 
                labels = c("Full Time", "Part Time", "No")),
    q3 = factor(q3, levels = 1:2, 
                labels = c("On-Campus", "Off-Campus")),
    q4 = factor(q4, levels = 1:2, 
                labels = c("19 hours or less", "More than 19 hours")),
    q6 = factor(q6, levels = c(1,3,4,6,8), 
                labels = c("Hispanic", "Asian", "Black/African American", "White/Caucasian",
                           "Other")),
    q7 = factor(q7, levels = 1:3, 
                labels = c("Female", "Male", "Non-binary/Non-conforming")),
    q9 = factor(q9, levels = c(1:5,12,13,11), 
                labels = c("< 10,000", "10,000-19,999", "20,000-29,999", "30,000-39,000",
                           "40,000-49,999", "50,000-69,999", "70,000-99,999", ">=100,000")),
    q10 = factor(q10, levels = 1:6, 
                 labels = c("Freshman", "Sophomore", "Junior", "Senior",
                            "Masters", "Doctoral/Professional")),
    q11 = factor(q11, levels = 1:10, 
                 labels = c("Business", "Education", "Engineering", "Health Sciences",
                            "Liberal Arts", "Science", "Nursing", "Pharmacy",
                            "Other", "More than one")),
    q12 = factor(q12, levels = c(1,9,4,10,8,11), 
                 labels = c("Car (alone)", "Car (someone drives) & Carpool",
                            "Bus/Public transport", "Bike, Trolley, or Walk",
                            "Other", "Not applicable")),
    q13 = factor(q13, levels = 1:4, 
                 labels = c("Not reliable at all", "Somewhat reliable",
                            "Fairly reliable", "Very reliable")),
    q14 = factor(q14, levels = 1:2, 
                 labels = c("Yes", "No")),
    q15 = factor(q15, levels = 1:2, 
                 labels = c("Yes", "No")),
    q17 = factor(q17, levels = 1:2, 
                 labels = c("Yes", "No")),
    q19 = factor(q19, levels = 1:4, 
                 labels = c("On-Campus", "Off-Campus with family", 
                            "Off-Campus not with family", "Other")),
    q20 = factor(q20, levels = 1:2, 
                 labels = c("Yes", "No")),
    q22 = factor(q22, levels = 1:3, 
                 labels = c("Rarely", "Sometimes", "Often")),
    q23 = factor(q23, levels = 1:2, 
                 labels = c("Yes", "No")),
    q25 = factor(q25, levels = c(1:3,5,8,7,6 ), 
                 labels = c("Grants", "Work Study", "Loans", "Scholarship", 
                            "Multiple Aids", "Other", "None")),
    q26 = factor(q26, levels = 1:3, 
                 labels = c("Never true", "Sometimes true", "Often true")),
    q27 = factor(q27, levels = 1:3, 
                 labels = c("Never true", "Sometimes true", "Often true")),
    q28 = factor(q28, levels = 1:2, 
                 labels = c("Yes", "No")),
    q30 = factor(q30, levels = 1:2, 
                 labels = c("Yes", "No")),
    q31 = factor(q31, levels = 1:2, 
                 labels = c("Yes", "No")),
    q32 = factor(q32, levels = 1:3, 
                 labels = c("Decreased", "About the same", "Increased")),
    q33 = factor(q33, levels = 1:3, 
                 labels = c("Decreased", "About the same", "Increased")),
    q34 = factor(q34, levels = 1:3, 
                 labels = c("Decreased", "About the same", "Increased")),
    q35 = factor(q35, levels = 1:3, 
                 labels = c("Decreased", "About the same", "Increased")),
  )

##------------- Renaming the variables ---------------------
var_names <- c("respondent_id", "enrollment", "employment", "employment_type", 
               "weekly_work_hrs", "ethnicity", "gender", "total_income", 
               "academic_level", "college/school", "mode_transport", 
               "transport_reliability", "living_alone", "dependents",
               "household_head", "residence", "permanent_address",
               "spent_night_elsewhere", "know_homelessness_studt", 
               "federal_aid", "FI_q26", "FI_q27", "FI_q28", "FI_q30", 
               "FI_q31", "expenditures_changed", "income_changed",
               "fed_aid_changed", "debt_changed")

names(FIHI_sub2) <- var_names
names(FIHI_sub3) <- var_names



##------------------- Saving the partially clean data for analysis--------------
# write.csv(FIHI_sub3, paste0(file_path, '/datcsv_clean.csv'))

save(FIHI_sub2, FIHI_sub3, file=paste0(file_path, '/FIHI_clean.RData'))



### Inspecting missing values

# obtaining the missing percentage of each variable
missing_rate <- data.frame()
nr <- NROW(data)
nc <- NCOL(data)
Var_name <- variable.names(data)
for (i in 1:nc) {
  na <- sum(is.na(data[,i]))
  na_rate <- (na/nr)*100
  result <- list(Variable = Var_name[i],Number_Missing = na, Missing_Rate = na_rate 
  )
  missing_rate <- rbind(missing_rate, result, stringsAsFactors = F)
}
kable(missing_rate, align = "lcc", caption = "Table : Missing values table displaying percentages") %>%
  kable_paper("hover", full_width = T)%>% 
  kable_styling(font_size = 12, latex_options = c("HOLD_position"))



# Make data for plotting

# For creating bar graphs of the response variables
mybarplot <- function(var, palette="Set2",
                      xlab="", ylab="",title="",dat='') 
{
  # palette=c("Set2","Set3","Accent","Set1","Paired","Dark2")
  ylab = ifelse(ylab=="", "Number of Respondents", ylab)
  
  if(!(class(dat) %in% c('data.frame','tbl','tbl_df'))) dat <- FIHI_sub3
  
  if(ncol(dat) == 4)
  {
    # A plotting data was provided
    plotdf <- dat
  } else {
    # prepare data frame for plotting
    plotdf <- dat %>%
      group_by({{ var }}) %>%
      summarise(n=n()) %>%
      mutate(pct = n/sum(n),
             lbl = percent(pct)) 
  }
  
  # create the plot
  return(
    ggplot(plotdf, aes({{ var }},n, fill={{ var }})) +
      geom_bar(stat = "identity",position = "dodge")  +
      geom_text(aes(label = n), size=3, position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette=palette) +
      labs(x=xlab, y=ylab, title=title) +
      theme_classic() +
      theme(legend.position = "none")
  )
}

### Distribution of Housing Insecurity Responses

# Permanent Address
p1 <- mybarplot(permanent_address, 
                xlab = "Had permanent address in the past 12 months?"
)

# Spending night elsewhere
plotdf <- FIHI_sub3 %>%
  filter(permanent_address=='No') %>%
  group_by(spent_night_elsewhere) %>%
  summarise(n=n()) %>% 
  mutate(pct = n/sum(n),
         lbl = percent(pct))
p2 <- mybarplot(spent_night_elsewhere, 
                xlab = "How fequently did you spend the night elsewhere?",
                dat = plotdf
)
# Display plots side by side with the help of the patchwork package
p1 + p2



### Distribution of Food Insecurity Responses


# Q26: Food bought didn't last
p1 <- mybarplot(FI_q26, xlab = "Food bought did not last")

# Q27: Balanced diet
p2 <- mybarplot(FI_q27, xlab = "Couldn't afford balanced meals")

# Q28: 
p3 <- mybarplot(FI_q28, xlab = "Ever cut the size of meals?")

# Q3: 
p4 <- mybarplot(FI_q30, xlab = "Ever ate less than you should?")

# Q31
p5 <- mybarplot(FI_q31, xlab = "Ever got hungry but didn't eat?")



## Visualizing Missing Data 
# install.packages("naniar")
# install.packages("UpSetR")

# explore the patterns
gg_miss_upset(FIHI_sub3)

# explore missingness in variables with gg_miss_var
miss_var_summary(FIHI_sub3) %>% kable()

gg_miss_var(FIHI_sub3) + ylab("Number of missing values")

gg_miss_var(FIHI_sub3, show_pct = T) 


#==============================================================================
#                      Modeling
#==============================================================================

data_pa<- data %>%
  dplyr::select(-starts_with("FI_"), -ends_with("_changed"), -`college/school`) %>%
  filter(permanent_address != "NA") 

# dim(data_pa)

set.seed(123)
suppressPackageStartupMessages(library(mice))
data_imputed <- mice(data_pa[,-15], printFlag = F)
data_1 <- complete(data_imputed, 1)
data1 <- as.data.frame(data_1)
data_pad<-cbind("permanent_address"=data_pa$permanent_address,data1)
rm(data_imputed, data_1, data1)


# Treating imbalance classification


data_pad_balance<-ovun.sample(permanent_address ~ ., data = data_pad, method = "both", p=0.5,                             N=NROW(data_pad), seed = 125)$data
# dim(data_pad_balance)


data_pad_balance <- data_pad_balance %>%
  mutate(across(everything(), as.factor))


# PARTITION DATA
set.seed(123)
intrain <- createDataPartition(y=1:NROW(data_pad_balance), p= 0.67, list = FALSE)
training <- data_pad_balance[intrain,];  testing <- data_pad_balance[-intrain,]

# dim(training) # training data
# dim(testing)

#------ Model building -----------
# Create a wrapper function to abstract away the common aspects of model fitting
formula<- permanent_address~.
fit.model <- function(method, tunegrid="", data=NULL, formula=NULL) {
  
  data <- training
  if(is.null(formula)) formula<- permanent_address~.
  
  # Train the model
  train(
    formula,
    data = data,
    method = method,
    trControl = trainControl(method = "cv", 5),
    preProcess = c("center","scale"),
    tuneGrid = tunegrid)
  
}


# Logistic Regression
log <-train(formula,
            data=training,
            method="glm",
            family = binomial(link = "logit"),
            trControl = trainControl(method = "cv", 5),
            preProcess = c("center", "scale")) 


# LDA
lda <- train(formula,
             data=training,
             method="lda",
             trControl = trainControl(method = "cv", 5),
             preProcess = c("center", "scale"))


#-------------- Elastic Net Models -------------------
# fit a LASSO model
lasso <- fit.model("glmnet", expand.grid(.alpha=1, .lambda=seq(0,0.1,0.01)))

# Fit a Ridge regression model
ridge <- fit.model("glmnet", expand.grid(.alpha=0, .lambda=seq(0,0.1,0.01)))


#-------------- 

# Support Vector Machine with linear kernel

trctrl <- trainControl(method = "cv", number=5)
svc <- train(formula, data = training, method = "svmLinear",
             trControl=trctrl, prob.model=T,
             tuneLength = 10)


# Support Vector Machine with radial kernel
trctrl <- trainControl(method = "cv", number=5)
svmR <- train(formula , data = training, method = "svmRadial",
              trControl=trctrl, prob.model=T,
              tuneLength = 10)



## Making predictions 


# Create a custom confusion matrix with performance metrics
metrics <- function(model_object, response="", test_data=NULL) {
  # response = "permanent_address"
  # model_object <- log
  # 
  if(is.null(test_data)) test_data <- testing
  
  # make predictions
  prediction <- predict(model_object, test_data) 
  
  target <- test_data[, response]
  
  cmat <- confusionMatrix(prediction, target, mode = "prec_recall")
  
  misscal<- round(mean(prediction != target),digits = 2)
  
  # Returned outputs
  return(list(
    accuracy = (1-misscal),
    mcr = misscal,
    sens = round(cmat$byClass[1],2),
    spec = round(cmat$byClass[2],2),
    fbeta = round(cmat$byClass[7],2)
  ))
  
}

metric_log <- metrics(lasso, response = "permanent_address")
#------- Compute performance metrics for the full models ---------------
log.metric <- metrics(log, response = "permanent_address")
lda.metric <- metrics(lda, response = "permanent_address")
# knn.metric <- metrics(knn)
lasso.metric <- metrics(lasso, response = "permanent_address")
ridge.metric <- metrics(ridge, response = "permanent_address")
# bag.metric <- metrics(bag, response = "permanent_address")
# rf.metric <- metrics(rf)
svc.metric <- metrics(svc, response = "permanent_address")
# svmP.metric <- metrics(svmP)
svmR.metric <- metrics(svmR, response = "permanent_address")

mod.sum <- data.frame(rbind(
  c("Logistic", log.metric$mcr, log.metric$accuracy, log.metric$sens, log.metric$spec, log.metric$fbeta),
  c("LDA",  lda.metric$mcr, lda.metric$accuracy, lda.metric$sens, lda.metric$spec, lda.metric$fbeta),
  c("LASSO", lasso.metric$mcr, lasso.metric$accuracy, lasso.metric$sens, lasso.metric$spec, lasso.metric$fbeta),
  c("Ridge", ridge.metric$mcr, ridge.metric$accuracy, ridge.metric$sens, ridge.metric$spec, ridge.metric$fbeta),
  # c("Bagging", bag.metric$mcr, bag.metric$accuracy, bag.metric$sens, bag.metric$spec, bag.metric$fbeta),
  c("SVC",  svc.metric$mcr, svc.metric$accuracy, svc.metric$sens, svc.metric$spec, svc.metric$fbeta),
  c("SVM (Radial Kernel)", svmR.metric$mcr, svmR.metric$accuracy, svmR.metric$sens, svmR.metric$spec, svmR.metric$fbeta)))

names(mod.sum) <- c("Model", "Misclassification Rate", "Accuracy", "Sensitivity", "Specificity", "fbeta")

kable(mod.sum, align = "lccccc", caption = "Table : Evaluation metrics for Housing Insecurity with Permanent Address as a response") %>%
  kable_paper("hover", full_width = F)%>% 
  kable_styling(font_size = 12, latex_options = c("HOLD_position"))




Var <- varImp(svmR, scale = FALSE)
plot(Var, main ="Figure: Variable of Importance")



#-------------------------- END OF FILE ---------------------------------------





