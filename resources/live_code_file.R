## We share necessary codes here while working together.



```{r}

training <- dat$train %>% 
  mutate(FI_q31 = factor(FI_q31, levels = 1:2, 
                         labels = c("Yes", "No")))

testing <- dat$test %>% 
  mutate(FI_q31 = factor(FI_q31, levels = 1:2, 
                         labels = c("Yes", "No")))

set.seed(100)
trctrl <- trainControl(
  method="cv",
  number=5,
  savePredictions="final",
  classProbs=TRUE,
  sampling = "up",
  index=createResample(training$FI_q31, 5),
  summaryFunction=twoClassSummary
)

model_list <- caretList(
  FI_q31~., data=training,
  trControl=trctrl,
  metric = "ROC",
  methodList=c("rpart"),
  continue_on_fail = FALSE
)



# p <- as.data.frame(predict(model_list, newdata=(testing)))
# print(p)
# 
# # Generating predicted classes
# p <- p %>%
#   mutate(glm_class = as.factor(ifelse(glm < 0.5, "No", "Yes" )),
#          lda_class = as.factor(ifelse(lda < 0.5, "No", "Yes" )),
#          glm_class = factor(glm_class, levels = c("Yes", "No"))
#          )
# 
# table(p$glm_class, testing$FI_q31)

ensemble_1 <- caretEnsemble(model_list, 
                            metric = "ROC", 
                            trControl = trctrl)

plot(ensemble_1)

Var <- varImp(model_list$rpart, scale = FALSE)
plot(Var, main ="Figure: Variable of Importance")



library(doParallel)
# x_train <- training[,-which(names(training)=="FI_q31")]
# y_train <- training[, "FI_q31"]
registerDoParallel(7)
getDoParWorkers()
set.seed(123)
my_control <- trainControl(method = "cv", # for "cross-validation"
                           number = 5, # number of k-folds
                           savePredictions = "final",
                           classProbs=TRUE,
                           index=createResample(training$FI_q31, 5),
                           allowParallel = TRUE)

set.seed(222)
model_list <- caretList(FI_q31~., data=training,
                        trControl = my_control,
                        methodList = c("glm"),
                        tuneList = NULL,
                        continue_on_fail = FALSE)
```
