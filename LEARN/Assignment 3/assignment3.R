# ----------------------------
# Environment Setup
# ----------------------------
# Load libraries
library(tidyverse)
library(caret)
library(caretEnsemble)
library(rpart)
library(C50)
library(gbm)
library(randomForest)
library(doParallel)
library(pROC)

# Load dataset
cancer <- read.csv("dataR2.csv")


# ----------------------------
# Initial Data Inspection and Transformation
# ----------------------------
# Initial data inspection
str(cancer)
summary(cancer)
colSums(is.na(cancer)) # no missing data

# Convert outcome variable to factor format (1 = healthy, 2 = patient [cancer])
cancer$Classification <- factor(cancer$Classification, levels = c(1, 2),
                                labels = c("Healthy", "Patient"))

# Confirm conversion and target variable class distribution
str(cancer)
table(cancer$Classification) # 52 healthy, 64 patient

# Near-zero variance analysis
nearZeroVar(cancer, saveMetrics = TRUE) # no nzv/zero-variance


# ----------------------------
# Train/Test Split
# ----------------------------
set.seed(500)
trainIndex <- createDataPartition(cancer$Classification, p = 0.8, list = FALSE)
train <- cancer[trainIndex, ]
test <- cancer[-trainIndex, ]


# ----------------------------
# Training Controls
# ----------------------------
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


# ----------------------------
# Bagging Models
# ----------------------------
# Enable parellelization
cl <- makeCluster(4)
registerDoParallel(cl)

# Treebag
set.seed(500)
tb_model <- train(Classification ~ .,
                  data = train,
                  method = "treebag",
                  trControl = ctrl,
                  metric = "ROC"
                  )

# Random forest (bagging with feature sampling)
set.seed(500)
rf_model <- train(Classification ~ .,
                  data = train,
                  method = "rf",
                  trControl = ctrl,
                  tuneLength = 5,
                  metric = "ROC"
                  )

# Summarize results
bagging_results <- resamples(list(treebag = tb_model, rf = rf_model))
summary(bagging_results)
dotplot(bagging_results)
modelCor(bagging_results)
splom(bagging_results)



# ----------------------------
# Boosting Models
# ----------------------------
# C5.0 Boosting
set.seed(500)
c50_model <- train(Classification ~ .,
                   data = train,
                   method = "C5.0",
                   trControl = ctrl,
                   tuneLength = 5,
                   metric = "ROC"
                   )

# Gradient Boosting (GBM)
set.seed(500)
gbm_model <- train(Classification ~ .,
                   data = train,
                   method = "gbm",
                   trControl = ctrl,
                   verbose = FALSE,
                   metric = "ROC"
                   )

# Summarize results
boosting_results <- resamples(list(c50 = c50_model, gbm = gbm_model))
summary(boosting_results)
dotplot(boosting_results)
modelCor(boosting_results)
splom(boosting_results)


# ----------------------------
# Stacking Models
# ----------------------------
# First create candidate models for stacking
stack_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              savePredictions = "final",
                              classProbs = TRUE)

# Create model list
algorithms <- c("rf", "C5.0", "glm")  # RF, C5.0, and Logistic Regression

set.seed(500)
stack_models <- caretList(
  Classification ~ .,
  data = train,
  trControl = stack_control,
  methodList = algorithms,
  metric = "ROC"
)

# Combine models with logistic regression meta-model
set.seed(500)
ensemble <- caretStack(
  stack_models,
  method = "glm",
  metric = "ROC",
  trControl = trainControl(
    method = "boot",
    number = 200,
    savePredictions = "final",
    classProbs = TRUE
  )
)

stopImplicitCluster()

# Summarize results
print(stack_models)
print(ensemble)
summary(ensemble)
dotplot(ensemble)


# ----------------------------
# Model Evaluation
# ----------------------------
# Treebag
pred_tb <- predict(tb_model, test, type = "prob")
roc_tb <- roc(test$Classification, pred_tb$Patient)
plot.roc(roc_tb, print.auc = TRUE, main = "Treebag")
confusionMatrix(predict(tb_model, test), test$Classification,
                mode = "everything")

# Bagged RF
pred_rf <- predict(rf_model, test, type = "prob")
roc_rf <- roc(test$Classification, pred_rf$Patient)
plot.roc(roc_rf, print.auc = TRUE, main = "Bagged Random Forest")
confusionMatrix(predict(rf_model, test), test$Classification,
                mode = "everything")

# C5.0 Boosted
pred_c50 <- predict(c50_model, test, type = "prob")
roc_c50 <- roc(test$Classification, pred_c50$Patient)
plot.roc(roc_c50, print.auc = TRUE, main = "C5.0 Boosted")
confusionMatrix(predict(c50_model, test), test$Classification,
                mode = "everything")

# GBM
pred_gbm <- predict(gbm_model, test, type = "prob")
roc_gbm <- roc(test$Classification, pred_gbm$Patient)
plot.roc(roc_gbm, print.auc = TRUE, main = "Gradient Boosting")
confusionMatrix(predict(gbm_model, test), test$Classification,
                mode = "everything")

# Stack GLM
pred_stack_glm <- predict(stack_models, test)
roc_stack_glm <- roc(test$Classification, pred_stack_glm$glm)
plot.roc(roc_stack_glm, print.auc = TRUE, main = "Stack GLM")
confusionMatrix(predict(stack_models$glm, test), test$Classification,
                mode = "everything")

# Stack C5.0
pred_stack_c50 <- predict(stack_models, test)
roc_stack_c50 <- roc(test$Classification, pred_stack_c50$C5.0)
plot.roc(roc_stack_c50, print.auc = TRUE, main = "Stack C5.0")
confusionMatrix(predict(stack_models$C5.0, test), test$Classification,
                mode = "everything")

# Stack RF
pred_stack_rf <- predict(stack_models, newdata = test)
roc_stack_rf <- roc(test$Classification, pred_stack_rf$rf)
plot.roc(roc_stack_rf, print.auc = TRUE, main = "Stack Random Forest")
confusionMatrix(predict(stack_models$rf, test), test$Classification,
                mode = "everything")

# Stacked Ensemble
pred_ensemble <- predict(ensemble, test)
roc_ensemble <- roc(test$Classification, pred_ensemble$Patient)
plot.roc(roc_ensemble, print.auc = TRUE, main = "Stacked Ensemble")

ensemble_class <- ifelse(pred_ensemble$Patient > 0.5, "Patient", "Healthy")
ensemble_class <- factor(ensemble_class, levels = levels(test$Classification))
confusionMatrix(data = ensemble_class, reference = test$Classification,
                mode = "everything")



# Model comparison by summary table and visualization
model_names <- c("Treebag", "RF", "C5.0", "GBM", "Stack_RF", "Stack_C50",
                 "Stack_GLM", "Ensemble")
auc_scores <- c(roc_tb$auc, roc_rf$auc, roc_c50$auc, roc_gbm$auc,
                roc_stack_rf$auc, roc_stack_c50$auc, roc_stack_glm$auc,
                roc_stack_ensemble$auc)
results <- data.frame(Model = model_names, AUC = auc_scores)
results[order(-results$AUC), ]

library(ggplot2)
ggplot(results, aes(x = reorder(Model, AUC), y = AUC)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(AUC, 3)), hjust = -0.1) +
  coord_flip(ylim = c(0.5, 1)) +
  labs(title = "Model Performance Comparison", 
       x = "Model", 
       y = "ROC AUC") +
  theme_minimal()


# Check correlation between base model predictions
cor_matrix <- cor(data.frame(
  RF = predict(stack_models$rf, test, type = "prob")$Patient,
  C50 = predict(stack_models$C5.0, test, type = "prob")$Patient,
  GLM = predict(stack_models$glm, test, type = "prob")$Patient
))
print(cor_matrix)


## Stacked GLM
# Get probabilities
probs <- predict(stack_models$glm, test, type = "prob")$Patient

# Find optimal threshold
roc_obj <- roc(test$Classification, probs)
coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Create confusion matrix at optimal threshold
optimal_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")$threshold
pred_class <- ifelse(probs > optimal_threshold, "Patient", "Healthy") %>% 
  factor(levels = levels(test$Classification))

# Detailed performance
confusionMatrix(pred_class, test$Classification, mode = "everything")
