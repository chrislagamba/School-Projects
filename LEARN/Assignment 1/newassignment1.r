# ----------------------------
# Data Loading & Preparation
# ----------------------------
library(naniar)
library(summarytools)
library(caret)
library(knitr)
library(doParallel)
library(missForest)
library(RSBID)
library(tidyverse)
library(caret)
library(pROC)
library(rpart.plot)


# Load dataset
cc_risk <- read.csv("risk_factors_cervical_cancer.csv",
                    na.strings = "?",
                    stringsAsFactors = FALSE)


# ----------------------------
# Initial Data Inspection and Cleaning
# ----------------------------
# Initial data inspection
dfSummary(cc_risk)
gg_miss_var(cc_risk, show_pct = TRUE)

# Clean up column names to avoid errors
column_names <- colnames(cc_risk)
names(cc_risk) <- gsub("\\.", "", names(cc_risk))
cleaned_column_names <- colnames(cc_risk)
print(column_names)
print(cleaned_column_names)
rm(column_names, cleaned_column_names) # clean up of environment
# "STDsTimesince" variables missing 91.7%; several others missing ~12.2-13.6%
# severe class imbalance of target variable (DxCancer): only 2.1% positive cases
# several variables with little or no predictive power based on distribution

# Remove known leakage variables (tests confirming cervical cancer)
leakage_vars <- c("Biopsy", "Hinselmann", "Citology", "Schiller")

cc_risk <- cc_risk %>% select(-all_of(leakage_vars))

# Remove variables with excessive missingness (>90%)
cc_risk <- cc_risk %>%
  select(-STDsTimesincefirstdiagnosis, -STDsTimesincelastdiagnosis)

# Convert integers to numeric
cc_risk$Age <- as.numeric(cc_risk$Age)
cc_risk$STDsNumberofdiagnosis <- as.numeric(cc_risk$STDsNumberofdiagnosis)
str(cc_risk)


# ----------------------------
# Train/Test Split
# ----------------------------
set.seed(250)
trainIndex <- createDataPartition(cc_risk$DxCancer, p = 0.8, list = FALSE)
train <- cc_risk[trainIndex, ]
test <- cc_risk[-trainIndex, ]


# ----------------------------
# Near-Zero-Variance Analysis
# ----------------------------
# Identify zero- or near-zero-variance variables
nzv <- nearZeroVar(train, saveMetrics = TRUE)
kable(nzv, caption = "Near-Zero and Zero-Variance Variables")

zero_var <- nzv %>%
  filter(zeroVar == TRUE)
kable(zero_var, caption = "Zero-Variance Variables")

near_zero <- nzv %>%
  filter(nzv == TRUE & zeroVar != TRUE)
kable(near_zero, caption = "Near-Zero Variance Variables")

table(train$STDsvaginalcondylomatosis) # 3 yes; predictive power?
table(train$STDsgenitalherpes) # 1 yes; no pred. power?
table(train$STDsHIV)
table(train$STDsHepatitisB) # 1 yes; no pred. power?
table(train$STDsHPV) # 2 yes; DxHPV similar variable
table(train$DxHPV)
table(train$Dx)
table(train$STDssyphilis)
table(train$DxCIN) # 6 yes; predictive power?
table(train$STDspelvicinflammatorydisease) # 1 yes, pred. power?

## lit review:
# Hep B linked to cervical cancer (https://pubmed.ncbi.nlm.nih.gov/34672431/;
# https://pubmed.ncbi.nlm.nih.gov/36320009/)

# genital herpes linked (https://pubmed.ncbi.nlm.nih.gov/35494213/;
# https://pubmed.ncbi.nlm.nih.gov/37997472/)

# pelvic inflammatory disease associated with cervical cancer:
# https://pubmed.ncbi.nlm.nih.gov/39327298/

# HIV associated with cervical cancer: USPSTF CC screening guidelines

# syphilis associated with cerv cancer: https://pubmed.ncbi.nlm.nih.gov/35813967/

# smoking has a known association with increased risk of all cancer

# HPV strongly associated with increased risk of cancer, especially with other STIs

# IUD: 1 SR/MA suggesting decreased risk (https://pubmed.ncbi.nlm.nih.gov/29112647/);
# 1 SR/MA suggesting no correlation (https://pubmed.ncbi.nlm.nih.gov/34752778/)

# remaining variables not associated based on lit review


# Remove due to zero variance or lack of predictive power/no clinical relevance
vars_to_remove <- c("STDsAIDS", "STDscervicalcondylomatosis", "STDsHPV",
                    "STDsmolluscumcontagiosum", "STDsgenitalherpes",
                    "STDsHepatitisB", "STDsvaginalcondylomatosis",
                    "STDspelvicinflammatorydisease")

train <- train %>%
  select(-all_of(vars_to_remove))

test <- test %>%
  select(-all_of(vars_to_remove))

# Environment clean up
rm(zero_var, near_zero, leakage_vars, nzv, vars_to_remove)


# ----------------------------
# Correlation Heatmap and Matrix
# ----------------------------
# Correlation heatmap for better visualization
library(corrplot)
cor_matrix <- cor(train, use = "complete.obs")
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         hclust.method = "ward.D2",
         tl.col = "black",
         tl.cex = 0.5,
         addCoef.col = "black",
         number.cex = 0.5,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Collinearity Heatmap of All Variables",
         mar = c(0, 0, 1, 0))

# Convert cor_matrix to a df, remove duplicates, and print results
cor_df <- as.data.frame.table(cor_matrix, stringsAsFactors = FALSE) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) %>%
  filter(
    abs(Correlation) >= 0.70 &    # Absolute correlation > 0.7
      Var1 != Var2                 # Remove variable vs itself
  ) %>%
  arrange(desc(abs(Correlation)))

cor_df <- cor_df %>%       # Remove duplicate pairs (A-B vs B-A)
  mutate(pair = paste(pmin(Var1, Var2), pmax(Var1, Var2))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

kable(cor_df, caption = "Initial Variable Correlations (|r| >/= 0.7)")

# several similar variables but DT and RF handle multicollinearity well;
# focus on years (IUDyears, Smokesyears, etc)?


# Remove due to no clinical significance and/or high correlation
train <- train %>%
  select(-STDscondylomatosis, -STDsvulvoperinealcondylomatosis, -DxHPV, -Dx)

test <- test %>%
  select(-STDscondylomatosis, -STDsvulvoperinealcondylomatosis, -DxHPV, -Dx)


# ----------------------------
# Missing Data Analysis
# ----------------------------
# Check NAs across all columns
colSums(is.na(train))
gg_miss_var(train)

# Closer look at numeric variables to determine best imputation
table(train$HormonalContraceptives) # mode is 1
kable(table(train$HormonalContraceptivesyears)) # mean 2.3; median 0.5
hist(train$HormonalContraceptivesyears)
# # binary would impute as "Yes", years should match binary - use mean?

table(train$STDs)
table(train$STDsnumber) # missing 85, mean 0.2; use mode
table(train$STDsNumberofdiagnosis) # no NA
# keep Numberofdiagnosis since no NAs and remove STDsnumber

table(train$Numofpregnancies) # missing 47; mean 2.3; median 2
table(train$Numberofsexualpartners) # mean 2.5; median 2
# use median for these due to mean being unrealistic

table(train$Smokes) # missing 12; would impute as no
kable(table(train$Smokesyears)) # use mode
kable(table(train$Smokespacksyear)) # use mode
# use mode for all for consistency

# several variables have low or no predictive power due to distributions
# several variables are missing > 10% of values
# try missForest to see if distributions change?


# ----------------------------
# Missing Data Imputation Using missForest
# ----------------------------
# Create a copy of the train set prior to imputation
train_f <- train

# Verify column names
colnames(train_f)

# Identify binary/numeric variables based on mode imputation and impute missing
binary_vars <- c(
  "Smokes", "HormonalContraceptives", "IUD", "STDs", "STDssyphilis", 
  "STDsHIV", "DxCancer", "DxCIN")

# Convert binary variables to factors
train_f[binary_vars] <- lapply(
  train_f[binary_vars],
  function(x) {
    factor(
      x,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  }
)
str(train_f)

# Impute missing data using missForest
cl <- makeCluster(8) # set number of cores
registerDoParallel(cl)
set.seed(250)
imputed_data <- missForest(
  train_f,
  maxiter = 5,
  ntree = 1000,
  verbose = TRUE
)
stopImplicitCluster()

# Extract imputed data
train <- imputed_data$ximp

# Verify no NAs remain and correct values imputed for binary_vars (only 0/1)
colSums(is.na(train))
rm(train_f, cor_df)

# Check numeric imputations
table(train$Numberofsexualpartners) # need to round
table(train$Firstsexualintercourse) # need to round

# Round numeric values imputed by missForest that should be whole numbers
numeric_vars <- c("Age", "Numberofsexualpartners", "Firstsexualintercourse",
                  "Numofpregnancies", "STDsnumber")

train[numeric_vars] <- lapply(train[numeric_vars],
                              round, digits = 0)

table(train$Numberofsexualpartners)
table(train$STDsnumber)
table(train$IUD)
table(train$Smokes)
table(train$Firstsexualintercourse)

# Convert factors back to numeric for imputation into test set
train[binary_vars] <- lapply(
  train[binary_vars],
  function(x) as.numeric(x) - 1
)
str(train)

# Impute missing data in test set using train set to avoid leakage
library(recipes)
impute_recipe <- recipe(DxCancer ~ ., data = train) %>%
  step_impute_mode(all_nominal()) %>%   # Binary variables
  step_impute_median(all_numeric()) %>% # Median for numeric vars
  prep(training = train)    # Train imputation rules

test <- bake(impute_recipe, new_data = test)
colSums(is.na(test))

# Clean up environment
rm(imputed_data, impute_recipe, numeric_vars, cor_matrix)


# ----------------------------
# Correlation Heatmap Revisited
# ----------------------------
# New correlation heatmap after missForest
cor_matrix <- cor(train, use = "complete.obs")
corrplot_2 <- corrplot(cor_matrix,
  method = "color", type = "upper", order = "hclust",
  hclust.method = "ward.D2", tl.col = "black", tl.cex = 0.8,
  addCoef.col = "black", number.cex = 0.6,
  col = colorRampPalette(c("blue", "white", "red"))(200),
  title = "Collinearity Heatmap of All Variables After Imputation",
  mar = c(0, 0, 2, 0)
)

# Convert cor_matrix_forest to a df, remove duplicates, and print results
cor_df <- as.data.frame.table(cor_matrix, stringsAsFactors = FALSE) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) %>%
  filter(
    abs(Correlation) >= 0.70 &    # Absolute correlation > 0.7
      Var1 != Var2                 # Remove variable vs itself
  ) %>%
  arrange(desc(abs(Correlation)))

cor_df <- cor_df %>%       # Remove duplicate pairs (A-B vs B-A)
  mutate(pair = paste(pmin(Var1, Var2), pmax(Var1, Var2))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

kable(cor_df, caption = "High Correlations (|r| >/= 0.7) [missForest]")

filtered_vars <- c("STDs", "STDsNumberofdiagnosis", "IUD", "Smokesyears")
 
train <- train %>%
  select(-all_of(filtered_vars))

test <- test %>%
  select(-all_of(filtered_vars))

colnames(train)


# ----------------------------
# SMOTE-NC Application
# ----------------------------
str(train)

# Convert binary variables to factors
print(binary_vars)
colnames(train)
binary_vars <- c(
  "Smokes", "HormonalContraceptives", "STDssyphilis", "STDsHIV", "DxCIN",
  "DxCancer")

train[binary_vars] <- lapply(
  train[binary_vars],
  function(x) {
    factor(
      x,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  }
)
str(train)

# Backup of train - will impute data into this df
train_b <- train

table(train_b$DxCancer) # 13 positive
train_b <- SMOTE_NC(train, "DxCancer", 100, 5)
table(train_b$DxCancer) # 674 negative, 674 positive

dfSummary(train)
dfSummary(train_b)

table(train_b$STDsNumberofdiagnosis)
table(train_b$STDsnumber) # need to round
table(train_b$IUDyears) # round to nearest month
table(cc_risk$IUDyears) # nearest month if <1, else nearest year (only 1 half-year value)
table(train_b$Numofpregnancies) # need to round
table(train_b$Firstsexualintercourse) # need to round
table(train_b$Age) # need to round
table(train_b$Numberofsexualpartners) # need to round
table(train_b$HormonalContraceptivesyears) # round to nearest month?
table(cc_risk$HormonalContraceptivesyears) # half-year until 7 years
table(train_b$Smokespacksyear) # leave as is?
table(cc_risk$Smokespacksyear)
table(train_b$Smokesyears)
table(cc_risk$Smokesyears)

# Numeric variables to round
numeric_vars <- c(
  "Age", "Numberofsexualpartners", "Firstsexualintercourse", "Numofpregnancies",
  "STDsnumber"
)

# Round numeric_vars to nearest integer and confirm rounding
train_b[, numeric_vars] <- lapply(
  train_b[, numeric_vars],
  function(x) round(x, 0)
)

table(train_b$Age) # yes
table(train_b$STDsnumber) # yes
table(train_b$Numofpregnancies) # yes
table(train_b$Firstsexualintercourse) # yes
table(train_b$Age) # yes
table(train_b$Numberofsexualpartners) # yes

# Round IUDyears to nearest month if < 1, else nearest year
train_b$IUDyears <- ifelse(
  train_b$IUDyears < 1,
  round(train_b$IUDyears * 12) / 12,
  round(train_b$IUDyears)
) |>
pmax(0)

table(train_b$IUDyears)

# Round HormonalContraceptivesyears to closest month if < 1 year,
# else closest half-year until 7 years, else closest year after 7 years
train_b$HormonalContraceptivesyears <- ifelse(
  train_b$HormonalContraceptivesyears < 1,
  round(train_b$HormonalContraceptivesyears * 12) / 12,
  ifelse(
    train_b$HormonalContraceptivesyears < 7,
    round(train_b$HormonalContraceptivesyears * 2) / 2,
    round(train_b$HormonalContraceptivesyears)
    )
) |>
pmax(0)

table(train_b$HormonalContraceptivesyears)

# Last dfSummary before modeling
dfSummary(train_b)

# Verify train and test set variables match
colnames(train_b)
colnames(test)

# Verify structure of train and test sets
str(train_b) # good to model
str(test) # change all binary to factors

# Change binary variables to factors in test set
# binary_vars <- c(binary_vars, "DxCancer")
test[binary_vars] <- lapply(
  test[binary_vars],
  function(x) {
    factor(
      x,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  }
)
str(test)

# Final clean-up of environment
rm(filtered_vars, numeric_vars, binary_vars, cor_df, cor_matrix, cc_risk,
   corrplot_2)


# ----------------------------
# Decision Tree Modeling (Initial boot632)
# ----------------------------
# Modeling controls
control <- trainControl(
  method = "boot632",
  number = 100,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final"
)

# Tuning grid
tuna_dt <- expand.grid(cp = seq(0.0001, 0.5, 0.0005))

# enable parallelization
cl <- makeCluster(8)
registerDoParallel(cl)

# Decision tree model
set.seed(250)
dt_boot1 <- train(
  DxCancer ~ .,
  data = train_b,
  method = "rpart",
  metric = "ROC",
  trControl = control,
  tuneGrid = tuna_dt,
  parms = list(split = "information")
)
stopImplicitCluster()

# Inspect and summarize accuracy of model
summary(dt_boot1)
print(dt_boot1)
plot(dt_boot1)
varImp(dt_boot1)

# ROC curve for training model
trainroc_dt_boot1 <- plot.roc(dt_boot1$pred$obs, dt_boot1$pred$Yes, 
                              print.auc = TRUE, debug = FALSE,
                              main = paste("Decision Tree - Boot632 - Training 
                                           (Initial)"))

# Make predictions on the test sets
dt_pred_prob <- predict(dt_boot1, test, type = "prob")
dt_pred <- predict(dt_boot1, test, type = "raw")
dt_boot1_cm <- confusionMatrix(dt_pred, test$DxCancer, mode = "everything",
                               positive = "Yes")
dt_boot1_cm # Sens 0.80, Spec 0.98193, Recall 0.80, F1 = 0.66667, Prec 0.57143

plot.roc(test$DxCancer, dt_pred_prob$Yes, print.auc = TRUE, debug = FALSE,
         axes = TRUE, main = paste("Decision Tree - Boot632 - Test 
                                   (Initial)")) -> dt_boot_roc1

rm(dt_pred, dt_pred_prob)
dt_boot1_summary <- list(dt_boot1, varImp(dt_boot1), dt_boot1_cm, dt_boot_roc1,
                         trainroc_dt_boot1)

# Pruned DT based on best cp
best_cp <- dt_boot1$bestTune$cp
pruned_dt <- dt_boot1$finalModel
rpart.plot(pruned_dt)
varImp(pruned_dt)


# ----------------------------
# Decision Tree Modeling (Tuned boot632)
# ----------------------------
# Modeling controls
control <- trainControl(
  method = "boot632",
  number = 100,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final"
)

# Tuning grid
tuna_dt <- expand.grid(cp = best_cp)

# enable parallelization
cl <- makeCluster(8)
registerDoParallel(cl)

# Decision tree model
set.seed(250)
dt_boot2 <- train(
  DxCancer ~ .,
  data = train_b,
  method = "rpart",
  metric = "ROC",
  trControl = control,
  tuneGrid = tuna_dt
)
stopImplicitCluster()

# Inspect and summarize accuracy of model
summary(dt_boot2)
print(dt_boot2)
varImp(dt_boot2)

# ROC curve for training model
trainroc_dt_boot2 <- plot.roc(dt_boot2$pred$obs, dt_boot2$pred$Yes, 
                              print.auc = TRUE, debug = FALSE,
                              main = paste("Decision Tree - Boot632 - Training 
                                           (Tuned)"))

# Make predictions on the test sets
dt_pred_prob <- predict(dt_boot2, test, type = "prob")
dt_pred <- predict(dt_boot2, test, type = "raw")
dt_boot2_cm <- confusionMatrix(dt_pred, test$DxCancer, mode = "everything",
                               positive = "Yes")
dt_boot2_cm # Sens 0.80, Spec 0.98193, Recall 0.80, F1 = 0.66667, Prec 0.57143

plot.roc(test$DxCancer, dt_pred_prob$Yes, print.auc = TRUE, debug = FALSE,
         axes = TRUE, main = paste("Decision Tree - Boot632 - Test 
                                   (Tuned)")) -> dt_boot_roc2

rm(dt_pred, dt_pred_prob)
dt_boot2_summary <- list(dt_boot2, varImp(dt_boot2), dt_boot2_cm, dt_boot_roc2,
                         trainroc_dt_boot2)


# ----------------------------
# Decision Tree Modeling (Pruned boot632)
# ----------------------------
# For pruning
train_boot <- train_b
test_boot <- test

train_boot <- train_boot %>%
  select(-STDsnumber, -Numberofsexualpartners, -Smokes, -STDssyphilis, -STDsHIV,
         -DxCIN, -HormonalContraceptivesyears, -Smokespacksyear,
         -Numofpregnancies)

test_boot <- test_boot %>%
  select(-STDsnumber, -Numberofsexualpartners, -Smokes, -STDssyphilis, -STDsHIV,
         -DxCIN, -HormonalContraceptivesyears, -Smokespacksyear,
         -Numofpregnancies)

# Modeling controls
control <- trainControl(
  method = "boot632",
  number = 100,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final"
)

# Tuning grid
tuna_dt <- expand.grid(cp = best_cp)

# enable parallelization
cl <- makeCluster(8)
registerDoParallel(cl)

# Decision tree model
set.seed(250)
dt_boot3 <- train(
  DxCancer ~ .,
  data = train_boot,
  method = "rpart",
  metric = "ROC",
  trControl = control,
  tuneGrid = tuna_dt
)
stopImplicitCluster()

# Inspect and summarize accuracy of model
summary(dt_boot3)
print(dt_boot3)
varImp(dt_boot3)

# ROC curve for training model
trainroc_dt_boot3 <- plot.roc(dt_boot3$pred$obs, dt_boot3$pred$Yes, 
                              print.auc = TRUE, debug = FALSE,
                              main = paste("Decision Tree - Boot632 - Training 
                                           (Pruned)"))

# Make predictions on the test sets
dt_pred_prob <- predict(dt_boot3, test_boot, type = "prob")
dt_pred <- predict(dt_boot3, test_boot, type = "raw")
dt_boot3_cm <- confusionMatrix(dt_pred, test_boot$DxCancer, mode = "everything",
                               positive = "Yes")
dt_boot3_cm # Sens 0.80, Spec 0.98193, Recall 0.80, F1 = 0.66667, Prec 0.57143

plot.roc(test_boot$DxCancer, dt_pred_prob$Yes, print.auc = TRUE, debug = FALSE,
         axes = TRUE, main = paste("Decision Tree - Boot632 - Test 
                                   (Pruned)")) -> dt_boot_roc3

rm(dt_pred, dt_pred_prob)
dt_boot3_summary <- list(dt_boot3, varImp(dt_boot3), dt_boot3_cm, dt_boot_roc3,
                         trainroc_dt_boot3)


# ----------------------------
# Decision Tree Modeling (Pruned/Tuned boot632)
# ----------------------------
# For pruning
train_boot <- train_b
test_boot <- test

train_boot <- train_boot %>%
  select(-Dx)

test_boot <- test_boot %>%
  select(-Dx)

# Modeling controls
control <- trainControl(
  method = "boot632",
  number = 500,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final"
)

# Tuning grid
tuna_dt <- expand.grid(cp = best_cp)

# enable parallelization
cl <- makeCluster(2)
registerDoParallel(cl)

# Decision tree model
set.seed(250)
dt_boot4 <- train(
  DxCancer ~ .,
  data = train_boot,
  method = "rpart",
  metric = "ROC",
  trControl = control,
  tuneGrid = tuna_dt
)
stopImplicitCluster()

# Inspect and summarize accuracy of model
summary(dt_boot4)
varImp(dt_boot4)

# ROC curve for training model
trainroc_dt_boot4 <- plot.roc(dt_boot4$pred$obs, dt_boot4$pred$Yes, 
                              print.auc = TRUE, debug = FALSE,
                              main = paste("Decision Tree - Boot632 - Training 
                                           (Pruned - 500 Boots)"))

# Make predictions on the test sets
dt_pred_prob <- predict(dt_boot4, test_boot, type = "prob")
dt_pred <- predict(dt_boot4, test_boot, type = "raw")
dt_boot4_cm <- confusionMatrix(dt_pred, test_boot$DxCancer, mode = "everything",
                               positive = "Yes")
dt_boot4_cm # Sens 0.40, Spec 0.85542, Recall 0.40, F1 = 0.12903, Prec 0.07692

plot.roc(test_boot$DxCancer, dt_pred_prob$Yes, print.auc = TRUE, debug = FALSE,
         axes = TRUE, main = paste("Decision Tree - Boot632 - Test 
                                   (Pruned - 500 Boots)")) -> dt_boot_roc4

rm(dt_pred, dt_pred_prob)
dt_boot4_summary <- list(dt_boot4, varImp(dt_boot4), dt_boot4_cm, dt_boot_roc4,
                         trainroc_dt_boot4)


# ----------------------------
# Random Forest Modeling (Initial Boot632)
# ----------------------------
# Modeling controls
control <- trainControl(
  method = "boot632",
  number = 100,
  summaryFunction = prSummary,
  classProbs = TRUE,
  savePredictions = "final",
)

# Tuning grid
tuna_rf <- expand.grid(mtry = 1:10)

# enable parallelization
cl <- makeCluster(8)
registerDoParallel(cl)

# Random forest model
set.seed(250)
rf_boot1 <- train(
  DxCancer ~ .,
  data = train_b,
  method = "rf",
  metric = "AUC",
  trControl = control,
  tuneGrid = tuna_rf,
  ntrees = 1000
)
stopImplicitCluster()

# Check OOB
oob_rf_boot1 <- rf_boot1$finalModel
plot(oob_rf_boot1, main = "OOB Error vs. Number of Trees (Repeated CV Tuned, 1000 Trees)")

# Inspect and summarize accuracy of model
summary(rf_boot1)
print(rf_boot1)
plot(rf_boot1)
varImp(rf_boot1)
print(rf_boot1$results)

# ROC curve for training model
trainroc_rf_boot1 <- plot.roc(rf_boot1$pred$obs, rf_boot1$pred$Yes, 
                              print.auc = TRUE, debug = FALSE,
                              main = paste("RF - Boot 632 - Training 
                                           (Initial)"))

# Make predictions on the test sets
rf_pred_prob <- predict(rf_boot1, test, type = "prob")
rf_pred <- predict(rf_boot1, test, type = "raw")
rf_boot1_cm <- confusionMatrix(rf_pred, test$DxCancer, mode = "everything",
                               positive = "Yes")
rf_boot1_cm # Sens 0.40, Spec 0.98193, Recall 0.40, F1 = 0.40, Prec 0.40

plot.roc(test$DxCancer, rf_pred_prob$Yes, print.auc = TRUE, debug = FALSE,
         axes = TRUE, main = paste("Random Forest - Boot632 - Test 
                                   (Initial)")) -> rf_boot_roc1

rm(rf_pred, rf_pred_prob)
rf_boot1_summary <- list(rf_boot1, varImp(rf_boot1), rf_boot1_cm, rf_boot_roc3,
                         trainroc_rf_boot1)

# Best mtry
best_mtry <- rf_boot1$bestTune$mtry


# ----------------------------
# Random Forest Modeling (Tuned Boot632)
# ----------------------------
# Modeling controls
control <- trainControl(
  method = "boot632",
  number = 100,
  summaryFunction = prSummary,
  classProbs = TRUE,
  savePredictions = "final",
)

# Tuning grid
tuna_rf <- expand.grid(mtry = best_mtry)

# enable parallelization
cl <- makeCluster(8)
registerDoParallel(cl)

# Random forest model
set.seed(250)
rf_boot2 <- train(
  DxCancer ~ .,
  data = train_b,
  method = "rf",
  metric = "AUC",
  trControl = control,
  tuneGrid = tuna_rf,
  ntrees = 1000
)
stopImplicitCluster()

# Check OOB
oob_rf_boot2 <- rf_boot2$finalModel
plot(oob_rf_boot2, main = "OOB Error vs. Number of Trees (Boot632 Tuned)")

# Inspect and summarize accuracy of model
summary(rf_boot2)
print(rf_boot2)
varImp(rf_boot2)
print(rf_boot2$results)

# ROC curve for training model
trainroc_rf_boot2 <- plot.roc(rf_boot2$pred$obs, rf_boot2$pred$Yes, 
                              print.auc = TRUE, debug = FALSE,
                              main = paste("RF - Boot 632 - Training 
                                           (Tuned)"))

# Make predictions on the test sets
rf_pred_prob <- predict(rf_boot2, test, type = "prob")
rf_pred <- predict(rf_boot2, test, type = "raw")
rf_boot2_cm <- confusionMatrix(rf_pred, test$DxCancer, mode = "everything",
                               positive = "Yes")
rf_boot2_cm # Sens 0.40, Spec 0.98193, Recall 0.40, F1 = 0.40, Prec 0.40

plot.roc(test$DxCancer, rf_pred_prob$Yes, print.auc = TRUE, debug = FALSE,
         axes = TRUE, main = paste("Random Forest - Boot632 - Test 
                                   (Tuned)")) -> rf_boot_roc2

rm(rf_pred, rf_pred_prob)
rf_boot2_summary <- list(rf_boot2, varImp(rf_boot2), rf_boot2_cm, rf_boot_roc2,
                         trainroc_rf_boot2)

