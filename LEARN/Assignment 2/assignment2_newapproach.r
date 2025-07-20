# ----------------------------
# Data Loading & Preparation
# ----------------------------

# Load libraries
library(tidyverse)
library(summarytools)
library(caret)
library(knitr)
library(missForest)
library(pROC)
library(doParallel)
library(car)
library(ROSE)
library(e1071)

# Load dataset
cc_risk <- read.csv("risk_factors_cervical_cancer.csv",
                    na.strings = "?",
                    stringsAsFactors = FALSE)
View(cc_risk)


# ----------------------------
# Initial Data Inspection and Cleaning
# ----------------------------
# Clean up column names to avoid errors
column_names <- colnames(cc_risk)
names(cc_risk) <- gsub("\\.", "", names(cc_risk))
cleaned_column_names <- colnames(cc_risk)
print(column_names)
print(cleaned_column_names)
rm(column_names, cleaned_column_names) # clean up of environment

# In-depth data inspection with dfSummary
dfSummary(cc_risk)
# "STDsTimesince" variables missing 91.7%; several others missing ~12.2-13.6%
# severe class imbalance of target variable (DxCancer): only 2.1% positive cases
# several variables with little or no predictive power based on distribution

# Remove known leakage variables (tests confirming cervical cancer)
leakage_vars <- c("Biopsy", "Citology", "Hinselmann", "Schiller")

cc_risk <- cc_risk %>% 
  select(-all_of(leakage_vars))

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
set.seed(75)
trainIndex <- createDataPartition(cc_risk$DxCancer, p = 0.8, list = FALSE)
train <- cc_risk[trainIndex, ]
test <- cc_risk[-trainIndex, ]


# ----------------------------
# Near-Zero-Variance Analysis
# ----------------------------
# Identify zero- or near-zero-variance variables
nzv <- nearZeroVar(train, saveMetrics = TRUE)
kable(nzv, caption = "Near-Zero and Zero-Variance Variables")

zero_variance <- nzv %>%
  filter(zeroVar == TRUE)
kable(zero_variance, caption = "Zero-Variance Variables")
# STDsAIDS, STDscervicalcondylomatosis

near_zero <- nzv %>%
  filter(nzv == TRUE & zeroVar != TRUE)
kable(near_zero, caption = "Near-Zero Variance Variables")

table(train$STDsvaginalcondylomatosis) # 4 positive
table(train$STDssyphilis) # 16 positive
table(train$STDspelvicinflammatorydisease) # 1 positive
table(train$STDsgenitalherpes) # 1 positive
table(train$STDsmolluscumcontagiosum) # 1 positive
table(train$STDsHIV) # 11 positive
table(train$STDsHepatitisB) # 1 positive
table(train$STDsHPV) # 2 positive; similar to DxHPV
table(train$DxCIN) # 7 positive
table(train$DxHPV) # 12 positive

# Remove zero variance variables
zero_variance_var <- c("STDscervicalcondylomatosis", "STDsAIDS", "STDsHPV",
                       "STDsmolluscumcontagiosum", "STDsgenitalherpes",
                       "STDspelvicinflammatorydisease", "STDsHepatitisB")

train <- train %>%
  select(-all_of(zero_variance_var))

test <- test %>%
  select(-all_of(zero_variance_var))

colnames(train)
colnames(test)


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
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.6,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Collinearity Heatmap of All Variables",
         mar = c(0, 0, 0, 0))

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

kable(cor_df, caption = "High Correlations (|r| >/= 0.7)")

# consider removing: STDscondylomatosis, STDsvulvoperinealcondylomatosis,
# STDsvaginalcondylomatosis, Smokes, IUDyears, STDs, DxHPV, Dx, IUD/IUDyears
# STDsnumber/STDsNumberofdiagnosis

## lit review:
# Hepatitis B linked to cervical cancer (https://pubmed.ncbi.nlm.nih.gov/34672431/; https://pubmed.ncbi.nlm.nih.gov/36320009/)
# genital herpes linked to cervical cancer (https://pubmed.ncbi.nlm.nih.gov/35494213/; https://pubmed.ncbi.nlm.nih.gov/37997472/)
# pelvic inflammatory disease associated with cervical cancer: https://pubmed.ncbi.nlm.nih.gov/39327298/
# HIV associated with cervical cancer: https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/cervical-cancer-screening
# syphilis associated with cervical cancer: https://pubmed.ncbi.nlm.nih.gov/35813967/
# smoking has a known association with increased risk of all cancer
# HPV strongly associated with increased risk of cancer, especially in conjunction with other STIs
# IUD: 1 SR/MA suggesting decreased risk of cervical cancer (https://pubmed.ncbi.nlm.nih.gov/29112647/); 1 SR/MA suggesting no correlation due to lack of evidence (https://pubmed.ncbi.nlm.nih.gov/34752778/)
# remaining variables not associated based on lit review

# Remove variables based on high correlations/NZV/lit review
train <- train %>%
  select(-STDsvaginalcondylomatosis, -STDsvulvoperinealcondylomatosis,
         -STDscondylomatosis, -STDs)

test <- test %>%
  select(-STDsvaginalcondylomatosis, -STDsvulvoperinealcondylomatosis,
         -STDscondylomatosis, -STDs)

# principal components analysis (prcomp) does not handle NA values
# impute and complete prcomp analysis before removing variables?
# consider principal components, LASSO, or ridge regressions? 
# need to impute missing values for all 4


# ----------------------------
# Missing Data Analysis
# ----------------------------
# Summary of training set
dfSummary(train)
# several variables have low or no predictive power due to distributions
# try missForest to see if distributions change?
# try median/mean/mode imputation and compare distributions of both methods?

# Closer look at numeric variables to determine best imputation
table(train$IUD) # missing 84 and would impute as 0 based on mode
table(train$IUDyears) # most values <=2 years; use mode
hist(train$IUDyears)
# both missing 84; IUD would impute as 0; assume missing is "No" for each?

table(train$Smokes)
kable(table(train$Smokespacksyear)) # mean 0.4, median 0
kable(table(train$Smokesyears)) # mean 1.3, median 0
# all match with "No/0" values and missing is 9 each; assume all missing is "No"?

table(train$HormonalContraceptives) # mode is 1
kable(table(train$HormonalContraceptivesyears)) # mean 2.4; median 0.5
hist(train$HormonalContraceptivesyears)
# binary would impute as "Yes", years should match binary - use mean

table(train$STDs) # missing 75; would impute as 0 based on mode
table(train$STDsnumber) # mean 0.2, median 0; use mode
table(train$STDsNumberofdiagnosis) # mean 0.1; use mode
# mode for STDsnumber for consistency with STDs imputation and because
# you cannot have "0.2" of an STD; remaining binary STDs would impute as 0

# Check NAs across all columns before imputation
colSums(is.na(train))


# ----------------------------
# Missing Data Imputation Using missForest
# ----------------------------
# Convert binary variables to factors
colnames(train)
binary_vars <- c(
  "Smokes", "HormonalContraceptives", "IUD", "STDssyphilis", "STDsHIV", "DxCIN",
  "DxCancer", "DxHPV", "Dx")

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

# Impute missing data using missForest
set.seed(75)
imputed_data <- missForest(
  train,
  maxiter = 5,
  ntree = 1000,
  verbose = TRUE
)

# Extract imputed data
train_imp <- imputed_data$ximp

# Verify no NAs remain
colSums(is.na(train_imp))
train <- train_imp

# Assess missForest imputation
dfSummary(train)
table(train$Numberofsexualpartners) # need to round
table(train$Smokesyears)
table(train$STDsnumber)

# Round numeric values imputed by missForest that should be whole numbers
numeric_vars <- c("Age", "Numberofsexualpartners", "Firstsexualintercourse",
                  "Numofpregnancies", "STDsnumber", "STDsNumberofdiagnosis")

train[numeric_vars] <- lapply(train[numeric_vars],
                              round, digits = 0)

table(train$Numberofsexualpartners)
table(train$Firstsexualintercourse)
table(train$Numofpregnancies)
table(train$STDsnumber)
table(train$STDsNumberofdiagnosis)

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

# Clean up of environment
rm(imputed_data, impute_recipe, numeric_vars, nzv, near_zero,
   zero_variance, zero_variance_var, train_imp, leakage_vars)


# ----------------------------
# Correlation Matrix and Heatmap Revisited
# ----------------------------
cor_forest <- cor(train, use = "complete.obs")
corrplot <- corrplot(cor_forest,
                     method = "color",
                     type = "upper",
                     order = "hclust",
                     hclust.method = "ward.D2",
                     tl.col = "black",
                     tl.cex = 0.8,
                     addCoef.col = "black",
                     number.cex = 0.6,
                     col = colorRampPalette(c("blue", "white", "red"))(200),
                     title = "Collinearity Heatmap of All Variables (missForest Imputation)",
                     mar = c(0, 0, 2, 0))

# Convert cor_matrix_forest to a df, remove duplicates, and print results
cor_df <- as.data.frame.table(cor_forest, stringsAsFactors = FALSE) %>%
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

kable(cor_df, caption = "High Correlations (|r| >/= 0.7)")

# Remove variables based on correlation/redundancy
filtered_vars <- c("DxHPV", "STDsnumber", "Smokesyears")

train <- train %>%
  select(-all_of(filtered_vars))

test <- test %>%
  select(-all_of(filtered_vars))


# ----------------------------
# Principal Components Analysis
# ----------------------------
library(factoextra)
# Principal Components Analysis (relationships between variables)
normalized_forest <- scale(train)
head(normalized_forest)
forest_pca <- princomp(normalized_forest)
summary(forest_pca)
pca_loadings <- forest_pca$loadings
print(pca_loadings)
# comp.1, 9, 10, 16 most likely cervical cancer components
# comp.2, 3, 7, 8 have negative scores for DxCancer

# Visualise the PCA
fviz_eig(forest_pca, addlabels = TRUE)
fviz_pca_var(forest_pca, col.var = "black", repel = TRUE)
# fviz_pca_biplot(forest_pca, repel = TRUE,
#                 col.var = "black", # Variables color
#                 col.ind = "gray"   # Individuals color
#                 )

# ----------------------------
# Principal Components Regression
# ----------------------------
library(pls)
# comp.1 model
set.seed(75)
comp1_pcr <- pcr(DxCancer ~ Age + Numofpregnancies + Smokespacksyear +
                 HormonalContraceptives + HormonalContraceptivesyears +
                 IUD + IUDyears + DxCIN + Dx, 
                 data = train, scale = TRUE, validation = "CV")

summary(comp1_pcr) # 6 comps = 16%, 7 = 60%, 8 = 83% explained
validationplot(comp1_pcr)
validationplot(comp1_pcr, val.type = "MSEP")
validationplot(comp1_pcr, val.type = "R2")
comp1_pcr$loadings

# Make predictions using the model
pcr_pred <- predict(comp1_pcr, test, ncomp = 8)
sqrt(mean((pcr_pred - test$DxCancer)^2)) # 0.1314232


# comp.9 model
set.seed(75)
comp9_pcr <- pcr(DxCancer ~ Numberofsexualpartners + Firstsexualintercourse +
                 Numofpregnancies + Smokes + Smokespacksyear + STDssyphilis +
                 STDsHIV + DxCIN,
                 data = train, scale = TRUE, validation = "CV")

summary(comp9_pcr) # only 0.6745% explained with all components


# comp.10 model
set.seed(75)
comp10_pcr <- pcr(DxCancer ~ Numberofsexualpartners + Firstsexualintercourse +
                 Numofpregnancies + HormonalContraceptives + IUD + STDssyphilis +
                 STDsHIV + STDsNumberofdiagnosis + DxCIN,
                 data = train, scale = TRUE, validation = "CV")

summary(comp10_pcr) # only 1.043% explained


# comp.16 model
set.seed(75)
comp16_pcr <- pcr(DxCancer ~ DxCIN + Dx,
                 data = train, scale = TRUE, validation = "CV")

summary(comp16_pcr) # 84% explained
validationplot(comp16_pcr)
validationplot(comp16_pcr, val.type = "MSEP")
validationplot(comp16_pcr, val.type = "R2")
comp16_pcr$loadings

# Make predictions using the model
pcr_pred <- predict(comp16_pcr, test, ncomp = 2)
sqrt(mean((pcr_pred - test$DxCancer)^2)) # 0.1327228


# ----------------------------
# LASSO
# ----------------------------
library(glmnet)
# Define predictor and response variables
y <- train$DxCancer
x <- data.matrix(train[,c("Age", "Numberofsexualpartners", "Numofpregnancies", 
                          "Firstsexualintercourse", "Smokes", "Smokespacksyear",
                          "HormonalContraceptives", "HormonalContraceptivesyears",
                          "IUD", "IUDyears", "STDssyphilis", "STDsHIV", "DxCIN",
                          "Dx", "STDsNumberofdiagnosis")])

# Create lasso model
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Determine best lambda
best_lambda <- lasso_model$lambda.min
best_lambda
plot(lasso_model)

# Create best model and view coefficients
best_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_lasso)
# Firstsexualintercourse, Smokes, IUD, DxCIN, Dx


# ----------------------------
# ROSE Application
# ----------------------------
# Convert binary variables to factors
binary_vars <- c(
  "DxCancer", "Dx", "STDsHIV", "STDssyphilis", "IUD",
  "Smokes", "DxCIN"
)

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

# ROSE for missForest train set
table(train$DxCancer) # 12 positive

train_b <- ovun.sample(DxCancer ~ ., data = train, method = "both",
                       p = 0.5, seed = 75)$data

table(train_b$DxCancer) # 358 positive


# ----------------------------
# Preprocess train and test sets
# ----------------------------
# Preprocess: Scale features (center and scale)
preproc <- preProcess(train[, -ncol(train)], method = c("center", "scale"))
train_scaled <- predict(preproc, train)
test_scaled <- predict(preproc, test)
train_b_scaled <- predict(preproc, train_b)


# ----------------------------
# Unbalanced Modeling (All Features)
# ----------------------------
# Tuning grid
tune_grid <- expand.grid(
  gamma = c(0.001, 0.01, 0.1, 1, 10),  # RBF kernel width
  cost = c(0.01, 0.1, 1, 10, 100)      # Regularization strength
)


# Enable parallelization
cl <- makeCluster(10)
registerDoParallel(cl)

## Radial SVM
set.seed(75)
radial_unbalanced <- svm(
  DxCancer ~ .,
  data = train_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE
  )

print(radial_unbalanced)
predictions <- predict(radial_unbalanced, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Unbalanced Data (Untuned)")

## Radial SVM Tuned
radial_unbalanced_tune <- tune.svm(
  DxCancer ~ .,
  data = train_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)
print(radial_unbalanced_tune)
plot(radial_unbalanced_tune, main = "Radial SVM Unbalanced Data Tuning Performance")
best_svm <- radial_unbalanced_tune$best.model
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, 
     print.auc = TRUE,
     auc.polygon = TRUE,
     grid = c(0.1, 0.2),
     print.thres = "best",  # Show optimal threshold
     main = "SVM ROC Curve",
     col = "blue",
     lwd = 2)


## Linear SVM
set.seed(75)
linear_unbalanced <- svm(
  DxCancer ~ .,
  data = train_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE
)

# Assess model performance
print(linear_unbalanced)

# Confusion matrix
predictions <- predict(linear_unbalanced, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Unbalanced Data (Untuned)")

## Linear SVM Tuned
linear_unbalanced_tune <- tune.svm(
  DxCancer ~ .,
  data = train_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(linear_unbalanced_tune)
plot(linear_unbalanced_tune, main = "Linear SVM Unbalanced Data Tuning Performance")
best_svm <- linear_unbalanced_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Unbalanced Data (Tuned - Best Model)")


## Polynomial SVM
set.seed(75)
polynomial_unbalanced <- svm(
  DxCancer ~ .,
  data = train_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE
  )

summary(polynomial_unbalanced)
predictions <- predict(polynomial_unbalanced, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Unbalanced Data (Untuned)")
stopImplicitCluster()


# ----------------------------
# Balanced Modeling (All Features)
# ----------------------------
# Tuning grid
tune_grid <- expand.grid(
  gamma = c(0.001, 0.01, 0.1, 1, 10),  # RBF kernel width
  cost = c(0.01, 0.1, 1, 10, 100)      # Regularization strength
)

# Enable parallelization
cl <- makeCluster(10)
registerDoParallel(cl)

## Radial SVM Untuned
set.seed(75)
radial_balanced <- svm(
  DxCancer ~ .,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE
)

# Assess model performance
print(radial_balanced)

# Confusion matrix
predictions <- predict(radial_balanced, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Balanced Data (Untuned)")


## Radial SVM Tuned
radial_balanced_tune <- tune.svm(
  DxCancer ~ .,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(radial_balanced_tune)
plot(radial_balanced_tune, main = "Radial SVM Unbalanced Data Tuning Performance")
best_svm <- radial_balanced_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Balanced Data (Tuned - Best Model)")


## Linear SVM
# Tuning grid
tune_grid <- expand.grid(
  gamma = c(0.001, 0.01, 0.1, 1, 10),  # RBF kernel width
  cost = c(0.01, 0.1, 1, 10, 100)      # Regularization strength
)

# Enable parallelization
cl <- makeCluster(10)
registerDoParallel(cl)

set.seed(75)
linear_balanced <- svm(
  DxCancer ~ .,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE
)

# Assess model performance
print(linear_balanced)

# Confusion matrix
predictions <- predict(linear_balanced, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Balanced Data (Untuned)")

## Linear SVM Tuned
linear_balanced_tune <- tune.svm(
  DxCancer ~ .,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(linear_balanced_tune)
plot(linear_balanced_tune, main = "Linear SVM Balanced Data Tuning Performance")
best_svm <- linear_balanced_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Balanced Data (Tuned - Best Model)")


## Polynomial SVM
# Tuning grid
tune_grid <- expand.grid(
  gamma = c(0.001, 0.01, 0.1, 1, 10),  # RBF kernel width
  cost = c(0.01, 0.1, 1, 10, 100)      # Regularization strength
)

# Enable parallelization
cl <- makeCluster(10)
registerDoParallel(cl)

set.seed(75)
polynomial_balanced <- svm(
  DxCancer ~ .,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE
)

# Assess model performance
print(polynomial_balanced)

# Confusion matrix
predictions <- predict(polynomial_balanced, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Balanced Data (Untuned)")

## polynomial SVM Tuned
polynomial_balanced_tune <- tune.svm(
  DxCancer ~ .,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(polynomial_balanced_tune)
plot(polynomial_balanced_tune, main = "Polynomial SVM Balanced Data Tuning Performance")
best_svm <- polynomial_balanced_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Balanced Data (Tuned - Best Model)")



# ----------------------------
# Balanced Modeling (LASSO)
# ----------------------------
# Tuning grid
tune_grid <- expand.grid(
  gamma = c(0.001, 0.01, 0.1, 1, 10),  # RBF kernel width
  cost = c(0.01, 0.1, 1, 10, 100)      # Regularization strength
)

# Enable parallelization
cl <- makeCluster(10)
registerDoParallel(cl)

## Radial SVM Untuned
set.seed(75)
radial_bal_lasso <- svm(
  DxCancer ~ Firstsexualintercourse + Smokes + IUD + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE
)

# Assess model performance
print(radial_bal_lasso)

# Confusion matrix
predictions <- predict(radial_bal_lasso, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Balanced Data (LASSO - Untuned)")

## Radial SVM Tuned
radial_bal_lasso_tune <- tune.svm(
  DxCancer ~ Firstsexualintercourse + Smokes + IUD + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(radial_bal_lasso_tune)
plot(radial_bal_lasso_tune, main = "Radial SVM Balanced Data Tuning 
     Performance (LASSO)")
best_svm <- radial_bal_lasso_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Balanced Data (LASSO Tuned - Best Model)")


## Linear SVM Untuned
set.seed(75)
linear_bal_lasso <- svm(
  DxCancer ~ Firstsexualintercourse + Smokes + IUD + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE
)

# Assess model performance
print(linear_bal_lasso)

# Confusion matrix
predictions <- predict(linear_bal_lasso, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Balanced Data (LASSO - Untuned)")

## Linear SVM Tuned
linear_bal_lasso_tune <- tune.svm(
  DxCancer ~ Firstsexualintercourse + Smokes + IUD + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(linear_bal_lasso_tune)
plot(linear_bal_lasso_tune, main = "Linear SVM Balanced Data Tuning 
     Performance (LASSO)")
best_svm <- linear_bal_lasso_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Balanced Data (LASSO Tuned - Best Model)")


## Polynomial SVM Untuned
set.seed(75)
polynomial_bal_lasso <- svm(
  DxCancer ~ Firstsexualintercourse + Smokes + IUD + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE
)

# Assess model performance
print(polynomial_bal_lasso)

# Confusion matrix
predictions <- predict(polynomial_bal_lasso, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Balanced Data (LASSO - Untuned)")

## Linear SVM Tuned
polynomial_bal_lasso_tune <- tune.svm(
  DxCancer ~ Firstsexualintercourse + Smokes + IUD + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(polynomial_bal_lasso_tune)
plot(polynomial_bal_lasso_tune, main = "Polynomial SVM Balanced Data Tuning 
     Performance (LASSO)")
best_svm <- polynomial_bal_lasso_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Balanced Data (LASSO Tuned - Best Model)")


# ----------------------------
# Balanced Modeling (PCR Component 1)
# ----------------------------
# Tuning grid
tune_grid <- expand.grid(
  gamma = c(0.001, 0.01, 0.1, 1, 10),  # RBF kernel width
  cost = c(0.01, 0.1, 1, 10, 100)      # Regularization strength
)

# Enable parallelization
cl <- makeCluster(10)
registerDoParallel(cl)

## Radial SVM Untuned
set.seed(75)
radial_bal_pcr <- svm(
  DxCancer ~ Age + Numofpregnancies + Smokespacksyear + HormonalContraceptives +
    HormonalContraceptivesyears + IUD + IUDyears + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE
)

# Assess model performance
print(radial_bal_pcr)

# Confusion matrix
predictions <- predict(radial_bal_pcr, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Balanced Data (PCR - Untuned)")

## Radial SVM Tuned
radial_bal_pcr_tune <- tune.svm(
  DxCancer ~ Age + Numofpregnancies + Smokespacksyear + HormonalContraceptives +
    HormonalContraceptivesyears + IUD + IUDyears + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "radial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(radial_bal_pcr_tune)
plot(radial_bal_pcr_tune, main = "Radial SVM Balanced Data Tuning 
     Performance (PCR)")
best_svm <- radial_bal_pcr_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Radial SVM - Balanced Data (PCR Tuned - Best Model)")


## Linear SVM Untuned
set.seed(75)
linear_bal_pcr <- svm(
  DxCancer ~ Age + Numofpregnancies + Smokespacksyear + HormonalContraceptives +
    HormonalContraceptivesyears + IUD + IUDyears + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE
)

# Assess model performance
print(linear_bal_pcr)

# Confusion matrix
predictions <- predict(linear_bal_pcr, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Balanced Data (PCR - Untuned)")

## Linear SVM Tuned
linear_bal_pcr_tune <- tune.svm(
  DxCancer ~ Age + Numofpregnancies + Smokespacksyear + HormonalContraceptives +
    HormonalContraceptivesyears + IUD + IUDyears + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "linear",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(linear_bal_pcr_tune)
plot(linear_bal_pcr_tune, main = "Linear SVM Balanced Data Tuning 
     Performance (PCR)")
best_svm <- linear_bal_pcr_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Linear SVM - Balanced Data (PCR Tuned - Best Model)")


## Polynomial SVM Untuned
set.seed(75)
polynomial_bal_pcr <- svm(
  DxCancer ~ Age + Numofpregnancies + Smokespacksyear + HormonalContraceptives +
    HormonalContraceptivesyears + IUD + IUDyears + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE
)

# Assess model performance
print(polynomial_bal_pcr)

# Confusion matrix
predictions <- predict(polynomial_bal_pcr, test_scaled, probability = TRUE)
probs <- attr(predictions, "probabilities")[, "Yes"]
conf_matrix <- confusionMatrix(predictions, test_scaled$DxCancer)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = probs
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Balanced Data (PCR - Untuned)")

## Linear SVM Tuned
polynomial_bal_pcr_tune <- tune.svm(
  DxCancer ~ Age + Numofpregnancies + Smokespacksyear + HormonalContraceptives +
    HormonalContraceptivesyears + IUD + IUDyears + DxCIN + Dx,
  data = train_b_scaled,
  type = "C-classification",
  kernel = "polynomial",
  probability = TRUE,
  gamma = tune_grid$gamma,
  cost = tune_grid$cost,
  tunecontrol = tune.control(
    sampling = "cross",
    cross = 10,          # 10-fold CV
    best.model = TRUE,   # Return best model
    performances = TRUE  # Return accuracy metrics
  )
)

# Assess model performance and determine best model
print(polynomial_bal_pcr_tune)
plot(polynomial_bal_pcr_tune, main = "Polynomial SVM Balanced Data Tuning 
     Performance (PCR Tuned)")
best_svm <- polynomial_bal_pcr_tune$best.model

# Confusion matrix
test_pred <- predict(best_svm, newdata = test_scaled)
test_probs <- attr(predict(best_svm, newdata = test_scaled, 
                           probability = TRUE), 
                   "probabilities")[, "Yes"]  # Positive class
conf_matrix <- confusionMatrix(
  data = test_pred,
  reference = test_scaled$DxCancer,
  positive = "Yes"  # Set positive class
)
print(conf_matrix)

# Plot ROC Curve
roc_obj <- roc(
  response = test_scaled$DxCancer,
  predictor = test_probs,
)
plot(roc_obj, print.auc = TRUE, axes = TRUE, col = "red", lwd = 2, main = 
       "Polynomial SVM - Balanced Data (PCR Tuned - Best Model)")
