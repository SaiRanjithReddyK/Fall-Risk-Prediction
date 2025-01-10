# Install and load required libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(caret)) install.packages("caret")
if (!require(glmnet)) install.packages("glmnet")
if (!require(randomForest)) install.packages("randomForest")
if (!require(pROC)) install.packages("pROC")
if (!require(rms)) install.packages("rms")
if (!require(corrplot)) install.packages("corrplot")
if (!require(ggplot2)) install.packages("ggplot2")

# Load libraries
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)
library(pROC)
library(rms)
library(corrplot)
library(ggplot2)
# Load the dataset
data <- read.csv("C:/Users/saire/OneDrive/Desktop/healtcare_Analytics/Project/fall_risk_data_final9v3.csv")

# Handle missing values for numeric columns
data <- data %>%
  mutate(
    AverageCalculatedBMI = ifelse(is.na(AverageCalculatedBMI), mean(AverageCalculatedBMI, na.rm = TRUE), AverageCalculatedBMI),
    A1CLabResult = ifelse(is.na(A1CLabResult), mean(A1CLabResult, na.rm = TRUE), A1CLabResult)
  ) %>%
  filter(AverageCalculatedBMI <= 50, A1CLabResult > 0 & A1CLabResult < 20)  # Remove outliers

# Replace blanks in categorical variables
data$Ethnicity[data$Ethnicity == ""] <- NA
data$RacePrimary[data$RacePrimary == "Unknown"] <- NA

# Convert categorical variables to factors
data <- data %>%
  mutate(
    HistoryOfFall = as.factor(HistoryOfFall),
    ComplicationCategory = as.factor(ComplicationCategory),
    Ethnicity = as.factor(Ethnicity),
    RacePrimary = as.factor(RacePrimary),
    CurrentlyOnPainMedication = as.factor(CurrentlyOnPainMedication),
    HistoryofStroke = as.factor(HistoryofStroke),
    HistoryofOsteoporosis = as.factor(HistoryofOsteoporosis),
    HistoryofArthritis = as.factor(HistoryofArthritis),
    DiabeticComplications = as.factor(DiabeticComplications)
  )

# Define FutureFallRisk
data <- data %>%
  mutate(FutureFallRisk = ifelse(
    HistoryOfFall %in% c(1, 2, 3) | 
      Age > 75 | 
      AverageCalculatedBMI > 35 | AverageCalculatedBMI < 18.5 | 
      A1CLabResult > 8, 
    1, 0)) %>%
  mutate(FutureFallRisk = as.factor(FutureFallRisk))

# Feature Engineering
data <- data %>%
  mutate(
    Age_Risk = case_when(
      Age >= 55 & Age < 65 ~ "Low Risk",
      Age >= 65 & Age < 75 ~ "Moderate Risk",
      Age >= 75 ~ "High Risk"
    ),
    BMI_Risk = case_when(
      AverageCalculatedBMI < 18.5 ~ "Underweight",
      AverageCalculatedBMI >= 18.5 & AverageCalculatedBMI < 25 ~ "Normal",
      AverageCalculatedBMI >= 25 & AverageCalculatedBMI < 30 ~ "Overweight",
      AverageCalculatedBMI >= 30 ~ "Obese"
    ),
    A1C_Control = case_when(
      A1CLabResult < 7 ~ "Well Controlled",
      A1CLabResult >= 7 & A1CLabResult < 8 ~ "Moderately Controlled",
      A1CLabResult >= 8 ~ "Poorly Controlled"
    )
  )
# Descriptive Statistics
summary_stats <- data %>%
  summarise(
    Age_Mean = mean(Age, na.rm = TRUE),
    BMI_Mean = mean(AverageCalculatedBMI, na.rm = TRUE),
    A1C_Mean = mean(A1CLabResult, na.rm = TRUE)
  )
print(summary_stats)

# Visualizations
## Age Distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

## Scatterplot: Age vs BMI by Fall Risk
ggplot(data, aes(x = Age, y = AverageCalculatedBMI, color = FutureFallRisk)) +
  geom_point(alpha = 0.7) +
  labs(title = "Age vs BMI by Future Fall Risk", x = "Age", y = "BMI") +
  theme_minimal()

## Boxplot: A1C Lab Results by Fall Risk
ggplot(data, aes(x = FutureFallRisk, y = A1CLabResult, fill = FutureFallRisk)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot of A1C Lab Results by Future Fall Risk", x = "Future Fall Risk", y = "A1C Lab Result") +
  theme_minimal()

## Correlation Matrix
numeric_data <- data %>% select(Age, AverageCalculatedBMI, A1CLabResult)
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", tl.col = "black", tl.srt = 45)

# Step 5: Split Data into Training and Testing Sets
set.seed(123)
train_index <- createDataPartition(data$FutureFallRisk, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Step 6: Train Logistic Regression Model
log_model <- glm(FutureFallRisk ~ Age + AverageCalculatedBMI + A1CLabResult + 
                   CurrentlyOnPainMedication + HistoryofStroke + 
                   HistoryofOsteoporosis + HistoryofArthritis + 
                   ComplicationCategory + Age_Risk + BMI_Risk + A1C_Control, 
                 data = train_data, family = binomial)

# Model summary
summary(log_model)

# Step 7: Predict and Evaluate on Test Data
log_probs <- predict(log_model, newdata = test_data, type = "response")
log_preds <- ifelse(log_probs > 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(as.factor(log_preds), test_data$FutureFallRisk)
# ROC-AUC
log_model <- roc(as.numeric(as.character(test_data$FutureFallRisk)), log_probs)
plot(log_model, main = "ROC Curve: Logistic Regression", col = "blue")
cat("Logistic Regression AUC:", auc(log_model), "\n")
# Install and load glmnet
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Prepare data for glmnet (requires matrix format for predictors)
x_train <- model.matrix(FutureFallRisk ~ Age + AverageCalculatedBMI + A1CLabResult + 
                          CurrentlyOnPainMedication + HistoryofStroke + 
                          HistoryofOsteoporosis + HistoryofArthritis + 
                          ComplicationCategory + Age_Risk + BMI_Risk + A1C_Control, 
                        data = train_data)[, -1]  # Remove intercept column
y_train <- as.numeric(as.character(train_data$FutureFallRisk))

x_test <- model.matrix(FutureFallRisk ~ Age + AverageCalculatedBMI + A1CLabResult + 
                         CurrentlyOnPainMedication + HistoryofStroke + 
                         HistoryofOsteoporosis + HistoryofArthritis + 
                         ComplicationCategory + Age_Risk + BMI_Risk + A1C_Control, 
                       data = test_data)[, -1]

# Fit Lasso (L1) Regression
lasso_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)  # Lasso (alpha=1)
plot(lasso_model)
best_lambda <- lasso_model$lambda.min  # Best lambda value
print(best_lambda)

# Predict on the test set
lasso_preds <- predict(lasso_model, newx = x_test, s = best_lambda, type = "response")
lasso_classes <- ifelse(lasso_preds > 0.5, 1, 0)

# Confusion Matrix for Lasso
confusionMatrix(as.factor(lasso_classes), as.factor(test_data$FutureFallRisk))

# Install and load car package
if (!require(car)) install.packages("car")
library(car)

# Fit a standard logistic regression model to calculate VIF
vif_model <- glm(FutureFallRisk ~ Age + AverageCalculatedBMI + A1CLabResult + 
                   CurrentlyOnPainMedication + HistoryofStroke + 
                   HistoryofOsteoporosis + HistoryofArthritis + 
                   ComplicationCategory + Age_Risk + BMI_Risk + A1C_Control, 
                 data = train_data, family = binomial)

# Calculate VIF for predictors
vif_values <- vif(vif_model)
print(vif_values)

# Identify predictors with high VIF (> 5 indicates multicollinearity)
high_vif <- vif_values[vif_values > 5]
print(high_vif)

# Refit Logistic Regression Model Without High-VIF Predictors
reduced_log_model <- glm(FutureFallRisk ~ Age + A1CLabResult + 
                           CurrentlyOnPainMedication + HistoryofStroke + 
                           HistoryofOsteoporosis + BMI_Risk + A1C_Control, 
                         data = train_data, family = binomial)

# Model summary
summary(reduced_log_model)

# Predict and Evaluate on Test Data
reduced_log_probs <- predict(reduced_log_model, newdata = test_data, type = "response")
reduced_log_preds <- ifelse(reduced_log_probs > 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(as.factor(reduced_log_preds), test_data$FutureFallRisk)
# ROC-AUC
reduced_log_probs <- roc(as.numeric(as.character(test_data$FutureFallRisk)), lasso_preds)
plot(reduced_log_probs, main = "ROC Curve: Lasso Regression", col = "green")
cat("Lasso Regression AUC:", auc(reduced_log_probs), "\n")

#####------RF----------
# Load necessary libraries
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)
library(caret)

# Step 1: Prepare the Data
# Exclude unnecessary columns
rf_train_data <- train_data %>%
  select(Age, AverageCalculatedBMI, A1CLabResult, CurrentlyOnPainMedication, 
         HistoryofStroke, HistoryofOsteoporosis, HistoryofArthritis, 
         ComplicationCategory, Age_Risk, BMI_Risk, A1C_Control, FutureFallRisk)

rf_test_data <- test_data %>%
  select(Age, AverageCalculatedBMI, A1CLabResult, CurrentlyOnPainMedication, 
         HistoryofStroke, HistoryofOsteoporosis, HistoryofArthritis, 
         ComplicationCategory, Age_Risk, BMI_Risk, A1C_Control, FutureFallRisk)

# Step 2: Train the Random Forest Model
set.seed(123)
rf_model <- randomForest(FutureFallRisk ~ ., 
                         data = rf_train_data, 
                         ntree = 500, 
                         mtry = sqrt(ncol(rf_train_data) - 1), 
                         importance = TRUE)

# Print the model summary
print(rf_model)

# Step 3: Evaluate the Model
# Predict on the test set
rf_preds <- predict(rf_model, newdata = rf_test_data)

# Confusion Matrix
confusionMatrix(rf_preds, rf_test_data$FutureFallRisk)

# Predict probabilities for the test set
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, 2]  # Extract probabilities for class "1"

# Compute ROC and AUC
rf_roc <- roc(as.numeric(as.character(test_data$FutureFallRisk)), rf_probs)
plot(rf_roc, main = "ROC Curve: Random Forest", col = "red")
cat("Random Forest AUC:", auc(rf_roc), "\n")

# Step 4: Feature Importance
# Plot variable importance
varImpPlot(rf_model)

# Plot variable importance
varImpPlot(rf_model, main = "Feature Importance", n.var = 10)  # Top 10 features

# Extract variable importance scores
importance_scores <- importance(rf_model)
print(importance_scores)

# Install and load caret
if (!require(caret)) install.packages("caret")
library(caret)

# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)  # Number of variables tried at each split
)

# Use caret to perform cross-validation
set.seed(123)
tuned_rf <- train(
  FutureFallRisk ~ ., 
  data = rf_train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),  # 5-fold cross-validation
  tuneGrid = tune_grid,
  ntree = 500  # Fix the number of trees at 500
)

# Best hyperparameters
print(tuned_rf$bestTune)

# Final model performance
print(tuned_rf)

# Create a comparison table
comparison <- data.frame(
  Model = c("Logistic Regression", "Lasso Regression", "Random Forest"),
  Accuracy = c(0.8387, 0.8855, 0.9057),
  Sensitivity = c(0.40507, 0.6928, 0.8652),
  Specificity = c(0.93985, 0.9304, 0.9152),
  Balanced_Accuracy = c(0.67246, 0.8116, 0.8902),
  Kappa = c(0.3961, 0.6252, 0.7174)
)

# Print the comparison
print(comparison)

# Optional: Visualize the comparison
library(ggplot2)
comparison_long <- comparison %>% pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
ggplot(comparison_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison", y = "Value", x = "Model") +
  theme_minimal()

# Load the rms package
if (!require(rms)) install.packages("rms")
library(rms)

# Fit the logistic regression model with x = TRUE and y = TRUE
calibration_model <- lrm(
  FutureFallRisk ~ Age + AverageCalculatedBMI + A1CLabResult +
    CurrentlyOnPainMedication + HistoryofStroke +
    HistoryofOsteoporosis + Age_Risk + BMI_Risk + A1C_Control,
  data = rf_train_data,
  x = TRUE, y = TRUE
)

# Generate calibration curve
calibration_curve <- calibrate(calibration_model, method = "boot", B = 1000)

# Plot the calibration curve
plot(calibration_curve, main = "Calibration Curve")
rf_probs <- predict(rf_model, newdata = rf_test_data, type = "prob")[, 2]  # Predicted probabilities
rf_calibration <- calibration(rf_test_data$FutureFallRisk ~ rf_probs, method = "boot", B = 1000)
plot(rf_calibration, main = "Calibration Curve: Random Forest")



