# Load libraries needed for plotting variable importance
library(caret)       # Machine learning and cross-validation
library(ggplot2)     # Data visualization
library(ggcorrplot)  # Correlation matrix visualization
library(car)         # Variance Inflation Factor (VIF)


# Check RF variable importance (business/economic reasoning):
varImpPlot(final_rf_model)  # Visualize
importance_matrix <- importance(final_rf_model)  # numeric measure
print(importance_matrix)


# Train logistic regression using repeated stratified CV
set.seed(42)
lr_model <- train(
  default ~ .,
  data = df_training_data_win_log,  # dataset with winsorization + log
  method = "glm",
  metric = "ROC",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
)

# Print a summary of the final logistic regression model
summary(lr_model$finalModel)

# Calculate variable importance from the logistic model
lr_importance <- varImp(lr_model, scale = TRUE)
print(lr_importance)

# Plot the importance
plot(lr_importance, top = 20, main = "Variable Importance - Logistic Regression")



# Calculate the correlation matrix among numeric predictors 
numeric_data <- df_training_data_win_log[, -1]  # drop the 'default' column
corr_mat <- cor(numeric_data, use = "complete.obs")
# Visualize the correlation matrix
ggcorrplot(corr_mat, lab = FALSE)



# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(lr_model$finalModel)  # logistic model object

