# Update file paths as needed:
# Lines 19-20: Specify dataset paths
# Line 483: Specify output CSV file path
# Recommendation: Run doParallel_CPU.R beforehand for faster calculations.

# Load necessary libraries
library(readr)       # For reading tabular data (e.g., CSV files).
library(dplyr)       # For data manipulation and transformation.
library(tidyr)       # For tidying and reshaping data.
library(randomForest) # For random forest models (classification/regression).
library(gbm)         # For gradient boosting machine models.
library(pROC)        # For ROC curve analysis.
library(caret)       # For training and evaluating machine learning models.
library(ggplot2)     # For creating visualizations.
library(ROCR)        # For classification model performance evaluation.
library(berryFunctions) # Utility functions for visualization and summaries.

# Load the datasets
training_dataset_path <- "C:/Users/mikaz/OneDrive/Рабочий стол/Data/Training_Dataset.csv"
test_dataset_path <- "C:/Users/mikaz/OneDrive/Рабочий стол/Data/Test_Dataset.csv"

# Read the training and test datasets
training_data_original <- read.csv(training_dataset_path, sep=';', encoding='latin1', stringsAsFactors=FALSE)
training_data <- read.csv(training_dataset_path, sep=';', encoding='latin1', stringsAsFactors=FALSE)
test_data <- read.csv(test_dataset_path, sep=';', encoding='latin1', stringsAsFactors=FALSE)

###########################################################
### Part 1: Data Cleaning, Feature Engineering, Outliers ###
###########################################################

clean_data <- function(data) {
  
  # Make a copy to not affect original data directly
  df <- data
  
  ### Imputation and Cleaning Steps ###
  
  # Impute sales and gross_performance
  if("sales" %in% colnames(df) & "gross_performance" %in% colnames(df)){
    df$sales[is.na(df$sales)] <- df$gross_performance[is.na(df$sales)]
    df$gross_performance[is.na(df$gross_performance)] <- df$sales[is.na(df$gross_performance)]
  }
  
  # Impute gross_profit using earn_from_op
  if(all(c("earn_from_op", "gross_profit") %in% colnames(df))) {
    df$gross_profit[is.na(df$gross_profit)] <- df$earn_from_op[is.na(df$gross_profit)]
  }
  
  # Impute oth_interest_exp using weighted average (if available)
  if(all(c("oth_interest_exp", "total_liabilities_st", "total_liabilities_mt", "total_liabilities_lt") %in% colnames(df))) {
    df$total_liabilities <- df$total_liabilities_st + df$total_liabilities_mt + df$total_liabilities_lt
    # Avoid division by zero or missing sums
    valid_oth <- !is.na(df$oth_interest_exp) & !is.na(df$total_liabilities) & df$total_liabilities > 0
    if(any(valid_oth)) {
      average_debt_interest <- sum(df$oth_interest_exp[valid_oth])/sum(df$total_liabilities[valid_oth])
    } else {
      average_debt_interest <- 0
    }
    df$oth_interest_exp[is.na(df$oth_interest_exp)] <- df$total_liabilities[is.na(df$oth_interest_exp)] * average_debt_interest
  }
  
  # Impute total_equity as total_assets - total_liabilities
  if(all(c("total_assets", "total_equity", "total_liabilities") %in% colnames(df))) {
    imp_equity <- (df$total_assets - df$total_liabilities)[is.na(df$total_equity)]
    df$total_equity[is.na(df$total_equity)] <- ifelse(imp_equity < 0,0,imp_equity)
  }
  
  # Impute fin_result as total_result - earn_from_op
  if(all(c("total_result", "earn_from_op", "fin_result") %in% colnames(df))) {
    df$fin_result[is.na(df$fin_result)] <- (df$total_result - df$earn_from_op)[is.na(df$fin_result)]
  }
  
  # Impute all remaining NA values with zero
  df[is.na(df)] <- 0
  
  # Replacing total_assets values if it's less than liabilities + equity
  if(all(c("total_assets", "total_liabilities", "total_equity") %in% colnames(df))) {
    df$total_assets <- ifelse(
      df$total_assets < (df$total_liabilities + df$total_equity),
      df$total_liabilities + df$total_equity,
      df$total_assets
    )
  }
  
  ### Feature Engineering: Adding Financial Ratios ###
  
  # Calculate profitability ratios safely by checking for required columns and avoiding division by zero 
  # (using ifelse to replace denominators < 1 with 1 for stability).
  
  # Profitability Ratios
  if(all(c("gross_profit","sales") %in% colnames(df))) {
    df$gross_profit_margin <- df$gross_profit / ifelse(df$sales < 1, 1, df$sales)
  }
  if(all(c("earn_from_op","sales") %in% colnames(df))) {
    df$ebit_margin <- df$earn_from_op / ifelse(df$sales < 1, 1, df$sales)
  }
  if(all(c("annual_profit","sales") %in% colnames(df))) {
    df$net_profit_margin <- df$annual_profit / ifelse(df$sales < 1, 1, df$sales)
  }
  if(all(c("annual_profit","total_assets") %in% colnames(df))) {
    df$ROA <- df$annual_profit / ifelse(df$total_assets < 1, 1, df$total_assets)
  }
  if(all(c("annual_profit","total_equity") %in% colnames(df))) {
    df$ROE <- df$annual_profit / ifelse(df$total_equity < 1, 1, df$total_equity)
  }
  
  # Solvency Ratios
  if(all(c("total_liabilities", "total_equity") %in% colnames(df))) {
    df$debt_to_equity_ratio <- df$total_liabilities / ifelse(df$total_equity < 1, 1, df$total_equity)
  }
  if(all(c("total_liabilities","total_assets") %in% colnames(df))) {
    df$debt_to_assets <- df$total_liabilities / ifelse(df$total_assets < 1, 1, df$total_assets)
  }
  if(all(c("earn_from_op","oth_interest_exp") %in% colnames(df))) {
    df$interest_coverage <- df$earn_from_op / ifelse(df$oth_interest_exp < 1, 1, df$oth_interest_exp)
  }
  if(all(c("total_liabilities_lt","total_assets") %in% colnames(df))) {
    df$lt_debt_to_assets <- df$total_liabilities_lt / ifelse(df$total_assets < 1, 1, df$total_assets)
  }
  
  # Cash Flow Ratios
  if(all(c("cf_operating","total_liabilities") %in% colnames(df))) {
    df$cf_op_to_debt <- df$cf_operating / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
  }
  if(all(c("cf_operating","sales") %in% colnames(df))) {
    df$cf_op_to_sales <- df$cf_operating / ifelse(df$sales < 1, 1, df$sales)
  }
  if(all(c("cf_investment","total_assets") %in% colnames(df))) {
    df$cf_inv_to_assets <- df$cf_investment / ifelse(df$total_assets < 1, 1, df$total_assets)
  }
  if(all(c("cf_financing","total_assets") %in% colnames(df))) {
    df$cf_fin_to_assets <- df$cf_financing / ifelse(df$total_assets < 1, 1, df$total_assets)
  }
  
  # Turnovers
  if(all(c("trade_payables_st","trade_payables_mt","trade_payables_lt","sales") %in% colnames(df))) {
    df$payables_turnover_days <- (df$trade_payables_st + df$trade_payables_mt + df$trade_payables_lt) * 365 / ifelse(df$sales < 1, 1, df$sales)
  }
  if(all(c("trade_receivables_st","trade_receivables_lt","sales") %in% colnames(df))) {
    df$receivables_turnover_days <- (df$trade_receivables_st + df$trade_receivables_lt) * 365 / ifelse(df$sales < 1, 1, df$sales)
  }
  
  # Liquidity Ratios
  if(all(c("monetary_current_assets","total_liabilities_st") %in% colnames(df))) {
    df$current_ratio <- df$monetary_current_assets / ifelse(df$total_liabilities_st < 1, 1, df$total_liabilities_st)
  }
  
  # Additional Feature Engineering
  
  # Profitability Ratios
  if(all(c("annual_profit","earn_from_op") %in% colnames(df))) {
    df$return_on_operating_profit <- df$annual_profit / ifelse(df$earn_from_op < 1, 1, df$earn_from_op)
  }
  
  # Leverage Ratios
  if(all(c("total_liabilities_st","total_liabilities_mt","total_liabilities_lt","total_liabilities") %in% colnames(df))) {
    df$short_term_debt_ratio <- df$total_liabilities_st / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
    df$medium_term_debt_ratio <- df$total_liabilities_mt / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
    df$long_term_debt_ratio <- df$total_liabilities_lt / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
  }
  
  if(all(c("bank_liabilities_st","bank_liabilities_mt","bank_liabilities_lt","total_liabilities") %in% colnames(df))) {
    df$bank_debt_ratio <- (df$bank_liabilities_st + df$bank_liabilities_mt + df$bank_liabilities_lt) / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
  }
  
  if(all(c("trade_payables_st","trade_payables_mt","trade_payables_lt","total_liabilities") %in% colnames(df))) {
    df$trade_payables_ratio <- (df$trade_payables_st + df$trade_payables_mt + df$trade_payables_lt) / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
  }
  
  if(all(c("bonds_payables_st","bonds_payables_mt","bonds_payables_lt","total_liabilities") %in% colnames(df))) {
    df$bond_debt_ratio <- (df$bonds_payables_st + df$bonds_payables_mt + df$bonds_payables_lt) / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
  }
  
  # Liquidity Ratios
  if(all(c("cash","total_liabilities_st") %in% colnames(df))) {
    df$cash_ratio <- df$cash / ifelse(df$total_liabilities_st < 1, 1, df$total_liabilities_st)
    df$quick_ratio <- (df$monetary_current_assets) / ifelse(df$total_liabilities_st < 1, 1, df$total_liabilities_st)
  }
  
  # Activity Ratios
  if(all(c("trade_payables_st","trade_payables_mt","trade_payables_lt","sales") %in% colnames(df))) {
    df$payables_turnover <- (df$trade_payables_st + df$trade_payables_mt + df$trade_payables_lt) / ifelse(df$sales < 1, 1, df$sales)
  }
  
  if(all(c("trade_receivables_st","trade_receivables_lt","sales") %in% colnames(df))) {
    df$receivables_turnover <- (df$trade_receivables_st + df$trade_receivables_lt) / ifelse(df$sales < 1, 1, df$sales)
  }
  
  # Cash Flow Ratios
  if(all(c("cf_operating","total_liabilities") %in% colnames(df))) {
    df$cash_flow_to_total_debt <- df$cf_operating / ifelse(df$total_liabilities < 1, 1, df$total_liabilities)
  }
  
  if(all(c("cf_operating","oth_interest_exp") %in% colnames(df))) {
    df$cash_flow_coverage_ratio <- df$cf_operating / ifelse(df$oth_interest_exp < 1, 1, df$oth_interest_exp)
  }
  
  # Save dataset after cleaning and feature engineering (with outliers)
  data_with_outliers <- df
  
  ### Outliers / Winsorization and Log Transformations ###
  
  # 1) Create Winsorized version limit extreme values.
  # 2) Create Winsorized + Log version
  
  # Core variables for transformations:
  winsor_vars <- c("ebit_margin", "net_profit_margin", "cf_op_to_debt", 
                   "cf_op_to_sales", "cf_inv_to_assets", "cf_fin_to_assets", 
                   "current_ratio","interest_coverage", "return_on_operating_profit", "ROA", "ROE",
                   "short_term_debt_ratio", "medium_term_debt_ratio", "long_term_debt_ratio", 
                   "debt_to_equity_ratio", "cash_ratio", "quick_ratio", "current_ratio",
                   "payables_turnover_days", "receivables_turnover_days",
                   "payables_turnover", "receivables_turnover",
                   "cash_flow_to_total_debt", "cash_flow_coverage_ratio")
  
  set_value_vars <- c("payables_turnover_days", "receivables_turnover_days")
  
  # Create Winsorized dataset
  df_winsor <- data_with_outliers
  for (var in winsor_vars) {
    if(var %in% colnames(df_winsor)) {
      p_lower <- quantile(df_winsor[[var]], 0.01, na.rm = TRUE)
      p_higher <- quantile(df_winsor[[var]], 0.99, na.rm = TRUE)
      df_winsor[[var]] <- pmin(pmax(df_winsor[[var]], p_lower), p_higher)
    }
  }
  
  for (var in set_value_vars) {
    if(var %in% colnames(df_winsor)) {
      df_winsor[[var]] <- pmin(df_winsor[[var]], 365)
    }
  }
  
  data_winsorized <- df_winsor
  
  # Now create Winsorized + Log dataset
  # Start again from data_with_outliers
  df_win_log <- data_with_outliers
  
  winsor_vars_log <- c("ebit_margin", "net_profit_margin", "cf_op_to_debt", 
                       "cf_op_to_sales", "cf_inv_to_assets", "cf_fin_to_assets",
                       "return_on_operating_profit", "ROA", "ROE",
                       "short_term_debt_ratio", "medium_term_debt_ratio", "long_term_debt_ratio", 
                       "debt_to_equity_ratio", "cash_ratio", "quick_ratio", "current_ratio",
                       "payables_turnover_days", "receivables_turnover_days",
                       "payables_turnover", "receivables_turnover",
                       "cash_flow_to_total_debt", "cash_flow_coverage_ratio", "cf_op_to_debt", "cf_op_to_sales")
  
  log_vars <- c("cf_op_to_sales", "interest_coverage","return_on_operating_profit","ROE","return_on_operating_profit",
                "cash_flow_coverage_ratio")
  
  for (var in winsor_vars_log) {
    if(var %in% colnames(df_win_log)) {
      p_lower <- quantile(df_win_log[[var]], 0.01, na.rm = TRUE)
      p_higher <- quantile(df_win_log[[var]], 0.99, na.rm = TRUE)
      df_win_log[[var]] <- pmin(pmax(df_win_log[[var]], p_lower), p_higher)
    }
  }
  
  for (var in set_value_vars) {
    if(var %in% colnames(df_win_log)) {
      df_win_log[[var]] <- pmin(df_win_log[[var]], 365)
    }
  }
  
  custom_log_transform <- function(x) {
    min_val <- min(x, na.rm = TRUE)
    shifted_x <- x + abs(min_val) + 1
    log_transformed <- log(shifted_x)
    return(log_transformed)
  }
  
  for (var in log_vars) {
    if(var %in% colnames(df_win_log)) {
      df_win_log[[var]] <- custom_log_transform(df_win_log[[var]])
    }
  }
  
  data_win_log <- df_win_log
  
  # Return a list of all three
  list(
    with_outliers = data_with_outliers,
    winsorized = data_winsorized,
    win_log = data_win_log
  )
}

# Apply the cleaning function to the training data
training_data_list <- clean_data(training_data)
training_data_with_outliers <- training_data_list$with_outliers
training_data_winsorized <- training_data_list$winsorized
training_data_win_log <- training_data_list$win_log

#######################################################
### Part 2: Data Analysis (Model Training & CV)     ###
#######################################################

# Define core variables
core_variables <- c(
  "ebit_margin", "net_profit_margin", "ROA", "ROE", "debt_to_assets", "debt_to_equity_ratio",
  "interest_coverage", "cf_op_to_debt", "cf_op_to_sales",
  "cf_inv_to_assets", "cf_fin_to_assets", "payables_turnover_days",
  "receivables_turnover_days", "current_ratio",
  "return_on_operating_profit",
  "short_term_debt_ratio", "medium_term_debt_ratio", "long_term_debt_ratio",
  "bank_debt_ratio", "trade_payables_ratio", "bond_debt_ratio",
  "cash_ratio", "cash_flow_coverage_ratio"
)

# Ensure default is factor
if("default" %in% colnames(training_data_winsorized)) {
  training_data_winsorized$default <- factor(training_data_winsorized$default, levels = c(0, 1), labels = c("No", "Yes"))
}
if("default" %in% colnames(training_data_win_log)) {
  training_data_win_log$default <- factor(training_data_win_log$default, levels = c(0, 1), labels = c("No", "Yes"))
}

# Create final training sets for modeling
df_training_data_winsorized <- training_data_winsorized[, c("default", core_variables)]
df_training_data_win_log <- training_data_win_log[, c("default", core_variables)]

### Define a function to calculate the Gini coefficient
gini_coefficient <- function(actual, predicted) {
  # Calculate the AUC (Area Under the Curve) for the given actual and predicted values
  auc_value <- auc(actual, predicted)
  
  # Convert the AUC to the Gini coefficient using the formula: Gini = 2 * AUC - 1
  # The Gini coefficient measures the model's ability to separate classes, 
  # with higher values indicating better performance.
  return(2 * auc_value - 1)
}

### Generalized CV function for any model (Logistic, RF, GBM)
perform_cv <- function(data, formula, method) {
  set.seed(42)
  
  # performs repeated cross-validation (5 repeats of 10-fold CV) for more stable results.
  cv_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    classProbs = TRUE, 
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  model <- train(
    formula, 
    data = data, 
    method = method, 
    metric = "ROC",
    trControl = cv_control
  )
  
  preds <- model$pred
  gini_scores <- sapply(split(preds, preds$Resample), function(fold_data) {
    gini_coefficient(factor(fold_data$obs, levels = c("No", "Yes")), fold_data$Yes)
  })
  
  return(gini_scores)
}

### Perform CV for Logistic Regression
gini_winsorized_lr <- perform_cv(df_training_data_winsorized, default ~ ., method = "glm")
gini_win_log_lr <- perform_cv(df_training_data_win_log, default ~ ., method = "glm")

### Perform CV for Random Forest
gini_winsorized_rf <- perform_cv(df_training_data_winsorized, default ~ ., method = "rf")
gini_win_log_rf <- perform_cv(df_training_data_win_log, default ~ ., method = "rf")

### Perform CV for Gradient Boosting
gini_winsorized_gb <- perform_cv(df_training_data_winsorized, default ~ ., method = "gbm")
gini_win_log_gb <- perform_cv(df_training_data_win_log, default ~ ., method = "gbm")

### Prepare data for boxplot
gini_results <- data.frame(
  Model = c(
    rep("LR Winsorized", length(gini_winsorized_lr)),
    rep("LR Winsorized + Log", length(gini_win_log_lr)),
    rep("RF Winsorized", length(gini_winsorized_rf)),
    rep("RF Winsorized + Log", length(gini_win_log_rf)),
    rep("GB Winsorized", length(gini_winsorized_gb)),
    rep("GB Winsorized + Log", length(gini_win_log_gb))
  ),
  Gini = c(
    gini_winsorized_lr,
    gini_win_log_lr,
    gini_winsorized_rf,
    gini_win_log_rf,
    gini_winsorized_gb,
    gini_win_log_gb
  )
)

### Create boxplot
print(
  ggplot(gini_results, aes(x = Model, y = Gini, fill = Model)) +
    geom_boxplot() +
    labs(title = "Comparison of Gini Coefficients", 
         y = "Gini Coefficient", 
         x = "Model Type") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
)

### Print summary means
cat("Logistic Regression Mean Gini (Winsorized):", mean(gini_winsorized_lr), "\n")
cat("Logistic Regression Mean Gini (Winsorized + Log):", mean(gini_win_log_lr), "\n")
cat("Random Forest Mean Gini (Winsorized):", mean(gini_winsorized_rf), "\n")
cat("Random Forest Mean Gini (Winsorized + Log):", mean(gini_win_log_rf), "\n")
cat("Gradient Boosting Mean Gini (Winsorized):", mean(gini_winsorized_gb), "\n")
cat("Gradient Boosting Mean Gini (Winsorized + Log):", mean(gini_win_log_gb), "\n")


############################################################
### Part 3: Apply the Cleaning Function to the Test Dataset ###
############################################################

# Function to align column data types
align_types <- function(train, test) {
  for (col in colnames(train)) {
    if (col %in% colnames(test)) {
      train_type <- class(train[[col]])
      if (train_type == "integer") {
        test[[col]] <- as.integer(as.numeric(test[[col]]))
      } else if (train_type == "numeric" || train_type == "double") {
        test[[col]] <- as.numeric(test[[col]])
      } else if (train_type == "factor" || train_type == "character") {
        test[[col]] <- as.character(test[[col]])
      }
    }
  }
  return(test)
}

# Align test dataset to training dataset types
test_data <- align_types(training_data, test_data)

# Core variables to check for NAs and replace with median
core_variables_to_impute <- c("sales", "gross_profit", "cash", "total_assets")

# Replace NAs in each core variable with the median of the entire test dataset
for (var in core_variables_to_impute) {
  if (var %in% colnames(test_data)) {
    median_value <- median(test_data[[var]], na.rm = TRUE)
    test_data[[var]][is.na(test_data[[var]])] <- median_value
  }
}

# Clean the test data using the same function
test_data_list <- clean_data(test_data)
test_data_win_log <- test_data_list$win_log

###########################################################
### Part 4: Use the Chosen Model to Predict on Test Data ###
###########################################################

# From the printed results, we see:
# "Random Forest Mean Gini (Winsorized + Log): 0.5284268" is the best model.

# We will now train the final Random Forest model on the entire final df_training_data_win_log
set.seed(42)
final_rf_model <- randomForest(
  default ~ ., 
  data = df_training_data_win_log,
  importance = TRUE
)

# Predict default probabilities on the cleaned test data
predictions <- predict(final_rf_model, newdata = test_data_win_log, type = "prob")

# Create a CSV output with columns: id and default (probability)
output <- data.frame(
  id = test_data_win_log$id,
  group_4 = predictions[, "Yes"]  # Ensure "Yes" probabilities are selected
)

# Write the output to CSV
write.csv(output, '/Users/sebastianrous/Documents/Studium/Master/Riskomanagement/SE/Project Versions/Final R/Group_4_Upload.csv', row.names = FALSE, quote = FALSE)