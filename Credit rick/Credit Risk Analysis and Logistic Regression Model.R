# Load the required libraries
library(caret)
library(nnet)# For Logistic Regression
library(ROCR)
library(dplyr)
library(ggpubr)
library(MLmetrics)
library(stringr)

# Load the dataset
data <- read.csv("C:/Users/bismi/OneDrive/Desktop/Credit Risk Files/CreditRisk_Verify.csv")

# Save the variable columns
default_column <- data$DEFAULT
duration_column <- data$DURATION
num_credits_column <- data$NUM_CREDITS
age_column <- data$AGE
install_rate_column <- data$INSTALL_RATE
foreign_column <- data$FOREIGN
amount_column <- data$AMOUNT
other_install_column <- data$OTHER_INSTALL
new_car_column <- data$NEW_CAR
used_car_column <- data$USED_CAR
furniture_column <- data$FURNITURE
radio.tv_column <- data$RADIO.TV
education_column <- data$EDUCATION
retraining_column <- data$RETRAINING
male_div_column <- data$MALE_DIV
male_single_column <- data$MALE_SINGLE
male_mar_div_column <- data$MALE_MAR_or_WID
co.applicant_column <- data$CO.APPLICANT
guarantor_column <- data$GUARANTOR
real_estate_column <- data$REAL_ESTATE
prop_unknown_none_column <- data$PROP_UNKN_NONE
rent_column <- data$RENT
own_res_column <- data$OWN_RES
num_dependents_column <- data$NUM_DEPENDENTS
telephone_column <- data$TELEPHONE


# Explicitly specify levels for categorical variables
data$CHK_ACCT <- factor(str_trim(data$CHK_ACCT), levels = c("0", "1", "2", "3"))
data$HISTORY <- factor(str_trim(data$HISTORY), levels = c("0", "1", "2", "3", "4"))
data$SAV_ACCT <- factor(str_trim(data$SAV_ACCT), levels = c("0", "1", "2", "3", "4"))
data$EMPLOYMENT <- factor(str_trim(data$EMPLOYMENT), levels = c("0", "1", "2", "3", "4"))
data$PRESENT_RESIDENT <- factor(str_trim(data$PRESENT_RESIDENT), levels = c("1", "2", "3", "4"))
data$JOB <- factor(str_trim(data$JOB), levels = c("0", "1", "2", "3"))

# Define the main categorical variables
main_categorical_vars <- c("SAV_ACCT", "JOB", "EMPLOYMENT", "HISTORY", "PRESENT_RESIDENT", "CHK_ACCT")

# Apply one-hot encoding to the categorical variables using the caret package
dummy_variables <- dummyVars( ~ ., data = data[, main_categorical_vars], fullRank = FALSE)
data <- as.data.frame(predict(dummy_variables, newdata = data))

# Add back relevant columns
data$DEFAULT <- default_column
data$DURATION <- duration_column
data$AGE <- age_column
data$AMOUNT <- amount_column
data$NUM_CREDITS <- num_credits_column
data$INSTALL_RATE <- install_rate_column
data$FOREIGN <- foreign_column
data$OTHER_INSTALL <- other_install_column
data$NEW_CAR <- new_car_column
data$USED_CAR <- used_car_column
data$FURNITURE <- furniture_column
data$RADIO.TV <- radio.tv_column
data$EDUCATION <- education_column
data$RETRAINING <- retraining_column
data$CO.APPLICANT <- co.applicant_column
data$GUARANTOR <- guarantor_column
data$REAL_ESTATE <- real_estate_column
data$PPROP_UNKN_NONE <- prop_unknown_none_column
data$RENT <- rent_column
data$OWN_RES <- own_res_column
data$NUM_DEPENDENTS <- num_dependents_column
data$TELEPHONE <- telephone_column

# Create a new column "Combined_CHK_ACCT"
data$NO_CHK_OR_SAV <- ifelse(rowSums(data[, c("CHK_ACCT.3", "SAV_ACCT.4")]) > 0, 1, 0)

# Remove the original columns if needed
data <- data[, !names(data) %in% c("CHK_ACCT.3", "SAV_ACCT.4", "HISTORY.3")]

# Check for Missing Values'
apply(data, 2, function(x) sum(is.na(x)))

# Verify the updated structure
str(data)
summary(data)


############################################# Correlation #####################################################


# Select variables for correlation analysis
numeric_variables <- data[, sapply(data, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_variables, use = "complete.obs")
options(max.print = 3000)
# Display correlations
print(correlation_matrix)


# Select variables for correlation analysis on DEFAULT
numeric_variables <- data[, sapply(data, is.numeric)]

# Calculate the correlation between numeric variables and the 'DEFAULT' variable
correlation_with_default <- sapply(numeric_variables, function(var) cor(var, data$DEFAULT, use = "complete.obs"))

# Display the correlations
print(correlation_with_default)


########################################## Variable Selection ################################################


top_n_influential <- names(correlation_with_default)[1:30]

# View the names of the most influential variables
print(top_n_influential)

# Feature Selection - Exclude variables not contributing to model performance
exclude_vars <- c("RADIO.TV", "USED_CAR", "MALE_DIV", "OWN_RES", "FURNITURE", "TELEPHONE", "MALE_MAR_or_WID", "RETRAINING", "CO.APPLICANT", "EDUCATION", "REAL_ESTATE", "NUM_DEPENDENTS", "PRESENT_RESIDENT.4", "EMPLOYMENT.4", "JOB.3")

# Remove unnecessary variables
data <- data[, !names(data) %in% exclude_vars]

# Make sure the "DEFAULT" variable remains a factor
data$DEFAULT <- as.factor(data$DEFAULT)

# Rebuild the Logistic Regression model
logistic_model <- glm(DEFAULT ~ ., data = data, family = binomial)

# Split the data into training and testing sets after data preprocessing
set.seed(123)  # For reproducibility
splitIndex <- createDataPartition(data$DEFAULT, p = 0.7, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]


################################### Model #################################################


# Make predictions on the test data
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Fine-tune the classification threshold (adjust threshold value as needed)
threshold <- 0.56
logistic_predictions <- ifelse(logistic_predictions >= threshold, 1, 0)

# Make sure the levels of logistic_predictions match the levels of test_data$DEFAULT
logistic_predictions <- factor(logistic_predictions, levels = levels(test_data$DEFAULT))

# Reevaluate the model's performance
library(caret)
library(dplyr)

confusion_matrix <- confusionMatrix(logistic_predictions, test_data$DEFAULT)
print(confusion_matrix)


##################################################### Lift #######################################################


#Make predictions
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Sort
sorted_predictions <- data.frame(Prob = logistic_predictions, Observed = as.numeric(test_data$DEFAULT))
sorted_predictions <- sorted_predictions[order(-sorted_predictions$Prob), ]
deciles <- 10
total_records <- nrow(test_data)

# Calculate cumulative lift
lift_values <- rep(0, deciles)
for (i in 1:deciles) {
  records_in_decile <- floor(total_records * (i / deciles))
  subset_decile <- sorted_predictions[1:records_in_decile, ]
  lift_values[i] <- sum(subset_decile$Observed) / sum(sorted_predictions$Observed) * deciles
}
show(logistic_predictions)
show(lift_values)
# Plot the lift chart
plot(1:deciles, lift_values, type = "b", xlab = "Deciles", ylab = "Lift", main = "Lift Chart")



####################################################### Visuals #############################################################


# Variable Importance Plot
var_imp <- abs(coef(logistic_model)[-1])
names(var_imp) <- names(coef(logistic_model)[-1])
var_imp <- sort(var_imp, decreasing = TRUE)
par(cex.axis = 0.3)
barplot(var_imp, main = "Variable Importance", col = "blue")

# ROC Curve
library(pROC)

roc_obj <- roc(test_data$DEFAULT, as.numeric(logistic_predictions))
roc_auc <- auc(roc_obj)

plot(roc_obj, main = paste("ROC Curve (AUC =", round(roc_auc, 2), ")", sep = ""), print.auc = TRUE, auc.polygon = TRUE, grid = TRUE)

# Model Calibration Plot
library(ggplot2)

calibration_data <- data.frame(Observed = as.numeric(test_data$DEFAULT), Predicted = as.numeric(logistic_predictions))
ggplot(calibration_data, aes(x = Predicted, y = Observed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Model Calibration Plot", x = "Predicted Probabilities", y = "Observed")



# Fit a logistic regression model using the caret package to get variable importance
logistic_model_caret <- train(DEFAULT ~ ., data = train_data, method = "glm", family = binomial)
var_imp <- varImp(logistic_model_caret)

#check variables in the logistic model for issues
summary(logistic_model_caret$finalModel)

# Plot Variable Importance
plot(var_imp, main = "Variable Importance", col = "blue")

# Get coefficients from the logistic_model_caret
coef_values <- coef(logistic_model_caret$finalModel)[-1]
var_imp <- abs(coef_values)

# Sort variable importance
var_imp <- sort(var_imp, decreasing = TRUE)

# Plot Variable Importance
barplot(var_imp, main = "Variable Importance", font = 1, col = "blue")





# Variable Importance Plot with ggplot2
library(ggplot2)

var_imp <- abs(coef(logistic_model)[-1])
names(var_imp) <- names(coef(logistic_model)[-1])
var_imp <- sort(var_imp, decreasing = TRUE)

# Create a data frame for ggplot
var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)

# Barplot with ggplot2
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ROC Curve with ggplot2
library(pROC)
roc_obj <- roc(test_data$DEFAULT, as.numeric(logistic_predictions))
roc_auc <- auc(roc_obj)

# Create a data frame for ggplot
roc_df <- data.frame(FPR = roc_obj$specificities, TPR = roc_obj$sensitivities)

# Plot ROC Curve with ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = paste("ROC Curve (AUC =", round(roc_auc, 2), ")", sep = ""),
       x = "False Positive Rate", y = "True Positive Rate")

# Calibration Plot with ggplot2
calibration_data <- data.frame(Observed = as.numeric(test_data$DEFAULT), Predicted = as.numeric(logistic_predictions))
ggplot(calibration_data, aes(x = Predicted, y = Observed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Model Calibration Plot", x = "Predicted Probabilities", y = "Observed")
