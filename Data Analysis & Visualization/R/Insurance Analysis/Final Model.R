# Load required packages
library(randomForest)
library(rpart)
library(rpart.plot)

# Read the training dataset
train_data <- read.csv("C:/Users/bismi/OneDrive/Desktop/DAT 640/TicdataTraining.csv")
View(train_data)
# Select the desired columns
selected_columns <- c("MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF", "MOSHOOFD", "MGODRK", "MGODPR",
                      "MGODOV", "MGODGE", "MRELGE", "MRELSA", "MRELOV", "MFALLEEN", "MFGEKIND",
                      "MFWEKIND", "MOPLHOOG", "MOPLMIDD", "MOPLLAAG", "MBERHOOG", "MBERZELF",
                      "MBERBOER", "MBERMIDD", "MBERARBG", "MBERARBO", "MSKA", "MSKB1", "MSKB2",
                      "MSKC", "MSKD", "MHHUUR", "MHKOOP", "MAUT1", "MAUT2", "MAUT0", "MZFONDS",
                      "MZPART", "MINKM30", "MINK3045", "MINK4575", "MINK7512", "MINK123M",
                      "MINKGEM", "MKOOPKLA", "PWAPART", "PWABEDR", "PWALAND", "PPERSAUT",
                      "PBESAUT", "PMOTSCO", "PVRAAUT", "PAANHANG", "PTRACTOR", "PWERKT", "PBROM",
                      "PLEVEN", "PPERSONG", "PGEZONG", "PWAOREG", "PBRAND", "PZEILPL", "PPLEZIER",
                      "PFIETS", "PINBOED", "PBYSTAND", "AWAPART", "AWABEDR", "AWALAND", "APERSAUT",
                      "ABESAUT", "AMOTSCO", "AVRAAUT", "AAANHANG", "ATRACTOR", "AWERKT", "ABROM",
                      "ALEVEN", "APERSONG", "AGEZONG", "AWAOREG", "ABRAND", "AZEILPL", "APLEZIER",
                      "AFIETS", "AINBOED", "ABYSTAND", "CARAVAN")

# Subset the training data based on selected columns
selected_columns <- intersect(selected_columns, colnames(train_data))
train_data <- train_data[selected_columns]

# Check the structure of the training dataset
str(train_data)

# Summary statistics of the training dataset
summary(train_data)

# Convert the response variable to a factor
train_data$CARAVAN <- as.factor(train_data$CARAVAN)

################################## Early Visuals ##############################

# Load required packages
library(ggplot2)

# Early Data Evaluation - Boxplot for Average Income
ggplot(train_data, aes(x = CARAVAN, y = MINKGEM)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  ggtitle("Boxplot of Average Income by Caravan Insurance") +
  xlab("Caravan Insurance") +
  ylab("Average Income")

# Early Data Evaluation - Histogram for Frequency of Type
ggplot(train_data, aes(x = MOSTYPE, fill = CARAVAN)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "white") +
  ggtitle("Histogram of Type") +
  xlab("MOSTYPE") +
  ylab("Frequency") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "darkred"))

# Early Data Evaluation - Scatterplot for Age-to-Income
ggplot(train_data, aes(x = MGEMLEEF, y = MINKGEM, color = CARAVAN)) +
  geom_point(alpha = 0.7) +
  ggtitle("Scatterplot of Age-to-Income by Caravan Insurance") +
  xlab("Age") +
  ylab("Average Income") +
  scale_color_manual(values = c("No" = "lightblue", "Yes" = "darkred"))

################################## Forest Model #################################

# Build the random forest model
rf_model <- randomForest(CARAVAN ~ ., data = train_data, ntree = 100)

# Check important features
varImpPlot(rf_model)

################################### Decision Tree ##############################

# Load the required packages
library(rpart)
library(rpart.plot)

# Build the decision tree model
dt_model <- rpart(CARAVAN ~ ., data = train_data, method = "class", control = rpart.control(minsplit = 30, cp = 0.001))

# Plot the decision tree
rpart.plot(dt_model, tweak = 1.2)

############################ Model & Tree Performance #############################

# Assess model performance on training data
train_predictions <- predict(rf_model, train_data)
train_accuracy <- sum(train_predictions == train_data$CARAVAN) / nrow(train_data)
print(paste("Training Accuracy (Random Forest):", train_accuracy))

# Assess model performance on training data (Decision Tree)
dt_train_predictions <- predict(dt_model, train_data, type = "class")
dt_train_accuracy <- sum(dt_train_predictions == train_data$CARAVAN) / nrow(train_data)
print(paste("Training Accuracy (Decision Tree):", dt_train_accuracy))
