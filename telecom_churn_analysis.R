# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)
library(ROCR)

# Load the dataset
data <- read.csv('telecom.csv')

# Print basic information of the dataset
cat('Basic information of the dataset:\n')
str(data)

# Check the number of rows and columns in the dataset
rows <- nrow(data)
columns <- ncol(data)

if (rows < 100 && columns < 20) {
  # For short - form data (less than 100 rows and less than 20 columns), view the full data information
  cat('Full content information of the data:\n')
  print(data, na.print = 'nan')
} else {
  # For long - form data, view the first few rows of data information
  cat('Information of the first few rows of the data:\n')
  print(head(data), na.print = 'nan')
}

# Data visualization
# Plot a bar chart of the distribution of Churn
churn_distribution_plot <- ggplot(data, aes(x = Churn)) +
  geom_bar() +
  labs(title = 'Distribution of Churn', x = 'Churn', y = 'Count')
print(churn_distribution_plot)

# Plot a box plot of MonthlyCharges grouped by Churn
monthly_charges_boxplot <- ggplot(data, aes(x = Churn, y = MonthlyCharges)) +
  geom_boxplot() +
  labs(title = 'Monthly Charges by Churn', x = 'Churn', y = 'Monthly Charges')
print(monthly_charges_boxplot)

# Data preprocessing
# Handle missing values
data <- na.omit(data)

# Encode categorical variables
data$Churn <- as.factor(data$Churn)
data <- data %>% mutate_if(is.character, as.factor)

# Split the dataset into training and test sets
set.seed(42)
train_index <- createDataPartition(data$Churn, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the Random Forest model
rf_model <- randomForest(Churn ~ ., data = train_data, ntree = 200, maxdepth = 10)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, test_data$Churn)
print(conf_matrix)

# Plot the heatmap of the confusion matrix
conf_matrix_df <- as.data.frame(conf_matrix$table)
conf_matrix_heatmap <- ggplot(conf_matrix_df, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(title = 'Confusion Matrix', x = 'Predicted', y = 'Actual')
print(conf_matrix_heatmap)

# Calculate the ROC curve
pred <- prediction(as.numeric(predictions), as.numeric(test_data$Churn))
perf <- performance(pred, "tpr", "fpr")
roc_plot <- plot(perf, main = "ROC Curve")
abline(a = 0, b = 1)
    
