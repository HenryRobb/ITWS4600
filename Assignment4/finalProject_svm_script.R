library(e1071)
library(caret)
library(readr)
library(ggplot2)
library(cv)
library(ggfortify)
library(class)

# Load the training data
d <- read_csv("train.csv")

# Remove rows with missing values
d <- na.omit(d)

# Convert the target variable to a factor
d$SHOT_MADE <- as.factor(d$SHOT_MADE)

# Split the data into training and testing sets
n <- nrow(d)
sample_d <- sample(n, n * 0.8)
d.train <- d[sample_d, ]
d.test <- d[-sample_d, ]

# Downsample training data
train_size <- nrow(d.train)
sample_train_indices <- sample(1:train_size, size = floor(0.02 * train_size), replace = FALSE)
d.train_downsampled <- d.train[sample_train_indices, ]

# Downsample testing data
test_size <- nrow(d.test)
sample_test_indices <- sample(1:test_size, size = floor(0.02 * test_size), replace = FALSE)
d.test_downsampled <- d.test[sample_test_indices, ]

# Convert GAME_DATE to Date objects
d.train_downsampled$GAME_DATE <- as.Date(d.train_downsampled$GAME_DATE, format = "%m-%d-%Y")
d.test_downsampled$GAME_DATE <- as.Date(d.test_downsampled$GAME_DATE, format = "%m-%d-%Y")

# Extract month
d.train_downsampled$GAME_MONTH <- as.factor(format(d.train_downsampled$GAME_DATE, "%m"))
d.test_downsampled$GAME_MONTH <- as.factor(format(d.test_downsampled$GAME_DATE, "%m"))

# Extract day of the week
d.train_downsampled$GAME_DAY_WEEK <- as.factor(format(d.train_downsampled$GAME_DATE, "%a"))
d.test_downsampled$GAME_DAY_WEEK <- as.factor(format(d.test_downsampled$GAME_DATE, "%a"))

# Identify infrequent players based on the training data
player_counts_train <- table(d.train_downsampled$PLAYER_NAME)
infrequent_threshold <- 50
infrequent_players <- names(player_counts_train)[player_counts_train < infrequent_threshold]

# Create reduced player name column for training set
d.train_downsampled$PLAYER_NAME_REDUCED <- ifelse(d.train_downsampled$PLAYER_NAME %in% infrequent_players, "Other", as.character(d.train_downsampled$PLAYER_NAME))
d.train_downsampled$PLAYER_NAME_REDUCED <- as.factor(d.train_downsampled$PLAYER_NAME_REDUCED)

# Create reduced player name column for test set
d.test_downsampled$PLAYER_NAME_REDUCED <- ifelse(d.test_downsampled$PLAYER_NAME %in% infrequent_players, "Other", as.character(d.test_downsampled$PLAYER_NAME))
d.test_downsampled$PLAYER_NAME_REDUCED <- as.factor(d.test_downsampled$PLAYER_NAME_REDUCED)

# Ensure that the levels in the test set's PLAYER_NAME_REDUCED are also in the training set's levels
train_player_levels <- levels(d.train_downsampled$PLAYER_NAME_REDUCED)
test_player_levels <- levels(d.test_downsampled$PLAYER_NAME_REDUCED)

new_levels_in_test <- setdiff(test_player_levels, train_player_levels)

# Reassign any new levels in the test set to "Other"
for (level in new_levels_in_test) {
  d.test_downsampled$PLAYER_NAME_REDUCED[d.test_downsampled$PLAYER_NAME_REDUCED == level] <- "Other"
}

# Factor the test set's PLAYER_NAME_REDUCED after reassigning
d.test_downsampled$PLAYER_NAME_REDUCED <- as.factor(d.test_downsampled$PLAYER_NAME_REDUCED)

# Define categorical and numerical columns AFTER feature engineering
categorical_cols <- c("SEASON_2", "TEAM_NAME", "PLAYER_NAME_REDUCED", "POSITION_GROUP", "POSITION", "GAME_MONTH", "GAME_DAY_WEEK", "HOME_TEAM", "AWAY_TEAM", "ACTION_TYPE", "SHOT_TYPE", "BASIC_ZONE", "ZONE_NAME", "ZONE_ABB", "ZONE_RANGE")
numerical_cols <- c("SEASON_1", "TEAM_ID", "PLAYER_ID", "GAME_ID", "LOC_X", "LOC_Y", "SHOT_DISTANCE", "QUARTER", "MINS_LEFT", "SECS_LEFT")

# prepping training data

# One-hot encode categorical features
dummy_model_train <- dummyVars(paste("~", paste(categorical_cols, collapse = "+")), data = d.train_downsampled)
encoded_train <- predict(dummy_model_train, newdata = d.train_downsampled)
encoded_train_df <- as.data.frame(encoded_train)

# Scale numerical features
scaler_train <- preProcess(d.train_downsampled[, numerical_cols], method = c("center", "scale"))
scaled_train <- predict(scaler_train, d.train_downsampled[, numerical_cols])
scaled_train_df <- as.data.frame(scaled_train)

# Combine encoded categorical and scaled numerical features for training
train_features <- cbind(encoded_train_df, scaled_train_df)
train_target <- d.train_downsampled$SHOT_MADE

resample <- sample(nrow(train_features), nrow(train_features) * 0.8)


# prepping testing data

# One-hot encode categorical features using the same model trained on the training data
encoded_test <- predict(dummy_model_train, newdata = d.test_downsampled)
encoded_test_df <- as.data.frame(encoded_test)

# Scale numerical features using the same scaler trained on the training data
scaled_test <- predict(scaler_train, d.test_downsampled[, numerical_cols])
scaled_test_df <- as.data.frame(scaled_test)

# Combine encoded categorical and scaled numerical features for testing
test_features <- cbind(encoded_test_df, scaled_test_df)
test_target <- d.test_downsampled$SHOT_MADE

# Train SVM model with radial kernel
svm_model_radial <- svm(train_features[resample, ], train_target[resample], kernel = "radial")


train_cols <- colnames(train_features)
test_cols <- colnames(test_features)

missing_cols <- setdiff(train_cols, test_cols)

for (col in missing_cols) {
  test_features[[col]] <- 0
}

# Ensure the order of columns in test_features is the same as in train_features
test_features <- test_features[, train_cols]

# Make predictions on the test set
prediction_svm_radial <- predict(svm_model_radial, test_features)

# Evaluate the model
cm <- as.matrix(table(Actual = test_target, Predicted = prediction_svm_radial))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

correctly_classified <- sum(diag(cm))
accuracy <- correctly_classified / n

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1, accuracy)

library(reshape2)

# Convert the confusion matrix to a data frame for ggplot
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Predicted", "Actual", "Count")

cm_df <- as.data.frame.matrix(cm)
cm_df$Actual <- rownames(cm_df)
cm_melted <- melt(cm_df, id.vars = "Actual", variable.name = "Predicted", value.name = "Count")

# Create the heatmap
ggplot(data = cm_melted, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix", fill = "Count")

metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision (FALSE)", "Precision (TRUE)", "Recall (FALSE)", "Recall (TRUE)", "F1 (FALSE)", "F1 (TRUE)"),
  Value = c(accuracy, precision[1], precision[2], recall[1], recall[2], f1[1], f1[2])
)

ggplot(data = metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.3) +
  ylim(0, 1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.title.x = element_blank()) +
  labs(title = "Performance Metrics", y = "Value", fill = "Metric") # Keep y-axis label



