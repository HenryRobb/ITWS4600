library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)
library(ggfortify)
library(class)
library(randomForest)


## read data
d <- read_csv("train.csv")
d <- na.omit(d)
d$SHOT_MADE <- as.factor(d$SHOT_MADE)

n = nrow(d)

sample_d <- sample(n, n*.8)

# split into train and test
d.train <- d[sample_d,]
d.test <- d[-sample_d, ]

n_train <- nrow(d.train)

# further sample
sample_size <- floor(0.1 * n_train)

set.seed(123)
train_sample_indices <- sample(1:n_train, size = sample_size)

# make new set for further sampled data
d.train_sampled <- d.train[train_sample_indices, ]

d.train_sampled$SHOT_MADE <- as.factor(d.train_sampled$SHOT_MADE)

rf.classifier_sampled <- randomForest(SHOT_MADE ~., data = d.train_sampled)

## predict class labels
predictions <- predict(rf.classifier_sampled, d.test)

cm = as.matrix(table(Actual = d.test$SHOT_MADE, Predicted = predictions))

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
  labs(title = "Performance Metrics", y = "Value", fill = "Metric")

importance <- importance(rf.classifier_sampled)
importance_df <- as.data.frame(importance)
importance_df$Variable <- rownames(importance_df)

ggplot(data = importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance", x = "Variable", y = "Mean Decrease in Gini")
