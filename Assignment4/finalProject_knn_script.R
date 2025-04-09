library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)
library(ggfortify)
library(class)

## read data
d <- read_csv("train.csv")

n = nrow(d)

sample_d <- sample(n, n*.8)


## d <- d[, c("ACTION_TYPE", "POSITION", "SHOT_DISTANCE")]

d$ACTION_TYPE <- as.numeric(factor(d$ACTION_TYPE))
d$POSITION <- as.numeric(factor(d$POSITION))
d$SHOT_MADE <- as.factor(d$SHOT_MADE)
d$SHOT_DISTANCE <- scale(d$SHOT_DISTANCE)

d.train <- d[sample_d,]
d.test <- d[-sample_d, ]

d.train <- d.train[complete.cases(d.train[, c("ACTION_TYPE", "POSITION", "SHOT_DISTANCE", "SHOT_MADE")]), ]


## train and evaluate multiple knn models to find optimal k
knn.model <- train(x = d.train[, c("ACTION_TYPE", "POSITION", "SHOT_DISTANCE")],
                   y = d.train$SHOT_MADE,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1, 3, 5, 7)),
                   trControl = trainControl(method = "cv"))

knn.predictions <- predict(knn.model, newdata = d.test)

cm = as.matrix(table(Actual = d.test$SHOT_MADE, Predicted = knn.predictions))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)