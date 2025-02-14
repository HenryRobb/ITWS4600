############################################
##### k-Nearest Neighbors Evaluation  ######
############################################

library(class)
library(caret)

# read dataset
dataset <- iris

# sample create list of 105 (70% of 150) numbers randomly sampled from 1-150
n = 150
s_iris <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
dataset.train <- dataset[s_iris,]

dataset.test <- dataset[-s_iris,]

# simple estimate of k
k = round(sqrt(n))

k <- k-1


########################
# Train & Evaluate knn #
########################

## train model & predict in one step ('knn' function from 'class' library)
knn.predicted <- knn(train = dataset.train[,1:4], test = dataset.test[,1:4], cl = dataset.train$Species, k = 11)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$Species, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset.test$Species)


##################
# Find optimal k #
##################


## train knn models for multiple values of k and plot accuracies

# list of k
k.list <- c(7,9,11,13,15,17,19)

# empty list for accuracy
accuracy.list <- c()

# loop: train&predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset[,1:4], test = dataset[,1:4], cl = dataset$Species, k = k)
  
  contingency.table <- table(knn.predicted, dataset$Species, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$Species)
  
  accuracy.list <- c(accuracy.list,accuracy)
  
}


# plot acccuracy with k, limiting values of y axis between .9 & 1
plot(k.list,accuracy.list,type = "b", ylim = c(.9,1))

## Alternatively:

## train and evaluate multiple knn models to find optimal k
knn.model <- train(dataset[,1:4], dataset$Species, method = "knn", tuneLength = 10, trControl = trainControl(method = "cv"))

# print model outputs
print(knn.model)


#### END ####