###################
##### Abalone #####
###################
library(class)
library(caret)

# read dataset
abalone <- read.csv("abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$agegroups <- cut(dataset$rings, br=c(0,8,11,35), labels = c('young', 'middle age', 'old'))


# sample create list of 105 (70% of 150) numbers randomly sampled from 1-150
n = 4100
sampleAbalone <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
dataset.train <- dataset[sampleAbalone,]

dataset.test <- dataset[-sampleAbalone,]

# simple estimate of k
k = round(sqrt(n))

k <- k-1


########################
# Train & Evaluate knn #
########################

## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the length, diameter
knn.predicted <- knn(train = dataset.train[,2:3], test = dataset.test[,2:3], cl = dataset.train$agegroups, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$agegroups, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset.test$agegroups)


## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the height, whole weight
knn.predicted <- knn(train = dataset.train[,3:4], test = dataset.test[,3:4], cl = dataset.train$agegroups, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$agegroups, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset.test$agegroups)


## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the length, whole weight
knn.predicted <- knn(train = dataset.train[,c(2,6)], test = dataset.test[,c(2,6)], cl = dataset.train$agegroups, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$agegroups, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset.test$agegroups)



# we have that the final combination is the most accurate at .635
## train knn models for multiple values of k and plot accuracies

# list of k
k.list <- c(7,9,11,13,15,17,19, 21, 25, 29, 33, 37)

# empty list for accuracy
accuracy.list <- c()

# loop: train&predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset[,c(2,6)], test = dataset[,c(2,6)], cl = dataset$agegroups, k = k)
  
  contingency.table <- table(knn.predicted, dataset$agegroups, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$agegroups)
  
  accuracy.list <- c(accuracy.list,accuracy)
  
}


# plot acccuracy with k, limiting values of y axis between .9 & 1
plot(k.list,accuracy.list,type = "b", ylim = c(.5,1))

## Alternatively:

## train and evaluate multiple knn models to find optimal k
knn.model <- train(dataset[,c(2,6)], dataset$agegroups, method = "knn", tuneLength = 10, trControl = trainControl(method = "cv"))

# print model outputs
print(knn.model)



## we now have the most accurate k, so now we will look at kmeans

## run kmeans
k = 3
abalone.km <- kmeans(dataset[,c(2,6)], centers = k)


## get and plot clustering output 

assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(dataset, aes(x = length, y = whole_weight, colour = as.factor(assigned.clusters))) +
  geom_point()


## WCSS: total within cluster sum of squares
abalone.km$tot.withinss

abalone.km$cluster


## run tests with multiple k values and plot WCSS
k.list <- c(2,3,4,5,6)

wcss.list <- c()

for (k in k.list) {
  
  abalone.km <- kmeans(dataset[,c(2,6)], centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  ## get and plot clustering output 
  assigned.clusters <- as.factor(abalone.km$cluster)
  
  ggplot(dataset, aes(x = length, y = whole_weight, colour = assigned.clusters)) +
    geom_point()
  
}

plot(k.list,wcss.list,type = "b")
