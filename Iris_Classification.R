library("e1071")
library("class")
library("ggplot2")


## read dataset
iris.data <- iris


## scatter plot of 2 variables
ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


## Naive Bayes
nb.classifier <- naiveBayes(iris[,1:4], iris$Species)

## predict class labels
predictions <- predict(nb.classifier, iris[,1:4])

## confusion matrix/contingency table
table(predictions, iris[,5], dnn=list('predicted','actual'))


## kNN
knn.predictions <- knn(iris[,1:4], iris[,1:4], iris$Species, k=3)

## confusion matrix/contingency table
table(knn.predictions, iris[,5], dnn=list('predicted','actual'))


## Random Forest
rf.classifier <- randomForest(Species~., data=iris.data, proximity=TRUE)

## predict class labels
predictions <- predict(rf.classifier, iris.data)

## confusion matrix/contingency table
table(predictions,iris.data$Species, dnn=list('predicted','actual'))


