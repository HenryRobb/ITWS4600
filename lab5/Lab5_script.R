## Establish Wine Dataset

library(caret)
library(ggfortify)
library(class)
library(e1071)


wine <- read.csv("wine.data", header = FALSE)
colnames(wine) <- c(
  "class",
  "Alcohol",
  "Malic acid",
  "Ash",
  "Alcalinity of ash",  
  "Magnesium",
  "Total phenols",
  "Flavanoids",
  "Nonflavanoid phenols",
  "Proanthocyanins",
  "Color intensity",
  "Hue",
  "OD280/OD315 of diluted wines",
  "Proline"
)
head(wine)

wine$class <- as.character(wine$class)
wine$class <- as.factor(wine$class)

# ## split train/test
n <- nrow(wine)
train.indexes <- sample(n,0.7*n)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,2:14]
Y <- train[,1]

## feature boxplots
boxplot(X, main="wine features")
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")
scales <- list(x=list(relation="free"), y=list(relation="free"))

featurePlot(x=X, y=Y, plot="density", scales=scales)


## class label distributions
plot(Y)


###########################################
###########################################
## train SVM model - linear kernel########
svm.mod0 <- svm(class ~ Hue + Proline, data = train, kernel = 'linear')

svm.mod0

plot(svm.mod0, data = train, formula = Hue~Proline, svSymbol = "x", dataSymbol = "o")


train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$class, Predicted = train.pred))

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

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Magnesium = x1, Poline = x2)
}

x <- train[,:c(12,14)]
y <- as.numeric(train$class)
y[y==2] <- -1

xgrid = make.grid(x)
xgrid[1:10,]

ygrid = predict(svm.mod0, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)

points(x, col = y + 3, pch = 19)
points(x[svm.mod0$index,], pch = 5, cex = 2)


###########################################
###########################################
## train SVM model - polynomial kernel#####
svm.mod1 <- svm(class ~ Hue+Proline, data = train, kernel = 'radial')

svm.mod1

plot(svm.mod1, train, Hue~Proline)

train.pred <- predict(svm.mod1, train)

x <- train[,c(12,14)]
y <- as.numeric(train$class)
y[y==2] <- -1

xgrid = make.grid(x)
xgrid[1:10,]

ygrid = predict(svm.mod1, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)

points(x, col = y + 3, pch = 19)
points(x[svm.mod1$index,], pch = 5, cex = 2)

cm = as.matrix(table(Actual = train$class, Predicted = train.pred))

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


## Tuned SVM - polynomial
tuned.svm <- tune.svm(class ~ Hue + Proline, data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(iris),1, .01), cost = 2^seq(-6, 4, 2))

tuned.svm

svm.mod2 <- svm(class ~ Hue + Proline, data = train, kernel = 'polynomial', gamma = .81, cost = 1)

svm.mod2

train.pred <- predict(svm.mod2, train)

x <- train[,c(12,14)]
y <- as.numeric(train$class)
y[y==2] <- -1

xgrid = make.grid(x)
# xgrid[1:10,]

ygrid = predict(svm.mod2, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)

points(x, col = y + 3, pch = 19)
points(x[svm.mod2$index,], pch = 5, cex = 2)

cm = as.matrix(table(Actual = train$class, Predicted = train.pred))

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


### Test set prediction ###

###########################################
###########################################
## model 0 - linear kernel
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$class, Predicted = test.pred))

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

###########################################
###########################################
## model 1 - polynomial kernel
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$class, Predicted = test.pred))

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

## model 2- polynomial kernel with tuned parameters
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$class, Predicted = test.pred))

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


## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the length, diameter
knn.predicted <- knn(train = train[,c(12,14)], test = test[,c(12,14)], cl = train$class, k = 3)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, test$class, dnn=list('predicted','actual'))

cm = as.matrix(contingency.table)
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
