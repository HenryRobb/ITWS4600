##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(caret)
library(ggfortify)
library(class)


# PCA with wine dataset
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

X <- wine[,2:14]
Y <- wine[,1]

####### PCA #######

principal_components <- princomp(X, cor = TRUE, score = TRUE)

summary(principal_components)

principal_components$loadings

principal_components$scores

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# find new X without poor contributors to 1st PC
new_X <- subset(X, select = -c(`Alcalinity of ash`, `Nonflavanoid phenols`, `Malic acid`))

#rerun PCA
principal_components <- princomp(new_X, cor = TRUE, score = TRUE)


#find size of wines
n <- nrow(wine)

#create sample indices
sampleWines <- sample(n,n*.8)


## create train & test sets based on sampled indexes 
dataset.train <- wine[sampleWines,]

dataset.test <- wine[-sampleWines,]


## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the length, diameter
knn.predicted <- knn(train = dataset.train[,2:14], test = dataset.test[,2:14], cl = dataset.train$class, k = 3)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$class, dnn=list('predicted','actual'))

contingency.table

#find accuracy
sum(diag(contingency.table))/length(dataset.test$class)

TP1 <- contingency.table[1, 1]  # True Positives for Class 1
TP2 <- contingency.table[2, 2]  # True Positives for Class 2
TP3 <- contingency.table[3, 3]  # True Positives for Class 3

FP1 <- sum(contingency.table[, 1]) - TP1  # False Positives for Class 1
FP2 <- sum(contingency.table[, 2]) - TP2  # False Positives for Class 2
FP3 <- sum(contingency.table[, 3]) - TP3  # False Positives for Class 3

FN1 <- sum(contingency.table[1, ]) - TP1  # False Negatives for Class 1
FN2 <- sum(contingency.table[2, ]) - TP2  # False Negatives for Class 2
FN3 <- sum(contingency.table[3, ]) - TP3  # False Negatives for Class 3

# Compute Precision, Recall, and F1 for each class
precision1 <- TP1 / (TP1 + FP1)
precision2 <- TP2 / (TP2 + FP2)
precision3 <- TP3 / (TP3 + FP3)

recall1 <- TP1 / (TP1 + FN1)
recall2 <- TP2 / (TP2 + FN2)
recall3 <- TP3 / (TP3 + FN3)

f1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
f1_score3 <- 2 * (precision3 * recall3) / (precision3 + recall3)

cat("Precisions: ", precision1, precision2, precision3)
cat("Recalls: ", recall1, recall2, recall3)
cat("Fscores: ", f1_score1, f1_score2, f1_score3)

#using PCA scores to train a KNN model
knn.predicted <- knn(train = principal_components$scores[,1:3], test = principal_components$scores[,1:3], cl = wine$class, k = 3)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, wine$class, dnn=list('predicted','actual'))

contingency.table

#find accuracy
sum(diag(contingency.table))/length(wine$class)

TP1 <- contingency.table[1, 1]  # True Positives for Class 1
TP2 <- contingency.table[2, 2]  # True Positives for Class 2
TP3 <- contingency.table[3, 3]  # True Positives for Class 3

FP1 <- sum(contingency.table[, 1]) - TP1  # False Positives for Class 1
FP2 <- sum(contingency.table[, 2]) - TP2  # False Positives for Class 2
FP3 <- sum(contingency.table[, 3]) - TP3  # False Positives for Class 3

FN1 <- sum(contingency.table[1, ]) - TP1  # False Negatives for Class 1
FN2 <- sum(contingency.table[2, ]) - TP2  # False Negatives for Class 2
FN3 <- sum(contingency.table[3, ]) - TP3  # False Negatives for Class 3

# Compute Precision, Recall, and F1 for each class
precision1 <- TP1 / (TP1 + FP1)
precision2 <- TP2 / (TP2 + FP2)
precision3 <- TP3 / (TP3 + FP3)

recall1 <- TP1 / (TP1 + FN1)
recall2 <- TP2 / (TP2 + FN2)
recall3 <- TP3 / (TP3 + FN3)

f1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
f1_score3 <- 2 * (precision3 * recall3) / (precision3 + recall3)

cat("Precisions: ", precision1, precision2, precision3)
cat("Recalls: ", recall1, recall2, recall3)
cat("Fscores: ", f1_score1, f1_score2, f1_score3)

