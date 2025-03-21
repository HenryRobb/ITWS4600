library(caret)
library(ggfortify)
library(class)
library("randomForest")
library("e1071")
library("ggplot2")
library("dplyr")
library("igraph")
library("ggraph")
library("DescTools")


# import the housing dataset
housing <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# make it Brooklyn specific
m_housing <- housing[(housing$BOROUGH == "3" | housing$BOROUGH == "brooklyn"), ]

#making a histogram for 1b to see where most of the sale prices lie
hist(log10(m_housing$SALE.PRICE), 
     col = "blue",
     main = "Log-Transformed Sales Price Distribution - Manhattan",
     xlab = "Log10(Sales Price)",
     breaks = 50)

#trying to make some good regression models for 1c

#clean the data
clean_m_housing <- m_housing[(housing$SALE.PRICE > 1000 & housing$SALE.PRICE < 500000000), ]

clean_m_housing$LAND.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", clean_m_housing$LAND.SQUARE.FEET))
clean_m_housing$GROSS.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", clean_m_housing$GROSS.SQUARE.FEET))
clean_m_housing$NUMERIC.TAX.CLASS <- as.numeric(gsub("[^0-9]", "", clean_m_housing$TAX.CLASS.AT.TIME.OF.SALE))
clean_m_housing$NUMERIC.TAX.CLASS <- 100 * clean_m_housing$NUMERIC.TAX.CLASS
clean_m_housing$TOTAL.UNITS.SCALED <- 100 * clean_m_housing$TOTAL.UNITS

lmod <- lm(log10(SALE.PRICE+1) ~ 
             (log10(LAND.SQUARE.FEET + 1) + 
                log10(GROSS.SQUARE.FEET + 1) + 
                TOTAL.UNITS.SCALED + 
                NUMERIC.TAX.CLASS
             ), 
           data = clean_m_housing)

# print model output
summary(lmod)

# predict neighborhood based on price and area
# Remove rows where either LAND.SQUARE.FEET or SALE.PRICE are NA
clean_m_housing$LAND.SQUARE.FEET <- as.numeric(clean_m_housing$LAND.SQUARE.FEET)
clean_m_housing$SALE.PRICE <- as.numeric(clean_m_housing$SALE.PRICE)
## Naive Bayes
nb.classifier <- naiveBayes(clean_m_housing[,c("LAND.SQUARE.FEET", "SALE.PRICE")], clean_m_housing$NEIGHBORHOOD)

## predict class labels
predictions <- predict(nb.classifier, clean_m_housing[,c("LAND.SQUARE.FEET", "SALE.PRICE")])

## confusion matrix
actual <- factor(clean_m_housing$NEIGHBORHOOD, levels = levels(predictions))

# compute confusion matrix
conf_matrix <- confusionMatrix(predictions, actual)
print(conf_matrix)

# find precision, recall, and F1-score
precision <- conf_matrix$byClass[, "Pos Pred Value"]
recall <- conf_matrix$byClass[, "Sensitivity"]
f1_score <- conf_matrix$byClass[, "F1"]
precision[is.nan(precision)| is.na(precision)] <- 0
recall[is.nan(recall)| is.na(recall)] <- 0 
f1_score[is.nan(f1_score) | is.na(f1_score)] <- 0

# output precision, recall, and F1-score
print("Precision:")
print(precision)

print("Recall:")
print(recall)

print("F1-score:")
print(f1_score)


## kNN
clean_m_housing <- clean_m_housing[
  !is.na(clean_m_housing$SALE.PRICE) &
    !is.na(clean_m_housing$LAND.SQUARE.FEET)
  ,]
clean_m_housing <- unique(clean_m_housing)
clean_m_housing <- clean_m_housing[!duplicated(clean_m_housing[, c("LAND.SQUARE.FEET", "SALE.PRICE")]), ]

knn.predictions <- class::knn(
  train = clean_m_housing[, c("LAND.SQUARE.FEET", "SALE.PRICE")],
  test = clean_m_housing[, c("LAND.SQUARE.FEET", "SALE.PRICE")],   
  cl = clean_m_housing$NEIGHBORHOOD,
  k = 40
)

actual <- factor(clean_m_housing$NEIGHBORHOOD, levels = levels(knn.predictions))

# compute confusion matrix
conf_matrix <- confusionMatrix(knn.predictions, actual)
print(conf_matrix)

# find precision, recall, and F1-score
precision <- conf_matrix$byClass[, "Pos Pred Value"]
recall <- conf_matrix$byClass[, "Sensitivity"]
f1_score <- conf_matrix$byClass[, "F1"]
precision[is.nan(precision)| is.na(precision)] <- 0
recall[is.nan(recall)| is.na(recall)] <- 0 
f1_score[is.nan(f1_score) | is.na(f1_score)] <- 0

# output precision, recall, and F1-score
print("Precision:")
print(precision)

print("Recall:")
print(recall)

print("F1-score:")
print(f1_score)


clean_m_housing$NEIGHBORHOOD <- as.factor(clean_m_housing$NEIGHBORHOOD)

## Random Forest
rf.classifier <- randomForest(NEIGHBORHOOD~SALE.PRICE + LAND.SQUARE.FEET, data=clean_m_housing, proximity=TRUE)

## predict class labels
predictions <- predict(rf.classifier, clean_m_housing)
print(predictions)
actual <- factor(clean_m_housing$NEIGHBORHOOD, levels = levels(predictions))

# compute confusion matrix
conf_matrix <- confusionMatrix(predictions, actual)
print(conf_matrix)

# find precision, recall, and F1-score
precision <- conf_matrix$byClass[, "Pos Pred Value"]
recall <- conf_matrix$byClass[, "Sensitivity"]
f1_score <- conf_matrix$byClass[, "F1"]
precision[is.nan(precision)| is.na(precision)] <- 0
recall[is.nan(recall)| is.na(recall)] <- 0 
f1_score[is.nan(f1_score) | is.na(f1_score)] <- 0

# output precision, recall, and F1-score
print("Precision:")
print(precision)

print("Recall:")
print(recall)

print("F1-score:")
print(f1_score)

