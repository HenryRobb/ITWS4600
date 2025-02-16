library(class)
library(caret)

dataset <- read.csv("epi_results_2024_pop_gdp.csv") 

## 1) Derive 2 subsets, each for a different region.
EasternEurope = dataset[dataset$region=="Eastern Europe",]
GlobalWest = dataset[dataset$region=="Global West",]

## 1.1. Plot histograms for a variable of your choice for both regions with density lines overlayed.
hist(EasternEurope$gdp, 
     col = "blue",
     main = "GDP Distribution - Eastern Europe",
     xlab = "GDP",
     probability = TRUE)
lines(density(EasternEurope$gdp, bw = 1000), col = "red", lwd = 3)

hist(GlobalWest$gdp, 
     probability = TRUE,
     col = "blue",
     main = "GDP Distribution - Global West",
     xlab = "GDP")
lines(density(GlobalWest$gdp, bw = 1000), col = "red", lwd = 3)

## 1.2. Plot QQ plots for both variables compared to known probability distributions
qqplot(rnorm(250),
       EasternEurope$gdp, 
       main = "QQ Plot - Eastern Europe GDP", 
       col = "blue")
qqline(EasternEurope$gdp, col = "red", lwd = 2)
# Make a Q-Q plot against the generating distribution by: 

qqplot(rnorm(250),
       GlobalWest$gdp, 
       main = "QQ Plot - Global West GDP", 
       col = "blue")
qqline(GlobalWest$gdp, col = "red", lwd = 2)


## 2) Fit linear models as follows:

## 2.1. Choose 2 variables and fit linear models with these variables as response and choose either population or gdp (or both) as predictors. For each model print the model summary stats and plot the most significant predictor vs the response as well as the residuals. Apply transformations (e.g. log) to variables if needed.

lmod_gdpandpop_epi <- lm((gdp+log10(population))~log10(EPI.new), data = dataset)
summary(lmod_gdpandpop_epi)

lmod_gdpandpop_bdh <- lm((gdp+log10(population))~BDH.new, data = dataset)
summary(lmod_gdpandpop_bdh)


ggplot(dataset, aes(x = (gdp+log10(population)), y = log10(EPI.new))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


## 2.2. Repeat the previous models with a subset of 1 region and in 1-2 sentences explain which model is a better fit and why you think that is the case.

lmod_gdpandpop_epi_GW <- lm((gdp+log10(population))~log10(EPI.new), data = GlobalWest)
summary(lmod_gdpandpop_epi_GW)

lmod_gdpandpop_bdh_GW <- lm((gdp+log10(population))~BDH.new, data = GlobalWest)
summary(lmod_gdpandpop_bdh_GW)


ggplot(GlobalWest, aes(x = (gdp+log10(population)), y = log10(EPI.new))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


EEandGW <- dataset[dataset$region %in% c("Eastern Europe", "Global West") & !is.na(dataset$region), ]

unique(EEandGW$region)


# sample create list of 105 (70% of 150) numbers randomly sampled from 1-150
n = nrow(EEandGW)
sampleEEandGW <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
EEandGW.train <- EEandGW[sampleEEandGW,]

EEandGW.test <- EEandGW[-sampleEEandGW,]

print(unique(EEandGW.train$region))  

# Check the unique regions in test set
print(unique(EEandGW.test$region))

# simple estimate of k
k = 2


########################
# Train & Evaluate knn #
########################

## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the length, diameter
knn.predicted <- knn(train = EEandGW.train[,c(8,10,12)], test = EEandGW.test[,c(8,10,12)], cl = EEandGW.train$region, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, EEandGW.test$region, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(EEandGW.test$region)

########################
# Train & Evaluate knn #
########################

## train model & predict in one step ('knn' function from 'class' library)
## trying to predict the age group by using the length, diameter
knn.predicted <- knn(train = EEandGW.train[,c(7,9,11)], test = EEandGW.test[,c(8,10,12)], cl = EEandGW.train$region, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, EEandGW.test$region, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(EEandGW.test$region)


