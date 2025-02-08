# find correct libraries
library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## sqft versus price


## graph data with regression line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## fit linear model
lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
## print model output
summary(lmod)


##beds+baths versus price


## filter data
bedbathandbeyond <- dataset[dataset$BEDS<20,]

bedbathandbeyond <- bedbathandbeyond[bedbathandbeyond$BATH<20,]


## graph data with regression line
ggplot(bedbathandbeyond, aes(x = log10(BEDS+BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


## fit linear model
lmod <- lm(log10(PRICE)~log10(BEDS+BATH), data = bedbathandbeyond)
## print model output
summary(lmod)



##500xbaths+sqft versus price


## graph data with regression line
ggplot(bedbathandbeyond, aes(x = log10(500*BATH+PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## fit linear model
lmod <- lm(log10(PRICE)~log10(500*BATH+PROPERTYSQFT), data = bedbathandbeyond)
## print model output
summary(lmod)

#result
