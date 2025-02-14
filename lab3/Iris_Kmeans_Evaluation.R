################################
##### K-Means Evaluation  ######
################################

dataset <- iris

## plot dataset colored by class
ggplot(dataset, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

## set random number generator start value
# set.seed(123)

## run kmeans
k = 3
iris.km <- kmeans(dataset[,1:4], centers = k)


## get and plot clustering output 

assigned.clusters <- as.factor(iris.km$cluster)

ggplot(dataset, aes(x = Petal.Length, y = Petal.Width, colour = as.factor(assigned.clusters))) +
  geom_point()


## WCSS: total within cluster sum of squares
iris.km$tot.withinss

iris.km$cluster


## run tests with multiple k values and plot WCSS
k.list <- c(2,3,4,5,6)

wcss.list <- c()

for (k in k.list) {
  
  iris.km <- kmeans(dataset[,1:4], centers = k)
  
  wcss <- iris.km$tot.withinss
    
  wcss.list <- c(wcss.list,wcss)
  
  ## get and plot clustering output 
  assigned.clusters <- as.factor(iris.km$cluster)
  
  ggplot(dataset, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
          geom_point()
  
}

plot(k.list,wcss.list,type = "b")


#### END ####