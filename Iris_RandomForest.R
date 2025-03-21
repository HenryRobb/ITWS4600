library("randomForest")
library("caret")

library("dplyr")
library("igraph")
library("ggraph")
library("DescTools")

## read dataset
iris.data <- iris

names(iris)

## scatter plot of 2 variables
ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

## train random forest model
model_rf1 <- randomForest(Species~., data=iris.data, proximity=TRUE)

## view model output
model_rf1

## predict class labels
predictions <- predict(model_rf1, iris.data)

## confusion matrix/contingency table
table(predictions,iris.data$Species, dnn=list('predicted','actual'))

## train random forest model
model_rf2 <- train(Species~., data=iris.data, method='rf', metric='Accuracy')

## view model output
model_rf2

## predict class labels
predictions <- predict(model_rf2, iris.data)

## confusion matrix/contingency table
table(predictions,iris.data$Species, dnn=list('predicted','actual'))

## find shortest tree
model_rf2$finalModel$forest$ndbigtree[order(model_rf2$finalModel$forest$ndbigtree)]

min_tree_num <- which(model_rf2$finalModel$forest$ndbigtree == min(model_rf2$finalModel$forest$ndbigtree))

tree_func(final_model = model_rf2$finalModel, min_tree_num[153])

## find longest tree
max_tree_num <- which(model_rf2$finalModel$forest$ndbigtree == max(model_rf2$finalModel$forest$ndbigtree))

tree_func(final_model = model_rf2$finalModel, max_tree_num)
tree_func(final_model = model_rf2$finalModel, max_tree_num[1])


Gini(iris.data$Petal.Length)
Gini(iris.data$Petal.Width)
Gini(iris.data$Sepal.Length)
Gini(iris.data$Sepal.Width)


tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}
