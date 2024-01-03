install.packages("tidyverse")
install.packages("factoextra")
install.packages("dplyr")
install.packages("purrr")
install.packages("ggplot2")
install.packages("factoextra")
library(tidyverse)
library(factoextra)
library(dplyr)
library(purrr)
library(ggplot2)
library(factoextra)


# Load the dataset
dataset <- read.csv("Heart_disease_statlog.csv")

# Remove the target column for clustering
data_for_clustering <- dataset %>% select(-target)

# Scale the data
data_for_clustering <- scale(data_for_clustering)

set.seed(123)
wss <- map_dbl(1:10, ~kmeans(data_for_clustering, .x)$tot.withinss)
elbow_plot <- tibble(k = 1:10, wss = wss)

ggplot(elbow_plot, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method for Choosing k")

# clusters are 2 based on the Elbow method
set.seed(123)
kmeans_result <- kmeans(data_for_clustering, centers = 2)

# Add cluster results to the original data
dataset$cluster <- kmeans_result$cluster

cluster_summary <- aggregate(. ~ cluster, dataset, mean)
print(cluster_summary)

# Visualize clusters
fviz_cluster(kmeans_result, data = data_for_clustering)
