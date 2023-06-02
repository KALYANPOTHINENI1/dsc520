# Assignment: ASSIGNMENT 11_02
# Name: Pothineni, Kalyan
# Date: 2023-05-27


library(ggplot2)
library(cluster)
library(pathviewr)

# Load the dataset
dataset <- read.csv("data/clustering-data.csv")

# Plot the dataset
ggplot(dataset, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "Scatter Plot of Dataset")

# Fit the dataset using k-means
k_values <- 2:12
distances <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  kmeans_model <- kmeans(dataset, centers = k)
  cluster_assignments <- kmeans_model$cluster
  cluster_centers <- kmeans_model$centers
  
  # Calculate average distance from cluster centers
  total_distance <- 0
  for (j in 1:nrow(dataset)) {
    data_point <- dataset[j, ]
    cluster_center <- cluster_centers[cluster_assignments[j], ]
    distance <- sqrt(sum((data_point - cluster_center)^2))
    total_distance <- total_distance + distance
  }
  avg_distance <- total_distance / nrow(dataset)
  
  distances[i] <- avg_distance
  
  ggplot(dataset, aes(x = x, y = y, color = factor(cluster_assignments))) +
    geom_point() +
    geom_point(data = as.data.frame(kmeans_model$centers), aes(x = x, y = y), shape = 4, size = 4) +
    labs(title = paste0("k-Means Clustering (k = ", k, ")")) +
    theme(legend.position = "none")
}

# Create line chart
distance_df <- data.frame(k = k_values, distance = distances)

ggplot(distance_df, aes(x = factor(k), y = distance )) +
  geom_line() +
  geom_point() +
  labs(title = "Average Distance from Cluster Centers",
       x = "k (Number of Clusters)",
       y = "Average Distance",
  scale_x_continuous(breaks = as.character(k)))

print(distance_df)

find_curve_elbow(distance_df, export_type = "row_num", plot_curve = FALSE)