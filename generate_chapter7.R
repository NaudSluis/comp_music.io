library(tidyverse)
library(tidymodels)
library(ggplot2)
library(rpart.plot)
library(cluster)
library(jsonlite)
library(reticulate)

source("k_means_knn.R")

# Load and preprocess data
data <- read_csv("balanced_hiphop_corpus.csv")
data <- preprocess_hiphop_data(data)

# K-Means
kmeans_result <- apply_kmeans(data, k = 2)

# KNN
knn_result <- train_knn(data)

# Decision Tree
tree_result <- train_decision_tree(data)

# Load XGBoost results
feature_importance <- read_csv("feature_importance.csv", show_col_types = FALSE)
misclassified_songs <- read_csv("misclassified_songs.csv", show_col_types = FALSE)

# Collect results
results <- list(
  kmeans = list(
    silhouette = kmeans_result$silhouette,
    wcss = kmeans_result$model$tot.withinss
  ),
  knn = list(
    best_k = as.list(knn_result$best_k),
    metrics = as.data.frame(collect_metrics(knn_result$tune_results))
  ),
  decision_tree = list(
    best_params = as.list(tree_result$best_params),
    metrics = as.data.frame(collect_metrics(tree_result$tune_results))
  ),
  xgboost = list(
    feature_importance = as.data.frame(feature_importance),
    misclassified_songs = as.data.frame(misclassified_songs)
  )
)

# Save to JSON
write_json(results, "portfolio/chapter7_results.json", pretty = TRUE)