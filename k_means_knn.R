library(tidyverse)
library(tidymodels)
library(ggplot2)
library(rpart.plot)
library(cluster)  # For silhouette

# Function to preprocess data
preprocess_hiphop_data <- function(data) {
  # Drop identifiers
  data <- data %>%
    select(-c(`Track URI`, `Track Name`, `Album Name`, Artist_Name, `Release Date`, `Added By`, `Added At`, Genres, `Record Label`, `Duration (ms)`, Popularity, Explicit))

  # Feature engineering
  data <- data %>%
    mutate(
      energy_danceability = Energy * Danceability,
      valence_energy = Valence * Energy,
      mood_score = (Valence + Energy) / 2
    )

  # Encode target
  data <- data %>%
    mutate(region = factor(region, levels = c("east", "west"), labels = c("East Coast", "West Coast")))

  return(data)
}

# K-Means Clustering Function
apply_kmeans <- function(data, k = 2) {
  # Preprocess without target for unsupervised
  features <- data %>%
    select(-region) %>%
    mutate(across(where(is.character), as.factor))  # Handle any remaining chars

  # One-hot encode categoricals
  recipe <- recipe(~ ., data = features) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())

  prepped <- prep(recipe, data = features)
  features_scaled <- bake(prepped, new_data = features)

  # Apply K-means
  set.seed(42)
  kmeans_result <- kmeans(features_scaled, centers = k, nstart = 25)

  # Add cluster to original data
  data$cluster <- factor(kmeans_result$cluster)

  # Evaluate
  sil <- silhouette(kmeans_result$cluster, dist(features_scaled))
  avg_sil <- mean(sil[, 3])

  return(list(data = data, model = kmeans_result, silhouette = avg_sil))
}


# KNN Classification Function
train_knn <- function(data) {
  # Preprocess

  # Recipe
  recipe <- recipe(region ~ ., data = data) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())

  # Model
  knn_model <- nearest_neighbor(neighbors = tune()) %>%
    set_mode("classification") %>%
    set_engine("kknn")

  # Workflow
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(knn_model)

  # CV
  set.seed(42)
  folds <- vfold_cv(data, v = 5, strata = region)

  # Tune
  tune_results <- tune_grid(
    wf,
    resamples = folds,
    grid = grid_regular(neighbors(range = c(1, 20)), levels = 10),
    metrics = metric_set(accuracy, f_meas)
  )

  # Best
  best_k <- select_best(tune_results, metric = "f_meas")

  # Final model
  final_wf <- finalize_workflow(wf, best_k)
  final_fit <- fit(final_wf, data = data)

  return(list(fit = final_fit, tune_results = tune_results, best_k = best_k))
}

# Decision Tree Function
train_decision_tree <- function(data) {
  # Preprocess

  # Recipe
  recipe <- recipe(region ~ ., data = data) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors())

  # Model
  tree_model <- decision_tree(cost_complexity = tune(), tree_depth = tune()) %>%
    set_mode("classification") %>%
    set_engine("rpart")

  # Workflow
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(tree_model)

  # CV
  set.seed(42)
  folds <- vfold_cv(data, v = 5, strata = region)

  # Tune
  tune_results <- tune_grid(
    wf,
    resamples = folds,
    grid = grid_regular(cost_complexity(range = c(-10, -1)), tree_depth(range = c(1, 10)), levels = 5),
    metrics = metric_set(accuracy, f_meas)
  )

  # Best
  best_params <- select_best(tune_results, metric = "f_meas")

  # Final model
  final_wf <- finalize_workflow(wf, best_params)
  final_fit <- fit(final_wf, data = data)

  return(list(fit = final_fit, tune_results = tune_results, best_params = best_params))
}

# Decision Tree Visualization Function
visualize_decision_tree <- function(tree_fit, title = "How the Model Predicts: East vs West Coast") {
  # Extract the rpart model
  rpart_model <- extract_fit_engine(tree_fit)

  rpart.plot(
    rpart_model,
    type = 5,                # Only draws labels for internal nodes; much cleaner
    extra = 104,             # Shows % of data and the probability (easy to explain)
    box.palette = "RdBu",    # Red for East, Blue for West (classic contrast)
    shadow.col = 0,          # Removes shadows for a flat, modern look
    nn = FALSE,              # Removes node numbers (confuses non-tech users)
    branch.lty = 1,          # Solid lines are easier to follow than dotted
    branch.lwd = 2,          # Thicker lines for better visibility
    under = TRUE,
    fallen.leaves = TRUE,
    main = title,
    tweak = 1.2,             # Slightly increases text size
    roundint = FALSE         # Prevents weird rounding labels if data is numeric
  )
}

run_kmeans_knn <- function() {
  # Load data
  data <- read_csv("balanced_hiphop_corpus.csv")

  # Preprocess
  data <- preprocess_hiphop_data(data)

  # KNN
  knn_result <- train_knn(data)
  print(knn_result$best_k)

  # K-Means
  kmeans_result <- apply_kmeans(data, k = 2)
  print(paste("Average Silhouette Width:", round(kmeans_result$silhouette, 3)))
  visualize_kmeans(kmeans_result)

  

  # Decision Tree
  tree_result <- train_decision_tree(data)
  print(tree_result$best_params)
  visualize_decision_tree(tree_result$fit)
}

visualize_knn <- function(knn_result, original_data, title = "AI Predictions: Model Guesses vs. Reality") {
  
  # 1. Get predictions from the fitted model
  # We use the final fitted workflow stored in knn_result$fit
  predictions <- predict(knn_result$fit, new_data = original_data)
  
  # 2. Combine with original data
  plot_data <- original_data
  plot_data$Predicted <- predictions$.pred_class
  plot_data$Actual <- original_data$region
  
  # 3. Perform PCA to squash all numeric features into 2D (for plotting)
  features <- plot_data %>% 
    select(-where(is.factor), -where(is.character), -Predicted, -Actual)
  
  pca <- prcomp(features, scale. = TRUE)
  pca_data <- as.data.frame(pca$x[, 1:2])
  
  # Add labels back to the PCA data
  pca_data$Predicted <- plot_data$Predicted
  pca_data$Actual <- plot_data$Actual
  
  # 4. Create the plot
  ggplot(pca_data, aes(x = PC1, y = PC2)) +
    # Add light "bubbles" to show the territory the AI associates with East vs West
    stat_ellipse(aes(fill = Predicted), geom = "polygon", alpha = 0.15, color = NA) +
    
    # Points: Shape represents ACTUAL region, Color represents PREDICTED region
    geom_point(aes(color = Predicted, shape = Actual), size = 3, alpha = 0.8) +
    
    # Using Red/Blue to match the Decision Tree's "RdBu" aesthetic
    scale_color_manual(values = c("East Coast" = "#B2182B", "West Coast" = "#2166AC")) +
    scale_fill_manual(values = c("East Coast" = "#B2182B", "West Coast" = "#2166AC")) +
    
    theme_minimal(base_size = 12) +
    labs(
      title = title,
      subtitle = "Shapes = Actual Region | Colors = Region KNN classified",
      x = "Primary Characteristic (PC1)", 
      y = "Secondary Characteristic (PC2)",
      color = "KNN classification",
      fill = "KNN classification",
      shape = "Actual Region"
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

visualize_kmeans <- function(kmeans_result, title = "Spotting Patterns: Computer Clusters vs. Real Regions") {
  data <- kmeans_result$data
  
  # PCA for 2D (Standard logic)
  features <- data %>% select(-where(is.factor), -where(is.character))
  pca <- prcomp(features, scale. = TRUE)
  pca_data <- as.data.frame(pca$x[, 1:2])
  pca_data$region <- as.factor(data$region)
  pca_data$cluster <- as.factor(data$cluster)

  ggplot(pca_data, aes(x = PC1, y = PC2)) +
    # Add light "bubbles" around clusters to show the computer's groupings
    stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.15, color = NA) +
    # Points: Shape represents the ACTUAL region, Color represents the CLUSTER
    geom_point(aes(color = cluster, shape = region), size = 3, alpha = 0.8) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 12) +
    labs(
      title = title,
      subtitle = "Shapes = Actual Regions | Colors = Groups K-Means found",
      x = "Primary Characteristic (PC1)", 
      y = "Secondary Characteristic (PC2)",
      color = "AI Grouping",
      fill = "AI Grouping",
      shape = "Actual Label"
      
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}
