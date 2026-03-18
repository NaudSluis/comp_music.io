# Install packages if you don't have them
# install.packages("xgboost")
# install.packages("DiagrammeR")

library(xgboost)
library(DiagrammeR)

# 1. Load the native XGBoost JSON model
xgb_model <- xgb.load("../xgb_hiphop_model.json")

# 2. Define the feature names (from the Python training)
feature_list <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'energy_danceability', 'valence_energy', 'mood_score', 'key_1', 'key_2', 'key_3', 'key_4', 'key_5', 'key_6', 'key_7', 'key_8', 'key_9', 'key_10', 'key_11', 'mode_1', 'time signature_5')

# 3. Safely inject the feature names into the C++ booster object
setinfo(xgb_model, "feature_name", feature_list)

# 4. Visualize the first tree (index 0)
# Note: XGBoost is an ensemble of many trees. You usually visualize them one at a time.
xgb.plot.tree(
  model = xgb_model,
  tree_idx = 1,          # First tree
  plot_width = 1200,     
  plot_height = 800
)