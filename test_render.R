library(tidyverse)
library(tidymodels)
source("k_means_knn.R")

data_raw <- read_csv("balanced_hiphop_corpus.csv")
data <- preprocess_hiphop_data(data_raw)

cat("Testing apply_kmeans...\n")
result <- apply_kmeans(data, k = 2)
cat("✓ apply_kmeans works\n")
