library(tidyverse)

# 1. Load the CSVs
east <- read_csv("East_Coast_Classics.csv")
west <- read_csv("West_Side_Classics.csv")

# 2. Add the initial 'region' column
east <- east %>% mutate(region = "east")
west <- west %>% mutate(region = "west")

# 3. Combine into one dataset
corpus <- bind_rows(east, west)

# 4. Correct mislabeled artists using your exact column name
east_artists <- c(
  "50 Cent",
  "The Notorious B.I.G.",
  "DMX",
  "Wu-Tang Clan",
  "Big Pun;Fat Joe"
)

# Reassign the region to 'east' if the artist is in the list
corpus <- corpus %>%
  mutate(region = ifelse(Artist_Name %in% east_artists, "east", region))

# 5. Print the amount of West and East coast songs before downsizing
cat("\n--- Song Counts Before Balancing ---\n")
print(table(corpus$region))

# 6. Randomly downsize the East Coast songs to match West Coast
set.seed(123) 

# Get the exact count of west coast songs
west_count <- sum(corpus$region == "west")

# Filter and sample the east coast songs
east_balanced <- corpus %>%
  filter(region == "east") %>%
  sample_n(west_count)

# Keep all the west coast songs
west_all <- corpus %>%
  filter(region == "west")

# Re-combine the balanced data
final_corpus <- bind_rows(east_balanced, west_all)

# 7. Print the final balanced counts
cat("\n--- Song Counts After Balancing ---\n")
print(table(final_corpus$region))

# Save your clean, balanced corpus to a new CSV
write_csv(final_corpus, "balanced_hiphop_corpus.csv")