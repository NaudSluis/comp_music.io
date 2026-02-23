library(dplyr)

# Load CSV files
west_east_cleaned <- read.csv("C:/Users/Naud/Documents/GitHub/comp_music/west_east_cleaned.csv", stringsAsFactors = FALSE)


# Add identifier column before combining
west <- west_east_cleaned |> filter(region == "west")
east <- west_east_cleaned |> filter(region == "east")

# Randomly remove 55 rows from East
set.seed(42)  # for reproducibility
east <- east %>% slice_sample(n = nrow(east) - 38)

# Combine the datasets
west_east <- rbind(west, east)

# Save combined CSV
write.csv(west_east, "C:/Users/Naud/Documents/GitHub/comp_music/west_east_cleaned.csv", row.names = FALSE)

cat("Files successfully combined (55 East rows removed) and saved as west_east.csv\n")