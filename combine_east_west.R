library(dplyr)

# Load CSV files
west <- read.csv("Downloads/west.csv", stringsAsFactors = FALSE)
east <- read.csv("Downloads/east.csv", stringsAsFactors = FALSE)

# Add identifier column before combining
west$region <- "west"
east$region <- "east"

# Randomly remove 55 rows from East
set.seed(42)  # for reproducibility
east <- east %>% slice_sample(n = nrow(east) - 55)

# Combine the datasets
west_east <- rbind(west, east)

# Example: highlight "Track A" and "Track B"
highlight_tracks <- c('California Love - Original Version', 'Still D.R.E.')

west_east$highlight <- ifelse(west_east$Track.Name %in% highlight_tracks, "yes", "no")


# Save combined CSV
write.csv(west_east, "~/Documents/GitHub/comp_music/west_east.csv", row.names = FALSE)

cat("Files successfully combined (55 East rows removed) and saved as west_east.csv\n")
