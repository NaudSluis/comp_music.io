library(tidyverse)

west_east <- read.csv(
  "C:/Users/Naud/Documents/GitHub/comp_music/west_east.csv",
  stringsAsFactors = FALSE
)

# Artists that should be east
east_artists <- c(
  "50 Cent",
  "The Notorious B.I.G.",
  "DMX",
  "Wu-Tang Clan",
  "Big Pun;Fat Joe"
)

# Reassign regions and remove OutKast
west_east_clean <- west_east |>
  mutate(
    region = case_when(
      Artist.Name.s. %in% east_artists ~ "east",
      TRUE ~ region
    )
  ) |>
  filter(Artist.Name.s. != "Outkast")

# Recreate east and west dfs
east <- west_east_clean |> filter(region == "east")
west <- west_east_clean |> filter(region == "west")

# Count songs
east_count <- nrow(east)
west_count <- nrow(west)

print(east_count)
print(west_count)

# Combine again
combined <- bind_rows(east, west)

# Export CSV
write.csv(combined,
          "C:/Users/Naud/Documents/GitHub/comp_music/west_east_cleaned.csv",
          row.names = FALSE)