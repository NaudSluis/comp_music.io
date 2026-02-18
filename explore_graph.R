library(tidyverse)
library(compmus)

west_east <- read.csv("~/Documents/GitHub/comp_music/west_east.csv", stringsAsFactors = FALSE)

ggplot(west_east, aes(x = Danceability, y = Valence)) +
  geom_point(aes(color = highlight, alpha = Energy, size = Popularity)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_manual(values = c("no" = "grey70", "yes" = "red")) +
  facet_wrap(~ region) +
  labs(
    title = "Valence vs Danceability in East vs West Coast Rap",
    subtitle = "Red points = highlighted tracks | Size = Popularity | Transparency = Energy",
    x = "Danceability",
    y = "Valence"
  ) +
  theme_minimal() +
  theme(legend.position = "none")