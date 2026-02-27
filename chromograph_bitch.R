library(tidyverse)
library(compmus)

bitchp <- read_csv("~/Documents/GitHub/comp_music/bitch_please_w_chromagram_v3.csv")

bitchp |>
  compmus_wrangle_chroma() |> 
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile(aes(width = duration)) +
  labs(
    x = "Time (s)",
    y = NULL,
    fill = "Magnitude",
    title = "Chromagram of Bitch Please by Snoop Dogg & Xzibit",
    subtitle = "Normalization with Chebyshev method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_fill_viridis_c()