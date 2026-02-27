library(tidyverse)
library(compmus)

bp <- read_csv("~/Documents/GitHub/comp_music/cepstrogram_bitch_please_w_v2.csv")

bp |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "chebyshev")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude",
    title = "Cepstrogram of Bitch Please by Snoop Dogg & Xzibit",
    subtitle = "Normalization with Chebyshev method"
  ) +
  scale_fill_viridis_c() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  theme_classic()