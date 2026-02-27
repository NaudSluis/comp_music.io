library(tidyverse)
library(compmus)

bp <- read_csv("~/Documents/GitHub/comp_music/cepstrogram_bitch_please_w.csv")

bp |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude",
       title = "Cepstrogram of Bitch Please by Snoop Dogg & Xzibit",
       subtitle = "Normalization with Chebyshev method") +
  scale_fill_viridis_c() +                              
  theme_classic()