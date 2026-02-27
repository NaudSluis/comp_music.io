library(tidyverse)
library(compmus)

bp <- read_csv("~/Documents/GitHub/comp_music/cepstrogram_bitch_please_w_v2.csv")

bp |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "chebyshev")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  labs() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "",
       title = "SSM of Bitch Please by Snoop Dogg & Xzibit based on timbre",
       subtitle = "Normalization with Chebyshev method")