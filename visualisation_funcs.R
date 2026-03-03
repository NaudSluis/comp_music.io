library(tidyverse)
library(compmus)

# This function generates a cepstrogram, based on a csv, a method, the title of the song, arist(s) and give the output file
  gen_cepstrogram <- function(csv, normalization_method, output_file, title, artists) {
    song <- read_csv(csv)
    song |>
      compmus_wrangle_timbre() |> 
      mutate(timbre = map(timbre, compmus_normalise, normalization_method)) |>
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
        title = paste0("Cepstrogram of ", title, " by ", artists),
        subtitle = paste0("Normalization with ", normalization_method, " method")
      ) +
      scale_fill_viridis_c() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      theme_classic()
    ggsave(output_file)
  }

# This function generates a chromagram, based on a csv, a method, the title of the song, arist(s) and give the output file
  gen_chromagram <- function(csv, normalization_method, output_file, title, artists) {
    song <- read_csv(csv)
    song |>
      compmus_wrangle_chroma() |> 
      mutate(pitches = map(pitches, compmus_normalise, normalization_method)) |>
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
        title = paste0("Chromagram of ", title, " by ", artists),
        subtitle = paste0("Normalization with ", normalization_method, " method")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      scale_fill_viridis_c()
    ggsave(output_file)
  }

# This function generates a Self Similarity Matrix (SSM) based on a csv, a method, 
# the title of the song, arist(s) and give the output file for timbre features.
  gen_ssm <- function(csv, normalization_method, output_file, title, artists) {
    song <- read_csv(csv)
    song |>
      compmus_wrangle_timbre() |> 
      filter(row_number() %% 50L == 0L) |> 
      mutate(timbre = map(timbre, compmus_normalise, normalization_method)) |>
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
           title = paste0("SSM of ", title, " by ", artists),
           subtitle = paste0("Normalization with ", normalization_method, " method"))
    ggsave(output_file)
  }