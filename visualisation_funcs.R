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

# This function genrates a grid of histograms with all different features of the spotify API against there count for both west and east region from the csv west_east_cleaned.csv.
# These features are: "Artist.Name.s.","Release.Date","Duration..ms.","Popularity","Genres","Record.Label","Danceability","Energy","Key","Loudness","Mode","Speechiness","Acousticness","Instrumentalness","Liveness","Valence","Tempo","Time.Signature"
  library(patchwork) # This package is magic for combining multiple plots
  
  gen_histogram_grid <- function(csv, output_file) {
    # 1. Read data and get total song count for the title
    data <- read_csv(csv)
    total_songs <- nrow(data)
    
    numeric_features <- c(
      "Duration..ms.", "Popularity", "Danceability", "Energy", "Key", 
      "Loudness", "Mode", "Speechiness", "Acousticness", "Instrumentalness", 
      "Liveness", "Valence", "Tempo", "Time.Signature"
    )
    
    # 2. Wrangle data
    data_long <- data |>
      select(region, all_of(numeric_features)) |>
      pivot_longer(cols = -region, names_to = "feature", values_to = "value") |>
      mutate(value = as.numeric(value)) |>
      drop_na(value, region)
    
    # 3. Helper function to create a grid for a specific region
    create_region_grid <- function(region_name, bar_color) {
      ggplot(data_long |> filter(region == region_name), aes(x = value)) +
        geom_histogram(bins = 30, fill = bar_color, color = "black") +
        facet_wrap(~ feature, scales = "free", ncol = 4) +
        theme_minimal() +
        labs(
          subtitle = paste(str_to_title(region_name), "Coast Region"),
          x = "Feature Value", 
          y = "Count"
        )
    }
    
    # 4. Generate the two separate grids
    grid_east <- create_region_grid("east", "#1f78b4")
    grid_west <- create_region_grid("west", "#ff7f00")
    
    # 5. Combine grids using patchwork (the '/' stacks them vertically)
    # We also add the main title with the song count here
    combined_plot <- (grid_east / grid_west) +
      plot_annotation(
        title = paste("Distribution of Spotify Features | Total Songs Analyzed:", total_songs),
        theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
      )
    
    # 6. Save the plot (Height increased to accommodate two full grids)
    ggsave(output_file, plot = combined_plot, width = 20, height = 24)
  }
   gen_histogram_grid("Documents/GitHub/comp_music/west_east_cleaned.csv", "Documents/GitHub/comp_music/visualisations/spotify_region_comparison.pdf")
