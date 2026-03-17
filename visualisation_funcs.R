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
gen_chromagram <- function(csv, normalization_method, title, artists) {
  read_csv(csv) |>
    compmus_wrangle_chroma() |> 
    mutate(pitches = map(pitches, compmus_normalise, normalization_method)) |>
    compmus_gather_chroma() |> 
    ggplot(aes(x = start + duration / 2, y = pitch_class, fill = value)) +
    geom_tile(aes(width = duration)) +
    labs(
      x = "Time (s)", y = NULL, fill = "Value",
      title = title, subtitle = artists
    ) +
    theme_minimal()
}

gen_ssm_chroma <- function(csv, normalization_method, title, artists) {
  read_csv(csv) |>
    compmus_wrangle_chroma() |> 
    filter(row_number() %% 50L == 0L) |> 
    mutate(pitches = map(pitches, compmus_normalise, normalization_method)) |>
    compmus_self_similarity(pitches, normalization_method) |> 
    ggplot(aes(x = xstart + xduration / 2, y = ystart + yduration / 2, fill = d)) +
    geom_tile(aes(width = 50 * xduration, height = 50 * yduration)) +
    labs(x = "Time (s)", y = "Time (s)", fill = "Value") +
    coord_fixed() + # This causes the "small" look; we'll fix it in layout
    theme_classic()
}

# This function generates a Self Similarity Matrix (SSM) based on a csv, a method, 
# the title of the song, arist(s) and give the output file for timbre features.
  gen_ssm_timbre <- function(csv, normalization_method, title, artists) {
    song <- read_csv(csv)
    plot <-song |>
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
    return(plot)
  }

# This function genrates a grid of histograms with all different features of the spotify API against there count for both west and east region from the csv west_east_cleaned.csv.
# These features are: "Artist.Name.s.","Release.Date","Duration..ms.","Popularity","Genres","Record.Label","Danceability","Energy","Key","Loudness","Mode","Speechiness","Acousticness","Instrumentalness","Liveness","Valence","Tempo","Time.Signature"

  
gen_density_grid <- function(csv, output_file) {
  # 1. Read data and get total song count for the title
  data <- read_csv(csv)
  total_songs <- nrow(data)
  
  numeric_features <- c(
    "Duration (ms)", "Popularity", "Danceability", "Energy", "Key", 
    "Loudness", "Mode", "Speechiness", "Acousticness", "Instrumentalness", 
    "Liveness", "Valence", "Tempo", "Time Signature"
  )
  
  # 2. Wrangle data
  data_long <- data |>
    select(region, all_of(numeric_features)) |>
    pivot_longer(cols = -region, names_to = "feature", values_to = "value") |>
    mutate(value = as.numeric(value)) |>
    drop_na(value, region)
  
  # 3. Create a single overlaid grid
  combined_plot <- ggplot(data_long, aes(x = value, fill = region)) +
    geom_density(alpha = 0.5) + # Alpha controls transparency so you can see the overlap
    facet_wrap(~ feature, scales = "free", ncol = 4) +
    scale_fill_manual(
      values = c("east" = "#1f78b4", "west" = "#ff7f00"),
      labels = c("east" = "East Coast", "west" = "West Coast") # Cleans up the legend
    ) +
    theme_minimal() +
    labs(
      title = paste("Distribution of Spotify Features | Total Songs Analyzed:", total_songs),
      x = "Feature Value", 
      y = "Density",
      fill = "Region" # Titles the legend
    ) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      legend.position = "top", # Puts the legend at the top so it doesn't squash the graphs
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold")
    )
  
  # 4. Save the plot (Height adjusted to 12 since it's only one grid now)
  ggsave(output_file, plot = combined_plot, width = 20, height = 12)
}

gen_freqpoly_grid <- function(csv, output_file) {
  # 1. Read data and get total song count for the title
  data <- read_csv(csv)
  total_songs <- nrow(data)
  
  numeric_features <- c(
    "Duration (ms)", "Popularity", "Danceability", "Energy", "Key", 
    "Loudness", "Mode", "Speechiness", "Acousticness", "Instrumentalness", 
    "Liveness", "Valence", "Tempo", "Time Signature"
  )
  
  # 2. Wrangle data
  data_long <- data |>
    select(region, all_of(numeric_features)) |>
    pivot_longer(cols = -region, names_to = "feature", values_to = "value") |>
    mutate(value = as.numeric(value)) |>
    drop_na(value, region)
  
  # 3. Create a single overlaid grid with frequency polygons
  combined_plot <- ggplot(data_long, aes(x = value, color = region)) +
    geom_freqpoly(bins = 30, linewidth = 1) + # bins=30 is the default, linewidth makes it pop
    facet_wrap(~ feature, scales = "free", ncol = 4) +
    scale_color_manual(
      values = c("east" = "#1f78b4", "west" = "#ff7f00"),
      labels = c("east" = "East Coast", "west" = "West Coast")
    ) +
    theme_minimal() +
    labs(
      title = paste("Distribution of Spotify Features | Total Songs Analyzed:", total_songs),
      x = "Feature Value", 
      y = "Count", # The y-axis now shows raw song counts!
      color = "Region" # Updated from 'fill' to 'color'
    ) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      legend.position = "top", 
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size = 12, face = "bold") # Makes the facet labels clearer
    )
  
  # 4. Save the plot
  ggsave(output_file, plot = combined_plot, width = 20, height = 12)
}

gen_overlaid_histogram_grid <- function(csv, output_file) {
  # 1. Read data and get total song count for the title
  data <- read_csv(csv)
  total_songs <- nrow(data)
  
  numeric_features <- c(
    "Duration (ms)", "Popularity", "Danceability", "Energy", "Key", 
    "Loudness", "Mode", "Speechiness", "Acousticness", "Instrumentalness", 
    "Liveness", "Valence", "Tempo", "Time Signature"
  )
  
  # 2. Wrangle data
  data_long <- data |>
    select(region, all_of(numeric_features)) |>
    pivot_longer(cols = -region, names_to = "feature", values_to = "value") |>
    mutate(value = as.numeric(value)) |>
    drop_na(value, region)
  
  # 3. Create a single overlaid grid with histograms
  combined_plot <- ggplot(data_long, aes(x = value, fill = region)) +
    geom_histogram(
      position = "identity", # Crucial: forces bars to overlap instead of stacking
      alpha = 0.5,           # 50% transparency to see the overlap
      bins = 30,             # Adjust this to make bars wider/narrower
      color = "white",       # Adds a clean, thin white border around each bar
      linewidth = 0.2
    ) +
    facet_wrap(~ feature, scales = "free", ncol = 4) +
    scale_fill_manual(
      values = c("east" = "#1f78b4", "west" = "#ff7f00"),
      labels = c("east" = "East Coast", "west" = "West Coast")
    ) +
    theme_minimal() +
    labs(
      title = paste("Distribution of Spotify Features | Total Songs Analyzed:", total_songs),
      x = "Feature Value", 
      y = "Count", 
      fill = "Region"        # Switched back from 'color' to 'fill'
    ) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      legend.position = "top", 
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size = 12, face = "bold") 
    )
  
  # 4. Save the plot
  ggsave(output_file, plot = combined_plot, width = 20, height = 12)
}

  circshift <- function(v, n) {
    if (n == 0) v else c(tail(v, n), head(v, -n))
  }
  
  #      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
  major_chord <-
    c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
  minor_chord <-
    c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
  seventh_chord <-
    c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)
  
  major_key <-
    c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  minor_key <-
    c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  
  chord_templates <-
    tribble(
      ~name, ~template,
      "Gb:7", circshift(seventh_chord, 6),
      "Gb:maj", circshift(major_chord, 6),
      "Bb:min", circshift(minor_chord, 10),
      "Db:maj", circshift(major_chord, 1),
      "F:min", circshift(minor_chord, 5),
      "Ab:7", circshift(seventh_chord, 8),
      "Ab:maj", circshift(major_chord, 8),
      "C:min", circshift(minor_chord, 0),
      "Eb:7", circshift(seventh_chord, 3),
      "Eb:maj", circshift(major_chord, 3),
      "G:min", circshift(minor_chord, 7),
      "Bb:7", circshift(seventh_chord, 10),
      "Bb:maj", circshift(major_chord, 10),
      "D:min", circshift(minor_chord, 2),
      "F:7", circshift(seventh_chord, 5),
      "F:maj", circshift(major_chord, 5),
      "A:min", circshift(minor_chord, 9),
      "C:7", circshift(seventh_chord, 0),
      "C:maj", circshift(major_chord, 0),
      "E:min", circshift(minor_chord, 4),
      "G:7", circshift(seventh_chord, 7),
      "G:maj", circshift(major_chord, 7),
      "B:min", circshift(minor_chord, 11),
      "D:7", circshift(seventh_chord, 2),
      "D:maj", circshift(major_chord, 2),
      "F#:min", circshift(minor_chord, 6),
      "A:7", circshift(seventh_chord, 9),
      "A:maj", circshift(major_chord, 9),
      "C#:min", circshift(minor_chord, 1),
      "E:7", circshift(seventh_chord, 4),
      "E:maj", circshift(major_chord, 4),
      "G#:min", circshift(minor_chord, 8),
      "B:7", circshift(seventh_chord, 11),
      "B:maj", circshift(major_chord, 11),
      "D#:min", circshift(minor_chord, 3)
    )
  
  key_templates <-
    tribble(
      ~name, ~template,
      "Gb:maj", circshift(major_key, 6),
      "Bb:min", circshift(minor_key, 10),
      "Db:maj", circshift(major_key, 1),
      "F:min", circshift(minor_key, 5),
      "Ab:maj", circshift(major_key, 8),
      "C:min", circshift(minor_key, 0),
      "Eb:maj", circshift(major_key, 3),
      "G:min", circshift(minor_key, 7),
      "Bb:maj", circshift(major_key, 10),
      "D:min", circshift(minor_key, 2),
      "F:maj", circshift(major_key, 5),
      "A:min", circshift(minor_key, 9),
      "C:maj", circshift(major_key, 0),
      "E:min", circshift(minor_key, 4),
      "G:maj", circshift(major_key, 7),
      "B:min", circshift(minor_key, 11),
      "D:maj", circshift(major_key, 2),
      "F#:min", circshift(minor_key, 6),
      "A:maj", circshift(major_key, 9),
      "C#:min", circshift(minor_key, 1),
      "E:maj", circshift(major_key, 4),
      "G#:min", circshift(minor_key, 8),
      "B:maj", circshift(major_key, 11),
      "D#:min", circshift(minor_key, 3)
    )
  
gen_keygram <- function(csv, output_file, title) {
  csv <- read_csv(csv)
  csv |> 
    compmus_wrangle_chroma() |> 
    filter(row_number() %% 50L == 0L) |> 
    compmus_match_pitch_template(
      key_templates,         # Change to chord_templates if desired
      method = "euclidean",  # Try different distance metrics
      norm = "manhattan"     # Try different norms
    ) |>
    ggplot(
      aes(x = start + duration / 2, width = 50 * duration, y = name, fill = d)
    ) +
    geom_tile() +
    scale_fill_viridis_c(guide = "none") +
    theme_minimal() +
    labs(x = "Time (s)", y = "",
         title = title)
  ggsave(output_file)
}
#from teacher
gen_chordagram <- function(csv, output_file, title) {
  csv <- read_csv(csv)
  csv |> 
    compmus_wrangle_chroma() |> 
    filter(row_number() %% 50L == 0L) |> 
    compmus_match_pitch_template(
      chord_templates,         # Change to chord_templates if desired
      method = "euclidean",  # Try different distance metrics
      norm = "manhattan"     # Try different norms
    ) |>
    ggplot(
      aes(x = start + duration / 2, width = 50 * duration, y = name, fill = d)
    ) +
    geom_tile() +
    scale_fill_viridis_c(guide = "none") +
    theme_minimal() +
    labs(x = "Time (s)", y = "",
         title = title)
  ggsave(output_file)
}

#for facet
library(patchwork)

get_chordagram_plot <- function(csv, title) {
  df <- read_csv(csv, show_col_types = FALSE)
  
  plot <- df |> 
    compmus_wrangle_chroma() |> 
    filter(row_number() %% 50L == 0L) |> 
    compmus_match_pitch_template(
      chord_templates,       
      method = "euclidean",  
      norm = "manhattan"     
    ) |>
    ggplot(
      aes(x = start + duration / 2, width = 50 * duration, y = name, fill = d)
    ) +
    geom_tile() +
    scale_fill_viridis_c(guide = "none") +
    theme_minimal() +
    labs(x = "Time (s)", y = "", title = title)
    
  return(plot)
}

gen_patch_chordagram <- function() { 
  # 1. Generate individual plots
  east1 <- get_chordagram_plot("chromagram_cream.csv", "East: C.R.E.A.M. by Wu-Tang Clan")
  east2 <- get_chordagram_plot("chromagram_gmtl.csv", "East: Give me the Loot by The Notorius B.I.G.")
  
  west1 <- get_chordagram_plot("chromagram_bp.csv", "West: Bitch Please by Snoop Dogg and Xzibit")
  west2 <- get_chordagram_plot("chromagram_wm.csv", "West: Without Me by Eminem")
  
  # 2. Arrange them in a 2x2 grid (East on top, West on bottom)
  grid_plot <- (east1 | east2) / 
    (west1 | west2)
  
  # 3. Add a main title to the whole graphic
  final_plot <- grid_plot + 
    plot_annotation(title = "East Coast vs. West Coast Rap: Chordagram Comparison")
  
  # 4. Save the final grid
  ggsave("coast_comparison_chordagrams.png", final_plot, width = 12, height = 8)
}


gen_tempogram <- function(method, csv, title, subtitle) {
  data <- read_csv(csv)
  
  if (method == "atc"){
    # Assign the plot to a variable 'p'
    p <- data |> 
      pivot_longer(-TIME, names_to = "tempo") |> 
      mutate(tempo = as.numeric(tempo)) |> 
      ggplot(aes(x = TIME, y = tempo, fill = value)) +
      geom_raster() +
      scale_y_continuous(transform = c("reciprocal", "reverse"), breaks = seq(50, 350, 100)) +    
      scale_fill_viridis_c(guide = "none") +
      labs(x = "Time (s)", y = "Tempo (BPM)", title = title, subtitle = subtitle) +
      theme_classic()
      
    return(p) # Return the variable
    
  } else if (method == "dft"){
    # Assign the plot to a variable 'p'
    p <- data |> 
      pivot_longer(-TIME, names_to = "tempo") |> 
      mutate(tempo = as.numeric(tempo)) |> 
      ggplot(aes(x = TIME, y = tempo, fill = value)) +
      geom_raster() +
      scale_fill_viridis_c(guide = "none") +
      labs(x = "Time (s)", y = "Tempo (BPM)", title = title, subtitle = subtitle) +
      theme_classic()
      
    return(p) # Return the variable
  }
}

gen_tempogram_grid <- function(song, title) {
  atc_plot <- gen_tempogram("atc", paste0("../data_sv/tempogram_atc_", song, ".csv"), paste0("ATC Tempogram"), "")
  dft_plot <- gen_tempogram("dft", paste0("../data_sv/tempogram_dft_", song, ".csv"), paste0("DFT Tempogram"), "")
  
  grid_plot <- (atc_plot | dft_plot)
  
  # 3. Add a main title to the whole graphic
  final_plot <- grid_plot + 
    plot_annotation(title = title)
  
  return(final_plot)
}