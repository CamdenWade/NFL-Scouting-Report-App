# R/player_data_prep.R

library(tidyverse)

build_player_summaries <- function(
    scouting_path = "data/processed/scouting_data.rds",
    save_path = "data/processed/player_summaries.rds"
) {
  data <- readRDS(scouting_path)
  
  required_cols <- c(
    "passer_player_name", "passer_player_id",
    "rusher_player_name", "rusher_player_id",
    "receiver_player_name", "receiver_player_id"
  )
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required player columns in scouting data:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  passers <- data %>%
    filter(play_type == "pass", !is.na(passer_player_name), !is.na(passer_player_id)) %>%
    group_by(season, posteam, passer_player_id, passer_player_name) %>%
    summarise(
      plays = n(),
      epa_per_play = mean(epa, na.rm = TRUE),
      explosive_rate = mean(explosive_play, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      team = posteam,
      player_id = passer_player_id,
      player = passer_player_name
    )
  
  rushers <- data %>%
    filter(play_type == "run", !is.na(rusher_player_name), !is.na(rusher_player_id)) %>%
    group_by(season, posteam, rusher_player_id, rusher_player_name) %>%
    summarise(
      carries = n(),
      yards_per_carry = mean(yards_gained, na.rm = TRUE),
      epa_per_play = mean(epa, na.rm = TRUE),
      explosive_rate = mean(explosive_play, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      team = posteam,
      player_id = rusher_player_id,
      player = rusher_player_name
    )
  
  receivers <- data %>%
    filter(play_type == "pass", !is.na(receiver_player_name), !is.na(receiver_player_id)) %>%
    group_by(season, posteam, receiver_player_id, receiver_player_name) %>%
    summarise(
      targets = n(),
      epa_per_target = mean(epa, na.rm = TRUE),
      explosive_rate = mean(explosive_play, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      team = posteam,
      player_id = receiver_player_id,
      player = receiver_player_name
    )
  
  out <- list(
    passers = passers,
    rushers = rushers,
    receivers = receivers
  )
  
  saveRDS(out, save_path)
  
  out
}