library(tidyverse)
library(nflreadr)

build_scouting_data <- function(
    seasons = 2023:2025,
    save_path = "data/processed/scouting_data.rds",
    fourth_down_save_path = "data/processed/fourth_down_data.rds"
) {
  
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  
  pbp_raw <- load_pbp(seasons)
  
  # -----------------------------
  # Main scouting dataset
  # -----------------------------
  scouting_data <- pbp_raw %>%
    filter(
      play_type %in% c("run", "pass"),
      !is.na(posteam),
      !is.na(defteam),
      !is.na(down),
      !is.na(ydstogo),
      !is.na(yardline_100),
      !is.na(qtr),
      !is.na(week),
      !is.na(game_id),
      !is.na(play_id),
      !is.na(posteam_score),
      !is.na(defteam_score),
      !is.na(half_seconds_remaining)
    ) %>%
    mutate(
      posteam = nflreadr::clean_team_abbrs(posteam),
      defteam = nflreadr::clean_team_abbrs(defteam),
      
      is_pass = if_else(play_type == "pass", 1, 0),
      is_run = if_else(play_type == "run", 1, 0),
      
      score_diff = posteam_score - defteam_score,
      
      neutral_situation = qtr %in% c(1, 2, 3) &
        abs(score_diff) <= 8 &
        down %in% c(1, 2),
      
      early_down = down %in% c(1, 2),
      third_down = down == 3,
      red_zone = yardline_100 <= 20,
      goal_to_go = yardline_100 <= 10,
      short_yardage = ydstogo <= 2,
      long_yardage = ydstogo >= 8,
      two_minute = half_seconds_remaining <= 120,
      
      distance_group = case_when(
        ydstogo <= 2 ~ "Short",
        ydstogo <= 6 ~ "Medium",
        TRUE ~ "Long"
      ),
      
      field_zone = case_when(
        yardline_100 <= 10 ~ "Goal-to-go",
        yardline_100 <= 20 ~ "Red zone",
        yardline_100 <= 50 ~ "Plus territory",
        TRUE ~ "Own territory"
      ),
      
      explosive_play = case_when(
        play_type == "run" & yards_gained >= 10 ~ TRUE,
        play_type == "pass" & yards_gained >= 20 ~ TRUE,
        TRUE ~ FALSE
      ),
      
      success = case_when(
        down == 1 & yards_gained >= 0.40 * ydstogo ~ TRUE,
        down == 2 & yards_gained >= 0.60 * ydstogo ~ TRUE,
        down %in% c(3, 4) & yards_gained >= ydstogo ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # Fit on all qualifying plays so all rows can be scored
  model_data <- scouting_data %>%
    mutate(
      down = factor(down, levels = c(1, 2, 3, 4)),
      qtr = factor(qtr, levels = c(1, 2, 3, 4, 5))
    )
  
  pass_model <- glm(
    is_pass ~ down + ydstogo + yardline_100 + score_diff + qtr + half_seconds_remaining,
    data = model_data,
    family = binomial()
  )
  
  # Convert first, then predict
  scouting_data <- scouting_data %>%
    mutate(
      down = factor(down, levels = c(1, 2, 3, 4)),
      qtr = factor(qtr, levels = c(1, 2, 3, 4, 5))
    )
  
  scouting_data <- scouting_data %>%
    mutate(
      expected_pass_prob = predict(pass_model, newdata = scouting_data, type = "response"),
      proe_play = is_pass - expected_pass_prob
    )
  
  # -----------------------------
  # Separate true 4th-down dataset
  # -----------------------------
  fourth_down_data <- pbp_raw %>%
    filter(
      down == 4,
      play_type %in% c("run", "pass", "punt", "field_goal"),
      !is.na(posteam),
      !is.na(defteam),
      !is.na(ydstogo),
      !is.na(yardline_100),
      !is.na(week),
      !is.na(game_id),
      !is.na(play_id)
    ) %>%
    mutate(
      posteam = nflreadr::clean_team_abbrs(posteam),
      defteam = nflreadr::clean_team_abbrs(defteam),
      went_for_it = if_else(play_type %in% c("run", "pass"), 1, 0),
      fourth_distance_group = case_when(
        ydstogo <= 1 ~ "1 yard",
        ydstogo <= 3 ~ "2-3 yards",
        ydstogo <= 5 ~ "4-5 yards",
        TRUE ~ "6+ yards"
      ),
      field_zone = case_when(
        yardline_100 <= 10 ~ "Goal-to-go",
        yardline_100 <= 20 ~ "Red zone",
        yardline_100 <= 50 ~ "Plus territory",
        TRUE ~ "Own territory"
      )
    )
  
  saveRDS(scouting_data, save_path)
  saveRDS(fourth_down_data, fourth_down_save_path)
  saveRDS(pass_model, "data/processed/pass_model.rds")
  
  scouting_data
}