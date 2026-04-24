library(tidyverse)
library(slider)

get_team_unit_data <- function(data, team, season, unit = "offense") {
  unit <- tolower(unit)
  
  if (unit == "offense") {
    data %>% filter(posteam == team, season == !!season)
  } else {
    data %>% filter(defteam == team, season == !!season)
  }
}

get_team_unit_situation_data <- function(data, team, season, unit = "offense", situation = "all") {
  base <- get_team_unit_data(data, team, season, unit)
  filter_situation(base, situation)
}

get_league_unit_data <- function(data, season, unit = "offense") {
  data %>% filter(season == !!season)
}

get_matchup_data <- function(data, offense_team, defense_team, season, situation = "all") {
  base <- data %>%
    filter(
      season == !!season,
      posteam == offense_team,
      defteam == defense_team
    )
  
  filter_situation(base, situation)
}

calculate_team_overview <- function(team_data, unit = "offense") {
  if (tolower(unit) == "offense") {
    tibble(
      plays = nrow(team_data),
      pass_rate = mean(team_data$is_pass, na.rm = TRUE),
      epa_per_play = mean(team_data$epa, na.rm = TRUE),
      success_rate = mean(team_data$success, na.rm = TRUE),
      explosive_rate = mean(team_data$explosive_play, na.rm = TRUE)
    )
  } else {
    tibble(
      plays = nrow(team_data),
      pass_rate = mean(team_data$is_pass, na.rm = TRUE),
      epa_per_play = mean(-team_data$epa, na.rm = TRUE),
      success_rate = mean(!team_data$success, na.rm = TRUE),
      explosive_rate = mean(!team_data$explosive_play, na.rm = TRUE)
    )
  }
}

calculate_neutral_metrics <- function(team_data, unit = "offense") {
  neutral_data <- team_data %>% filter(neutral_situation)
  
  if (tolower(unit) == "offense") {
    tibble(
      neutral_plays = nrow(neutral_data),
      neutral_pass_rate = mean(neutral_data$is_pass, na.rm = TRUE),
      neutral_epa = mean(neutral_data$epa, na.rm = TRUE)
    )
  } else {
    tibble(
      neutral_plays = nrow(neutral_data),
      neutral_pass_rate_allowed = mean(neutral_data$is_pass, na.rm = TRUE),
      neutral_epa_allowed = mean(neutral_data$epa, na.rm = TRUE)
    )
  }
}

calculate_proe_summary <- function(team_data, unit = "offense") {
  tibble(
    proe = mean(team_data$proe_play, na.rm = TRUE),
    expected_pass_rate = mean(team_data$expected_pass_prob, na.rm = TRUE),
    actual_pass_rate = mean(team_data$is_pass, na.rm = TRUE)
  )
}

calculate_league_baselines <- function(league_data, unit = "offense") {
  if (tolower(unit) == "offense") {
    tibble(
      league_pass_rate = mean(league_data$is_pass, na.rm = TRUE),
      league_epa_per_play = mean(league_data$epa, na.rm = TRUE),
      league_success_rate = mean(league_data$success, na.rm = TRUE),
      league_explosive_rate = mean(league_data$explosive_play, na.rm = TRUE),
      league_proe = mean(league_data$proe_play, na.rm = TRUE)
    )
  } else {
    tibble(
      league_pass_rate = mean(league_data$is_pass, na.rm = TRUE),
      league_epa_per_play = mean(-league_data$epa, na.rm = TRUE),
      league_success_rate = mean(!league_data$success, na.rm = TRUE),
      league_explosive_rate = mean(!league_data$explosive_play, na.rm = TRUE),
      league_proe = mean(league_data$proe_play, na.rm = TRUE)
    )
  }
}

calculate_down_tendencies <- function(team_data, unit = "offense") {
  if (tolower(unit) == "offense") {
    team_data %>%
      group_by(down) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(epa, na.rm = TRUE),
        success_rate = mean(success, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    team_data %>%
      group_by(down) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(-epa, na.rm = TRUE),
        success_rate = mean(!success, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

calculate_down_distance_tendencies <- function(team_data, unit = "offense") {
  if (tolower(unit) == "offense") {
    team_data %>%
      group_by(down, distance_group) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(epa, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    team_data %>%
      group_by(down, distance_group) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(-epa, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

calculate_field_zone_tendencies <- function(team_data, unit = "offense") {
  if (tolower(unit) == "offense") {
    team_data %>%
      group_by(field_zone) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(epa, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    team_data %>%
      group_by(field_zone) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(-epa, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

calculate_weekly_trends <- function(team_data, unit = "offense") {
  if (tolower(unit) == "offense") {
    team_data %>%
      group_by(week) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(epa, na.rm = TRUE),
        success_rate = mean(success, na.rm = TRUE),
        explosive_rate = mean(explosive_play, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    team_data %>%
      group_by(week) %>%
      summarise(
        plays = n(),
        pass_rate = mean(is_pass, na.rm = TRUE),
        epa_per_play = mean(-epa, na.rm = TRUE),
        success_rate = mean(!success, na.rm = TRUE),
        explosive_rate = mean(!explosive_play, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

calculate_weekly_proe <- function(team_data, unit = "offense") {
  team_data %>%
    group_by(week) %>%
    summarise(
      proe = mean(proe_play, na.rm = TRUE),
      expected_pass_rate = mean(expected_pass_prob, na.rm = TRUE),
      actual_pass_rate = mean(is_pass, na.rm = TRUE),
      .groups = "drop"
    )
}

calculate_rolling_trends <- function(team_data, unit = "offense", window = 3) {
  weekly <- calculate_weekly_trends(team_data, unit)
  
  if (nrow(weekly) == 0) return(weekly)
  
  weekly %>%
    arrange(week) %>%
    mutate(
      rolling_epa = slider::slide_dbl(
        epa_per_play,
        mean,
        .before = window - 1,
        .complete = FALSE,
        na.rm = TRUE
      ),
      rolling_success = slider::slide_dbl(
        success_rate,
        mean,
        .before = window - 1,
        .complete = FALSE,
        na.rm = TRUE
      ),
      rolling_pass_rate = slider::slide_dbl(
        pass_rate,
        mean,
        .before = window - 1,
        .complete = FALSE,
        na.rm = TRUE
      )
    )
}

calculate_fourth_down_profile <- function(fourth_data, team, season, unit = "offense") {
  unit <- tolower(unit)
  
  filtered <- if (unit == "offense") {
    fourth_data %>% filter(posteam == team, season == !!season)
  } else {
    fourth_data %>% filter(defteam == team, season == !!season)
  }
  
  if (nrow(filtered) == 0) {
    return(tibble(
      fourth_down_plays = 0,
      go_for_it_rate = NA_real_,
      avg_ydstogo = NA_real_,
      fourth_down_epa = NA_real_
    ))
  }
  
  filtered %>%
    summarise(
      fourth_down_plays = n(),
      go_for_it_rate = mean(went_for_it, na.rm = TRUE),
      avg_ydstogo = mean(ydstogo, na.rm = TRUE),
      fourth_down_epa = mean(epa, na.rm = TRUE)
    )
}

calculate_fourth_down_by_distance <- function(fourth_data, team, season, unit = "offense") {
  unit <- tolower(unit)
  
  filtered <- if (unit == "offense") {
    fourth_data %>% filter(posteam == team, season == !!season)
  } else {
    fourth_data %>% filter(defteam == team, season == !!season)
  }
  
  if (nrow(filtered) == 0) {
    return(tibble(
      fourth_distance_group = character(),
      plays = integer(),
      go_for_it_rate = double()
    ))
  }
  
  filtered %>%
    group_by(fourth_distance_group) %>%
    summarise(
      plays = n(),
      go_for_it_rate = mean(went_for_it, na.rm = TRUE),
      .groups = "drop"
    )
}

calculate_comparison_table <- function(overview, league_baselines) {
  tibble(
    metric = c("Pass Rate", "EPA/Play", "Success Rate", "Explosive Rate"),
    team_value = c(
      overview$pass_rate,
      overview$epa_per_play,
      overview$success_rate,
      overview$explosive_rate
    ),
    league_value = c(
      league_baselines$league_pass_rate,
      league_baselines$league_epa_per_play,
      league_baselines$league_success_rate,
      league_baselines$league_explosive_rate
    )
  ) %>%
    mutate(diff = team_value - league_value)
}

calculate_matchup_overview <- function(offense_data, defense_data) {
  offense_overview <- calculate_team_overview(offense_data, "offense")
  defense_overview <- calculate_team_overview(defense_data, "defense")
  offense_proe <- calculate_proe_summary(offense_data, "offense")
  defense_proe <- calculate_proe_summary(defense_data, "defense")
  
  tibble(
    metric = c("Pass Rate", "EPA/Play", "Success Rate", "Explosive Rate", "PROE"),
    offense_value = c(
      offense_overview$pass_rate,
      offense_overview$epa_per_play,
      offense_overview$success_rate,
      offense_overview$explosive_rate,
      offense_proe$proe
    ),
    defense_value = c(
      defense_overview$pass_rate,
      defense_overview$epa_per_play,
      defense_overview$success_rate,
      defense_overview$explosive_rate,
      defense_proe$proe
    ),
    matchup_edge = offense_value - defense_value
  )
}

calculate_matchup_down_table <- function(offense_data, defense_data) {
  offense_down <- calculate_down_tendencies(offense_data, "offense") %>%
    select(down, offense_pass_rate = pass_rate, offense_epa = epa_per_play)
  
  defense_down <- calculate_down_tendencies(defense_data, "defense") %>%
    select(down, defense_pass_rate = pass_rate, defense_epa = epa_per_play)
  
  full_join(offense_down, defense_down, by = "down") %>%
    mutate(
      pass_rate_gap = offense_pass_rate - defense_pass_rate,
      epa_gap = offense_epa - defense_epa
    )
}

calculate_matchup_field_zone_table <- function(offense_data, defense_data) {
  offense_zone <- calculate_field_zone_tendencies(offense_data, "offense") %>%
    select(field_zone, offense_pass_rate = pass_rate, offense_epa = epa_per_play)
  
  defense_zone <- calculate_field_zone_tendencies(defense_data, "defense") %>%
    select(field_zone, defense_pass_rate = pass_rate, defense_epa = epa_per_play)
  
  full_join(offense_zone, defense_zone, by = "field_zone") %>%
    mutate(
      pass_rate_gap = offense_pass_rate - defense_pass_rate,
      epa_gap = offense_epa - defense_epa
    )
}

calculate_matchup_situational_table <- function(offense_data, defense_data) {
  tibble(
    metric = c("Pass Rate", "EPA/Play", "Success Rate", "Explosive Rate", "PROE"),
    offense = c(
      mean(offense_data$is_pass, na.rm = TRUE),
      mean(offense_data$epa, na.rm = TRUE),
      mean(offense_data$success, na.rm = TRUE),
      mean(offense_data$explosive_play, na.rm = TRUE),
      mean(offense_data$proe_play, na.rm = TRUE)
    ),
    defense = c(
      mean(defense_data$is_pass, na.rm = TRUE),
      mean(-defense_data$epa, na.rm = TRUE),
      mean(!defense_data$success, na.rm = TRUE),
      mean(!defense_data$explosive_play, na.rm = TRUE),
      mean(defense_data$proe_play, na.rm = TRUE)
    )
  ) %>%
    mutate(edge = offense - defense)
}

get_team_player_leaders <- function(player_summaries, team, season) {
  list(
    passers = player_summaries$passers %>%
      filter(team == !!team, season == !!season) %>%
      arrange(desc(plays), desc(epa_per_play)) %>%
      slice_head(n = 5),
    
    rushers = player_summaries$rushers %>%
      filter(team == !!team, season == !!season) %>%
      arrange(desc(carries), desc(epa_per_play)) %>%
      slice_head(n = 5),
    
    receivers = player_summaries$receivers %>%
      filter(team == !!team, season == !!season) %>%
      arrange(desc(targets), desc(epa_per_target)) %>%
      slice_head(n = 5)
  )
}