library(tidyverse)
library(scales)
library(nflplotR)

plot_down_tendencies <- function(down_data, team, unit = "offense") {
  fill_col <- get_primary_color(team)
  
  title_text <- if (tolower(unit) == "offense") {
    paste(team, "Pass Rate by Down")
  } else {
    paste(team, "Pass Rate Faced by Down")
  }
  
  ggplot(down_data, aes(x = factor(down), y = pass_rate)) +
    geom_col(width = 0.65, fill = fill_col) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(title = title_text, x = "Down", y = "Pass Rate") +
    theme_minimal(base_size = 14)
}

plot_down_distance_heatmap <- function(dd_data, team, unit = "offense") {
  title_text <- if (tolower(unit) == "offense") {
    paste(team, "Pass Rate by Down and Distance")
  } else {
    paste(team, "Pass Rate Faced by Down and Distance")
  }
  
  ggplot(dd_data, aes(x = factor(down), y = distance_group, fill = pass_rate)) +
    geom_tile() +
    scale_fill_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = title_text, x = "Down", y = "Distance", fill = "Pass Rate") +
    theme_minimal(base_size = 14)
}

plot_weekly_epa <- function(weekly_data, team, unit = "offense") {
  line_col <- get_primary_color(team)
  y_lab <- if (tolower(unit) == "offense") "EPA per Play" else "Defensive EPA per Play"
  
  ggplot(weekly_data, aes(x = week, y = epa_per_play)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1, color = line_col) +
    geom_point(size = 2, color = line_col) +
    labs(title = paste(team, "Weekly EPA Trend"), x = "Week", y = y_lab) +
    theme_minimal(base_size = 14)
}

plot_weekly_proe <- function(weekly_proe, team, unit = "offense") {
  line_col <- get_primary_color(team)
  
  title_text <- if (tolower(unit) == "offense") {
    paste(team, "Weekly PROE")
  } else {
    paste(team, "Weekly Pass Rate Over Expected Allowed")
  }
  
  ggplot(weekly_proe, aes(x = week, y = proe)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 1, color = line_col) +
    geom_point(size = 2, color = line_col) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = title_text, x = "Week", y = "PROE") +
    theme_minimal(base_size = 14)
}

plot_field_zone_tendencies <- function(zone_data, team, unit = "offense") {
  fill_col <- get_primary_color(team)
  
  title_text <- if (tolower(unit) == "offense") {
    paste(team, "Pass Rate by Field Zone")
  } else {
    paste(team, "Pass Rate Allowed by Field Zone")
  }
  
  ggplot(zone_data, aes(x = reorder(field_zone, pass_rate), y = pass_rate)) +
    geom_col(width = 0.7, fill = fill_col) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(title = title_text, x = NULL, y = "Pass Rate") +
    theme_minimal(base_size = 14)
}

plot_team_identity_logo <- function(team, team_proe, league_baselines, unit = "offense") {
  subtitle_text <- if (tolower(unit) == "offense") {
    "Dashed lines show league offensive averages"
  } else {
    "Dashed lines show league defensive averages"
  }
  
  plot_data <- tibble(
    posteam = team,
    expected_pass_rate = team_proe$expected_pass_rate,
    proe = team_proe$proe
  )
  
  ggplot(plot_data, aes(
    x = expected_pass_rate,
    y = proe,
    team_abbr = posteam
  )) +
    geom_vline(
      xintercept = league_baselines$league_pass_rate,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_hline(
      yintercept = league_baselines$league_proe,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_nfl_logos(width = 0.12) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.3, 0.8)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste(team, "Passing Identity"),
      subtitle = subtitle_text,
      x = "Expected Pass Rate",
      y = "PROE"
    ) +
    theme_minimal(base_size = 14)
}

plot_fourth_down_aggressiveness <- function(fourth_data, team) {
  fill_col <- get_primary_color(team)
  
  ggplot(fourth_data, aes(x = fourth_distance_group, y = go_for_it_rate)) +
    geom_col(width = 0.7, fill = fill_col) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste(team, "4th Down Go-for-It Rate by Distance"),
      x = "Distance to Go",
      y = "Go-for-It Rate"
    ) +
    theme_minimal(base_size = 14)
}

plot_comparison_bars <- function(comparison_data, team) {
  team_col <- get_primary_color(team)
  
  plot_data <- comparison_data %>%
    select(metric, team_value, league_value) %>%
    pivot_longer(
      cols = c(team_value, league_value),
      names_to = "group",
      values_to = "value"
    ) %>%
    mutate(
      group = recode(group, team_value = team, league_value = "League Avg")
    )
  
  ggplot(plot_data, aes(x = metric, y = value, fill = group)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(
      values = setNames(
        c(team_col, "#9AA0A6"),
        c(team, "League Avg")
      )
    ) +
    labs(
      title = paste(team, "vs League Average"),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 14)
}

plot_rolling_trends <- function(rolling_data, team, metric = "rolling_epa") {
  line_col <- get_primary_color(team)
  
  metric_label <- case_when(
    metric == "rolling_epa" ~ "Rolling EPA/Play",
    metric == "rolling_success" ~ "Rolling Success Rate",
    metric == "rolling_pass_rate" ~ "Rolling Pass Rate",
    TRUE ~ metric
  )
  
  ggplot(rolling_data, aes(x = week, y = .data[[metric]])) +
    geom_line(linewidth = 1, color = line_col) +
    geom_point(size = 2, color = line_col) +
    labs(title = paste(team, metric_label), x = "Week", y = metric_label) +
    theme_minimal(base_size = 14)
}

plot_matchup_metric_bars <- function(matchup_data, offense_team, defense_team) {
  offense_col <- get_primary_color(offense_team)
  defense_col <- get_primary_color(defense_team)
  
  plot_data <- matchup_data %>%
    select(metric, offense_value, defense_value) %>%
    pivot_longer(
      cols = c(offense_value, defense_value),
      names_to = "side",
      values_to = "value"
    ) %>%
    mutate(
      side = recode(
        side,
        offense_value = paste(offense_team, "Offense"),
        defense_value = paste(defense_team, "Defense")
      )
    )
  
  ggplot(plot_data, aes(x = metric, y = value, fill = side)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(
      values = setNames(
        c(offense_col, defense_col),
        c(
          paste(offense_team, "Offense"),
          paste(defense_team, "Defense")
        )
      )
    ) +
    labs(
      title = paste(offense_team, "Offense vs", defense_team, "Defense"),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 14)
}

plot_matchup_down_edges <- function(matchup_down, offense_team, defense_team) {
  fill_col <- get_primary_color(offense_team)
  
  plot_data <- matchup_down %>%
    mutate(label = paste("Down", down))
  
  ggplot(plot_data, aes(x = label, y = pass_rate_gap)) +
    geom_col(width = 0.7, fill = fill_col) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste(offense_team, "Pass Tendencies vs", defense_team, "Pass Tendencies Faced"),
      subtitle = "Positive values favor the offense profile",
      x = NULL,
      y = "Pass Rate Gap"
    ) +
    theme_minimal(base_size = 14)
}

plot_matchup_rolling_epa <- function(offense_roll, defense_roll, offense_team, defense_team) {
  offense_col <- get_primary_color(offense_team)
  defense_col <- get_primary_color(defense_team)
  
  offense_plot <- offense_roll %>%
    transmute(
      week,
      value = rolling_epa,
      team = paste(offense_team, "Offense")
    )
  
  defense_plot <- defense_roll %>%
    transmute(
      week,
      value = rolling_epa,
      team = paste(defense_team, "Defense")
    )
  
  bind_rows(offense_plot, defense_plot) %>%
    ggplot(aes(x = week, y = value, color = team)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = setNames(
        c(offense_col, defense_col),
        c(
          paste(offense_team, "Offense"),
          paste(defense_team, "Defense")
        )
      )
    ) +
    labs(
      title = paste(offense_team, "Offense vs", defense_team, "Defense Rolling EPA"),
      x = "Week",
      y = "Rolling EPA/Play",
      color = NULL
    ) +
    theme_minimal(base_size = 14)
}
