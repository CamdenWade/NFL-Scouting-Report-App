library(scales)
library(tidyverse)

format_pct <- function(x) {
  ifelse(is.na(x), "N/A", scales::percent(x, accuracy = 0.1))
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), "N/A", format(round(x, digits), nsmall = digits))
}

filter_situation <- function(data, situation = "all") {
  switch(
    situation,
    "all" = data,
    "neutral" = data %>% filter(neutral_situation),
    "early_down" = data %>% filter(early_down),
    "third_down" = data %>% filter(third_down),
    "red_zone" = data %>% filter(red_zone),
    "short_yardage" = data %>% filter(short_yardage),
    "two_minute" = data %>% filter(two_minute),
    data
  )
}

build_summary_text <- function(overview, neutral_metrics, proe_summary, unit = "offense") {
  if (tolower(unit) == "offense") {
    paste0(
      "This offense ran ", overview$plays, " qualifying plays. ",
      "Its overall pass rate was ", format_pct(overview$pass_rate),
      ", with a neutral-situation pass rate of ", format_pct(neutral_metrics$neutral_pass_rate), ". ",
      "The offense posted ", format_num(overview$epa_per_play),
      " EPA per play, a success rate of ", format_pct(overview$success_rate),
      ", and an explosive play rate of ", format_pct(overview$explosive_rate), ". ",
      "Its pass rate over expected was ", format_pct(proe_summary$proe), "."
    )
  } else {
    paste0(
      "This defense faced ", overview$plays, " qualifying plays. ",
      "Opponents passed on ", format_pct(overview$pass_rate),
      " of snaps, with a neutral-situation pass rate of ", format_pct(neutral_metrics$neutral_pass_rate_allowed), ". ",
      "The defense posted ", format_num(overview$epa_per_play),
      " defensive EPA per play, a stop rate of ", format_pct(overview$success_rate),
      ", and prevented explosive plays at a rate of ", format_pct(overview$explosive_rate), ". ",
      "Opponent PROE against this defense was ", format_pct(proe_summary$proe), "."
    )
  }
}

build_matchup_summary_text <- function(offense_team, defense_team, matchup_overview) {
  pass_edge <- matchup_overview %>% filter(metric == "Pass Rate")
  epa_edge <- matchup_overview %>% filter(metric == "EPA/Play")
  success_edge <- matchup_overview %>% filter(metric == "Success Rate")
  explosive_edge <- matchup_overview %>% filter(metric == "Explosive Rate")
  
  pass_text <- if (pass_edge$matchup_edge > 0) {
    paste0(offense_team, " brings a more aggressive passing profile than what ", defense_team, " typically suppresses.")
  } else {
    paste0(defense_team, " has tended to control opponent passing behavior relative to ", offense_team, "'s baseline.")
  }
  
  epa_text <- if (epa_edge$matchup_edge > 0) {
    paste0("The efficiency profile leans toward ", offense_team, ".")
  } else {
    paste0("The efficiency profile leans toward ", defense_team, ".")
  }
  
  success_text <- if (success_edge$matchup_edge > 0) {
    paste0(offense_team, " holds the edge in sustaining successful plays.")
  } else {
    paste0(defense_team, " holds the edge in limiting successful plays.")
  }
  
  explosive_text <- if (explosive_edge$matchup_edge > 0) {
    paste0(offense_team, " appears better positioned to create explosive gains.")
  } else {
    paste0(defense_team, " appears better positioned to limit explosives.")
  }
  
  paste(pass_text, epa_text, success_text, explosive_text)
}
