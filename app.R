library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(nflplotR)
library(slider)
library(quarto)
library(gt)

source("R/helpers.R")
source("R/branding.R")
source("R/metrics.R")
source("R/plots.R")

scouting_data <- readRDS("data/processed/scouting_data.rds")
fourth_down_data <- readRDS("data/processed/fourth_down_data.rds")
player_summaries <- readRDS("data/processed/player_summaries.rds")

teams <- scouting_data %>%
  distinct(posteam) %>%
  arrange(posteam) %>%
  pull(posteam)

seasons <- scouting_data %>%
  distinct(season) %>%
  arrange(desc(season)) %>%
  pull(season)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B5ED7",
  base_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "NFL Scouting Report",
  theme = app_theme,
  stylesheet = "www/styles.css",
  
  sidebar = sidebar(
    width = 320,
    
    selectInput("team", "Team", choices = teams, selected = "KC"),
    selectInput("season", "Season", choices = seasons, selected = max(seasons)),
    
    radioButtons(
      "unit",
      "Perspective",
      choices = c("Offense" = "offense", "Defense" = "defense"),
      selected = "offense"
    ),
    
    selectInput(
      "situation",
      "Situation",
      choices = c(
        "All Plays" = "all",
        "Neutral" = "neutral",
        "Early Downs" = "early_down",
        "Third Down" = "third_down",
        "Red Zone" = "red_zone",
        "Short Yardage" = "short_yardage",
        "Two Minute" = "two_minute"
      ),
      selected = "all"
    ),
    
    selectInput(
      "rolling_metric",
      "Rolling Trend Metric",
      choices = c(
        "EPA / Play" = "rolling_epa",
        "Success Rate" = "rolling_success",
        "Pass Rate" = "rolling_pass_rate"
      ),
      selected = "rolling_epa"
    ),
    
    hr(),
    
    h5("Matchup Mode"),
    selectInput("matchup_offense", "Offense Team", choices = teams, selected = "KC"),
    selectInput("matchup_defense", "Defense Team", choices = teams, selected = "SF"),
    
    hr(),
    
    downloadButton("download_report", "Download scouting report")
  ),
  
  navset_tab(
    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(4, 4, 4),
        
        value_box(
          title = "Pass Rate",
          value = textOutput("pass_rate"),
          showcase = "Selected unit",
          theme = "primary"
        ),
        
        value_box(
          title = "EPA / Play",
          value = textOutput("epa_per_play"),
          showcase = "Efficiency",
          theme = "success"
        ),
        
        value_box(
          title = "PROE",
          value = textOutput("proe"),
          showcase = "Context-adjusted tendency",
          theme = "warning"
        )
      ),
      
      card(
        class = "plot-card",
        card_header("Team Summary"),
        card_body(
          p(textOutput("summary_text"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "plot-card",
          card_header("Passing Identity"),
          plotOutput("identity_plot", height = 320)
        ),
        
        card(
          class = "plot-card",
          card_header("Team vs League"),
          plotOutput("comparison_plot", height = 320)
        )
      )
    ),
    
    nav_panel(
      "Situational Tendencies",
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "plot-card",
          card_header("Pass Rate by Down"),
          plotOutput("down_plot", height = 320)
        ),
        
        card(
          class = "plot-card",
          card_header("Down & Distance Heatmap"),
          plotOutput("heatmap_plot", height = 320)
        )
      ),
      
      card(
        class = "plot-card",
        card_header("Field Zone Tendencies"),
        plotOutput("field_zone_plot", height = 320)
      )
    ),
    
    nav_panel(
      "Weekly Trends",
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "plot-card",
          card_header("Weekly EPA / Play"),
          plotOutput("weekly_epa_plot", height = 320)
        ),
        
        card(
          class = "plot-card",
          card_header("Weekly PROE"),
          plotOutput("weekly_proe_plot", height = 320)
        )
      ),
      
      card(
        class = "plot-card",
        card_header("Rolling Trend"),
        plotOutput("rolling_plot", height = 340)
      )
    ),
    
    nav_panel(
      "4th Down",
      layout_columns(
        col_widths = c(4, 4, 4),
        
        value_box(
          title = "4th Down Plays",
          value = textOutput("fourth_plays"),
          theme = "secondary"
        ),
        
        value_box(
          title = "Go-for-It Rate",
          value = textOutput("fourth_go_rate"),
          theme = "danger"
        ),
        
        value_box(
          title = "Avg Yards To Go",
          value = textOutput("fourth_avg_togo"),
          theme = "info"
        )
      ),
      
      card(
        class = "plot-card",
        card_header("4th Down Aggressiveness by Distance"),
        plotOutput("fourth_plot", height = 340)
      )
    ),
    
    nav_panel(
      "Matchup Mode",
      card(
        class = "plot-card",
        card_header("Matchup Summary"),
        card_body(
          p(textOutput("matchup_summary_text"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "plot-card",
          card_header("Matchup Overview"),
          plotOutput("matchup_metric_plot", height = 340)
        ),
        
        card(
          class = "plot-card",
          card_header("Down-by-Down Edge"),
          plotOutput("matchup_down_plot", height = 340)
        )
      ),
      
      card(
        class = "plot-card",
        card_header("Rolling EPA Comparison"),
        plotOutput("matchup_rolling_plot", height = 360)
      )
    ),
    
    nav_panel(
      "Player Layer",
      layout_columns(
        col_widths = c(4, 4, 4),
        
        card(
          class = "plot-card",
          card_header("Top Passers"),
          gt_output("passer_table")
        ),
        
        card(
          class = "plot-card",
          card_header("Top Rushers"),
          gt_output("rusher_table")
        ),
        
        card(
          class = "plot-card",
          card_header("Top Receivers"),
          gt_output("receiver_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_team_data <- reactive({
    get_team_unit_situation_data(
      data = scouting_data,
      team = input$team,
      season = input$season,
      unit = input$unit,
      situation = input$situation
    )
  })
  
  selected_league_data <- reactive({
    get_league_unit_data(
      data = scouting_data,
      season = input$season,
      unit = input$unit
    )
  })
  
  overview <- reactive({
    calculate_team_overview(selected_team_data(), input$unit)
  })
  
  neutral_metrics <- reactive({
    calculate_neutral_metrics(selected_team_data(), input$unit)
  })
  
  proe_summary <- reactive({
    calculate_proe_summary(selected_team_data(), input$unit)
  })
  
  league_baselines <- reactive({
    calculate_league_baselines(selected_league_data(), input$unit)
  })
  
  comparison_data <- reactive({
    calculate_comparison_table(overview(), league_baselines())
  })
  
  down_data <- reactive({
    calculate_down_tendencies(selected_team_data(), input$unit)
  })
  
  dd_data <- reactive({
    calculate_down_distance_tendencies(selected_team_data(), input$unit)
  })
  
  zone_data <- reactive({
    calculate_field_zone_tendencies(selected_team_data(), input$unit)
  })
  
  weekly_data <- reactive({
    calculate_weekly_trends(selected_team_data(), input$unit)
  })
  
  weekly_proe <- reactive({
    calculate_weekly_proe(selected_team_data(), input$unit)
  })
  
  rolling_data <- reactive({
    calculate_rolling_trends(selected_team_data(), input$unit, window = 3)
  })
  
  fourth_profile <- reactive({
    calculate_fourth_down_profile(
      fourth_data = fourth_down_data,
      team = input$team,
      season = input$season,
      unit = input$unit
    )
  })
  
  fourth_distance <- reactive({
    calculate_fourth_down_by_distance(
      fourth_data = fourth_down_data,
      team = input$team,
      season = input$season,
      unit = input$unit
    )
  })
  
  matchup_offense_data <- reactive({
    get_team_unit_situation_data(
      data = scouting_data,
      team = input$matchup_offense,
      season = input$season,
      unit = "offense",
      situation = input$situation
    )
  })
  
  matchup_defense_data <- reactive({
    get_team_unit_situation_data(
      data = scouting_data,
      team = input$matchup_defense,
      season = input$season,
      unit = "defense",
      situation = input$situation
    )
  })
  
  matchup_overview <- reactive({
    calculate_matchup_overview(matchup_offense_data(), matchup_defense_data())
  })
  
  matchup_down <- reactive({
    calculate_matchup_down_table(matchup_offense_data(), matchup_defense_data())
  })
  
  matchup_offense_roll <- reactive({
    calculate_rolling_trends(matchup_offense_data(), "offense", window = 3)
  })
  
  matchup_defense_roll <- reactive({
    calculate_rolling_trends(matchup_defense_data(), "defense", window = 3)
  })
  
  player_leaders <- reactive({
    get_team_player_leaders(player_summaries, input$team, input$season)
  })
  
  output$summary_text <- renderText({
    build_summary_text(overview(), neutral_metrics(), proe_summary(), input$unit)
  })
  
  output$matchup_summary_text <- renderText({
    build_matchup_summary_text(
      offense_team = input$matchup_offense,
      defense_team = input$matchup_defense,
      matchup_overview = matchup_overview()
    )
  })
  
  output$pass_rate <- renderText({
    format_pct(overview()$pass_rate)
  })
  
  output$epa_per_play <- renderText({
    format_num(overview()$epa_per_play)
  })
  
  output$proe <- renderText({
    format_pct(proe_summary()$proe)
  })
  
  output$fourth_plays <- renderText({
    fourth_profile()$fourth_down_plays
  })
  
  output$fourth_go_rate <- renderText({
    format_pct(fourth_profile()$go_for_it_rate)
  })
  
  output$fourth_avg_togo <- renderText({
    format_num(fourth_profile()$avg_ydstogo, 1)
  })
  
  output$identity_plot <- renderPlot({
    plot_team_identity_logo(
      team = input$team,
      team_proe = proe_summary(),
      league_baselines = league_baselines(),
      unit = input$unit
    )
  })
  
  output$comparison_plot <- renderPlot({
    plot_comparison_bars(comparison_data(), input$team)
  })
  
  output$down_plot <- renderPlot({
    plot_down_tendencies(down_data(), input$team, input$unit)
  })
  
  output$heatmap_plot <- renderPlot({
    plot_down_distance_heatmap(dd_data(), input$team, input$unit)
  })
  
  output$field_zone_plot <- renderPlot({
    plot_field_zone_tendencies(zone_data(), input$team, input$unit)
  })
  
  output$weekly_epa_plot <- renderPlot({
    plot_weekly_epa(weekly_data(), input$team, input$unit)
  })
  
  output$weekly_proe_plot <- renderPlot({
    plot_weekly_proe(weekly_proe(), input$team, input$unit)
  })
  
  output$rolling_plot <- renderPlot({
    req(nrow(rolling_data()) > 0)
    plot_rolling_trends(rolling_data(), input$team, input$rolling_metric)
  })
  
  output$fourth_plot <- renderPlot({
    req(nrow(fourth_distance()) > 0)
    plot_fourth_down_aggressiveness(fourth_distance(), input$team)
  })
  
  output$matchup_metric_plot <- renderPlot({
    plot_matchup_metric_bars(
      matchup_data = matchup_overview(),
      offense_team = input$matchup_offense,
      defense_team = input$matchup_defense
    )
  })
  
  output$matchup_down_plot <- renderPlot({
    req(nrow(matchup_down()) > 0)
    plot_matchup_down_edges(
      matchup_down = matchup_down(),
      offense_team = input$matchup_offense,
      defense_team = input$matchup_defense
    )
  })
  
  output$matchup_rolling_plot <- renderPlot({
    req(nrow(matchup_offense_roll()) > 0, nrow(matchup_defense_roll()) > 0)
    plot_matchup_rolling_epa(
      offense_roll = matchup_offense_roll(),
      defense_roll = matchup_defense_roll(),
      offense_team = input$matchup_offense,
      defense_team = input$matchup_defense
    )
  })
  
  output$passer_table <- render_gt({
    req(nrow(player_leaders()$passers) > 0)
    
    player_leaders()$passers %>%
      select(player_id, player, plays, epa_per_play, explosive_rate) %>%
      gt() %>%
      nflplotR::gt_nfl_headshots(columns = player_id, height = 40) %>%
      cols_label(
        player_id = "",
        player = "Player",
        plays = "Plays",
        epa_per_play = "EPA/Play",
        explosive_rate = "Explosive Rate"
      ) %>%
      fmt_number(columns = epa_per_play, decimals = 2) %>%
      fmt_percent(columns = explosive_rate, decimals = 1)
  })
  
  output$rusher_table <- render_gt({
    req(nrow(player_leaders()$rushers) > 0)
    
    player_leaders()$rushers %>%
      select(player_id, player, carries, yards_per_carry, epa_per_play) %>%
      gt() %>%
      nflplotR::gt_nfl_headshots(columns = player_id, height = 40) %>%
      cols_label(
        player_id = "",
        player = "Player",
        carries = "Carries",
        yards_per_carry = "Yds/Carry",
        epa_per_play = "EPA/Play"
      ) %>%
      fmt_number(columns = c(yards_per_carry, epa_per_play), decimals = 2)
  })
  
  output$receiver_table <- render_gt({
    req(nrow(player_leaders()$receivers) > 0)
    
    player_leaders()$receivers %>%
      select(player_id, player, targets, epa_per_target, explosive_rate) %>%
      gt() %>%
      nflplotR::gt_nfl_headshots(columns = player_id, height = 40) %>%
      cols_label(
        player_id = "",
        player = "Player",
        targets = "Targets",
        epa_per_target = "EPA/Target",
        explosive_rate = "Explosive Rate"
      ) %>%
      fmt_number(columns = epa_per_target, decimals = 2) %>%
      fmt_percent(columns = explosive_rate, decimals = 1)
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(
        input$matchup_offense, "_vs_", input$matchup_defense, "_",
        input$season, "_", input$situation, "_report.html"
      )
    },
    
    content = function(file) {
      # Find the app/project root. This assumes app.R is launched from the
      # project root, but also handles the case where the working directory
      # is accidentally set to the report/ folder.
      app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
      if (basename(app_root) == "report") {
        app_root <- dirname(app_root)
      }
      
      report_source <- file.path(app_root, "report", "scouting_report.qmd")
      if (!file.exists(report_source)) {
        stop("Could not find scouting_report.qmd at: ", report_source)
      }
      
      # Render in a clean temporary folder so Quarto output names are predictable.
      render_dir <- tempfile("quarto_report_")
      dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)
      
      temp_qmd <- file.path(render_dir, "scouting_report.qmd")
      ok <- file.copy(report_source, temp_qmd, overwrite = TRUE)
      if (!ok) {
        stop("Could not copy scouting_report.qmd to temporary render directory.")
      }
      
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(render_dir)
      
      quarto::quarto_render(
        input = "scouting_report.qmd",
        output_file = basename(file),
        execute_params = list(
          offense_team = input$matchup_offense,
          defense_team = input$matchup_defense,
          season = input$season,
          situation = input$situation,
          project_dir = app_root
        ),
        quiet = TRUE
      )
      
      rendered_file <- file.path(render_dir, basename(file))
      if (!file.exists(rendered_file)) {
        stop("Quarto report was not created at: ", rendered_file)
      }
      
      ok <- file.copy(rendered_file, file, overwrite = TRUE)
      if (!ok) {
        stop("Could not copy rendered report to Shiny download file.")
      }
    }
  )
}

shinyApp(ui = ui, server = server)