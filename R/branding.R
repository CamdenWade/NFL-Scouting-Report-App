library(tidyverse)

team_brand_colors <- tibble(
  team = c(
    "ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB",
    "HOU","IND","JAX","KC","LV","LAC","LAR","MIA","MIN","NE","NO","NYG",
    "NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS"
  ),
  primary = c(
    "#97233F","#A71930","#241773","#00338D","#0085CA","#0B162A","#FB4F14","#311D00",
    "#003594","#FB4F14","#0076B6","#203731","#03202F","#002C5F","#006778","#E31837",
    "#000000","#0080C6","#003594","#008E97","#4F2683","#002244","#D3BC8D","#0B2265",
    "#125740","#004C54","#FFB612","#002244","#AA0000","#D50A0A","#0C2340","#5A1414"
  ),
  secondary = c(
    "#FFB612","#000000","#9E7C0C","#C60C30","#101820","#C83803","#000000","#FF3C00",
    "#869397","#002244","#B0B7BC","#FFB612","#A71930","#A2AAAD","#9F792C","#FFB81C",
    "#A5ACAF","#FFC20E","#FFA300","#FC4C02","#FFC62F","#C60C30","#101820","#A71930",
    "#000000","#A5ACAF","#101820","#69BE28","#B3995D","#FF7900","#4B92DB","#FFB612"
  )
)

get_team_colors <- function(team) {
  team_brand_colors %>%
    filter(team == !!team)
}

get_primary_color <- function(team, default = "#0B5ED7") {
  cols <- get_team_colors(team)
  if (nrow(cols) == 0) default else cols$primary[[1]]
}
