library(tidyverse)
library(nflreadr)

season <- 2025
player_name <- "CeeDee Lamb"

player_names <- c(
   "Darnell Mooney", "George Pickens", "Jerry Jeudy",
  "Emeka Egbuka", "Marquise Brown", "Ladd McConkey",
  "Deebo Samuel Sr.", "Christian Watson", "Terry McLaurin",
  "A.J. Brown"
))


pbp <- load_pbp(season)

pbp <- pbp |>
  select(game_id, play_id, home_team, away_team, week, play_type, pass_length, yards_after_catch, epa, air_epa, yac_epa, wp, passer_player_name, receiver_player_name, receiving_yards)

participation <- load_participation(season, include_pbp = FALSE) |>
  select(nflverse_game_id, play_id, offense_players)

rosters <- load_rosters(season) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, full_name)

epa_on_off <- function(player_name) {
  player_id <- rosters$gsis_id[rosters$full_name == player_name]
  
  participation_player <- participation |>
    mutate(player_on_field = str_detect(offense_players, fixed(player_id)))
  
  pbp |>
    left_join(participation_player, by = c("game_id" = "nflverse_game_id", "play_id")) |>
    filter(!is.na(epa), play_type == "pass") |>
    group_by(player_on_field) |>
    summarise(
      plays = n(),
      epa_per_play = mean(epa),
      .groups = "drop"
    )
}

AJ_BROWN = epa_on_off("A.J. Brown")
DARNELL_MOONEY = epa_on_off("Darnell Mooney")
JERRY_JEUDY = epa_on_off("Jerry Jeudy")
EMEKA_EGBUKA = epa_on_off("Emeka Egbuka")
MARQUISE_BROWN = epa_on_off("Marquise brown")
LADD_MCCONKEY = epa_on_off("Ladd McConkey")
DEEBO_SAMUEL_SR = epa_on_off("Deebo Samuel Sr.")
CHRISTIAN_WATSON = epa_on_off("Christian Watson")
TERRY_MCLAURIN = epa_on_off("Terry McLaurin")
GEORGE_PICKENS = epa_on_off("George Pickens")

