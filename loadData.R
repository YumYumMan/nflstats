library(tidyverse)
library(nflreadr)

season <- 2025
player_name <- "CeeDee Lamb"

pbp <- load_pbp(season)

pbp <- pbp |>
  select(game_id, play_id, home_team, away_team, week, play_type, pass_length, yards_after_catch, epa, air_epa, yac_epa, wp, passer_player_name, receiver_player_name, receiving_yards)

participation <- load_participation(season, include_pbp = FALSE) |>
  select(nflverse_game_id, play_id, offense_players)

rosters <- load_rosters(season) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, full_name)

player_id <- rosters$gsis_id[rosters$full_name == player_name]

participation <- participation |>
  mutate(player_on_field = str_detect(offense_players, fixed(player_id)))

pbp <- pbp |>
  left_join(participation, by = c("game_id" = "nflverse_game_id", "play_id")) |>
  filter(!is.na(epa), play_type == "pass") |>
  group_by(player_on_field)# |>
  #summarise(plays = n(), epa_per_play = mean(epa), .groups = "drop")

filter(pbp, play_type == "pass") |>
  select(play_type)
