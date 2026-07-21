library(tidyverse)
library(nflreadr)

player_names <- c("Brian Thomas Jr.", "Ladd McConkey", "Puka Nacua", "Justin Jefferson", "Stefon Diggs",
                  "Chris Olave", "A.J. Brown", "DK Metcalf", "Jauan Jennings", "Jaxon Smith-Njigba")

pbp <- load_pbp(2025)

pbp = filter(pbp, week >= 1 & week <=18)

pbp <- pbp |>
  select(game_id, play_id, desc, home_team, posteam, away_team, week, play_type, pass_length, yards_after_catch, epa, air_epa, yac_epa, wp, passer_player_name, receiver_player_name, receiver_player_id, receiving_yards)

participation <- load_participation(2025, include_pbp = FALSE) |>
  select(nflverse_game_id, play_id, offense_players)

rosters <- load_rosters(2025) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, full_name)

epa_on_off <- function(player_name, team) {
  player_id <- rosters$gsis_id[rosters$full_name == player_name]
  
  participation_player <- participation |>
    mutate(player_on_field = str_detect(offense_players, fixed(player_id)))
  
  pbp = pbp |>
    left_join(participation_player, by = c("game_id" = "nflverse_game_id", "play_id")) |>
    filter(!is.na(epa), play_type == "pass", posteam == team,
           !is.na(receiver_player_id),
           receiver_player_id != player_id) |>
    group_by(player_on_field) |>
    summarise(
      plays = n(),
      epa_per_play = mean(epa),
      .groups = "drop"
    )
}

BTJ = epa_on_off("Brian Thomas Jr.", "JAC")
MCCONKEY = epa_on_off("Ladd McConkey", "LA")
NACUA = epa_on_off("Puka Nacua", "LA")
JEFFERSON = epa_on_off("Justin Jefferson", "MIN")
DIGGS = epa_on_off("Stefon Diggs", "NE")
OLAVE = epa_on_off("Chris Olave", "NO")
BROWN = epa_on_off("A.J. Brown", "PHI")
METCALF = epa_on_off("DK Metcalf", "PIT")
JENNINGS = epa_on_off("Jauan Jennings", "SF")
JSN = epa_on_off("Jaxon Smith-Njigba", "SEA")

btj_diff = BTJ$epa_per_play[BTJ$player_on_field == TRUE] - 
  BTJ$epa_per_play[BTJ$player_on_field == FALSE]

mcconkey_diff = MCCONKEY$epa_per_play[MCCONKEY$player_on_field == TRUE] - 
  MCCONKEY$epa_per_play[MCCONKEY$player_on_field == FALSE]

nacua_diff = NACUA$epa_per_play[NACUA$player_on_field == TRUE] - 
  NACUA$epa_per_play[NACUA$player_on_field == FALSE]

jefferson_diff = JEFFERSON$epa_per_play[JEFFERSON$player_on_field == TRUE] - 
  JEFFERSON$epa_per_play[JEFFERSON$player_on_field == FALSE]

diggs_diff = DIGGS$epa_per_play[DIGGS$player_on_field == TRUE] - 
  DIGGS$epa_per_play[DIGGS$player_on_field == FALSE]

olave_diff = OLAVE$epa_per_play[OLAVE$player_on_field == TRUE] - 
  OLAVE$epa_per_play[OLAVE$player_on_field == FALSE]

brown_diff = BROWN$epa_per_play[BROWN$player_on_field == TRUE] - 
  BROWN$epa_per_play[BROWN$player_on_field == FALSE]

metcalf_diff = METCALF$epa_per_play[METCALF$player_on_field == TRUE] - 
  METCALF$epa_per_play[METCALF$player_on_field == FALSE]

jennings_diff = JENNINGS$epa_per_play[JENNINGS$player_on_field == TRUE] - 
  JENNINGS$epa_per_play[JENNINGS$player_on_field == FALSE]

jsn_diff = JSN$epa_per_play[JSN$player_on_field == TRUE] - 
  JSN$epa_per_play[JSN$player_on_field == FALSE]

diff_tibble = tibble(
  player = c("Brian Thomas Jr.", "Ladd McConkey", "Puka Nacua", "Justin Jefferson", "Stefon Diggs",
             "Chris Olave", "A.J. Brown", "DK Metcalf"," Jaxon Smith-Njigba"),
  diff = c(btj_diff, mcconkey_diff, nacua_diff, jefferson_diff,
           metcalf_diff, olave_diff, brown_diff, metcalf_diff, jsn_diff))

diff_tibble


ggplot(data = diff_tibble) +
  geom_col(aes(x = player, y = diff, fill = diff)) +
  scale_fill_gradient2(low = "red", mid = "grey", high = "darkblue", midpoint = 0) +
  labs(
    title = "EPA Differential (On vs Off Field)",
    x = "Player",
    y = "EPA Difference",
    fill = "EPA Diff") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


