library(tidyverse)
library(nflreadr)

player_names <- c(
  "Zay Flowers", "Tetairoa McMillan", "Jerry Jeudy",
  "Khalil Shakir", "DJ Moore", "Ja'Marr Chase",
  "Courtland Sutton", "Amon-Ra St. Brown", "Nico Collins",
  "Michael Pittman", "Brian Thomas Jr.", "Ladd McConkey", "Puka Nacua", "Justin Jefferson", "Stefon Diggs",
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

FLOWERS =  epa_on_off("Zay Flowers", "BAL")
TETAIROA = epa_on_off("Tetairoa McMillan", "CAR")
SHAKIR =  epa_on_off("Khalil Shakir", "BUF")
DJ_MOORE =  epa_on_off("DJ Moore", "CHI")
CHASE =  epa_on_off("Ja'Marr Chase", "CIN")
SUTTON =  epa_on_off("Courtland Sutton", "DEN")
JEUDY = epa_on_off("Jerry Jeudy", "CLE")
ST_BROWN =  epa_on_off("Amon-Ra St. Brown", "DET")
COLLINS =  epa_on_off("Nico Collins", "HOU")
PITTMAN =  epa_on_off("Michael Pittman", "IND")

BTJ = epa_on_off("Brian Thomas Jr.", "JAX")
MCCONKEY = epa_on_off("Ladd McConkey", "LAC")
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



flowers_diff = FLOWERS$epa_per_play[FLOWERS$player_on_field == TRUE] - 
 FLOWERS$epa_per_play[FLOWERS$player_on_field == FALSE]

tet_diff = TETAIROA$epa_per_play[TETAIROA$player_on_field == TRUE] - 
  TETAIROA$epa_per_play[TETAIROA$player_on_field == FALSE]

shakir_diff = SHAKIR$epa_per_play[SHAKIR$player_on_field == TRUE] - 
  SHAKIR$epa_per_play[SHAKIR$player_on_field == FALSE]

djmoore_diff = DJ_MOORE$epa_per_play[DJ_MOORE$player_on_field == TRUE] - 
  DJ_MOORE$epa_per_play[DJ_MOORE$player_on_field == FALSE]

chase_diff = CHASE$epa_per_play[CHASE$player_on_field == TRUE] - 
  CHASE$epa_per_play[CHASE$player_on_field == FALSE]

sutton_diff = SUTTON$epa_per_play[SUTTON$player_on_field == TRUE] - 
  SUTTON$epa_per_play[SUTTON$player_on_field == FALSE]

jeudy_diff = JEUDY$epa_per_play[JEUDY$player_on_field == TRUE] - 
  JEUDY$epa_per_play[JEUDY$player_on_field == FALSE]

stbrown_diff = ST_BROWN$epa_per_play[ST_BROWN$player_on_field == TRUE] - 
  ST_BROWN$epa_per_play[ST_BROWN$player_on_field == FALSE]

collins_diff = COLLINS$epa_per_play[COLLINS$player_on_field == TRUE] - 
  COLLINS$epa_per_play[COLLINS$player_on_field == FALSE]

pittman_diff = PITTMAN$epa_per_play[PITTMAN$player_on_field == TRUE] - 
  PITTMAN$epa_per_play[PITTMAN$player_on_field == FALSE]


diff_tibble = tibble(
  player = c(
    "Zay Flowers", "Tetairoa McMillan", "Jerry Jeudy",
    "Khalil Shakir", "DJ Moore", "Ja'Marr Chase",
    "Courtland Sutton", "Amon-Ra St. Brown", "Nico Collins",
    "Michael Pittman", "Brian Thomas Jr.", "Ladd McConkey", "Puka Nacua", "Justin Jefferson", "Stefon Diggs",
    "Chris Olave", "A.J. Brown", "DK Metcalf", "Jauan Jennings", "Jaxon Smith-Njigba"),
  diff = c(flowers_diff, tet_diff, jeudy_diff,
           shakir_diff,djmoore_diff, chase_diff, sutton_diff,
           stbrown_diff, collins_diff, pittman_diff, btj_diff, mcconkey_diff, nacua_diff, jefferson_diff,
           diggs_diff, olave_diff, brown_diff, metcalf_diff, jennings_diff, jsn_diff)) |>
  arrange(diff)


diff_tibble


ggplot(data = diff_tibble) +
  geom_col(aes(x = reorder(player, -diff), y = diff, fill = diff)) +
  scale_fill_gradient2(low = "red", mid = "grey", high = "darkblue", midpoint = 0) +
  labs(
    title = "EPA Differential (On vs Off Field)",
    x = "Player",
    y = "EPA Difference",
    fill = "EPA Diff") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






