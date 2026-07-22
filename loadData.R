library(tidyverse)
library(nflreadr)

player_names <- c(
  "Zay Flowers", "Tetairoa McMillan", "Jerry Jeudy",
  "Khalil Shakir", "DJ Moore", "Ja'Marr Chase",
  "Courtland Sutton", "Amon-Ra St. Brown", "Nico Collins",
  "Michael Pittman", "Brian Thomas Jr.", "Ladd McConkey", "Puka Nacua", "Justin Jefferson", "Stefon Diggs",
  "Chris Olave", "A.J. Brown", "DK Metcalf", "Jauan Jennings", "Jaxon Smith-Njigba")

# load data
pbp <- load_pbp(2025)

# only week 1 to 18
pbp = filter(pbp, week >= 1 & week <=18)

# remove useless columns
pbp <- pbp |>
  select(game_id, play_id, desc, home_team, posteam, away_team, week, play_type, pass_length, yards_after_catch, epa, air_epa, yac_epa, wp, passer_player_name, receiver_player_name, receiver_player_id, receiving_yards)

# load offensive players
participation <- load_participation(2025, include_pbp = FALSE) |>
  select(nflverse_game_id, play_id, offense_players)

# load player names and ids
rosters <- load_rosters(2025) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, full_name)

activity <- read_csv("data/WR Data - Sheet1.csv")

# merge columns
activity <- activity |>
  pivot_longer(
    cols = starts_with("is_wr1_w"),
    names_to = "week",
    names_prefix = "is_wr1_w",
    values_to = "is_wr1"
  )

get_id <- function(player_name) {
  # get player id from the name
  rosters$gsis_id[rosters$full_name == player_name]
}

# make a function that returns the weeks a wr was wr1
wr1_on_weeks <- function(player_name) {
  activity |>
    filter(Name == player_name, is_wr1 == 1) |>
    pull(week)
}

# make function to get epa data
epa_on_off <- function(player_name, team) {
  player_id <- get_id(player_name)
  # check if the wr is on the field
  participation_player <- participation |>
    mutate(player_on_field = str_detect(offense_players, fixed(player_id)))
  
  # make a summary tibble for epa
  pbp |>
    # combine datasets
    left_join(participation_player, by = c("game_id" = "nflverse_game_id", "play_id")) |>
    # filter for only passes and wrs team
    filter(!is.na(epa), play_type == "pass", posteam == team,
           !is.na(receiver_player_id),
           # exclude plays to the wr1
           receiver_player_id != player_id,
           week %in% wr1_on_weeks(player_name)
           ) |>
    group_by(player_on_field) |>
    # calculate epa per play
    summarise(
      plays = n(),
      epa_per_play = mean(epa),
      .groups = "drop"
    )
}

# calculate epa for all players
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

calculate_diff <- function(player_epa) {
  player_epa$epa_per_play[player_epa$player_on_field == TRUE] - 
    player_epa$epa_per_play[player_epa$player_on_field == FALSE]
}

# calculate epa difference for all players
btj_diff = calculate_diff(BTJ)
mcconkey_diff = calculate_diff(MCCONKEY)
nacua_diff = calculate_diff(NACUA)
jefferson_diff = calculate_diff(JEFFERSON)
diggs_diff = calculate_diff(DIGGS)
olave_diff = calculate_diff(OLAVE)
brown_diff = calculate_diff(BROWN)
metcalf_diff = calculate_diff(METCALF)
jennings_diff = calculate_diff(JENNINGS)
jsn_diff = calculate_diff(JSN)
flowers_diff = calculate_diff(FLOWERS)
tet_diff = calculate_diff(TETAIROA)
shakir_diff = calculate_diff(SHAKIR)
djmoore_diff = calculate_diff(DJ_MOORE)
chase_diff = calculate_diff(CHASE)
sutton_diff = calculate_diff(SUTTON)
jeudy_diff = calculate_diff(JEUDY)
stbrown_diff = calculate_diff(ST_BROWN)
collins_diff = calculate_diff(COLLINS)
pittman_diff = calculate_diff(PITTMAN)

# make a tibble with the diifs
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
