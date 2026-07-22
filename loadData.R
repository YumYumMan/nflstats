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

# make a tibble with the diffs
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


#####################
### P Value stuff ###
#####################

epa_on_off_pval <- function(player_name, team,player_diff) {
  player_id <- rosters$gsis_id[rosters$full_name == player_name]
  
  participation_player <- participation |>
    mutate(player_on_field = str_detect(offense_players, fixed(player_id)))
  
  pbp_pval = pbp |>
    left_join(participation_player, by = c("game_id" = "nflverse_game_id", "play_id")) |>
    filter(!is.na(epa), play_type == "pass", posteam == team,
           !is.na(receiver_player_id),
           receiver_player_id != player_id)
  
  trues = sum(pbp_pval$player_on_field)
  
  sim_diffs = numeric(500)
  
  for (i in 1:500) {
    index = sample(pbp_pval$player_on_field, replace = FALSE, prob = NULL)
    
    pbp_sim = pbp_pval %>%
      mutate(fake_true = index)
    
    pbp_sim = pbp_sim |>
      left_join(participation_player, by = c("game_id" = "nflverse_game_id", "play_id")) |>
      filter(!is.na(epa), play_type == "pass", posteam == team,
             !is.na(receiver_player_id),
             receiver_player_id != player_id) |>
      group_by(fake_true) |>
      summarise(
        plays = n(),
        epa_per_play = mean(epa),
        .groups = "drop"
      )
    
    sim_diffs[i] = pbp_sim$epa_per_play[pbp_sim$fake_true == TRUE] - 
      pbp_sim$epa_per_play[pbp_sim$fake_true == FALSE]
  }
  
  p_value = mean(abs(player_diff)>=abs(sim_diffs))
  p_value
}


pittman_p = epa_on_off_pval("Michael Pittman", "IND",pittman_diff)
flowers_p =  epa_on_off_pval("Zay Flowers", "BAL", flowers_diff)
tet_p = epa_on_off_pval("Tetairoa McMillan", "CAR", tet_diff)
shakir_p =  epa_on_off_pval("Khalil Shakir", "BUF", shakir_diff)
djmoore_p=  epa_on_off_pval("DJ Moore", "CHI", djmoore_diff)
chase_p=  epa_on_off_pval("Ja'Marr Chase", "CIN", chase_diff)
sutton_p =  epa_on_off_pval("Courtland Sutton", "DEN", sutton_diff)
jeudy_p = epa_on_off_pval("Jerry Jeudy", "CLE", jeudy_diff)
stbrown_p =  epa_on_off_pval("Amon-Ra St. Brown", "DET", stbrown_diff)
collins_p =  epa_on_off_pval("Nico Collins", "HOU", collins_diff)

btj_p = epa_on_off_pval("Brian Thomas Jr.", "JAX", btj_diff)
mcconkey_p = epa_on_off_pval("Ladd McConkey", "LAC", mcconkey_diff)
nacua_p = epa_on_off_pval("Puka Nacua", "LA", nacua_diff)
jefferson_p = epa_on_off_pval("Justin Jefferson", "MIN", jefferson_diff)
diggs_p = epa_on_off_pval("Stefon Diggs", "NE", diggs_diff)
olave_p = epa_on_off_pval("Chris Olave", "NO", olave_diff)
brown_p = epa_on_off_pval("A.J. Brown", "PHI", brown_diff)
metcalf_p = epa_on_off_pval("DK Metcalf", "PIT", metcalf_diff)
jennings_p = epa_on_off_pval("Jauan Jennings", "SF", jennings_diff)
jsn_p = epa_on_off_pval("Jaxon Smith-Njigba", "SEA", jsn_diff)


pittman_p
flowers_p
tet_p
shakir_p
djmoore_p
chase_p
sutton_p
jeudy_p
stbrown_p
collins_p
btj_p
mcconkey_p
nacua_p
jefferson_p
diggs_p
olave_p
brown_p
metcalf_p
jennings_p
jsn_p


pval_tbl <- tibble(
  player = c(
    "Michael Pittman",
    "Zay Flowers",
    "Tetairoa McMillan",
    "Khalil Shakir",
    "DJ Moore",
    "Ja'Marr Chase",
    "Courtland Sutton",
    "Jerry Jeudy",
    "Amon-Ra St. Brown",
    "Nico Collins",
    "Brian Thomas Jr.",
    "Ladd McConkey",
    "Puka Nacua",
    "Justin Jefferson",
    "Stefon Diggs",
    "Chris Olave",
    "A.J. Brown",
    "DK Metcalf",
    "Jauan Jennings",
    "Jaxon Smith-Njigba"),
  p_value = c(
    pittman_p,
    flowers_p,
    tet_p,
    shakir_p,
    djmoore_p,
    chase_p,
    sutton_p,
    jeudy_p,
    stbrown_p,
    collins_p,
    btj_p,
    mcconkey_p,
    nacua_p,
    jefferson_p,
    diggs_p,
    olave_p,
    brown_p,
    metcalf_p,
    jennings_p,
    jsn_p ))

pval_tbl

ggplot(data = pval_tbl, aes(x = player, y = p_value, group = 1)) +
  geom_point(col = "magenta", size = 5) +
  geom_line( col = "black", linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", col = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
