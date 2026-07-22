library(tidyverse)
library(nflreadr)

player_names <- c(
  "Marvin Harrison Jr.",
  "Michael Wilson",
  "Drake London",
  "Darnell Mooney",
  "Zay Flowers",
  "Khalil Shakir",
  "Tetairoa McMillan",
  "DJ Moore",
  "Ja'Marr Chase",
  "Jerry Jeudy",
  "CeeDee Lamb",
  "George Pickens",
  "Courtland Sutton",
  "Amon-Ra St. Brown",
  "Christian Watson",
  "Romeo Doubs",
  "Nico Collins",
  "Michael Pittman",
  "Brian Thomas Jr.",
  "Rashee Rice",
  "Xavier Worthy",
  "Marquise Brown",
  "Jakobi Meyers",
  "Tre Tucker",
  "Ladd McConkey",
  "Puka Nacua",
  "Tyreek Hill",
  "Jaylen Waddle",
  "Justin Jefferson",
  "Stefon Diggs",
  "Chris Olave",
  "Malik Nabers",
  "Wan'Dale Robinson",
  "Garrett Wilson",
  "John Metchie III",
  "A.J. Brown",
  "DK Metcalf",
  "Jauan Jennings",
  "Jaxon Smith-Njigba",
  "Mike Evans",
  "Emeka Egbuka",
  "Calvin Ridley",
  "Elic Ayomanor",
  "Terry McLaurin",
  "Deebo Samuel Sr."
)
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
MARV = epa_on_off("Marvin Harrison Jr.", "ARI")
MICHAEL_WILSON = epa_on_off("Michael Wilson", "ARI")
DRAKE_LONDON = epa_on_off("Drake London", "ATL")
DARNELL_MOONEY = epa_on_off("Darnell Mooney", "ATL")
FLOWERS = epa_on_off("Zay Flowers", "BAL")
SHAKIR = epa_on_off("Khalil Shakir", "BUF")
TETAIROA = epa_on_off("Tetairoa McMillan", "CAR")
DJ_MOORE = epa_on_off("DJ Moore", "CHI")
CHASE = epa_on_off("Ja'Marr Chase", "CIN")
JEUDY = epa_on_off("Jerry Jeudy", "CLE")
CEEDEE = epa_on_off("CeeDee Lamb", "DAL")
PICKENS = epa_on_off("George Pickens", "DAL")
SUTTON = epa_on_off("Courtland Sutton", "DEN")
ST_BROWN = epa_on_off("Amon-Ra St. Brown", "DET")
WATSON = epa_on_off("Christian Watson", "GB")
DOUBS = epa_on_off("Romeo Doubs", "GB")
COLLINS = epa_on_off("Nico Collins", "HOU")
PITTMAN = epa_on_off("Michael Pittman", "IND")
BTJ = epa_on_off("Brian Thomas Jr.", "JAX")
RASHEE = epa_on_off("Rashee Rice", "KC")
WORTHY = epa_on_off("Xavier Worthy", "KC")
MARQUISE = epa_on_off("Marquise Brown", "KC")
MEYERS = epa_on_off("Jakobi Meyers", "LV")
TRE_TUCKER = epa_on_off("Tre Tucker", "LV")
MCCONKEY = epa_on_off("Ladd McConkey", "LAC")
NACUA = epa_on_off("Puka Nacua", "LA")
HILL = epa_on_off("Tyreek Hill", "MIA")
WADDLE = epa_on_off("Jaylen Waddle", "MIA")
JEFFERSON = epa_on_off("Justin Jefferson", "MIN")
DIGGS = epa_on_off("Stefon Diggs", "NE")
OLAVE = epa_on_off("Chris Olave", "NO")
NABERS = epa_on_off("Malik Nabers", "NYG")
WAN_DALE = epa_on_off("Wan'Dale Robinson", "NYG")
GARRETT_WILSON = epa_on_off("Garrett Wilson", "NYJ")
METCHIE = epa_on_off("John Metchie III", "NYJ")
BROWN = epa_on_off("A.J. Brown", "PHI")
METCALF = epa_on_off("DK Metcalf", "PIT")
JENNINGS = epa_on_off("Jauan Jennings", "SF")
JSN = epa_on_off("Jaxon Smith-Njigba", "SEA")
EVANS = epa_on_off("Mike Evans", "TB")
EGBUKA = epa_on_off("Emeka Egbuka", "TB")
RIDLEY = epa_on_off("Calvin Ridley", "TEN")
AYOMANOR = epa_on_off("Elic Ayomanor", "TEN")
MCLAURIN = epa_on_off("Terry McLaurin", "WAS")
DEEEBO = epa_on_off("Deebo Samuel Sr.", "WAS")

calculate_diff <- function(player_epa) {
  player_epa$epa_per_play[player_epa$player_on_field == TRUE] - 
    player_epa$epa_per_play[player_epa$player_on_field == FALSE]
}

# calculate epa difference for all players
marv_diff = calculate_diff(MARV)
michael_wilson_diff = calculate_diff(MICHAEL_WILSON)
drake_london_diff = calculate_diff(DRAKE_LONDON)
mooney_diff = calculate_diff(DARNELL_MOONEY)
flowers_diff = calculate_diff(FLOWERS)
shakir_diff = calculate_diff(SHAKIR)
tet_diff = calculate_diff(TETAIROA)
djmoore_diff = calculate_diff(DJ_MOORE)
chase_diff = calculate_diff(CHASE)
jeudy_diff = calculate_diff(JEUDY)
ceedee_diff = calculate_diff(CEEDEE)
pickens_diff = calculate_diff(PICKENS)
sutton_diff = calculate_diff(SUTTON)
stbrown_diff = calculate_diff(ST_BROWN)
watson_diff = calculate_diff(WATSON)
doubs_diff = calculate_diff(DOUBS)
collins_diff = calculate_diff(COLLINS)
pittman_diff = calculate_diff(PITTMAN)
btj_diff = calculate_diff(BTJ)
rashee_diff = calculate_diff(RASHEE)
worthy_diff = calculate_diff(WORTHY)
marquise_diff = calculate_diff(MARQUISE)
meyers_diff = calculate_diff(MEYERS)
tre_tucker_diff = calculate_diff(TRE_TUCKER)
mcconkey_diff = calculate_diff(MCCONKEY)
nacua_diff = calculate_diff(NACUA)
hill_diff = calculate_diff(HILL)
waddle_diff = calculate_diff(WADDLE)
jefferson_diff = calculate_diff(JEFFERSON)
diggs_diff = calculate_diff(DIGGS)
olave_diff = calculate_diff(OLAVE)
nabers_diff = calculate_diff(NABERS)
wandale_diff = calculate_diff(WAN_DALE)
garrett_wilson_diff = calculate_diff(GARRETT_WILSON)
metchie_diff = calculate_diff(METCHIE)
brown_diff = calculate_diff(BROWN)
metcalf_diff = calculate_diff(METCALF)
jennings_diff = calculate_diff(JENNINGS)
jsn_diff = calculate_diff(JSN)
evans_diff = calculate_diff(EVANS)
egbuka_diff = calculate_diff(EGBUKA)
ridley_diff = calculate_diff(RIDLEY)
ayomanor_diff = calculate_diff(AYOMANOR)
mclaurin_diff = calculate_diff(MCLAURIN)
deebo_diff = calculate_diff(DEEEBO)

# make a tibble with the diifs
diff_tibble = tibble(
  player = c(
    "Marvin Harrison Jr.", "Michael Wilson", "Drake London", "Darnell Mooney",
    "Zay Flowers", "Khalil Shakir", "Tetairoa McMillan", "DJ Moore",
    "Ja'Marr Chase", "Jerry Jeudy", "CeeDee Lamb", "George Pickens",
    "Courtland Sutton", "Amon-Ra St. Brown", "Christian Watson", "Romeo Doubs",
    "Nico Collins", "Michael Pittman", "Brian Thomas Jr.", "Rashee Rice",
    "Xavier Worthy", "Marquise Brown", "Jakobi Meyers", "Tre Tucker",
    "Ladd McConkey", "Puka Nacua", "Tyreek Hill", "Jaylen Waddle",
    "Justin Jefferson", "Stefon Diggs", "Chris Olave", "Malik Nabers",
    "Wan'Dale Robinson", "Garrett Wilson", "John Metchie III", "A.J. Brown",
    "DK Metcalf", "Jauan Jennings", "Jaxon Smith-Njigba", "Mike Evans",
    "Emeka Egbuka", "Calvin Ridley", "Elic Ayomanor", "Terry McLaurin",
    "Deebo Samuel Sr."
  ),
  diff = c(
    marv_diff, michael_wilson_diff, drake_london_diff, mooney_diff,
    flowers_diff, shakir_diff, tet_diff, djmoore_diff,
    chase_diff, jeudy_diff, ceedee_diff, pickens_diff,
    sutton_diff, stbrown_diff, watson_diff, doubs_diff,
    collins_diff, pittman_diff, btj_diff, rashee_diff,
    worthy_diff, marquise_diff, meyers_diff, tre_tucker_diff,
    mcconkey_diff, nacua_diff, hill_diff, waddle_diff,
    jefferson_diff, diggs_diff, olave_diff, nabers_diff,
    wandale_diff, garrett_wilson_diff, metchie_diff, brown_diff,
    metcalf_diff, jennings_diff, jsn_diff, evans_diff,
    egbuka_diff, ridley_diff, ayomanor_diff, mclaurin_diff,
    deebo_diff
  )
) |>
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
  
  p_value = mean(abs(sim_diffs) >= abs(player_diff))
  p_value
}


marv_p = epa_on_off_pval("Marvin Harrison Jr.", "ARI", marv_diff)
michael_wilson_p = epa_on_off_pval("Michael Wilson", "ARI", michael_wilson_diff)
drake_london_p = epa_on_off_pval("Drake London", "ATL", drake_london_diff)
mooney_p = epa_on_off_pval("Darnell Mooney", "ATL", mooney_diff)
flowers_p = epa_on_off_pval("Zay Flowers", "BAL", flowers_diff)
shakir_p = epa_on_off_pval("Khalil Shakir", "BUF", shakir_diff)
tet_p = epa_on_off_pval("Tetairoa McMillan", "CAR", tet_diff)
djmoore_p = epa_on_off_pval("DJ Moore", "CHI", djmoore_diff)
chase_p = epa_on_off_pval("Ja'Marr Chase", "CIN", chase_diff)
jeudy_p = epa_on_off_pval("Jerry Jeudy", "CLE", jeudy_diff)
ceedee_p = epa_on_off_pval("CeeDee Lamb", "DAL", ceedee_diff)
pickens_p = epa_on_off_pval("George Pickens", "DAL", pickens_diff)
sutton_p = epa_on_off_pval("Courtland Sutton", "DEN", sutton_diff)
stbrown_p = epa_on_off_pval("Amon-Ra St. Brown", "DET", stbrown_diff)
watson_p = epa_on_off_pval("Christian Watson", "GB", watson_diff)
doubs_p = epa_on_off_pval("Romeo Doubs", "GB", doubs_diff)
collins_p = epa_on_off_pval("Nico Collins", "HOU", collins_diff)
pittman_p = epa_on_off_pval("Michael Pittman", "IND", pittman_diff)
btj_p = epa_on_off_pval("Brian Thomas Jr.", "JAX", btj_diff)
rashee_p = epa_on_off_pval("Rashee Rice", "KC", rashee_diff)
worthy_p = epa_on_off_pval("Xavier Worthy", "KC", worthy_diff)
marquise_p = epa_on_off_pval("Marquise Brown", "KC", marquise_diff)
meyers_p = epa_on_off_pval("Jakobi Meyers", "LV", meyers_diff)
tre_tucker_p = epa_on_off_pval("Tre Tucker", "LV", tre_tucker_diff)
mcconkey_p = epa_on_off_pval("Ladd McConkey", "LAC", mcconkey_diff)
nacua_p = epa_on_off_pval("Puka Nacua", "LA", nacua_diff)
hill_p = epa_on_off_pval("Tyreek Hill", "MIA", hill_diff)
waddle_p = epa_on_off_pval("Jaylen Waddle", "MIA", waddle_diff)
jefferson_p = epa_on_off_pval("Justin Jefferson", "MIN", jefferson_diff)
diggs_p = epa_on_off_pval("Stefon Diggs", "NE", diggs_diff)
olave_p = epa_on_off_pval("Chris Olave", "NO", olave_diff)
nabers_p = epa_on_off_pval("Malik Nabers", "NYG", nabers_diff)
wandale_p = epa_on_off_pval("Wan'Dale Robinson", "NYG", wandale_diff)
garrett_wilson_p = epa_on_off_pval("Garrett Wilson", "NYJ", garrett_wilson_diff)
metchie_p = epa_on_off_pval("John Metchie III", "NYJ", metchie_diff)
brown_p = epa_on_off_pval("A.J. Brown", "PHI", brown_diff)
metcalf_p = epa_on_off_pval("DK Metcalf", "PIT", metcalf_diff)
jennings_p = epa_on_off_pval("Jauan Jennings", "SF", jennings_diff)
jsn_p = epa_on_off_pval("Jaxon Smith-Njigba", "SEA", jsn_diff)
evans_p = epa_on_off_pval("Mike Evans", "TB", evans_diff)
egbuka_p = epa_on_off_pval("Emeka Egbuka", "TB", egbuka_diff)
ridley_p = epa_on_off_pval("Calvin Ridley", "TEN", ridley_diff)
ayomanor_p = epa_on_off_pval("Elic Ayomanor", "TEN", ayomanor_diff)
mclaurin_p = epa_on_off_pval("Terry McLaurin", "WAS", mclaurin_diff)
deebo_p = epa_on_off_pval("Deebo Samuel Sr.", "WAS", deebo_diff)

pval_tbl <- tibble(
  player = c(
    "Marvin Harrison Jr.",
    "Michael Wilson",
    "Drake London",
    "Darnell Mooney",
    "Zay Flowers",
    "Khalil Shakir",
    "Tetairoa McMillan",
    "DJ Moore",
    "Ja'Marr Chase",
    "Jerry Jeudy",
    "CeeDee Lamb",
    "George Pickens",
    "Courtland Sutton",
    "Amon-Ra St. Brown",
    "Christian Watson",
    "Romeo Doubs",
    "Nico Collins",
    "Michael Pittman",
    "Brian Thomas Jr.",
    "Rashee Rice",
    "Xavier Worthy",
    "Marquise Brown",
    "Jakobi Meyers",
    "Tre Tucker",
    "Ladd McConkey",
    "Puka Nacua",
    "Tyreek Hill",
    "Jaylen Waddle",
    "Justin Jefferson",
    "Stefon Diggs",
    "Chris Olave",
    "Malik Nabers",
    "Wan'Dale Robinson",
    "Garrett Wilson",
    "John Metchie III",
    "A.J. Brown",
    "DK Metcalf",
    "Jauan Jennings",
    "Jaxon Smith-Njigba",
    "Mike Evans",
    "Emeka Egbuka",
    "Calvin Ridley",
    "Elic Ayomanor",
    "Terry McLaurin",
    "Deebo Samuel Sr."
  ),
  p_value = c(
    marv_p,
    michael_wilson_p,
    drake_london_p,
    mooney_p,
    flowers_p,
    shakir_p,
    tet_p,
    djmoore_p,
    chase_p,
    jeudy_p,
    ceedee_p,
    pickens_p,
    sutton_p,
    stbrown_p,
    watson_p,
    doubs_p,
    collins_p,
    pittman_p,
    btj_p,
    rashee_p,
    worthy_p,
    marquise_p,
    meyers_p,
    tre_tucker_p,
    mcconkey_p,
    nacua_p,
    hill_p,
    waddle_p,
    jefferson_p,
    diggs_p,
    olave_p,
    nabers_p,
    wandale_p,
    garrett_wilson_p,
    metchie_p,
    brown_p,
    metcalf_p,
    jennings_p,
    jsn_p,
    evans_p,
    egbuka_p,
    ridley_p,
    ayomanor_p,
    mclaurin_p,
    deebo_p
  )
)
