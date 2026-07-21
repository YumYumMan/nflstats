library(tidyverse)
library(nflreadr)

all_wr1s <- c("mharrisonjr",
              "mwilson",
              "dlondon",
              "dmooney",
              "zflowers",
              "kshakir",
              "tmcmillan",
              "dmoore",
              "jchase",
              "jjeudy",
              "clamb",
              "gpickens",
              "csutton",
              "astbrown",
              "cwatson",
              "rdoubs",
              "ncollins",
              "mpittmanjr",
              "bthomasjr",
              "rrice",
              "xworthy",
              "mbrown",
              "jmeyers",
              "ttucker",
              "lmcconkey",
              "pnacua",
              "thill",
              "jwaddle",
              "jjefferson",
              "sdiggs",
              "colave",
              "mnabers",
              "wrobinson",
              "gwilson",
              "jmetchieiii",
              "abrown",
              "dmetcalf",
              "jjennings",
              "jsmithnjigba",
              "mevans",
              "eegbuka",
              "cridley",
              "eayomanor",
              "tmclaurin",
              "dsamuelsr")

sample(all_wr1s, size = 10)

player_names <- c(
  "Darnell Mooney", "George Pickens", "Jerry Jeudy",
  "Emeka Egbuka", "Marquise Brown", "Ladd McConkey",
  "Deebo Samuel Sr.", "Christian Watson", "Terry McLaurin",
  "A.J. Brown"
)

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
  
  pbp |>
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

AJ_BROWN = epa_on_off("A.J. Brown", "PHI")
DARNELL_MOONEY = epa_on_off("Darnell Mooney", "ATL")
JERRY_JEUDY = epa_on_off("Jerry Jeudy", "DEN")
EMEKA_EGBUKA = epa_on_off("Emeka Egbuka", "TB")
MARQUISE_BROWN = epa_on_off("Marquise Brown", "KC")
LADD_MCCONKEY = epa_on_off("Ladd McConkey", "LAC")
DEEBO_SAMUEL_SR = epa_on_off("Deebo Samuel Sr.", "WAS")
CHRISTIAN_WATSON = epa_on_off("Christian Watson", "GB")
TERRY_MCLAURIN = epa_on_off("Terry McLaurin", "WAS")
GEORGE_PICKENS = epa_on_off("George Pickens", "DAL")



TERRY_MCLAURIN
JERRY_JEUDY
AJ_BROWN
