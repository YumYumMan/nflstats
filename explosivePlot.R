teams <- tibble(
  player = c(
    "Zay Flowers", "Xavier Worthy", "Wan'Dale Robinson", "Tyreek Hill",
    "Tre Tucker", "Tetairoa McMillan", "Terry McLaurin", "Stefon Diggs",
    "Romeo Doubs", "Rashee Rice", "Puka Nacua", "Nico Collins",
    "Mike Evans", "Michael Wilson", "Michael Pittman", "Marvin Harrison Jr.",
    "Marquise Brown", "Malik Nabers", "Ladd McConkey", "Khalil Shakir",
    "Justin Jefferson", "John Metchie III", "Jerry Jeudy", "Jaylen Waddle",
    "Jaxon Smith-Njigba", "Jauan Jennings", "Jakobi Meyers", "Ja'Marr Chase",
    "George Pickens", "Garrett Wilson", "Emeka Egbuka", "Elic Ayomanor",
    "Drake London", "DK Metcalf", "DJ Moore", "Deebo Samuel Sr.",
    "Darnell Mooney", "Courtland Sutton", "Christian Watson", "Chris Olave",
    "CeeDee Lamb", "Calvin Ridley", "Brian Thomas Jr.", "Amon-Ra St. Brown",
    "A.J. Brown"
  ),
  team = c(
    "BAL", "KC", "NYG", "MIA",
    "LV", "CAR", "WAS", "NE",
    "GB", "KC", "LA", "HOU",
    "TB", "ARI", "IND", "ARI",
    "KC", "NYG", "LAC", "BUF",
    "MIN", "NYJ", "CLE", "MIA",
    "SEA", "SF", "LV", "CIN",
    "DAL", "NYJ", "TB", "TEN",
    "ATL", "PIT", "CHI", "WAS",
    "ATL", "DEN", "GB", "NO",
    "DAL", "TEN", "JAX", "DET",
    "PHI"
  )
)

abbrev_to_fullname <- tibble(
  team = c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN",
           "DET","GB","HOU","IND","JAX","KC","LV","LAC","LA","MIA",
           "MIN","NE","NO","NYG","NYJ","PHI","PIT","SEA","SF","TB",
           "TEN","WAS"),
  team_full = c(
    "Cardinals","Falcons","Ravens","Bills",
    "Panthers","Bears","Bengals","Browns",
    "Cowboys","Broncos","Lions","Packers",
    "Texans","Colts","Jaguars","Chiefs",
    "Raiders","Chargers","Rams","Dolphins",
    "Vikings","Patriots","Saints","Giants",
    "Jets","Eagles","Steelers","Seahawks",
    "49ers","Buccaneers","Titans","Commanders"
  )
)

combined <- diff_tibble |>
  left_join(teams, by = "player") |>
  left_join(abbrev_to_fullname, by = "team")

explosive_2025 <- read_csv("data/explosive.csv") |>
  select(Team, `Explosive Play Rate`)

combined <- combined |>
  left_join(explosive_2025, by = c("team_full" = "Team"))

ggplot(data = combined, aes(x = diff, y = `Explosive Play Rate`)) +
  geom_point() +
  theme_minimal()
