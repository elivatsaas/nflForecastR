tidy_weekly <- prepare_weekly(2015:2023)
tidy_weekly <- na.omit(tidy_weekly)

tidy_games <- prepare_games(2015,2023, tidy_weekly)
tidy_games <- na.omit(tidy_games)


# Read and clean schedule
nfl_schedule_2024 <- read.csv("data-raw/nfl-2024-UTC.csv") %>%
  mutate(
    # Convert date to proper format
    Date = as.POSIXct(Date, format="%d/%m/%y %H:%M", tz="UTC"),
    # Convert team names to abbreviations
    home_team = as.character(NFL_TEAM_MAPPINGS[Home.Team]),
    away_team = as.character(NFL_TEAM_MAPPINGS[Away.Team]),
    # Ensure week is numeric
    week = as.numeric(week)
  )

# Save the cleaned data
usethis::use_data(tidy_weekly, overwrite = TRUE)
usethis::use_data(tidy_games, overwrite = TRUE)
usethis::use_data(nfl_schedule_2024, overwrite = TRUE)

