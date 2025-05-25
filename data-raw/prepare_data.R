tidy_weekly <- prepare_weekly(2015:2023)
tidy_weekly <- na.omit(tidy_weekly)

tidy_games <- prepare_games(2015,2023, tidy_weekly)
tidy_games <- na.omit(tidy_games)


nfl_schedule_2024 <- read.csv("data-raw/nfl-2024-UTC.csv") %>%
  mutate(
    # First standardize the format of all dates
    Date = case_when(
      # Convert dates like "13/09/2024 00:15" to "13/9/24 0:15" format
      grepl("/2024", Date) ~ sub("(\\d{1,2})/(\\d{2})/2024\\s+(\\d{2}):(\\d{2})", "\\1/\\2/24 \\3:\\4", Date),
      # Handle 2025 dates
      grepl("/2025", Date) ~ sub("(\\d{1,2})/(\\d{1,2})/2025\\s+(\\d{1,2}):(\\d{2})", "\\1/\\2/25 \\3:\\4", Date),
      # Handle dates already in correct format or other formats
      TRUE ~ Date
    ),
    # Convert to proper POSIXct format
    Date = as.POSIXct(
      Date,
      format = "%d/%m/%y %H:%M",
      tz = "UTC"
    ),
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

