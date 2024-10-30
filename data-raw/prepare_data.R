tidy_weeks <- prepare_weeks(2015:2023)
tidy_games <- prepare_games(2015,2023, tidy_weeks)
nfl_schedule_2024 <- read.csv(nfl-2024-UTC.csv)

usethis::use_data(tidy_weeks, tidy_games, nfl_schedule_2024, overwrite = TRUE)
