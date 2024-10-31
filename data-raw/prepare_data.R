tidy_weekly <- prepare_weekly(2015:2023)
tidy_weekly <- na.omit(tidy_weekly)
tidy_games <- prepare_games(2015,2023, tidy_weekly)
tidy_games <- na.omit(tidy_games)
nfl_schedule_2024 <- read.csv(file="data/nfl-2024-UTC.csv")

usethis::use_data(tidy_weekly, overwrite = TRUE)
usethis::use_data(tidy_games, overwrite = TRUE)
usethis::use_data(nfl_schedule_2024, overwrite = TRUE)

source("data-raw/prepare_data.R")
