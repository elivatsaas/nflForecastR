tidy_weekly <- prepare_weekly(2015:2023)
tidy_weekly <- na.omit(tidy_weekly)

tidy_games <- prepare_games(2015,2023, tidy_weekly)
tidy_games <- na.omit(tidy_games)

nfl_schedule_2024 <- read.csv(file="data-raw/nfl-2024-UTC.csv")

usethis::use_data(tidy_weekly, overwrite = TRUE)
usethis::use_data(tidy_games, overwrite = TRUE)
usethis::use_data(nfl_schedule_2024, overwrite = TRUE)
<<<<<<< HEAD
=======

>>>>>>> 3ad8f61628e6b12f1ecc01ff6a87ad3f86b7da4d
