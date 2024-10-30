tidy_weekly <- prepare_games(2015:2023)
tidy_games <- prepare_games(2015,2023,tidy_weekly)
usethis::use_data(tidy_weekly, overwrite = TRUE)
usethis::use_data(tidy_games, overwrite = TRUE)
