tidy_weekly <- prepare_games(2015:2023)
tidy_games <- prepare_games(2015,2023,tidy_weekly)
usethis::use_data(DATASET, overwrite = TRUE)
