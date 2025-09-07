#!/usr/bin/env Rscript
library(nflForecastR)

# 1. Build weekly and game data across seasons
weekly <- prepare_weekly(2019:2024)
games  <- prepare_games(start_year = 2019, end_year = 2024, weekly_data = weekly)

# 2. Split: train on 2019-2023, test on 2024
train_data <- subset(games, season <= 2023)
test_data  <- subset(games, season == 2024)

# 3. Fit a simple model using available features
formula <- point_differential ~ home.qb_passer_rating + away.qb_passer_rating
model   <- lm(formula, data = train_data)

# 4. Predict 2024 games
pred <- predict(model, newdata = test_data)

# 5. Display predictions
output <- cbind(test_data[, c("season", "week", "home_team", "away_team")], pred)
print(head(output))

# 6. Backtest 2024 week-by-week using prior data only
bt <- backtest_model(
  years = 2024,
  model_type = "lm",
  formula = formula,
  all_updated_games = games
)

cat("\nBacktest summary:\n")
print(bt$overall_metrics)
