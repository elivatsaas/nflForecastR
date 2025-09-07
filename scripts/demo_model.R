#!/usr/bin/env Rscript
library(nflForecastR)

# 1. Prepare data
games <- prepare_games(start_year = 2023, end_year = 2023, weekly_data = prepare_weekly(2023))

# 2. Engineer features and split
train_data <- head(games, -20)
test_data  <- tail(games, 20)

# 3. Fit a simple model using available features
model <- lm(point_differential ~ home.qb_passer_rating + away.qb_passer_rating, data = train_data)

# 4. Generate predictions
pred <- predict(model, newdata = test_data)

# 5. Display results
output <- cbind(test_data[, c("home_team", "away_team", "week")], pred)
print(output)
