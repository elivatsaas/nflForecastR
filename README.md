# nflForecastR

NFL game prediction and analysis tools in R, focused on predicting point differentials and analyzing betting lines. This package provides comprehensive utilities for preparing NFL data, training prediction models, and evaluating betting opportunities.

## ⚠️ Important Disclaimer

This package is provided for informational and educational purposes only. Any predictions, analyses, or insights generated by nflForecastR should not be considered as gambling or betting advice. Sports betting involves significant risk and can lead to financial losses. Users of this package assume full responsibility for:

- Any decisions made based on the package's outputs
- Compliance with all applicable local, state, and federal laws regarding sports betting
- Any financial losses that may occur from betting activities

The authors and contributors of nflForecastR are not responsible for any losses, damages, or legal issues that may arise from using this package for betting purposes. If you have a gambling problem, please call 1-800-GAMBLER for support.

## Features

- Data preparation and maintenance for NFL game statistics
- Multiple modeling approaches with cross-validation (Linear Models, Random Forests)
- Automated betting line analysis
- Customizable visualization tools
- Live odds integration via the-odds-api.com

## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("elivatsaas/nflForecastR")
```

## Core Data Sets

### tidy_weekly
Week-by-week team performance metrics (2015-2023):
- Offensive metrics (EPA, points, rushing/passing)
- Defensive metrics (EPA, points allowed)
- QB performance (completion %, passer rating)
- Efficiency stats (3rd down %, red zone %)
- Cumulative season statistics

```r
# Quick look at 2023 Chiefs data
library(nflForecastR)
library(dplyr)

kc_data <- tidy_weekly %>%
  filter(posteam == "KC", season == 2023) %>%
  select(week, off_epa, def_epa, points_scored)
```

### tidy_games
Game-level dataset optimized for prediction:
- Pre-game team statistics
- Vegas lines (spreads and totals)
- Game outcomes
- Divisional matchup indicators

```r
# View recent games
recent_games <- tidy_games %>%
  filter(season == 2023) %>%
  select(season, week, home_team, away_team, 
         point_differential, away.spread_line)
```

## Workflow Examples

## Complete Workflow Examples

### Base Model Workflow

```r
library(nflForecastR)
library(dplyr)
library(ranger)

# 1. Update and prepare data
updated_games <- update_games(2024)
scaled_games <- updated_games %>%
select(-point_differential) %>%
  mutate(across(where(is.numeric), scale)) %>%
  mutate(point_differential = updated_games$point_differential)

# 2a. Train and evaluate linear model
lm_cv_results <- lm_cv(
  point_differential ~ home.off_epa_cum + home.def_epa_cum + 
    away.off_epa_cum + away.def_epa_cum,
  data = scaled_games
)


# 2b. Train and evaluate random forest
rf_cv_results <- rf_cv(
  point_differential ~ home.off_epa_cum + home.def_epa_cum + 
    away.off_epa_cum + away.def_epa_cum,
  data = scaled_games
)


# 3. Train final models
lm_model <- lm(
  point_differential ~ home.off_epa_cum + home.def_epa_cum + 
    away.off_epa_cum + away.def_epa_cum,
  data = scaled_games
)

rf_model <- ranger(
  point_differential ~ home.off_epa_cum + home.def_epa_cum + 
    away.off_epa_cum + away.def_epa_cum, 
  data = scaled_games
)

weekly_2024 <- prepare_weekly(2024)

# 4. Get prediction data
pred_data <- prepare_predictions(weekly_2024)  # Gets current week's matchups with odds

# 5. Generate predictions from both models
lm_predictions <- predict_with_model(pred_data, lm_model, scaled_games)
rf_predictions <- predict_with_model(pred_data, rf_model, scaled_games)

# 6. Visualize predictions
lm_plot <- create_prediction_plot(lm_predictions)
lm_plot
rf_plot <- create_prediction_plot(rf_predictions)
rf_plot
```

### Extended Analysis Workflow

```r
# Extended Analysis Workflow

# 1. Get up-to-date data
updated_weekly <- update_weekly(2024)

# 2. Calculate season means and other derived statistics
extended_weekly <- calculate_means(updated_weekly)

# 3. Prepare game-level data with the extended statistics
extended_games <- prepare_games(2015, 2024, extended_weekly)
extended_games <- na.omit(extended_games)

# 4. Scale the data
scaled_extended <- extended_games %>%
  select(-point_differential) %>%
  mutate(across(where(is.numeric), scale)) %>%
  mutate(point_differential = extended_games$point_differential)

# 5. Train and evaluate model (using either lm_cv or rf_cv)
lm_extended_cv <- lm_cv(
  point_differential ~ home.off_epa_season_mean + home.def_epa_season_mean + 
    away.off_epa_season_mean + away.def_epa_season_mean,
  data = scaled_extended
)


# 2b. Train and evaluate random forest
rf_extended_cv <- rf_cv(
  point_differential ~ home.off_epa_season_mean + home.def_epa_season_mean + 
    away.off_epa_season_mean + away.def_epa_season_mean,
  data = scaled_extended
)


# 3. Train final models
lm_model_extended <- lm(
  point_differential ~ home.off_epa_season_mean + home.def_epa_season_mean + 
    away.off_epa_season_mean + away.def_epa_season_mean,
  data = scaled_extended
)

rf_model_extended <- ranger(
  point_differential ~ home.off_epa_season_mean + home.def_epa_season_mean + 
    away.off_epa_season_mean + away.def_epa_season_mean,
  data = scaled_extended
)

# 7. Get prediction data for current week
pred_data <- prepare_predictions(extended_weekly%>%filter(season==2024))

# 8. Make predictions
lm_predictions <- predict_with_model(pred_data, final_model, scaled_extended)
lm_predictions <- predict_with_model(pred_data, final_model, scaled_extended)

lm_predictions <- predict_with_model(pred_data, lm_model_extended, scaled_games)
rf_predictions <- predict_with_model(pred_data, rf_model_extended, scaled_games)
# 9. Visualize
plot <- create_prediction_plot(predictions)
plot
```

### Model Comparison and Selection

```r
# Compare any number of models
results <- compare_models(
  "Base LM" = lm_cv_results,
  "Base RF" = rf_cv_results,
  "Extended LM" = lm_extended_cv,
  "Extended RF" = rf_extended_cv
)

# Access specific metrics
best_rmse <- results %>%
  filter(RMSE_Best == TRUE) %>%
  select(Model, RMSE)

# Save comparison
write.csv(results, "model_comparison.csv")
```
## Key Functions

### Data Preparation
- `prepare_weekly()`: Process weekly team statistics
- `prepare_games()`: Create game-level prediction dataset
- `calculate_means()`: Compute rolling averages and season means
- `update_weekly()` / `update_games()`: Update datasets with new games

### Modeling
- `lm_cv()`: Linear model with cross-validation
- `rf_cv()`: Random forest with cross-validation
- `evaluate_formula()`: Assess model variable importance
- `predict_with_model()`: Generate and format predictions

### Visualization
- `create_prediction_plot()`: Create visual game predictions

## Data Requirements

### API Keys
- Required for live odds: Register at the-odds-api.com
- Set your API key before fetching odds:
```r
pred_data <- prepare_predictions("your-api-key-here")
```

## License

This project is licensed under the MIT License.

## Acknowledgments

Built using data from:
- nflfastR (play-by-play data)
- the-odds-api.com (live betting odds)
