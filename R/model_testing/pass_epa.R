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
  point_differential ~ home.pass_epa_cum + home.def.pass_epa_cum +
    away.pass_epa_cum + away.def.pass_epa_cum,
  data = scaled_games
)


# 2b. Train and evaluate random forest
rf_cv_results <- rf_cv(
  point_differential ~ home.pass_epa_cum + home.def.pass_epa_cum +
    away.pass_epa_cum + away.def.pass_epa_cum,
  data = scaled_games
)


# 3. Train final models
lm_model <- lm(
  point_differential ~ home.pass_epa_cum + home.def.pass_epa_cum +
    away.pass_epa_cum + away.def.pass_epa_cum,
  data = scaled_games
)

rf_model <- ranger(
  point_differential ~ home.pass_epa_cum + home.def.pass_epa_cum +
    away.pass_epa_cum + away.def.pass_epa_cum,
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
  "Base RF" = rf_cv_results
)

# Access specific metrics
best_rmse <- results %>%
  filter(RMSE_Best == TRUE) %>%
  select(Model, RMSE)

# Save comparison
write.csv(results, "model_comparison.csv")
