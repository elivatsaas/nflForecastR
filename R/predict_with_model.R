#' Predict with Model and Print Results
#'
#' @param prediction_data A data frame containing the prediction data
#' @param model A trained model (either lm or ranger random forest)
#' @param training_data The original training data used to fit the model
#' @return A data frame with the predicted results
#' @importFrom dplyr select mutate
#' @export
predict_with_model <- function(prediction_data, model, training_data) {

  # Get model metrics for ranger
  if (inherits(model, "ranger")) {
    model_cols <- model$forest$independent.variable.names
    rmse <- sqrt(model$prediction.error)  # Convert MSE to RMSE
    error_sd <- rmse
  } else if (inherits(model, "lm")) {
    summary_stats <- summary(model)
    error_sd <- summary_stats$sigma
    model_cols <- attr(terms(model), "term.labels")
    rmse <- error_sd
  } else {
    stop("Unsupported model type")
  }
  prediction_data_subset <- prediction_data[, model_cols, drop = FALSE]


  prediction_data_subset <- prediction_data_subset %>%
    mutate(across(where(is.numeric), scale))
  # Calculate prediction errors using training data
  if (inherits(model, "ranger")) {
    training_preds <- predict(model, data = training_data)$predictions
  } else {
    training_preds <- predict(model, newdata = training_data)
  }
  prediction_errors <- training_data$point_differential - training_preds

  prediction_data_subset <- prediction_data[, model_cols, drop = FALSE]

  prediction_data_subset <- prediction_data_subset %>%
    mutate(across(where(is.numeric), scale))
  # Handle type conversions
  for (col in model_cols) {
    if (col %in% names(prediction_data_subset)) {
      if (is.numeric(model$forest$independent.variable.columns[[col]])) {
        prediction_data_subset[[col]] <- as.numeric(prediction_data_subset[[col]])
      } else if (is.integer(model$forest$independent.variable.columns[[col]])) {
        prediction_data_subset[[col]] <- as.integer(prediction_data_subset[[col]])
      } else if (is.factor(model$forest$independent.variable.columns[[col]])) {
        prediction_data_subset[[col]] <- as.factor(prediction_data_subset[[col]])
      }
    }
  }

  # Check model type and make predictions accordingly
  predictions <- if (inherits(model, "ranger")) {
    predict(model, data = prediction_data_subset)$predictions
  } else {
    predict(model, newdata = prediction_data_subset)
  }

  # Create the result data frame
  result_df <- prediction_data %>%
    mutate(
      predicted_point_differential = predictions,
      chosen_moneyline = ifelse(predicted_point_differential > 0,
                             "Home", "Away"),
      chosen_spread = ifelse(predicted_point_differential > away.spread_line,
                             "Home", "Away"),
      # Calculate win probability based on historical prediction accuracy
      home_win_prob = pnorm(predicted_point_differential,
                            mean = mean(prediction_errors),
                            sd = error_sd)
    ) %>%
    select(home_team, away_team, predicted_point_differential,
           away.spread_line, chosen_moneyline, chosen_spread, home_win_prob)

  # Print formatted results with additional error information
  cat("\nModel Performance on Training Data:\n")
  cat(sprintf("Mean Prediction Error: %.2f\n", mean(prediction_errors)))
  cat(sprintf("Standard Deviation of Prediction Error: %.2f\n", error_sd))
  cat(sprintf("RMSE: %.2f\n", rmse))
  cat("\nPredicted Game Results:\n")
  cat("----------------------------------------\n")
  for(i in 1:nrow(result_df)) {
    cat(sprintf("%s vs %s\n", result_df$away_team[i], result_df$home_team[i]))
    cat(sprintf("Predicted Point Differential: %.1f (positive favors home team)\n",
                result_df$predicted_point_differential[i]))
    cat(sprintf("Vegas Spread: %.1f\n", result_df$away.spread_line[i]))
    cat(sprintf("Model Moneyline Pick: %s\n", result_df$chosen_moneyline[i]))
    cat(sprintf("Model Spread Pick: %s\n", result_df$chosen_spread[i]))
    cat(sprintf("Home Team Win Probability: %.1f%%\n",
                result_df$home_win_prob[i] * 100))
    # Add confidence interval based on error distribution
    ci_lower <- result_df$predicted_point_differential[i] +
      qnorm(0.025) * error_sd
    ci_upper <- result_df$predicted_point_differential[i] +
      qnorm(0.975) * error_sd
    cat(sprintf("95%% Prediction Interval: %.1f to %.1f\n", ci_lower, ci_upper))
    cat("----------------------------------------\n")
  }

  return(result_df)
}
