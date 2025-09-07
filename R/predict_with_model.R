#' Enhanced predict_with_model with Interaction Term Support
#'
#' Automatically handles interaction terms for both lm and ranger models and
#' returns win probabilities for the home team.
#'
#' @param prediction_data Data frame of games to predict
#' @param model Fitted `lm` or `ranger` model
#' @param training_data Training data used to fit the model
#' @return A data frame with predictions and `home_win_prob`
#' @importFrom stats pnorm
predict_with_model <- function(prediction_data, model, training_data) {
  
  # Helper function to create interaction terms in data
  create_interactions <- function(data, interaction_terms) {
    for(term in interaction_terms) {
      if(grepl(":", term)) {
        # Split interaction term (e.g., "A:B" -> c("A", "B"))
        variables <- strsplit(term, ":")[[1]]
        
        # Check if all variables exist in data
        if(all(variables %in% names(data))) {
          # Create interaction by multiplying variables
          interaction_values <- data[[variables[1]]]
          for(i in 2:length(variables)) {
            interaction_values <- interaction_values * data[[variables[i]]]
          }
          
          # Add interaction term to data
          data[[term]] <- interaction_values
        } else {
          warning(paste("Cannot create interaction term", term, "- missing variables:", 
                        paste(variables[!variables %in% names(data)], collapse = ", ")))
        }
      }
    }
    return(data)
  }
  
  # Get model information and required columns
  if (inherits(model, "ranger")) {
    model_cols <- model$forest$independent.variable.names
    training_preds <- predict(model, data = training_data)$predictions
  } else if (inherits(model, "lm")) {
    # For lm models, get all terms including interactions
    model_terms <- attr(terms(model), "term.labels")
    model_cols <- model_terms
    training_preds <- predict(model, newdata = training_data)
  } else {
    stop("Unsupported model type. Use 'lm' or 'ranger' models.")
  }
  
  # Identify interaction terms (contain ":")
  interaction_terms <- model_cols[grepl(":", model_cols)]
  base_terms <- model_cols[!grepl(":", model_cols)]
  
  # Create interactions in training data if needed (for lm models)
  if(length(interaction_terms) > 0 && inherits(model, "lm")) {
    cat("Creating", length(interaction_terms), "interaction terms in training data...\n")
    training_data <- create_interactions(training_data, interaction_terms)
  }
  
  # Create interactions in prediction data if needed
  if(length(interaction_terms) > 0) {
    cat("Creating", length(interaction_terms), "interaction terms in prediction data...\n")
    prediction_data <- create_interactions(prediction_data, interaction_terms)
  }
  
  # Check if all required columns exist in prediction data
  missing_cols <- model_cols[!model_cols %in% names(prediction_data)]
  if(length(missing_cols) > 0) {
    stop("Missing required columns in prediction data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Extract prediction columns and make predictions
  prediction_data_subset <- prediction_data[, model_cols, drop = FALSE]
  
  # Make predictions based on model type
  if (inherits(model, "ranger")) {
    predictions <- predict(model, data = prediction_data_subset)$predictions
  } else if (inherits(model, "lm")) {
    predictions <- predict(model, newdata = prediction_data_subset)
  }
  
  # Calculate performance metrics on training data
  rmse <- sqrt(mean((training_data$point_differential - training_preds)^2))
  mae <- mean(abs(training_data$point_differential - training_preds))
  median_ae <- median(abs(training_data$point_differential - training_preds))
  
  # Calculate classification accuracies on training data
  training_moneyline_correct <- mean((training_preds > 0) == (training_data$point_differential > 0))
  
  # Spread accuracy (assuming training_data has away.spread_line)
  if("away.spread_line" %in% names(training_data)) {
    training_spread_correct <- mean(
      (training_preds > training_data$away.spread_line) == 
        (training_data$point_differential > training_data$away.spread_line)
    )
  } else {
    training_spread_correct <- NA
  }
  
  # Estimate spread of residuals for win probability
  sd_res <- stats::sd(training_data$point_differential - training_preds)

  # Create results dataframe
  result_df <- prediction_data %>%
    mutate(
      predicted_point_differential = predictions,
      chosen_moneyline = ifelse(predicted_point_differential > 0, "Home", "Away"),
      chosen_spread = ifelse(predicted_point_differential > away.spread_line, "Home", "Away"),
      home_win_prob = stats::pnorm(predicted_point_differential, mean = 0, sd = sd_res)
    ) %>%
    select(any_of(c("home_team", "away_team")), predicted_point_differential,
           away.spread_line, chosen_moneyline, chosen_spread, home_win_prob)
  
  # Enhanced performance reporting
  cat("\n=== MODEL PERFORMANCE ===\n")
  cat(sprintf("Model Type: %s\n", class(model)[1]))
  cat(sprintf("Features Used: %d (%d base + %d interactions)\n", 
              length(model_cols), length(base_terms), length(interaction_terms)))
  if(length(interaction_terms) > 0) {
    cat("Interaction Terms:", paste(interaction_terms, collapse = ", "), "\n")
  }
  cat(sprintf("Training Games: %d\n", nrow(training_data)))
  cat(sprintf("MAE: %.2f points\n", mae))
  cat(sprintf("Median AE: %.2f points\n", median_ae))
  cat(sprintf("RMSE: %.2f points\n", rmse))
  cat(sprintf("Moneyline Accuracy: %.1f%% (%d/%d)\n", 
              training_moneyline_correct * 100,
              round(training_moneyline_correct * nrow(training_data)),
              nrow(training_data)))
  
  if(!is.na(training_spread_correct)) {
    cat(sprintf("Spread Accuracy: %.1f%% (%d/%d)\n", 
                training_spread_correct * 100,
                round(training_spread_correct * nrow(training_data)),
                nrow(training_data)))
  }
  
  # Show edge games count
  edge_games <- sum(
    (result_df$predicted_point_differential > result_df$away.spread_line & result_df$chosen_spread == "Home") |
      (result_df$predicted_point_differential < result_df$away.spread_line & result_df$chosen_spread == "Away")
  )
  
  cat(sprintf("Current Predictions: %d games\n", nrow(result_df)))
  cat(sprintf("Games with Edge: %d/%d (%.1f%%)\n", 
              edge_games, nrow(result_df), edge_games/nrow(result_df)*100))
  
  cat("\n=== PREDICTIONS ===\n")
  
  for(i in 1:nrow(result_df)) {
    if("home_team" %in% names(result_df) && "away_team" %in% names(result_df)) {
      cat(sprintf("%s @ %s\n", result_df$away_team[i], result_df$home_team[i]))
    } else {
      cat(sprintf("Game %d\n", i))
    }
    cat(sprintf("Predicted: %.1f | Vegas: %.1f | ML: %s | Spread: %s", 
                result_df$predicted_point_differential[i],
                result_df$away.spread_line[i],
                result_df$chosen_moneyline[i],
                result_df$chosen_spread[i]))
    
    # Show edge indicator
    if((result_df$predicted_point_differential[i] > result_df$away.spread_line[i] && result_df$chosen_spread[i] == "Home") ||
       (result_df$predicted_point_differential[i] < result_df$away.spread_line[i] && result_df$chosen_spread[i] == "Away")) {
      cat(" ⭐")
    }
    cat("\n")
    cat(paste(rep("-", 50), collapse=""), "\n")
  }
  
  return(result_df)
}