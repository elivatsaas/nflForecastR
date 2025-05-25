#' Run Random Forest with Cross-Validation and Evaluation Metrics
#'
#' @param formula A formula specifying the model structure
#' @param data A data frame containing the variables used in the formula
#' @return A list containing the cross-validation results and evaluation metrics
#' @importFrom caret train trainControl createFolds
#' @importFrom ranger ranger
#' @importFrom ModelMetrics rmse mae
#' @importFrom stats mad predict
#' @export
rf_cv <- function(formula, data, k = 5, seed = 42) {
  set.seed(seed)

  # Create k folds
  folds <- caret::createFolds(data$point_differential, k = k, list = TRUE)

  # Initialize storage for results
  results <- list()
  fold_metrics <- list()

  # Perform k-fold CV
  for(i in seq_along(folds)) {
    # Split data
    test_idx <- folds[[i]]
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]

    # Train model on training data
    rf_model <- ranger::ranger(
      formula = formula,
      data = train_data,
      num.trees = 500,
      seed = seed
    )

    # Make predictions on test data
    predictions <- predict(rf_model, data = test_data)$predictions

    # Store model and predictions
    results[[i]] <- list(model = rf_model, predictions = predictions)

    # Calculate metrics on test data
    fold_metrics[[i]] <- list(
      RMSE = sqrt(mean((predictions - test_data$point_differential)^2)),
      MAE = mean(abs(predictions - test_data$point_differential)),
      MAD = mad(test_data$point_differential - predictions),
      WinAccuracy = mean(
        (predictions > 0) == (test_data$point_differential > 0)
      ),
      SpreadAccuracy = mean(
        (predictions > test_data$away.spread_line) ==
          (test_data$point_differential > test_data$away.spread_line)
      )
    )
  }

  # Calculate average metrics across folds
  avg_metrics <- list(
    RMSE = mean(sapply(fold_metrics, function(x) x$RMSE)),
    MAE = mean(sapply(fold_metrics, function(x) x$MAE)),
    MAD = mean(sapply(fold_metrics, function(x) x$MAD)),
    WinAccuracy = mean(sapply(fold_metrics, function(x) x$WinAccuracy)),
    SpreadAccuracy = mean(sapply(fold_metrics, function(x) x$SpreadAccuracy))
  )

  # Calculate standard deviations of metrics
  sd_metrics <- list(
    RMSE = sd(sapply(fold_metrics, function(x) x$RMSE)),
    MAE = sd(sapply(fold_metrics, function(x) x$MAE)),
    MAD = sd(sapply(fold_metrics, function(x) x$MAD)),
    WinAccuracy = sd(sapply(fold_metrics, function(x) x$WinAccuracy)),
    SpreadAccuracy = sd(sapply(fold_metrics, function(x) x$SpreadAccuracy))
  )

  # Return results
  list(
    method = "random_forest",
    models = results,
    fold_metrics = fold_metrics,
    average_metrics = avg_metrics,
    sd_metrics = sd_metrics,
    formula = formula
  )
}
