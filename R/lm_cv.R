#' Perform Cross-Validation with Multiple Options
#'
#' @param formula Model formula
#' @param data Data frame containing the model data
#' @param method Character: "regular" (default), "forward", "backward"
#' @param k Number of folds (default = 5)
#' @param seed Random seed for reproducibility
#' @return List containing CV results and metrics
#' @importFrom caret createFolds
#' @importFrom stats lm predict mad
#' @importFrom ModelMetrics rmse mae
#' @export
lm_cv <- function(formula, data, method = "regular", k = 5, seed = 42) {
  set.seed(seed)

  # Create k folds
  folds <- caret::createFolds(data$point_differential, k = k, list = TRUE)

  # Initialize results storage
  results <- list()
  fold_metrics <- list()

  # Function to calculate metrics
  calculate_metrics <- function(actual, predicted, spread) {
    rmse <- sqrt(mean((predicted - actual)^2))
    mae <- mean(abs(predicted - actual))
    mad <- mad(actual - predicted)
    actual_wins <- ifelse(actual > 0, 1, 0)
    pred_wins <- ifelse(predicted > 0, 1, 0)
    win_accuracy <- mean(actual_wins == pred_wins)
    actual_spread_wins <- ifelse(actual > spread, 1, 0)
    pred_spread_wins <- ifelse(predicted > spread, 1, 0)
    spread_accuracy <- mean(actual_spread_wins == pred_spread_wins)

    list(
      RMSE = rmse,
      MAE = mae,
      MAD = mad,
      WinAccuracy = win_accuracy,
      SpreadAccuracy = spread_accuracy
    )
  }

  # Function to perform stepwise selection
  perform_stepwise <- function(train_data, test_data, direction) {
    if(direction == "forward") {
      step_model <- MASS::stepAIC(lm(point_differential ~ 1, data = train_data),
                                  scope = formula,
                                  direction = "forward",
                                  trace = FALSE)
    } else if(direction == "backward") {
      step_model <- MASS::stepAIC(lm(formula, data = train_data),
                                  direction = "backward",
                                  trace = FALSE)
    }
    pred <- predict(step_model, newdata = test_data)
    return(list(model = step_model, predictions = pred))
  }

  # Perform CV
  for(i in seq_along(folds)) {
    # Split data
    test_idx <- folds[[i]]
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]

    if(method == "regular") {
      # Regular CV
      model <- lm(formula, data = train_data)
      pred <- predict(model, newdata = test_data)
      results[[i]] <- list(model = model, predictions = pred)
    } else if(method %in% c("forward", "backward")) {
      results[[i]] <- perform_stepwise(train_data, test_data, method)
    }

    # Calculate metrics for this fold
    fold_metrics[[i]] <- calculate_metrics(
      actual = test_data$point_differential,
      predicted = results[[i]]$predictions,
      spread = test_data$away.spread_line
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
    method = method,
    models = results,
    fold_metrics = fold_metrics,
    average_metrics = avg_metrics,
    sd_metrics = sd_metrics,
    formula = formula
  )
}
