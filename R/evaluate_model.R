#' Evaluate Linear Model Performance for Formula Variables
#'
#' @param model Fitted linear model object
#' @param data Data frame containing the model data
#' @param target Character: name of target variable (default: "point_differential")
#' @return List containing model evaluation metrics for formula variables only
#' @importFrom stats cor vcov residuals fitted predict coef
#' @importFrom car vif
#' @export
evaluate_formula <- function(model, data, target = "point_differential") {
  # Get variables from model formula
  model_vars <- names(coef(model))[-1] # Exclude intercept

  # Extract only the variables used in the model plus target
  analysis_data <- data[c(target, model_vars)]

  # Calculate predictions
  predictions <- predict(model, newdata = data)
  actuals <- data[[target]]

  # Basic fit metrics
  rmse <- sqrt(mean((predictions - actuals)^2))
  mae <- mean(abs(predictions - actuals))
  r_squared <- summary(model)$r.squared
  adj_r_squared <- summary(model)$adj.r.squared

  # Correlation analysis for model variables only
  correlations <- cor(analysis_data, use = "pairwise.complete.obs")
  target_correlations <- correlations[target, model_vars, drop = FALSE]

  # Variable importance (standardized coefficients)
  scaled_data <- scale(data[model_vars])
  scaled_target <- scale(data[[target]])
  std_model <- lm(scaled_target ~ scaled_data)
  std_coef <- coef(std_model)[-1] # Exclude intercept

  # VIF analysis
  vif_values <- car::vif(model)

  # Predictor cross-correlations
  predictor_cors <- correlations[model_vars, model_vars]
  high_cors <- which(abs(predictor_cors) > 0.7 & predictor_cors != 1, arr.ind = TRUE)

  # Combine variable importance metrics
  var_importance <- data.frame(
    Variable = model_vars,
    Raw_Coefficient = coef(model)[-1],
    Standardized_Coef = std_coef,
    VIF = vif_values,
    Target_Correlation = as.numeric(target_correlations)
  ) %>%
    arrange(desc(abs(Standardized_Coef)))

  # Format high correlations
  if(nrow(high_cors) > 0) {
    high_correlations <- data.frame(
      Variable1 = rownames(predictor_cors)[high_cors[,1]],
      Variable2 = colnames(predictor_cors)[high_cors[,2]],
      Correlation = predictor_cors[high_cors]
    ) %>%
      arrange(desc(abs(Correlation)))
  } else {
    high_correlations <- "No high correlations found (threshold: 0.7)"
  }

  # Return results
  list(
    model_summary = list(
      RMSE = rmse,
      MAE = mae,
      R_squared = r_squared,
      Adj_R_squared = adj_r_squared
    ),
    variable_metrics = var_importance,
    correlation_matrix = correlations[c(target, model_vars), c(target, model_vars)],
    high_correlations = high_correlations,
    formula = formula(model)
  )
}

#' Print method for formula evaluation
#' @param x Formula evaluation object
#' @param digits Number of digits to display
#' @export
print.formula_evaluation <- function(x, digits = 3) {
  cat("\nModel Formula:\n")
  print(x$formula)

  cat("\nModel Fit Metrics:\n")
  print(round(unlist(x$model_summary), digits))

  cat("\nVariable Importance:\n")
  print(round(x$variable_metrics, digits))

  cat("\nCorrelation Matrix:\n")
  print(round(x$correlation_matrix, digits))

  cat("\nHigh Correlations Between Predictors:\n")
  print(x$high_correlations)
}
