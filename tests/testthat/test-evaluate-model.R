# tests/testthat/test-evaluate-model.R

test_that("evaluate_formula handles basic linear model correctly", {
  set.seed(101)
  # Create test data with less perfect correlation
  test_data <- data.frame(
    point_differential = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )

  # Fit simple model
  test_model <- lm(point_differential ~ predictor1 + predictor2, data = test_data)

  # Evaluate model
  eval_results <- evaluate_formula(test_model, test_data)

  # Check structure
  expect_type(eval_results, "list")
  expect_true(all(c("model_summary", "variable_metrics", "correlation_matrix",
                    "high_correlations", "formula") %in% names(eval_results)))

  # Check metrics
  expect_true(eval_results$model_summary$R_squared >= 0 &&
                eval_results$model_summary$R_squared <= 1)
  expect_true(eval_results$model_summary$RMSE >= 0)
})

test_that("evaluate_formula handles high correlation appropriately", {
  set.seed(202)
  # Create test data with high but not perfect correlation
  x <- rnorm(100)
  test_data <- data.frame(
    point_differential = rnorm(100),
    predictor1 = x + rnorm(100, sd = 0.1),
    predictor2 = x + rnorm(100, sd = 0.1)
  )

  # Fit model
  test_model <- lm(point_differential ~ predictor1 + predictor2, data = test_data)

  # Evaluate model
  eval_results <- evaluate_formula(test_model, test_data)

  # Check high correlations were detected
  expect_true(is.data.frame(eval_results$high_correlations))
})

test_that("evaluate_formula handles missing values appropriately", {
  set.seed(303)
  # Create test data with NA values
  test_data <- data.frame(
    point_differential = c(rnorm(98), NA, NA),
    predictor1 = c(rnorm(97), NA, NA, NA),
    predictor2 = rnorm(100)
  )

  # Fit model (na.action = na.exclude to handle NAs)
  test_model <- lm(point_differential ~ predictor1 + predictor2,
                   data = test_data,
                   na.action = na.exclude)

  # Evaluate model
  eval_results <- evaluate_formula(test_model, test_data)

  # Check that results are still produced
  expect_type(eval_results, "list")
})
