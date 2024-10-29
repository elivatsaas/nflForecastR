# tests/testthat/test-lm-cv.R

test_that("lm_cv performs basic cross-validation correctly", {
  # Create test data
  set.seed(42)
  test_data <- data.frame(
    point_differential = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )

  # Test formula
  test_formula <- point_differential ~ predictor1 + predictor2

  # Run cross-validation
  cv_results <- lm_cv(test_formula, data = test_data, method = "regular", k = 5)

  # Check structure
  expect_type(cv_results, "list")
  expect_true(all(c("method", "models", "fold_metrics", "average_metrics",
                    "sd_metrics", "formula") %in% names(cv_results)))

  # Check metrics
  expect_true(cv_results$average_metrics$RMSE > 0)
  expect_true(cv_results$average_metrics$MAE > 0)
  expect_true(cv_results$average_metrics$WinAccuracy >= 0 &&
                cv_results$average_metrics$WinAccuracy <= 1)
})

test_that("lm_cv handles different methods correctly", {
  # Create test data
  set.seed(42)
  test_data <- data.frame(
    point_differential = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    predictor3 = rnorm(100),
    away.spread_line = rnorm(100)  # Added required column
  )

  test_formula <- point_differential ~ predictor1 + predictor2 + predictor3

  # Test different methods
  methods <- c("regular", "forward", "backward")  # Simplified methods list

  for(method in methods) {
    cv_results <- lm_cv(test_formula, data = test_data, method = method, k = 5)
    expect_equal(cv_results$method, method)
    expect_type(cv_results$models, "list")
    expect_length(cv_results$models, 5) # k=5 folds
  }
})

test_that("lm_cv respects seed parameter", {
  # Create test data
  test_data <- data.frame(
    point_differential = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    away.spread_line = rnorm(100)  # Added required column
  )

  test_formula <- point_differential ~ predictor1 + predictor2

  # Run CV twice with same seed
  result1 <- lm_cv(test_formula, data = test_data, seed = 42)
  result2 <- lm_cv(test_formula, data = test_data, seed = 42)

  # Results should be identical
  expect_equal(result1$average_metrics, result2$average_metrics)
})

test_that("lm_cv handles edge cases", {
  # Test with minimal data
  small_data <- data.frame(
    point_differential = c(1, 2, 3, 4, 5),
    predictor = c(1, 2, 3, 4, 5),
    away.spread_line = c(0, 0, 0, 0, 0)  # Added required column
  )

  small_formula <- point_differential ~ predictor

  # Should still work with small dataset and k=2
  small_result <- lm_cv(small_formula, small_data, k = 2)
  expect_type(small_result, "list")
  expect_true(!is.null(small_result$average_metrics))
})
