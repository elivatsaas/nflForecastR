# tests/testthat/test-predict.R

test_that("predict_with_model generates correct predictions", {
  skip_if_not_installed("ranger")

  # Create training data
  set.seed(42)
  train_data <- data.frame(
    point_differential = rnorm(100),
    feature1 = rnorm(100),
    feature2 = rnorm(100)
  )

  # Create test data
  test_data <- data.frame(
    feature1 = rnorm(10),
    feature2 = rnorm(10),
    home_team = rep("KC", 10),
    away_team = rep("LV", 10),
    away.spread_line = rep(-3, 10)
  )

  # Train model
  rf_model <- ranger::ranger(
    point_differential ~ feature1 + feature2,
    data = train_data,
    importance = "impurity"
  )

  # Make predictions
  predictions <- predict_with_model(test_data, rf_model, train_data)

  # Check structure
  expect_s3_class(predictions, "data.frame")
  expect_true(all(c("home_team", "away_team", "predicted_point_differential",
                    "away.spread_line", "chosen_spread", "home_win_prob") %in% names(predictions)))

  # Check values
  expect_true(all(!is.na(predictions$predicted_point_differential)))
  expect_true(all(predictions$chosen_spread %in% c("Home", "Away")))
  expect_equal(nrow(predictions), 10)
  expect_true(all(predictions$home_win_prob >= 0 & predictions$home_win_prob <= 1))
})

test_that("predict_with_model handles type conversions correctly", {
  skip_if_not_installed("ranger")

  # Create training data with mixed types
  set.seed(42)
  train_data <- data.frame(
    point_differential = rnorm(100),
    numeric_feature = rnorm(100),
    integer_feature = sample(1:100, 100, replace = TRUE),
    factor_feature = factor(sample(letters[1:3], 100, replace = TRUE))
  )

  # Create test data with different types
  test_data <- data.frame(
    numeric_feature = as.character(rnorm(10)),  # wrong type
    integer_feature = as.numeric(sample(1:10, 10)),  # wrong type
    factor_feature = as.character(sample(letters[1:3], 10, replace = TRUE)),  # wrong type
    home_team = rep("KC", 10),
    away_team = rep("LV", 10),
    away.spread_line = rep(-3, 10)
  )

  # Train model
  rf_model <- ranger::ranger(
    point_differential ~ numeric_feature + integer_feature + factor_feature,
    data = train_data,
    importance = "impurity"
  )

  # Should handle type conversions and return predictions
  predictions <- predict_with_model(test_data, rf_model, train_data)
  expect_equal(nrow(predictions), 10)
  expect_true(all(!is.na(predictions$predicted_point_differential)))
  expect_true(all(predictions$home_win_prob >= 0 & predictions$home_win_prob <= 1))
})
