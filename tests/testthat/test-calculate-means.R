# tests/testthat/test-calculate-means.R

test_that("calculate_means handles numeric columns correctly", {
  # Create test data
  test_data <- data.frame(
    season = rep(2024, 3),
    posteam = rep("KC", 3),
    week = 1:3,
    points = c(24, 28, 31),
    yards = c(350, 380, 400),
    stringsAsFactors = FALSE
  )

  result <- calculate_means(test_data)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("points_last_3", "points_season_mean", "points_momentum") %in% names(result)))
  expect_true(all(c("yards_last_3", "yards_season_mean", "yards_momentum") %in% names(result)))

  # Check calculations
  expect_equal(result$points_last_3[3], mean(c(24, 28, 31)))
  expect_equal(result$points_season_mean[3], mean(c(24, 28, 31)))
})

test_that("calculate_means handles missing values appropriately", {
  # Create test data with NAs
  test_data <- data.frame(
    season = rep(2024, 4),
    posteam = rep("KC", 4),
    week = 1:4,
    points = c(24, NA, 31, 28),
    yards = c(350, 380, NA, 400),
    stringsAsFactors = FALSE
  )

  result <- calculate_means(test_data)

  # Check that NAs are handled properly
  expect_false(any(is.na(result$points_season_mean)))
  expect_false(any(is.na(result$yards_season_mean)))
})
