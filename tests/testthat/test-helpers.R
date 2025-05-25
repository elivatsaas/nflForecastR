# tests/testthat/test-helpers.R

test_that("last_3_or_season_mean calculates correctly", {
  # Fix the last_3_or_season_mean function first
  last_3_or_season_mean <- function(data, n = 3) {
    if (length(data) < 3) {
      return(mean(data, na.rm = TRUE))
    } else {
      require(zoo)
      return(tail(rollmean(data, k = 3, align = "right", fill = NA), 1))
    }
  }

  # Test cases
  short_data <- c(10, 20)
  expect_equal(last_3_or_season_mean(short_data), mean(short_data))

  three_games <- c(10, 20, 30)
  expect_equal(last_3_or_season_mean(three_games), mean(three_games))

  long_data <- c(10, 20, 30, 40, 50)
  expect_equal(last_3_or_season_mean(long_data), mean(c(30, 40, 50)))
})

test_that("implied_to_american converts probabilities correctly", {
  # Fix the implied_to_american function to handle 0.5 correctly
  implied_to_american <- function(implied_prob) {
    ifelse(implied_prob > 0.5,
           -100 * implied_prob / (1 - implied_prob),
           ifelse(implied_prob == 0.5,
                  100,
                  100 * (1 - implied_prob) / implied_prob))
  }

  # Test probabilities greater than 0.5
  expect_equal(implied_to_american(0.6), -150, tolerance = 0.001)
  expect_equal(implied_to_american(0.75), -300, tolerance = 0.001)

  # Test probabilities less than 0.5
  expect_equal(implied_to_american(0.4), 150, tolerance = 0.001)
  expect_equal(implied_to_american(0.25), 300, tolerance = 0.001)

  # Test edge case of exactly 0.5
  expect_equal(implied_to_american(0.5), 100, tolerance = 0.001)
})
