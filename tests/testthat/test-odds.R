# tests/testthat/test-odds.R

test_that("calculate_consensus_odds produces correct consensus values", {
  # Create test data with multiple games
  test_odds <- data.frame(
    game_id = rep(c("123", "456", "789"), each = 3),
    home_team = rep(c("ARI", "GB", "MIN"), each = 3),
    away_team = rep(c("CHI", "LA", "WAS"), each = 3),
    bookmaker = rep(c("fanduel", "draftkings", "betmgm"), 3),
    home_ml = c(-300, -305, -310, -280, -285, -290, -320, -325, -330),
    away_ml = c(240, 245, 250, 220, 225, 230, 260, 265, 270),
    spread = c(-7.5, -7.5, -7, -6.5, -6.5, -6, -8.5, -8.5, -8),
    spread_price = c(-110, -110, -115, -110, -110, -115, -110, -110, -115),
    total = c(47.5, 47.5, 47, 45.5, 45.5, 45, 49.5, 49.5, 49),
    total_over_price = c(-110, -110, -110, -110, -110, -110, -110, -110, -110),
    total_under_price = c(-110, -110, -110, -110, -110, -110, -110, -110, -110),
    stringsAsFactors = FALSE
  )

  result <- calculate_consensus_odds(test_odds)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)  # Should have one row per unique game_id

  # Check consensus calculations for first game
  first_game <- result[result$game_id == "123", ]
  expect_equal(first_game$home_ml, -305)
  expect_equal(first_game$spread, -7.5)
  expect_equal(first_game$total, 47.5)
  expect_equal(first_game$bookmaker_count, 3)
})

test_that("calculate_consensus_odds respects minimum bookmaker threshold", {
  # Create test data with varying bookmaker counts
  test_odds <- data.frame(
    game_id = c(rep("123", 3), rep("456", 2)),  # 3 bookmakers for game 123, 2 for game 456
    home_team = c(rep("ARI", 3), rep("GB", 2)),
    away_team = c(rep("CHI", 3), rep("LA", 2)),
    bookmaker = c("fanduel", "draftkings", "betmgm", "fanduel", "draftkings"),
    home_ml = c(-300, -305, -310, -280, -285),
    away_ml = c(240, 245, 250, 220, 225),
    spread = c(-7.5, -7.5, -7, -6.5, -6.5),
    spread_price = c(-110, -110, -115, -110, -110),
    total = c(47.5, 47.5, 47, 45.5, 45.5),
    total_over_price = c(-110, -110, -110, -110, -110),
    total_under_price = c(-110, -110, -110, -110, -110),
    stringsAsFactors = FALSE
  )

  result <- calculate_consensus_odds(test_odds, min_bookmakers = 3)

  # Should only return game with 3+ bookmakers
  expect_equal(nrow(result), 1)
  expect_equal(result$game_id, "123")
})

test_that("calculate_consensus_odds handles NA values correctly", {
  # Create test data with NA values
  test_odds <- data.frame(
    game_id = rep("123", 3),
    home_team = rep("ARI", 3),
    away_team = rep("CHI", 3),
    bookmaker = c("fanduel", "draftkings", "betmgm"),
    home_ml = c(-300, NA, -310),
    away_ml = c(240, 245, NA),
    spread = c(-7.5, NA, -7),
    spread_price = c(NA, -110, -115),
    total = c(47.5, NA, 47),
    total_over_price = c(-110, -110, NA),
    total_under_price = c(-110, NA, -110),
    stringsAsFactors = FALSE
  )

  result <- calculate_consensus_odds(test_odds)

  # Check that medians are calculated correctly even with NA values
  expect_false(any(is.na(result[, c("home_ml", "spread", "total")])))
  expect_equal(result$bookmaker_count, 3)
})

# Skip API tests for now as they need mock setup
test_that("get_nfl_odds handles successful API response", {
  skip("API mocking needs to be updated to use testthat3")
})

test_that("get_nfl_odds handles API errors appropriately", {
  skip("API mocking needs to be updated to use testthat3")
})
