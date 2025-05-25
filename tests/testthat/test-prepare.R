# tests/testthat/test-prepare.R

test_that("prepare_games works with valid input", {
  skip_if_not_installed("nflfastR")

  # Create test weekly data
  test_weekly <- data.frame(
    season = rep(2024, 10),
    week = rep(1:2, each = 5),
    posteam = rep(c("KC", "LV", "LAC", "DEN", "GB"), 2),
    points_scored = rnorm(10, 24, 7),
    points_allowed = rnorm(10, 21, 7),
    spread_line = rep(c(-3, 7, -1, 2.5, -4), 2),  # Fixed length
    total_line = rep(c(48.5, 47, 44.5, 46, 49.5), 2),  # Fixed length
    div_game = rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), 2),  # Fixed length
    posteam_type = rep(c("home", "away", "home", "away", "home"), 2),  # Fixed length
    game_id = paste0(
      "2024_",
      rep(1:2, each = 5),
      "_",
      rep(c("KC_LV", "LV_LAC", "LAC_DEN", "DEN_GB", "GB_KC"), 2)
    ),
    stringsAsFactors = FALSE
  )

  result <- prepare_games(2024, 2024, test_weekly)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("season", "week", "home_team", "away_team") %in% names(result)))

  # Check dimensions
  expect_gt(nrow(result), 0)
  expect_true(all(result$season == 2024))
  expect_true(all(result$week %in% 1:2))
})

test_that("prepare_games handles missing data appropriately", {
  # Create test weekly data with some NAs
  test_weekly <- data.frame(
    season = rep(2024, 10),
    week = rep(1:2, each = 5),
    posteam = rep(c("KC", "LV", "LAC", "DEN", "GB"), 2),
    points_scored = c(rnorm(8), NA, NA),
    points_allowed = c(rnorm(7), NA, NA, NA),
    spread_line = rep(c(-3, 7, -1, 2.5, -4), 2),
    total_line = rep(c(48.5, 47, 44.5, 46, 49.5), 2),
    div_game = rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), 2),
    posteam_type = rep(c("home", "away", "home", "away", "home"), 2),
    game_id = paste0(
      "2024_",
      rep(1:2, each = 5),
      "_",
      rep(c("KC_LV", "LV_LAC", "LAC_DEN", "DEN_GB", "GB_KC"), 2)
    ),
    stringsAsFactors = FALSE
  )

  result <- prepare_games(2024, 2024, test_weekly)

  # Check that NAs are handled
  expect_true(!any(is.na(result$season)))
  expect_true(!any(is.na(result$week)))
  expect_true(!any(is.na(result$home_team)))
  expect_true(!any(is.na(result$away_team)))

  # Additional checks for data integrity
  expect_true(all(result$home_team %in% c("KC", "LV", "LAC", "DEN", "GB")))
  expect_true(all(result$away_team %in% c("KC", "LV", "LAC", "DEN", "GB")))
})
