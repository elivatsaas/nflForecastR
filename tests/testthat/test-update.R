# tests/testthat/test-update.R

test_that("update_games handles invalid inputs", {
  # Create mock stored data
  mock_stored_data <- data.frame(
    season = rep(2020:2022, each = 5),
    week = rep(1:5, times = 3),
    home_team = rep(c("KC", "LV", "LAC", "DEN", "GB"), 3),
    away_team = rep(c("LV", "LAC", "DEN", "GB", "KC"), 3),
    stringsAsFactors = FALSE
  )

  # Create mock test weekly data
  mock_weekly_data <- data.frame(
    season = rep(2020:2022, each = 5),
    week = rep(1:5, times = 3),
    posteam = rep(c("KC", "LV", "LAC", "DEN", "GB"), 3),
    points_scored = rnorm(15, 24, 7),
    points_allowed = rnorm(15, 21, 7),
    spread_line = rep(c(-3, 7, -1, 2.5, -4), 3),
    total_line = rep(c(48.5, 47, 44.5, 46, 49.5), 3),
    div_game = rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), 3),
    posteam_type = rep(c("home", "away", "home", "away", "home"), 3),
    game_id = paste0(
      rep(2020:2022, each = 5), "_",
      rep(1:5, times = 3), "_",
      rep(c("KC_LV", "LV_LAC", "LAC_DEN", "DEN_GB", "GB_KC"), 3)
    ),
    stringsAsFactors = FALSE
  )

  # Mock the prepare functions
  mockery::stub(update_games, "prepare_weekly", function(years) {
    mock_weekly_data[mock_weekly_data$season %in% years, ]
  })

  mockery::stub(update_games, "prepare_games", function(start_year, end_year, weekly_data) {
    mock_stored_data[mock_stored_data$season %in% start_year:end_year, ]
  })

  # Temporarily assign mock data
  old_games <- nflForecastR::tidy_games
  assign("tidy_games", mock_stored_data, envir = parent.frame())

  # Run test
  expect_message(
    result <- update_games(2021),
    "No new data to process"
  )


  # Restore original data
  assign("tidy_games", old_games, envir = parent.frame())
})

test_that("update_weekly handles new years correctly", {
  skip_if_not_installed("nflfastR")
  skip("Requires nflfastR API")
})

test_that("update_weekly handles replacement correctly", {
  skip_if_not_installed("nflfastR")
  skip("Requires nflfastR API")
})

test_that("both update functions maintain data integrity", {
  skip_if_not_installed("nflfastR")
  skip("Requires nflfastR API")
})
