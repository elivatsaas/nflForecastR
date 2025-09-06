#' Prepare NFL Prediction Data (FIXED VERSION)
#'
#' Prepares NFL game data with team statistics and consensus odds for prediction.
#'
#' @param weekly_data Weekly team data from prepare_weekly()
#' @param target_week Optional: specify week to predict (if NULL, finds upcoming week)
#' @param target_season Optional: specify season (if NULL, uses current year)
#' @return A data frame of prepared prediction data
#' @importFrom dplyr inner_join filter select left_join mutate
#' @importFrom lubridate now floor_date with_tz as_date days year
#' @importFrom nflreadr load_schedules
#' @export
prepare_predictions <- function(weekly_data, target_week = NULL, target_season = NULL) {
  
  # Determine target season
  if(is.null(target_season)) {
    current_date <- lubridate::now()
    # If after June 1, use current year, otherwise use previous year
    if(lubridate::month(current_date) >= 6) {
      target_season <- lubridate::year(current_date)
    } else {
      target_season <- lubridate::year(current_date) - 1
    }
  }
  
  cat("Using season:", target_season, "\n")
  
  # Load the correct schedule
  tryCatch({
    schedule <- nflreadr::load_schedules(target_season)
    cat("Loaded schedule with", nrow(schedule), "games\n")
  }, error = function(e) {
    stop("Failed to load schedule for season ", target_season, ": ", e$message)
  })
  
  # Determine target week
  if(is.null(target_week)) {
    # Try to find upcoming week based on current date
    current_datetime <- lubridate::with_tz(lubridate::now(), "US/Eastern")
    current_date <- lubridate::as_date(current_datetime)
    
    # Find the next week with games
    upcoming_games <- schedule %>%
      mutate(gameday = lubridate::as_date(gameday)) %>%
      filter(gameday >= current_date, game_type == "REG") %>%
      arrange(gameday)
    
    if(nrow(upcoming_games) == 0) {
      # No upcoming games - either off-season or season ended
      # Default to Week 1 for testing/demo purposes
      target_week <- 1
      cat("No upcoming games found. Using Week 1 for demonstration.\n")
    } else {
      target_week <- upcoming_games$week[1]
      cat("Found upcoming games in Week", target_week, "\n")
    }
  }
  
  cat("Using week:", target_week, "\n")
  
  # Filter schedule to target week
  week_schedule <- schedule %>%
    filter(week == target_week, game_type == "REG")
  
  if(nrow(week_schedule) == 0) {
    stop("No games found for Week ", target_week, " in season ", target_season)
  }
  
  cat("Found", nrow(week_schedule), "games in Week", target_week, "\n")
  
  # Get the latest data for each team from weekly_data
  latest_data <- weekly_data %>%
    group_by(posteam) %>%
    filter(week == max(week)) %>%
    ungroup()
  
  cat("Latest weekly data from week", max(latest_data$week), "\n")
  
  # Prepare home and away team data
  home_data <- latest_data %>%
    filter(posteam %in% week_schedule$home_team) %>%
    select(-week) %>%
    rename_with(~paste0("home.", .), -posteam) %>%
    rename(home_team = posteam)
  
  away_data <- latest_data %>%
    filter(posteam %in% week_schedule$away_team) %>%
    select(-week) %>%
    rename_with(~paste0("away.", .), -posteam) %>%
    rename(away_team = posteam)
  
  # Start building prediction data with schedule
  prediction_data <- week_schedule %>%
    select(game_id, season, week, gameday, away_team, home_team, 
           spread_line, total_line, div_game) %>%
    # Convert nflreadr column names to match your expected format
    rename(
      Date = gameday,
      away.spread_line = spread_line,
      away.total_line = total_line
    ) %>%
    mutate(
      Date = lubridate::as_date(Date)
    )
  
  # Join with team data
  prediction_data <- prediction_data %>%
    left_join(home_data, by = "home_team") %>%
    left_join(away_data, by = "away_team")
  
  # Try to get consensus odds (with error handling)
  tryCatch({
    odds_data <- get_nfl_odds()
    if(nrow(odds_data) > 0) {
      consensus_odds <- calculate_consensus_odds(odds_data)
      
      # Map team names to match our format if needed
      consensus_odds <- consensus_odds %>%
        mutate(
          away_team = sapply(away_team, function(x) {
            if(x %in% names(NFL_TEAM_MAPPINGS)) NFL_TEAM_MAPPINGS[x] else x
          }),
          home_team = sapply(home_team, function(x) {
            if(x %in% names(NFL_TEAM_MAPPINGS)) NFL_TEAM_MAPPINGS[x] else x
          })
        )
      
      # Update with live odds if available
      prediction_data <- prediction_data %>%
        left_join(
          consensus_odds %>%
            select(home_team, away_team, spread, total) %>%
            rename(
              live_spread = spread,
              live_total = total
            ),
          by = c("home_team", "away_team")
        ) %>%
        # Use live odds if available, otherwise keep schedule odds
        mutate(
          away.spread_line = ifelse(!is.na(live_spread), live_spread, away.spread_line),
          away.total_line = ifelse(!is.na(live_total), live_total, away.total_line)
        ) %>%
        select(-live_spread, -live_total)
      
      cat("Updated with live odds for", sum(!is.na(consensus_odds$spread)), "games\n")
    }
  }, error = function(e) {
    cat("Could not fetch live odds:", e$message, "\n")
    cat("Using schedule odds instead\n")
  })
  
  # Filter to future games only (if using current date logic)
  if(is.null(target_week)) {
    current_datetime <- lubridate::with_tz(lubridate::now(), "US/Eastern")
    prediction_data <- prediction_data %>%
      filter(Date > lubridate::as_date(current_datetime))
  }
  prediction_data <- prediction_data %>%
    # Convert from nflverse_data to regular tibble  
    as_tibble() %>%
    # Ensure numeric columns are properly typed
    mutate(across(where(is.character), ~ ifelse(. == "", NA, .))) %>%
    mutate(across(contains(c("epa", "points", "yards", "spread", "total")), as.numeric)) %>%
    # Remove any problematic attributes
    as_tibble()
  
  # Ensure required columns exist and are properly formatted
  required_cols <- c("season", "week", "home_team", "away_team", "away.spread_line", "away.total_line")
  missing_cols <- required_cols[!required_cols %in% names(prediction_data)]
  
  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  cat("Successfully prepared", nrow(prediction_data), "games for prediction\n")
  return(prediction_data)
}
