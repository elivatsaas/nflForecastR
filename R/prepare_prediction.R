#' Prepare NFL Prediction Data
#'
#' Prepares NFL game data with team statistics and consensus odds for prediction.
#'
#' @param api_key Character string containing your API key for the-odds-api.com
#' @return A data frame of prepared prediction data
#' @importFrom dplyr inner_join filter select left_join
#' @importFrom lubridate now floor_date with_tz as_date
#' @export
prepare_predictions <- function(weekly_data) {
  # Get the current date and time in the US/Eastern time zone
  current_datetime <- lubridate::with_tz(lubridate::now(), "US/Eastern")

  # Check if it's Sunday before 1 PM EST
  is_sunday <- lubridate::wday(current_datetime) == 1

  # Find the upcoming Sunday
  upcoming_sunday <- if (is_sunday) {
    lubridate::floor_date(current_datetime, "day")
  } else {
    lubridate::ceiling_date(current_datetime, "week") + 7
  }

  data("nfl_schedule_2024")

  schedule <- nfl_schedule_2024 %>%
    mutate(Date = as_date(Date)) %>%
    filter(Date >= upcoming_sunday - days(1), Date <= upcoming_sunday + days(1)) %>%
    slice(1)


  upcoming_week <- schedule$week
  upcoming_schedule <- nfl_schedule_2024 %>%
    filter(week == upcoming_week)


  # Get the latest data for each team
  latest_data <- weekly_data %>%
    group_by(posteam) %>%
    filter(week == max(week)) %>%
    ungroup()

  # Prepare home and away team data
  home_data <- latest_data %>%
    filter(posteam %in% upcoming_schedule$home_team) %>%
    select(-week) %>%
    rename_with(~paste0("home.", .), -posteam) %>%
    rename(home_team = posteam)

  away_data <- latest_data %>%
    filter(posteam %in% upcoming_schedule$away_team) %>%
    select(-week) %>%
    rename_with(~paste0("away.", .), -posteam) %>%
    rename(away_team = posteam)

  # Get consensus odds
  odds_data <- get_nfl_odds()
  consensus_odds <- calculate_consensus_odds(odds_data)

  # Map team names to match our format
  consensus_odds <- consensus_odds %>%
    mutate(
      away_team = sapply(away_team, function(x) NFL_TEAM_MAPPINGS[x]),
      home_team = sapply(home_team, function(x) NFL_TEAM_MAPPINGS[x])
    )

  # Filter consensus odds to only include games from the upcoming week
  consensus_odds_upcoming <- consensus_odds %>%
    inner_join(upcoming_schedule, by = c("home_team", "away_team"))

  # Remove existing spread and total columns before joining
  prediction_data <- upcoming_schedule %>%
    left_join(
      home_data %>% select(-matches("away.spread_line|away.total_line")),
      by = "home_team"
    ) %>%
    left_join(
      away_data %>% select(-matches("away.spread_line|away.total_line")),
      by = "away_team"
    )
  # Now add the odds data with renamed columns
  prediction_data <- prediction_data %>%
    left_join(
      consensus_odds_upcoming %>%
        select(home_team, away_team, spread, total) %>%
        rename(
          away.total_line = total
        ),
      by = c("home_team", "away_team")
    )
  prediction_data$away.spread_line <- prediction_data$spread * -1


  prediction_data <- prediction_data %>%
    filter(Date > current_datetime)

  prediction_data$season <- 2024
  prediction_data$week <- upcoming_week

  prediction_data$div_game <- as.numeric(
    case_when(
      # AFC East
      prediction_data$home_team %in% c("BUF", "MIA", "NE", "NYJ") &
        prediction_data$away_team %in% c("BUF", "MIA", "NE", "NYJ") ~ TRUE,
      # AFC North
      prediction_data$home_team %in% c("BAL", "CIN", "CLE", "PIT") &
        prediction_data$away_team %in% c("BAL", "CIN", "CLE", "PIT") ~ TRUE,
      # AFC South
      prediction_data$home_team %in% c("HOU", "IND", "JAX", "TEN") &
        prediction_data$away_team %in% c("HOU", "IND", "JAX", "TEN") ~ TRUE,
      # AFC West
      prediction_data$home_team %in% c("DEN", "KC", "LV", "LAC") &
        prediction_data$away_team %in% c("DEN", "KC", "LV", "LAC") ~ TRUE,
      # NFC East
      prediction_data$home_team %in% c("DAL", "NYG", "PHI", "WAS") &
        prediction_data$away_team %in% c("DAL", "NYG", "PHI", "WAS") ~ TRUE,
      # NFC North
      prediction_data$home_team %in% c("CHI", "DET", "GB", "MIN") &
        prediction_data$away_team %in% c("CHI", "DET", "GB", "MIN") ~ TRUE,
      # NFC South
      prediction_data$home_team %in% c("ATL", "CAR", "NO", "TB") &
        prediction_data$away_team %in% c("ATL", "CAR", "NO", "TB") ~ TRUE,
      # NFC West
      prediction_data$home_team %in% c("ARI", "LA", "SF", "SEA") &
        prediction_data$away_team %in% c("ARI", "LA", "SF", "SEA") ~ TRUE,
      TRUE ~ FALSE
    )
  )
  return(prediction_data)
}
