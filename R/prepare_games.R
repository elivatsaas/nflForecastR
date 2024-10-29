#' Prepare NFL Schedule Data
#'
#' Prepares NFL schedule data with team statistics for prediction.
#'
#' @param start_year Starting season year
#' @param end_year Ending season year
#' @param weekly_data dataframe produced by "prepare_weekly"
#' @return A data frame of prepared schedule data
#' @importFrom dplyr distinct mutate left_join
#' @examples
#' \dontrun{
#' schedule_data <- prepare_games(2024, 2024)
#' }
#' @export
prepare_games <- function(start_year, end_year, weekly_data) {
    # Process NFL data

    # Create complete schedule of weeks
    all_weeks <- expand.grid(
      season = unique(weekly_data$season),
      week = 1:18,  # Or adjust based on season
      posteam = unique(weekly_data$posteam),
      stringsAsFactors = FALSE
    )

    # Join and fill data
    weekly_data <- all_weeks %>%
      left_join(weekly_data, by = c("season", "week", "posteam")) %>%
      group_by(season, posteam) %>%
      arrange(season, week) %>%
      fill(everything(), .direction = "down") %>%
      ungroup()

    # Create the schedule from weekly_data
    schedule <- weekly_data %>%
      distinct(season, week, game_id) %>%
      mutate(
        away_team = str_extract(game_id, "(?<=)[A-Z]+(?=_[A-Z]+$)"),
        home_team = str_extract(game_id, "[A-Z]+$"),
        away_team = map_team_abbreviation(away_team),
        home_team = map_team_abbreviation(home_team)
      ) %>%
      group_by(game_id) %>%
      slice(1) %>%
      ungroup()




    prepare_team_data <- function(data, team_type) {
      data %>%
        arrange(season, week) %>%
        group_by(season, posteam) %>%
        mutate(across(
          .cols = -c(week, game_id, points_scored, points_allowed, spread_line, total_line, div_game, posteam_type),
          ~if(cur_column() %in% c("points_scored", "points_allowed")) {
            .
          } else {
            coalesce(lead(.), .)
          }
        )) %>%
        ungroup() %>%
        rename_with(~paste0(team_type, ".", .), -c(season, week, posteam)) %>%
        rename(!!paste0(team_type, "_team") := posteam)
    }

    # Prepare home and away team data
    home_data <- prepare_team_data(weekly_data, "home")
    away_data <- prepare_team_data(weekly_data, "away")

    # Join schedule with team data
    schedule_with_data <- schedule %>%
      left_join(home_data, by = c("season", "week", "home_team")) %>%
      left_join(away_data, by = c("season", "week", "away_team"))

    # Calculate point differential
    schedule_with_data <- schedule_with_data %>%
      mutate(point_differential = home.points_scored - away.points_scored,
             div_game = home.div_game | away.div_game,
             total = home.points_scored + away.points_scored
      )

    final_data <- schedule_with_data %>%
                  select(-home.points_scored, -away.points_scored, -home.points_allowed, -away.points_allowed)

    season_max <- max(final_data$season)
    weekly_max <- final_data %>%
      filter(season == season_max) %>%
      pull(week) %>%
      max(na.rm = TRUE)

      final_data <- final_data %>%
      filter(!(season == season_max & week > weekly_max) | week == 1)

    return(final_data)
  }
