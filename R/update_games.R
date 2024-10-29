#' Update NFL Game Data
#'
#' @param years Vector of years to update
#' @param replace_existing Logical, whether to replace existing years' data
#' @return Updated game dataset
#' @export
update_games <- function(years, replace_existing = FALSE) {
  # Load stored data
  stored_data <- tidy_games

  # Get max year in stored data
  max_stored_year <- max(stored_data$season)

  # Determine which years to process
  years_to_process <- if(replace_existing) {
    years  # Process all requested years if replacing
  } else {
    years[years > max_stored_year]  # Only process new years
  }

  # Only process data if we have years to process
  if(length(years_to_process) > 0) {
    # Get new data
    weekly_data <- prepare_weekly(min(years_to_process):max(years_to_process))
    new_data <- prepare_games(min(years_to_process), max(years_to_process), weekly_data)

    # Remove years that will be replaced if necessary
    if(replace_existing) {
      stored_data <- stored_data %>%
        filter(!season %in% years_to_process)
    }

    # Combine data
    updated_data <- bind_rows(stored_data, new_data) %>%q
      arrange(season, week, home_team, away_team)

    return(updated_data)
  } else {
    message("No new data to process")
    return(stored_data)
  }
}
