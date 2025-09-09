#' Update NFL Game Data
#'
#' Builds/extends the modeling frame using prepare_weekly()/prepare_games().
#' - Optionally replaces stored years
#' - Includes injuries / coaching / referees
#' - Seeds Week 1 with prior-season finals when requested
#' - Ensures `away.spread_line` is present (aligned with "away-oriented" spread checks)
#'
#' @param years integer vector of seasons to (re)build, e.g. 2015:2024
#' @param replace_existing logical; if TRUE, rebuild exactly `years` from source
#' @param seed_week1 logical; carry prior-season stats into Week 1 lags
#' @param include_injuries logical; join injury impacts
#' @param include_coaching logical; join coach tiers + lagged QB stability
#' @param include_referee logical; keep referee metadata
#' @param return_all logical; if TRUE (default), return full dataset
#'   (stored +/- rebuilt). If FALSE, return only the `years` you asked for.
#' @return data.frame of updated games
#' @import dplyr tidyr purrr
#' @export
update_games <- function(years,
                         replace_existing = FALSE,
                         seed_week1       = TRUE,
                         include_injuries = TRUE,
                         include_coaching = TRUE,
                         include_referee  = TRUE,
                         return_all       = TRUE) {

  years <- sort(unique(years))
  
  # --- Load stored data if available ---
  stored_data <- tryCatch(nflForecastR::tidy_games, error = function(e) NULL)
  if (!is.null(stored_data) && !nrow(stored_data)) stored_data <- NULL
  
  # --- Determine which years to process ---
  if (replace_existing || is.null(stored_data)) {
    years_to_process <- years
  } else {
    max_stored_year <- max(stored_data$season, na.rm = TRUE)
    years_to_process <- years[years > max_stored_year]
  }
  
  # If nothing to process, optionally still normalize away.spread_line and return
  if (length(years_to_process) == 0) {
    message("No new data to process")
    res <- stored_data
    if (!is.null(res) && "spread_line" %in% names(res)) {
      # Force away orientation even if column existed
      res <- res %>% mutate(away.spread_line = -spread_line)
    }
    if (!return_all && !is.null(res)) {
      res <- res %>% filter(season %in% years)
    }
    return(res %>% arrange(season, week, home_team, away_team))
  }
  
  message("Rebuilding years: ", paste(years_to_process, collapse = ", "))
  
  # --- Build weekly features ---
  weekly_years <- if (seed_week1) {
    # need prior season for carryover into first requested season
    (min(years_to_process) - 1L):max(years_to_process)
  } else {
    min(years_to_process):max(years_to_process)
  }
  # guard: allow previous season for carry-over when seeding Week 1
  weekly_years <- weekly_years[weekly_years >= (min(years) - 1L)]
  
  weekly_data <- prepare_weekly(weekly_years)
  
  # --- Build games for the span we’re rebuilding ---
  new_data <- prepare_games(
    start_year       = min(years_to_process),
    end_year         = max(years_to_process),
    weekly_data      = weekly_data,
    include_injuries = include_injuries,
    include_coaching = include_coaching,
    include_referee  = include_referee,
    seed_week1       = seed_week1
  )
  
  # Force `away.spread_line` orientation (even if present from upstream)
  if ("spread_line" %in% names(new_data)) {
    new_data <- new_data %>% mutate(away.spread_line = -spread_line)
  }
  
  # --- Combine with stored data per mode ---
  if (!is.null(stored_data)) {
    if (replace_existing) {
      stored_data <- stored_data %>% filter(!season %in% years)
    }
    combined <- bind_rows(stored_data, new_data)
  } else {
    combined <- new_data
  }
  
  combined <- combined %>%
    distinct(season, week, home_team, away_team, game_id, .keep_all = TRUE) %>%
    arrange(season, week, home_team, away_team)
  
  if (return_all) {
    combined
  } else {
    combined %>% filter(season %in% years)
  }
}
