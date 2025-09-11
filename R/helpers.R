#' Calculate Last 3 Games Average
#' @keywords internal
#' Calculate mean of last \code{n} games or of the entire vector when fewer than \code{n}
#' games are available.
#'
#' @param data Numeric vector containing statistics
#' @param n Number of games to look back (default 3)
#' @return Numeric mean value
#' @importFrom utils tail
#' @importFrom zoo rollmean
#' @export
last_3_or_season_mean <- function(data, n = 3) {
  if (length(data) < n) {
    mean(data, na.rm = TRUE)
  } else {
    tail(rollmean(data, k = n, align = "right", fill = NA), 1)
  }
}

#' Map Team Abbreviations (single source of truth)
#' @keywords internal
#' @param abbr character vector of team abbreviations
#' @export
map_team_abbreviation <- function(abbr) {
  dplyr::case_when(
    abbr == "SD"   ~ "LAC",  # Chargers
    abbr == "STL"  ~ "LAR",  # Rams
    abbr == "OAK"  ~ "LV",   # Raiders
    abbr == "WSH"  ~ "WAS",  # Washington
    abbr == "LA"   ~ "LAR",  # generic LA -> Rams (older feeds)
    TRUE           ~ abbr
  )
}


#' Calculate Momentum Differential
#'
#' Computes a simple momentum metric based on current value, last 3-game
#' average, and season average.
#'
#' @param current Numeric current value
#' @param last_3 Numeric average over the last three games
#' @param season Numeric season average
#' @return Numeric momentum score
#' @export
calculate_momentum <- function(current, last_3, season) {
  (current - last_3) - (last_3 - season)
}

#' Fix All Cumulative Statistics
#'
#' Calculate cumulative statistics for all numeric columns in the dataset
#'
#' @param data Data frame containing NFL statistics
#'
#' @return Data frame with added cumulative statistics columns
#' @importFrom dplyr group_by mutate ungroup arrange
#' @importFrom tidyr fill
#' @importFrom rlang sym
#' @export
fix_all_cumulative_stats <- function(data) {
  # Get all cumulative stat columns (ends with _cum)
  cum_cols <- names(data)[grep("_cum$", names(data))]

  # Get their base column names (remove _cum suffix)
  base_cols <- gsub("_cum$", "", cum_cols)

  # For ratio stats, we need numerator and denominator
  ratio_pairs <- list(
    qb_deep_completion_percentage = c("qb_deep_completions", "qb_deep_passes"),
    def_red_zone_conversion_rate = c("def_red_zone_tds", "def_red_zone_attempts"),
    # Add other ratio pairs here
    third_down_conversion_rate = c("third_down_conversions", "third_down_attempts"),
    fourth_down_conversion_rate = c("fourth_down_conversions", "fourth_down_attempts"),
    qb_completion_percentage = c("qb_completions", "qb_attempts")
    # ... add other pairs as needed
  )

  fixed_data <- data %>%
    group_by(season, posteam) %>%
    arrange(season, week)

  # Fix simple cumulative stats
  for(col in base_cols) {
    if(col %in% names(data)) {
      cum_col <- paste0(col, "_cum")
      fixed_data <- fixed_data %>%
        mutate(!!cum_col := cumsum(!!rlang::sym(col)))
    }
  }

  # Fix ratio stats
  for(ratio_name in names(ratio_pairs)) {
    num_col <- ratio_pairs[[ratio_name]][1]
    denom_col <- ratio_pairs[[ratio_name]][2]

    if(all(c(num_col, denom_col) %in% names(data))) {
      cum_col <- paste0(ratio_name, "_cum")
      fixed_data <- fixed_data %>%
        mutate(!!cum_col := cumsum(!!rlang::sym(num_col)) / cumsum(!!rlang::sym(denom_col)))
    }
  }

  fixed_data <- fixed_data %>% ungroup()

  return(fixed_data)
}


#' Convert American Odds to Implied Probability
#'
#' @param american_odds Numeric vector of American odds
#' @return Numeric vector of implied probabilities
#' @export
#'
#' @examples
#' american_to_implied(100)
american_to_implied <- function(american_odds) {
  ifelse(american_odds >= 0,
         100 / (american_odds + 100),
         abs(american_odds) / (abs(american_odds) + 100))
}

#' Convert Implied Probability to American Odds
#'
#' @param implied_prob Numeric vector of probabilities (0-1)
#' @return Numeric vector of American odds
#' @export
#'
#' @examples
#' implied_to_american(0.5)
implied_to_american <- function(implied_prob) {
  ifelse(implied_prob > 0.5,
         -100 * implied_prob / (1 - implied_prob),
         ifelse(implied_prob == 0.5, 100,
                100 * (1 - implied_prob) / implied_prob))
}
