#' Calculate Consensus Odds
#'
#' @param odds_df Data frame from get_nfl_odds
#' @param min_bookmakers Minimum number of bookmakers required
#' @return Data frame with consensus odds
#' @export
calculate_consensus_odds <- function(odds_df, min_bookmakers = 3) {
  consensus <- odds_df %>%
    group_by(game_id, home_team, away_team) %>%
    summarise(
      bookmaker_count = n_distinct(bookmaker),
      home_ml = stats::median(home_ml, na.rm = TRUE),
      away_ml = stats::median(away_ml, na.rm = TRUE),
      spread = stats::median(spread, na.rm = TRUE),
      spread_price = stats::median(spread_price, na.rm = TRUE),
      total = stats::median(total, na.rm = TRUE),
      total_over_price = stats::median(total_over_price, na.rm = TRUE),
      total_under_price = stats::median(total_under_price, na.rm = TRUE)
    ) %>%
    filter(bookmaker_count >= min_bookmakers)

  return(consensus)
}
