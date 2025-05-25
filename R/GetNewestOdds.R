#' Fetch and Process NFL Game Odds
#'
#' @param api_key Character string containing your API key for the-odds-api.com
#' @param regions Character vector of regions (default: "us")
#' @param markets Character vector of desired markets (default: c("h2h", "spreads", "totals"))
#' @param odds_format Character string specifying odds format (default: "american")
#'
#' @return A data frame containing processed odds data with columns for:
#'   game_id, commence_time, home_team, away_team, and various odds markets
#'
#' @importFrom httr GET content
#' @importFrom dplyr %>% mutate select bind_rows filter
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom purrr map_dfr
#' @export
get_nfl_odds <- function(api_key,
                         regions = "us",
                         markets = c("h2h", "spreads", "totals"),
                         odds_format = "american") {

  # Construct API URL
  base_url <- "https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds"
  url <- sprintf("%s?regions=%s&markets=%s&oddsFormat=%s&apiKey=%s",
                 base_url,
                 paste(regions, collapse = ","),
                 paste(markets, collapse = ","),
                 odds_format,
                 api_key)

  # Make API request with error handling
  tryCatch({
    response <- httr::GET(url)
    httr::stop_for_status(response)
    data <- httr::content(response, "parsed")
  }, error = function(e) {
    stop("Error fetching odds data: ", e$message)
  })

  # Process the response into a data frame
  games_df <- purrr::map_dfr(data, function(game) {
    # Basic game info
    game_info <- data.frame(
      game_id = game$id,
      commence_time = as.POSIXct(game$commence_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      home_team = game$home_team,
      away_team = game$away_team,
      stringsAsFactors = FALSE
    )

    # Process bookmakers data
    bookmakers_data <- purrr::map_dfr(game$bookmakers, function(bookmaker) {
      markets_data <- purrr::map_dfr(bookmaker$markets, function(market) {
        if (market$key == "h2h") {
          # Process money lines
          home_line <- market$outcomes %>%
            purrr::keep(~.$name == game$home_team) %>%
            purrr::pluck(1, "price")
          away_line <- market$outcomes %>%
            purrr::keep(~.$name == game$away_team) %>%
            purrr::pluck(1, "price")

          return(data.frame(
            bookmaker = bookmaker$key,
            home_ml = home_line,
            away_ml = away_line
          ))
        } else if (market$key == "spreads") {
          # Process point spreads
          home_spread <- market$outcomes %>%
            purrr::keep(~.$name == game$home_team) %>%
            purrr::pluck(1, "point")
          home_spread_price <- market$outcomes %>%
            purrr::keep(~.$name == game$home_team) %>%
            purrr::pluck(1, "price")

          return(data.frame(
            bookmaker = bookmaker$key,
            spread = home_spread,
            spread_price = home_spread_price
          ))
        } else if (market$key == "totals") {
          # Process totals (over/under)
          over <- market$outcomes %>%
            purrr::keep(~.$name == "Over") %>%
            purrr::pluck(1)

          return(data.frame(
            bookmaker = bookmaker$key,
            total = over$point,
            total_over_price = over$price,
            total_under_price = market$outcomes[[2]]$price
          ))
        }
      })
      return(markets_data)
    })

    # Combine game info with odds data
    result <- cbind(game_info, bookmakers_data)
    return(result)
  })

  return(games_df)
}

#' Helper function to calculate consensus odds
#'
#' @param odds_df Data frame from get_nfl_odds
#' @param min_bookmakers Minimum number of bookmakers required for consensus
#' @return Data frame with consensus odds for each game
#' @export
calculate_consensus_odds <- function(odds_df, min_bookmakers = 3) {
  consensus <- odds_df %>%
    group_by(game_id) %>%
    summarise(
      bookmaker_count = n_distinct(bookmaker),
      home_ml = median(home_ml, na.rm = TRUE),
      away_ml = median(away_ml, na.rm = TRUE),
      spread = median(spread, na.rm = TRUE),
      spread_price = median(spread_price, na.rm = TRUE),
      total = median(total, na.rm = TRUE),
      total_over_price = median(total_over_price, na.rm = TRUE),
      total_under_price = median(total_under_price, na.rm = TRUE)
    ) %>%
    filter(bookmaker_count >= min_bookmakers)

  return(consensus)
}
