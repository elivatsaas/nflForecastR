#' Fetch NFL Game Odds
#'
#' @param api_key Character string containing your API key for the-odds-api.com
#' @param regions Character vector of regions (default: "us")
#' @param markets Character vector of desired markets (default: c("h2h", "spreads", "totals"))
#' @param odds_format Character string specifying odds format (default: "american")
#'
#' @return A data frame containing processed odds data
#' @export
get_nfl_odds <- function(regions = "us",
                         markets = c("h2h", "spreads", "totals"),
                         odds_format = "american") {
  api_key = "82f5b5c4043ffb1c600d99b32c912c87"


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

  # Create empty dataframe to store results
  results <- data.frame()

  # Process each game
  for (game in data) {
    # Extract basic game info
    game_info <- data.frame(
      game_id = game$id,
      commence_time = as.POSIXct(game$commence_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      home_team = game$home_team,
      away_team = game$away_team
    )

    # Process each bookmaker
    for (bookie in game$bookmakers) {
      game_odds <- game_info
      game_odds$bookmaker <- bookie$key

      # Initialize odds columns with NA
      game_odds$home_ml <- NA
      game_odds$away_ml <- NA
      game_odds$spread <- NA
      game_odds$spread_price <- NA
      game_odds$total <- NA
      game_odds$total_over_price <- NA
      game_odds$total_under_price <- NA

      # Process each market type
      for (market in bookie$markets) {
        if (market$key == "h2h") {
          # Process money lines
          for (outcome in market$outcomes) {
            if (outcome$name == game$home_team) {
              game_odds$home_ml <- outcome$price
            } else if (outcome$name == game$away_team) {
              game_odds$away_ml <- outcome$price
            }
          }
        } else if (market$key == "spreads") {
          # Process spreads
          for (outcome in market$outcomes) {
            if (outcome$name == game$home_team) {
              game_odds$spread <- outcome$point
              game_odds$spread_price <- outcome$price
            }
          }
        } else if (market$key == "totals") {
          # Process totals
          game_odds$total <- market$outcomes[[1]]$point
          game_odds$total_over_price <- market$outcomes[[1]]$price
          game_odds$total_under_price <- market$outcomes[[2]]$price
        }
      }

      # Add this bookmaker's odds to results
      results <- rbind(results, game_odds)
    }
  }

  return(results)
}
