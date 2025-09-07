#' @keywords internal
NULL

#' Load all files in the data directory
#' @importFrom utils data
.onLoad <- function(libname, pkgname) {
  # Load the datasets into the package environment
  utils::data("tidy_weekly", package = pkgname, envir = parent.env(environment()))
  utils::data("tidy_games", package = pkgname, envir = parent.env(environment()))
}

#' NFL Weekly Team Statistics
#'
#' A dataset containing weekly NFL team statistics processed from nflfastR play-by-play data.
#' This dataset provides team-level statistics for each week of play, 2015 to 2023.
#'
#' @format A data frame with rows representing team-weeks and 220 variables:
#' \describe{
#'   \item{season}{NFL season year}
#'   \item{week}{Game week number}
#'   \item{game_id}{Unique game identifier}
#'   \item{home_team}{Home team abbreviation}
#'   \item{away_team}{Away team abbreviation}
#' }
#' @source Processed from nflfastR play-by-play data
#' @note [team] represents either 'home' or 'away' prefix for all team statistics
#' @note Most statistics have both most recent game values and cumulative (_cum) versions
#' @note point_differential and total should never be included in a predictive model
"tidy_weekly"

#' NFL Game and Team Statistics
#'
#' @format A data frame with 2582 rows and 443 variables containing weekly pregame data for home and away teams,
#' excluding week 1. Includes game outcomes, betting lines, and detailed team statistics available before game.
#' \describe{
#'   \item{season}{NFL season year}
#'   \item{week}{Game week number}
#'   \item{game_id}{Unique game identifier}
#'   \item{home_team}{Home team abbreviation}
#'   \item{away_team}{Away team abbreviation}
#'   \item{point_differential}{Final score difference (home - away)}
#'   \item{div_game}{Indicator for divisional games}
#'   \item{total}{Total points scored in game}
#'   \item{home.spread_line}{Point spread from home team perspective}
#'   \item{home.total_line}{Over/under total points line}
#' }
#' @source Processed from nflfastR play-by-play data
#' @note [team] represents either 'home' or 'away' prefix for all team statistics
#' @note Most statistics have both most recent game values and cumulative (_cum) versions
#' @note point_differential and total should never be included in a predictive model
"tidy_games"


#' Required Play-by-Play Columns
#'
#' Vector of column names required for play-by-play data processing
#'
#' @format A character vector of required column names
#' @source Constructed based on nflfastR data requirements
"PBP_REQUIRED_COLS"

#' NFL Schedule 2024
#'
#' A dataset containing the NFL schedule for the 2024 season
#'
#' @format A data frame with rows and columns:
#' \describe{
#'   \item{match_number}{Game of the season}
#'   \item{away_team}{Three-letter abbreviation of away team}
#'   \item{home_team}{Three-letter abbreviation of home team}
#'   \item{Date}{Date of the game}
#'   \item{Location}{Location of Game}
#'   \item{week}{Week game was played}
#' }
#' @source {https://fixturedownload.com/download/csv/nfl-2024}
"nfl_schedule_2024"

