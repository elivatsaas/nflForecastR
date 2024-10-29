# R/data/tidy_weekly.R

#' Historical NFL Weekly Team Statistics
#'
#' A dataset containing weekly NFL team statistics processed from nflfastR play-by-play data.
#' This dataset provides team-level statistics for each week of play. 2015 to 2023.
#'
#' @format A data frame with rows representing team-weeks and 220 variables:
#' \describe{
#'   # Identification Variables
#'   \item{season}{NFL season year}
#'   \item{week}{Game week number}
#'   \item{posteam}{Team in possession}
#'   \item{game_id}{Unique game identifier}
#'   \item{posteam_type}{Home/Away designation}
#'
#'   # Core Offensive Statistics
#'   \item{points_scored}{Points scored by team}
#'   \item{off_epa}{Offensive Expected Points Added}
#'   \item{pass_epa}{EPA on passing plays}
#'   \item{rush_epa}{EPA on rushing plays}
#'   \item{off_plays}{Number of offensive plays}
#'   \item{off_drives}{Number of offensive drives}
#'   \item{off_scores}{Number of scoring drives}
#'
#'   # Quarterback Performance
#'   \item{qb_completions}{Pass completions}
#'   \item{qb_attempts}{Pass attempts}
#'   \item{qb_passing_yards}{Total passing yards}
#'   \item{qb_passing_tds}{Passing touchdowns}
#'   \item{qb_interceptions}{Interceptions thrown}
#'   \item{qb_epa}{Quarterback EPA}
#'   \item{qb_cpoe}{Completion Percentage Over Expected}
#'
#'   # QB Pressure Stats
#'   \item{qb_hits_taken}{Times quarterback was hit}
#'   \item{qb_hurries_taken}{Quarterback hurries}
#'   \item{qb_pressures_taken}{Total pressures on quarterback}
#'   \item{qb_sacks_taken}{Times quarterback was sacked}
#'
#'   # Defensive Statistics
#'   \item{points_allowed}{Points allowed by defense}
#'   \item{def_epa}{Defensive Expected Points Added}
#'   \item{def.pass_epa}{EPA against the pass}
#'   \item{def.rush_epa}{EPA against the run}
#'   \item{def.turnovers}{Turnovers forced}
#'
#'   # Success Rates
#'   \item{third_down_conversion_rate}{Third down conversion percentage}
#'   \item{fourth_down_conversion_rate}{Fourth down conversion percentage}
#'   \item{red_zone_conversion_rate}{Red zone touchdown percentage}
#'
#'   # Game Context
#'   \item{spread_line}{Point spread}
#'   \item{total_line}{Over/under line}
#'   \item{div_game}{Indicator for divisional games}
#'   \item{wp}{Win probability}
#'   \item{wpa}{Win probability added}
#'
#'   # Cumulative Statistics
#'   \item{*_cum}{Season-to-date cumulative versions of above statistics}
#' }
#' @source Processed from nflfastR play-by-play data
"weekly_data"

#' Historical NFL Game Statistics
#'
#' A dataset containing NFL game statistics with separate home and away team metrics,
#' processed from nflfastR play-by-play data. Contains pregame statistics and betting lines.
#'
#' @format A data frame with 2582 rows and 443 variables:
#' \describe{
#'   # Game Identification
#'   \item{season}{NFL season year}
#'   \item{week}{Game week number}
#'   \item{game_id}{Unique game identifier}
#'   \item{home_team}{Home team abbreviation}
#'   \item{away_team}{Away team abbreviation}
#'
#'   # Game Outcomes
#'   \item{point_differential}{Final score difference (home - away)}
#'   \item{total}{Total points scored}
#'   \item{div_game}{Indicator for divisional games}
#'
#'   # Team Statistics (prefixed with home. or away.)
#'   \item{[team].points_scored}{Points scored}
#'   \item{[team].off_epa}{Offensive Expected Points Added}
#'   \item{[team].pass_epa}{EPA on passing plays}
#'   \item{[team].rush_epa}{EPA on rushing plays}
#'
#'   # QB Performance (prefixed with home. or away.)
#'   \item{[team].qb_epa}{Quarterback EPA}
#'   \item{[team].qb_cpoe}{Completion Percentage Over Expected}
#'   \item{[team].qb_passer_rating}{NFL passer rating}
#'   \item{[team].qb_approx_qbr}{Approximate QBR}
#'
#'   # QB Protection/Pressure (prefixed with home. or away.)
#'   \item{[team].qb_pressure_percentage_taken}{Pressure rate on QB}
#'   \item{[team].qb_sack_percentage_taken}{Sack rate}
#'   \item{[team].qb_hurry_percentage_taken}{Hurry rate}
#'
#'   # Efficiency Metrics (prefixed with home. or away.)
#'   \item{[team].points_per_drive}{Points per drive}
#'   \item{[team].points_per_play}{Points per play}
#'   \item{[team].third_down_conversion_rate}{Third down conversion rate}
#'   \item{[team].red_zone_conversion_rate}{Red zone conversion rate}
#'
#'   # Cumulative Statistics
#'   \item{[team].[stat]_cum}{Season-to-date cumulative versions of above statistics}
#' }
#' @source Processed from nflfastR play-by-play data
#' @note [team] represents either 'home.' or 'away.' prefix
#' @note All statistics are calculated prior to the current game
"game_data"
