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
"tidy_weekly"


