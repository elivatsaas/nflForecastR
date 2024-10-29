#' Historical NFL Game and Team Statistics
#'
#' @format A data frame with 2582 rows and 443 variables containing weekly pregame data for home and away teams,
#' excluding week 1. Includes game outcomes, betting lines, and detailed team statistics available before game.
#' \describe{
#'   # Game Identification
#'   \item{season}{NFL season year}
#'   \item{week}{Game week number}
#'   \item{game_id}{Unique game identifier}
#'   \item{home_team}{Home team abbreviation}
#'   \item{away_team}{Away team abbreviation}
#'
#'   # Game Outcomes and Lines
#'   \item{point_differential}{Final score difference (home - away)}
#'   \item{div_game}{Indicator for divisional games}
#'   \item{total}{Total points scored in game}
#'   \item{home.spread_line}{Point spread from home team perspective}
#'   \item{home.total_line}{Over/under total points line}
#'
#'   # Team Statistics (prefixed with home. or away.)
#'   \item{[team].points_scored}{Points scored by team}
#'   \item{[team].off_epa}{Expected Points Added on offense}
#'   \item{[team].pass_epa}{EPA on passing plays}
#'   \item{[team].rush_epa}{EPA on rushing plays}
#'   \item{[team].turnovers}{Total turnovers}
#'   \item{[team].off_plays}{Number of offensive plays}
#'   \item{[team].off_drives}{Number of offensive drives}
#'   \item{[team].off_scores}{Total scoring drives}
#'
#'   # Quarterback Statistics
#'   \item{[team].qb_completions}{Pass completions}
#'   \item{[team].qb_attempts}{Pass attempts}
#'   \item{[team].qb_passing_yards}{Passing yards}
#'   \item{[team].qb_passing_tds}{Passing touchdowns}
#'   \item{[team].qb_interceptions}{Interceptions thrown}
#'   \item{[team].qb_epa}{Quarterback EPA}
#'   \item{[team].qb_cpoe}{Completion Percentage Over Expected}
#'   \item{[team].qb_passer_rating}{NFL passer rating}
#'   \item{[team].qb_approx_qbr}{Approximate QBR metric}
#'
#'   # QB Pressure Stats
#'   \item{[team].qb_hits_taken}{Times QB was hit}
#'   \item{[team].qb_pressures_taken}{Total pressures on QB}
#'   \item{[team].qb_sacks_taken}{Times QB was sacked}
#'
#'   # Defensive Statistics
#'   \item{[team].def_epa}{Defensive Expected Points Added}
#'   \item{[team].def.pass_epa}{EPA against pass}
#'   \item{[team].def.rush_epa}{EPA against rush}
#'   \item{[team].def.turnovers}{Turnovers forced}
#'
#'   # Conversion Rates
#'   \item{[team].third_down_conversion_rate}{Third down conversion percentage}
#'   \item{[team].fourth_down_conversion_rate}{Fourth down conversion percentage}
#'   \item{[team].red_zone_conversion_rate}{Red zone touchdown percentage}
#'
#'   # Penalty Statistics
#'   \item{[team].penalties_for}{Penalties committed}
#'   \item{[team].penalty_yards_for}{Penalty yards against}
#'   \item{[team].penalty_rate_for}{Penalties per play}
#'
#'   # Cumulative Statistics
#'   \item{[team].[stat]_cum}{Cumulative version of each statistic through season}
#' }
#' @source Processed from nflfastR play-by-play data
#' @note [team] represents either 'home' or 'away' prefix for all team statistics
#' @note Most statistics have both most recent game values and cumulative (_cum) versions
#' @note point_differential and total should never be included in a predictive model
"tidy_data"
