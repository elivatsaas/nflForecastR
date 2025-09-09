#' @export


#nflFastR Columns
PBP_REQUIRED_COLS <- c(
  "game_id", "posteam", "defteam", "passer_player_id", "epa", "wp",
  "pass_attempt", "rush_attempt", "posteam_score", "fixed_drive",
  "complete_pass", "passing_yards", "pass_touchdown", "interception",
  "qb_scramble", "rushing_yards", "rush_touchdown", "qb_epa", "cpoe",
  "air_yards", "yards_after_catch", "first_down_pass", "qb_dropback",
  "qb_hit", "sack", "penalty", "penalty_team", "penalty_yards", "down",
  "first_down", "play_type", "yardline_100", "touchdown",
  "field_goal_attempt", "field_goal_result", "spread_line", "total_line",
  "div_game", "wpa", "vegas_wpa", "vegas_home_wpa"
)

# Metrics to calculate
METRICS <- c(
  "points_scored", "off_epa", "pass_epa", "rush_epa",
  "turnovers", "off_plays", "off_drives", "off_scores",
  "qb_completions", "qb_attempts", "points_allowed", "def_epa", "def.pass_epa",
  "def.rush_epa", "def.turnovers",
  "def_plays", "def_drives", "def_scores", "points_per_drive",
  "points_allowed_per_drive", "points_per_play",
  "points_allowed_per_play", "qb_passing_yards", "qb_passing_tds", "qb_interceptions",
  "qb_sacks_taken", "qb_sack_yards_taken", "qb_rushing_attempts", "qb_rushing_yards", "qb_rushing_tds",
  "qb_completion_percentage", "qb_yards_per_attempt", "qb_td_percentage", "qb_int_percentage",
  "qb_pressure_percentage_taken", "qb_sack_percentage_taken", "qb_hurry_percentage_taken",
  "qb_pressure_percentage_caused", "qb_sack_percentage_caused", "qb_hurry_percentage_caused",
  "qb_passer_rating", "qb_epa", "qb_cpoe", "qb_air_yards_per_attempt",
  "qb_yac_per_completion", "qb_first_down_percentage",
  "qb_deep_percentage", "qb_deep_completion_percentage", "qb_approx_qbr",
  "primary_qb_majority_ratio", "season_starter_ratio", "qb_consistency_majority", "qb_consistency_starter",
  "penalties_for", "penalty_yards_for", "penalties_against", "penalty_yards_against",
  "penalties_1st_down", "penalties_2nd_down", "penalties_3rd_down", "penalties_4th_down",
  "third_down_conversion_rate", "fourth_down_conversion_rate", "red_zone_conversion_rate",
  "penalty_rate_for", "penalty_rate_against", "penalty_yards_per_play_for", "penalty_yards_per_play_against",
  "def_penalties_for", "def_penalty_yards_for", "def_penalties_against", "def_penalty_yards_against",
  "def_qb_completion_percentage", "def_qb_yards_per_attempt", "def_qb_td_percentage", "def_qb_int_percentage",
  "def_points_per_drive", "def_points_per_play",
  "def_third_down_conversion_rate", "def_fourth_down_conversion_rate", "def_red_zone_conversion_rate"
)

# Team abbreviation mapping
TEAM_MAPPING <- list(
  "SD" = "LAC",
  "STL" = "LA",
  "OAK" = "LV"
)


#' NFL Team Name Mappings
#'
#' A named vector mapping team abbreviations to full team names
#' Used for converting between ESPN abbreviations and full team names
#'
#' @format A named character vector
#' @export
NFL_TEAM_MAPPINGS <- c(
  "Arizona Cardinals" = "ARI",
  "Atlanta Falcons" = "ATL",
  "Baltimore Ravens" = "BAL",
  "Buffalo Bills" = "BUF",
  "Carolina Panthers" = "CAR",
  "Chicago Bears" = "CHI",
  "Cincinnati Bengals" = "CIN",
  "Cleveland Browns" = "CLE",
  "Dallas Cowboys" = "DAL",
  "Denver Broncos" = "DEN",
  "Detroit Lions" = "DET",
  "Green Bay Packers" = "GB",
  "Houston Texans" = "HOU",
  "Indianapolis Colts" = "IND",
  "Jacksonville Jaguars" = "JAX",
  "Kansas City Chiefs" = "KC",
  "Los Angeles Chargers" = "LAC",
  "Los Angeles Rams" = "LA",
  "Las Vegas Raiders" = "LV",
  "Miami Dolphins" = "MIA",
  "Minnesota Vikings" = "MIN",
  "New England Patriots" = "NE",
  "New Orleans Saints" = "NO",
  "New York Giants" = "NYG",
  "New York Jets" = "NYJ",
  "Philadelphia Eagles" = "PHI",
  "Pittsburgh Steelers" = "PIT",
  "San Francisco 49ers" = "SF",
  "Seattle Seahawks" = "SEA",
  "Tampa Bay Buccaneers" = "TB",
  "Tennessee Titans" = "TEN",
  "Washington Commanders" = "WAS",
  # legacy abbreviations mapped to current teams
  "OAK" = "LV",
  "SD"  = "LAC",
  "STL" = "LA",
  "WSH" = "WAS"
)


#' Function to clean team names using the mapping
#'
#' @param team_name Character string of team abbreviation or name
#' @return Character string of cleaned team name
#' @export
clean_team_name <- function(team_name) {
  if(team_name %in% names(NFL_TEAM_MAPPINGS)) {
    return(NFL_TEAM_MAPPINGS[team_name])
  }
  return(team_name)
}
