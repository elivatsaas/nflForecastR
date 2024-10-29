# R/globals.R
# R/globals.R

# Suppress R CMD check notes about global variables used in data.table/dplyr

# Suppress R CMD check notes about global variables
utils::globalVariables(c(
  ".", ":=", "season", "week", "posteam", "game_id",
  "home_team", "away_team", "points_scored", "points_allowed",
  "spread_line", "total_line", "div_game", "tidy_games", "tidy_weekly",
  "home.points_scored", "away.points_scored", "home.div_game", "away.div_game",
  "home.points_allowed", "away.points_allowed", "defteam", "passer_player_id",
  "snaps", "cumulative_snaps", "primary_qb_majority", "season_starter",
  "primary_qb_majority_snaps", "total_snaps", "season_starter_snaps",
  "posteam_type", "posteam_score", "epa", "pass_attempt", "rush_attempt",
  "fumble_lost", "interception", "fixed_drive", "touchdown", "field_goal_attempt",
  "field_goal_result", "complete_pass", "passing_yards", "pass_touchdown",
  "qb_scramble", "rushing_yards", "rush_touchdown", "qb_epa", "cpoe",
  "air_yards", "yards_after_catch", "first_down_pass", "qb_dropback",
  "qb_hit", "sack", "yards_gained", "penalty", "penalty_team", "penalty_yards",
  "down", "first_down", "play_type", "yardline_100", "wp", "wpa",
  "vegas_wpa", "vegas_home_wpa", "off_drives", "def_drives", "off_plays",
  "def_plays", "qb_completions", "qb_attempts", "qb_passing_yards",
  "qb_passing_tds", "qb_interceptions", "qb_pressures_taken", "qb_dropbacks",
  "qb_sacks_taken", "qb_hurries_taken", "qb_pressures_caused", "qb_sacks_caused",
  "qb_hurries_caused", "qb_deep_passes", "qb_deep_completions", "qb_air_yards",
  "qb_yac", "qb_first_downs", "primary_qb_majority_ratio", "season_starter_ratio",
  "third_down_conversions", "third_down_attempts", "fourth_down_conversions",
  "fourth_down_attempts", "red_zone_tds", "red_zone_attempts",
  "def_third_down_conversions", "def_third_down_attempts",
  "def_fourth_down_conversions", "def_fourth_down_attempts",
  "def_red_zone_tds", "def_red_zone_attempts", "penalties_for",
  "penalties_against", "penalty_yards_for", "penalty_yards_against",
  "def_qb_completions", "def_qb_attempts", "def_qb_passing_yards",
  "def_qb_passing_tds", "def_qb_interceptions", "def_wp", "correct_week",
  # Data tables
  "tidy_games", "tidy_weekly", "game_data", "weekly_data",

  # Common variables
  "season", "week", "posteam", "game_id", "home_team", "away_team",

  # Game-specific variables
  "points_scored", "points_allowed", "spread_line", "total_line", "div_game",
  "home.points_scored", "away.points_scored", "home.div_game", "away.div_game",
  "home.points_allowed", "away.points_allowed",

  # QB-related variables
  "passer_player_id", "snaps", "cumulative_snaps", "primary_qb_majority",
  "season_starter", "primary_qb_majority_snaps", "total_snaps",
  "season_starter_snaps", "primary_qb_majority_ratio", "season_starter_ratio",

  # Play variables
  "epa", "pass_attempt", "rush_attempt", "fumble_lost", "interception",
  "fixed_drive", "touchdown", "field_goal_attempt", "field_goal_result",
  "complete_pass", "passing_yards", "pass_touchdown", "qb_scramble",
  "rushing_yards", "rush_touchdown", "qb_epa", "cpoe", "air_yards",

  # All other variables
  ":=", "defteam", "correct_week", "yards_after_catch", "first_down_pass",
  "qb_dropback", "qb_hit", "sack", "yards_gained", "penalty", "penalty_team",
  "penalty_yards", "down", "first_down", "play_type", "yardline_100", "wp",
  "wpa", "vegas_wpa", "vegas_home_wpa", "points_scored", "off_drives",
  "points_allowed", "def_drives", "off_plays", "def_plays",

  # Statistics
  "qb_completions", "qb_attempts", "qb_passing_yards", "qb_passing_tds",
  "qb_interceptions", "qb_pressures_taken", "qb_dropbacks", "qb_sacks_taken",
  "qb_hurries_taken", "qb_pressures_caused", "qb_sacks_caused",
  "qb_hurries_caused", "qb_deep_passes", "qb_deep_completions", "qb_air_yards",
  "qb_yac", "qb_first_downs",

  # Down conversions
  "third_down_conversions", "third_down_attempts", "fourth_down_conversions",
  "fourth_down_attempts", "red_zone_tds", "red_zone_attempts",
  "def_third_down_conversions", "def_third_down_attempts",
  "def_fourth_down_conversions", "def_fourth_down_attempts",
  "def_red_zone_tds", "def_red_zone_attempts",

  # Penalties
  "penalties_for", "penalties_against", "penalty_yards_for",
  "penalty_yards_against",

  # Defense
  "def_qb_completions", "def_qb_attempts", "def_qb_passing_yards",
  "def_qb_passing_tds", "def_qb_interceptions", "def_wp", "posteam_type",
  "posteam_score"

))
