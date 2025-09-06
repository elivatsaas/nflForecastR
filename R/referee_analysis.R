# ==================== REFEREE_ANALYSIS.R ====================
# Comprehensive referee analysis and officiating tendencies

library(dplyr)
library(purrr)
library(stringr)

#' Analyze referee performance and officiating tendencies
#'
#' @param schedule_data Complete schedule data with referee assignments
#' @param pbp_data Play-by-play data for penalty analysis
#' @return Comprehensive referee analysis with bias detection
analyze_referee_performance <- function(schedule_data, pbp_data) {
  
  if (is.null(pbp_data)) {
    cat("No play-by-play data provided for referee analysis\n")
    return(data.frame())
  }
  
  if (!"referee" %in% names(schedule_data)) {
    cat("Warning: 'referee' column not found in schedule data\n")
    return(data.frame())
  }
  
  cat("Analyzing referee tendencies and officiating patterns...\n")
  
  # Join pbp with schedule (keep home/away for bias calc)
  pbp_with_ref <- pbp_data %>%
    left_join(
      schedule_data %>%
        select(season, week, game_id, referee, home_team, away_team) %>%
        filter(!is.na(referee), referee != ""),
      by = c("season", "week", "game_id")
    )
  
  if (!"referee" %in% names(pbp_with_ref) || all(is.na(pbp_with_ref$referee))) {
    cat("Warning: No referee data available after join\n")
    return(data.frame())
  }
  
  if (!"penalty" %in% names(pbp_with_ref)) {
    cat("Warning: 'penalty' column not in PBP\n")
    return(data.frame())
  }
  
  penalty_count <- sum(pbp_with_ref$penalty == 1, na.rm = TRUE)
  if (penalty_count == 0) {
    cat("Warning: No penalties in PBP\n")
    return(data.frame())
  }
  cat("Found", penalty_count, "total penalties in play-by-play data\n")
  
  # -------------------- Penalty extraction --------------------
  ref_penalties <- pbp_with_ref %>%
    filter(penalty == 1, !is.na(referee), referee != "") %>%
    mutate(
      penalty_on_home = penalty_team == home_team,
      penalty_on_away = penalty_team == away_team,
      penalty_category = case_when(
        str_detect(tolower(penalty_type), "holding|block") ~ "holding_blocking",
        str_detect(tolower(penalty_type), "false start|offside|neutral") ~ "pre_snap",
        str_detect(tolower(penalty_type), "pass interference|illegal contact") ~ "pass_interference",
        str_detect(tolower(penalty_type), "personal foul|unnecessary|roughing") ~ "personal_foul",
        str_detect(tolower(penalty_type), "delay|timeout|procedure") ~ "procedural",
        TRUE ~ "other"
      ),
      penalty_severity = case_when(
        penalty_yards >= 15 ~ "major",
        penalty_yards >= 10 ~ "moderate",
        penalty_yards >= 5 ~ "minor",
        TRUE ~ "minimal"
      )
    ) %>%
    group_by(season, week, game_id, referee) %>%
    summarise(
      total_penalties = n(),
      home_penalties  = sum(penalty_on_home, na.rm = TRUE),
      away_penalties  = sum(penalty_on_away, na.rm = TRUE),
      total_penalty_yards = sum(penalty_yards, na.rm = TRUE),
      home_penalty_yards  = sum(penalty_yards[penalty_on_home], na.rm = TRUE),
      away_penalty_yards  = sum(penalty_yards[penalty_on_away], na.rm = TRUE),
      holding_penalties   = sum(penalty_category == "holding_blocking"),
      pre_snap_penalties  = sum(penalty_category == "pre_snap"),
      pi_penalties        = sum(penalty_category == "pass_interference"),
      personal_foul_penalties = sum(penalty_category == "personal_foul"),
      first_half_penalties = sum(qtr <= 2, na.rm = TRUE),
      fourth_quarter_penalties = sum(qtr == 4, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -------------------- Rollup per referee --------------------
  referee_analysis <- ref_penalties %>%
    group_by(referee) %>%
    summarise(
      games_officiated = n(),
      total_penalties_called = sum(total_penalties),
      avg_penalties_per_game = mean(total_penalties),
      penalty_consistency = sd(total_penalties),
      avg_yards_per_penalty = sum(total_penalty_yards) / sum(total_penalties),
      total_home_penalties = sum(home_penalties),
      total_away_penalties = sum(away_penalties),
      home_bias_score = (sum(home_penalties) / (sum(home_penalties) + sum(away_penalties))) - 0.5,
      holding_rate = sum(holding_penalties) / sum(total_penalties),
      pre_snap_rate = sum(pre_snap_penalties) / sum(total_penalties),
      pi_rate       = sum(pi_penalties) / sum(total_penalties),
      personal_foul_rate = sum(personal_foul_penalties) / sum(total_penalties),
      first_half_rate = sum(first_half_penalties) / sum(total_penalties),
      fourth_quarter_rate = sum(fourth_quarter_penalties) / sum(total_penalties),
      seasons_active = n_distinct(season),
      first_season = min(season),
      last_season  = max(season),
      .groups = "drop"
    ) %>%
    filter(games_officiated >= 10) %>%
    mutate(
      bias_category = case_when(
        abs(home_bias_score) <= 0.03 ~ "neutral",
        home_bias_score > 0.03 ~ "away_favoring",
        home_bias_score < -0.03 ~ "home_favoring",
        TRUE ~ "slight_bias"
      ),
      consistency_level = case_when(
        penalty_consistency <= 2.5 ~ "very_consistent",
        penalty_consistency <= 4.0 ~ "consistent",
        penalty_consistency <= 6.0 ~ "moderate",
        TRUE ~ "inconsistent"
      ),
      referee_experience = case_when(
        seasons_active >= 15 ~ "veteran",
        seasons_active >= 8  ~ "experienced",
        seasons_active >= 4  ~ "developing",
        TRUE ~ "new"
      ),
      referee_quality = pmin(100, pmax(0,
                                       50 +
                                         (ifelse(consistency_level %in% c("consistent", "very_consistent"), 15, -10)) +
                                         (ifelse(bias_category == "neutral", 10, -5)) +
                                         (seasons_active * 1.5) +
                                         (ifelse(avg_penalties_per_game >= 8 & avg_penalties_per_game <= 15, 5, 0))
      ))
    ) %>%
    arrange(desc(referee_quality))
  
  cat("Referee analysis complete:", nrow(referee_analysis), "refs analyzed\n")
  return(referee_analysis)
}
