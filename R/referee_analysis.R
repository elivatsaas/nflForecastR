#' Enhanced Referee Performance Analysis
#' 
#' Analyzes referee performance and officiating tendencies with improved
#' error handling and bias detection
#' 
#' @param schedule_data Complete schedule data with referee assignments
#' @param pbp_data Play-by-play data for penalty analysis (optional)
#' @return Enhanced referee analysis with bias detection and quality scores
#' @import dplyr purrr stringr
analyze_referee_performance <- function(schedule_data, pbp_data = NULL) {
  
  cat("=== ENHANCED REFEREE ANALYSIS ===\n")
  
  if (!"referee" %in% names(schedule_data)) {
    cat("Warning: 'referee' column not found in schedule data\n")
    return(data.frame())
  }
  
  # Clean referee data first
  clean_schedule <- schedule_data %>%
    dplyr::filter(!is.na(referee), referee != "", referee != "TBD") %>%
    dplyr::mutate(
      referee_clean = stringr::str_trim(stringr::str_to_title(referee)),
      referee_clean = stringr::str_replace_all(referee_clean, "\\s+", " ")
    )
  
  if (nrow(clean_schedule) == 0) {
    cat("No referee data available in schedule\n")
    return(data.frame())
  }
  
  cat("Found", length(unique(clean_schedule$referee_clean)), "unique referees in", 
      nrow(clean_schedule), "games\n")
  
  # Basic referee game counts and experience
  referee_basics <- clean_schedule %>%
    dplyr::group_by(referee_clean) %>%
    dplyr::summarise(
      games_officiated = dplyr::n(),
      seasons_active = dplyr::n_distinct(season),
      first_season = min(season, na.rm = TRUE),
      last_season = max(season, na.rm = TRUE),
      playoff_games = sum(game_type %in% c("DIV", "CON", "SB", "WC"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(games_officiated >= 5) %>%  # Filter out very limited refs
    dplyr::mutate(
      referee_experience = dplyr::case_when(
        seasons_active >= 15 ~ "veteran",
        seasons_active >= 8 ~ "experienced", 
        seasons_active >= 4 ~ "developing",
        TRUE ~ "new"
      ),
      playoff_rate = playoff_games / games_officiated
    )
  
  # If PBP data is available, do penalty analysis
  if (!is.null(pbp_data) && "penalty" %in% names(pbp_data)) {
    cat("Analyzing penalty patterns with play-by-play data...\n")
    
    # Join pbp with referee info
    pbp_with_ref <- pbp_data %>%
      dplyr::left_join(
        clean_schedule %>%
          dplyr::select(season, week, game_id, referee_clean, home_team, away_team),
        by = c("season", "week", "game_id")
      ) %>%
      dplyr::filter(!is.na(referee_clean))
    
    if (nrow(pbp_with_ref) > 0 && sum(pbp_with_ref$penalty == 1, na.rm = TRUE) > 0) {
      
      penalty_count <- sum(pbp_with_ref$penalty == 1, na.rm = TRUE)
      cat("Analyzing", penalty_count, "penalties across", 
          length(unique(pbp_with_ref$referee_clean)), "referees\n")
      
      # Enhanced penalty analysis
      ref_penalties <- pbp_with_ref %>%
        dplyr::filter(penalty == 1, !is.na(referee_clean)) %>%
        dplyr::mutate(
          penalty_on_home = penalty_team == home_team,
          penalty_on_away = penalty_team == away_team,
          penalty_category = dplyr::case_when(
            stringr::str_detect(tolower(penalty_type), "holding|block") ~ "holding_blocking",
            stringr::str_detect(tolower(penalty_type), "false start|offside|neutral") ~ "pre_snap",
            stringr::str_detect(tolower(penalty_type), "pass interference|illegal contact") ~ "pass_interference", 
            stringr::str_detect(tolower(penalty_type), "personal foul|unnecessary|roughing") ~ "personal_foul",
            stringr::str_detect(tolower(penalty_type), "delay|timeout|procedure") ~ "procedural",
            TRUE ~ "other"
          ),
          penalty_severity = dplyr::case_when(
            penalty_yards >= 15 ~ "major",
            penalty_yards >= 10 ~ "moderate", 
            penalty_yards >= 5 ~ "minor",
            TRUE ~ "minimal"
          ),
          game_situation = dplyr::case_when(
            qtr <= 2 ~ "first_half",
            qtr == 4 & game_seconds_remaining <= 300 ~ "crunch_time",
            qtr == 4 ~ "fourth_quarter",
            TRUE ~ "third_quarter"
          )
        )
      
      # Game-level penalty summary
      game_penalties <- ref_penalties %>%
        dplyr::group_by(season, week, game_id, referee_clean) %>%
        dplyr::summarise(
          total_penalties = dplyr::n(),
          home_penalties = sum(penalty_on_home, na.rm = TRUE),
          away_penalties = sum(penalty_on_away, na.rm = TRUE),
          total_penalty_yards = sum(penalty_yards, na.rm = TRUE),
          home_penalty_yards = sum(penalty_yards[penalty_on_home], na.rm = TRUE),
          away_penalty_yards = sum(penalty_yards[penalty_on_away], na.rm = TRUE),
          holding_penalties = sum(penalty_category == "holding_blocking"),
          pre_snap_penalties = sum(penalty_category == "pre_snap"),
          pi_penalties = sum(penalty_category == "pass_interference"),
          personal_foul_penalties = sum(penalty_category == "personal_foul"),
          crunch_time_penalties = sum(game_situation == "crunch_time"),
          first_half_penalties = sum(game_situation == "first_half"),
          .groups = "drop"
        )
      
      # Referee-level penalty analysis
      referee_penalties <- game_penalties %>%
        dplyr::group_by(referee_clean) %>%
        dplyr::summarise(
          penalty_games = dplyr::n(),
          total_penalties_called = sum(total_penalties),
          avg_penalties_per_game = mean(total_penalties),
          penalty_consistency = sd(total_penalties),
          avg_yards_per_penalty = sum(total_penalty_yards) / sum(total_penalties),
          total_home_penalties = sum(home_penalties),
          total_away_penalties = sum(away_penalties),
          # Bias calculation (positive = favors away team)
          home_bias_score = (sum(home_penalties) / (sum(home_penalties) + sum(away_penalties))) - 0.5,
          holding_rate = sum(holding_penalties) / sum(total_penalties),
          pre_snap_rate = sum(pre_snap_penalties) / sum(total_penalties),
          pi_rate = sum(pi_penalties) / sum(total_penalties),
          personal_foul_rate = sum(personal_foul_penalties) / sum(total_penalties),
          crunch_time_rate = sum(crunch_time_penalties) / sum(total_penalties),
          first_half_rate = sum(first_half_penalties) / sum(total_penalties),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          bias_category = dplyr::case_when(
            abs(home_bias_score) <= 0.03 ~ "neutral",
            home_bias_score > 0.03 ~ "away_favoring", 
            home_bias_score < -0.03 ~ "home_favoring",
            TRUE ~ "slight_bias"
          ),
          consistency_level = dplyr::case_when(
            is.na(penalty_consistency) ~ "unknown",
            penalty_consistency <= 2.5 ~ "very_consistent",
            penalty_consistency <= 4.0 ~ "consistent",
            penalty_consistency <= 6.0 ~ "moderate",
            TRUE ~ "inconsistent"
          )
        )
      
      # Join penalty analysis with basic referee info
      referee_analysis <- referee_basics %>%
        dplyr::left_join(referee_penalties, by = c("referee_clean")) %>%
        dplyr::mutate(
          # Fill missing penalty stats for refs without penalty data
          avg_penalties_per_game = dplyr::coalesce(avg_penalties_per_game, 12.0),
          penalty_consistency = dplyr::coalesce(penalty_consistency, 3.5),
          bias_category = dplyr::coalesce(bias_category, "unknown"),
          consistency_level = dplyr::coalesce(consistency_level, "unknown"),
          home_bias_score = dplyr::coalesce(home_bias_score, 0)
        )
      
    } else {
      cat("No penalty data available in PBP\n")
      referee_analysis <- referee_basics %>%
        dplyr::mutate(
          avg_penalties_per_game = 12.0,  # League average estimate
          penalty_consistency = 3.5,
          bias_category = "unknown",
          consistency_level = "unknown", 
          home_bias_score = 0
        )
    }
    
  } else {
    cat("No play-by-play data provided, using basic analysis\n")
    referee_analysis <- referee_basics %>%
      dplyr::mutate(
        avg_penalties_per_game = 12.0,
        penalty_consistency = 3.5,
        bias_category = "unknown",
        consistency_level = "unknown",
        home_bias_score = 0
      )
  }
  
  # Calculate referee quality score
  final_analysis <- referee_analysis %>%
    dplyr::mutate(
      # Quality score components
      experience_score = pmin(25, seasons_active * 2.5),
      consistency_score = dplyr::case_when(
        consistency_level == "very_consistent" ~ 20,
        consistency_level == "consistent" ~ 15,
        consistency_level == "moderate" ~ 10,
        consistency_level == "inconsistent" ~ 5,
        TRUE ~ 12  # unknown
      ),
      bias_score = dplyr::case_when(
        bias_category == "neutral" ~ 15,
        bias_category %in% c("slight_bias") ~ 10,
        bias_category %in% c("home_favoring", "away_favoring") ~ 5,
        TRUE ~ 10  # unknown
      ),
      game_count_score = pmin(15, games_officiated * 0.5),
      playoff_score = pmin(10, playoff_games * 2),
      penalty_rate_score = dplyr::case_when(
        avg_penalties_per_game >= 8 & avg_penalties_per_game <= 16 ~ 15,
        avg_penalties_per_game >= 6 & avg_penalties_per_game <= 20 ~ 10,
        TRUE ~ 5
      ),
      # Total quality score (0-100)
      referee_quality = experience_score + consistency_score + bias_score + 
                       game_count_score + playoff_score + penalty_rate_score
    ) %>%
    dplyr::arrange(desc(referee_quality)) %>%
    dplyr::rename(referee = referee_clean)
  
  cat("Referee analysis complete for", nrow(final_analysis), "referees\n")
  cat("Quality score breakdown:\n")
  print(table(cut(final_analysis$referee_quality, breaks = c(0,50,70,85,100), 
                  labels = c("Below Average","Average","Good","Excellent"))))
  
  return(final_analysis)
}
