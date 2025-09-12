#' Enhanced Referee Performance Analysis with Bias Detection
#'
#' Comprehensive analysis of NFL referee performance that evaluates officiating
#' quality, bias tendencies, and penalty patterns. Integrates schedule data with
#' play-by-play information to provide detailed referee assessments including
#' home field bias detection and consistency measurements.
#'
#' @param schedule_data Data frame containing NFL schedule information with
#'   referee assignments. Required columns:
#'   \itemize{
#'     \item referee - Name of the head referee for each game
#'     \item season, week - Game timing information  
#'     \item home_team, away_team - Team abbreviations
#'     \item game_id - Unique game identifier
#'   }
#'   Optional columns:
#'   \itemize{
#'     \item game_type - Regular season vs playoff designation
#'   }
#'
#' @param pbp_data Optional data frame containing play-by-play information for
#'   enhanced penalty analysis. When provided, enables detailed penalty pattern
#'   analysis. Expected columns include:
#'   \itemize{
#'     \item season, week, game_id - Game identification
#'     \item penalty - Binary indicator for penalty plays (1 = penalty, 0 = no penalty)
#'     \item penalty_team - Team that committed the penalty
#'     \item penalty_type - Description of penalty type
#'     \item penalty_yards - Yards assessed for the penalty
#'     \item qtr - Quarter of the game
#'     \item game_seconds_remaining - Time remaining in game
#'   }
#'   If NULL, analysis proceeds with basic referee statistics only.
#'
#' @return Data frame containing comprehensive referee analysis with the following columns:
#'   \describe{
#'     \item{referee}{Cleaned referee name (title case, standardized)}
#'     \item{referee_quality}{Composite quality score (0-100) based on experience, 
#'       consistency, bias, and game assignments}
#'     \item{referee_experience}{Experience level: veteran, experienced, developing, new}
#'     \item{games_officiated}{Total number of games officiated in dataset}
#'     \item{seasons_active}{Number of seasons active as NFL referee}
#'     \item{playoff_games}{Number of playoff games officiated}
#'     \item{bias_category}{Home field bias classification: neutral, home_favoring,
#'       away_favoring, slight_bias, unknown}
#'     \item{home_bias_score}{Numerical bias score where positive values indicate
#'       away team favoritism, negative values indicate home team favoritism}
#'     \item{consistency_level}{Penalty calling consistency: very_consistent,
#'       consistent, moderate, inconsistent, unknown}
#'     \item{avg_penalties_per_game}{Average penalties called per game}
#'   }
#'   Additional columns when pbp_data is provided:
#'   \describe{
#'     \item{penalty_consistency}{Standard deviation of penalties per game}
#'     \item{holding_rate, pre_snap_rate, etc.}{Rates for different penalty categories}
#'     \item{crunch_time_rate}{Rate of penalties called in final 5 minutes}
#'   }
#'
#' @details
#' The referee analysis employs several sophisticated methodologies:
#'
#' **Quality Score Calculation** (0-100 scale):
#' \itemize{
#'   \item Experience Score: Up to 25 points based on seasons active (2.5 points per season)
#'   \item Consistency Score: 5-20 points based on penalty calling consistency
#'   \item Bias Score: 5-15 points based on neutrality (higher for neutral referees)
#'   \item Game Count Score: Up to 15 points based on total games officiated
#'   \item Playoff Score: Up to 10 points for playoff game assignments
#'   \item Penalty Rate Score: Up to 15 points for appropriate penalty rates (8-16 per game)
#' }
#'
#' **Bias Detection**: Analyzes the distribution of penalties called on home vs away teams:
#' \itemize{
#'   \item Neutral: Home penalty rate between 47-53% (±3% from expected 50%)
#'   \item Home Favoring: Home penalty rate >53% (calls more penalties on away team)
#'   \item Away Favoring: Home penalty rate <47% (calls more penalties on home team)
#'   \item Slight Bias: Moderate deviations from neutral
#' }
#'
#' **Penalty Pattern Analysis** (when pbp_data provided):
#' \itemize{
#'   \item Categorizes penalties into types: holding/blocking, pre-snap, pass interference,
#'     personal fouls, procedural violations
#'   \item Analyzes penalty timing: first half vs late game vs crunch time (final 5 minutes)
#'   \item Measures consistency through standard deviation of penalties per game
#' }
#'
#' **Data Quality Handling**:
#' \itemize{
#'   \item Filters referees with <5 games to ensure statistical reliability
#'   \item Standardizes referee names (title case, spacing, common corrections)
#'   \item Provides default values for referees without sufficient penalty data
#'   \item Handles missing play-by-play data gracefully
#' }
#'
#' @examples
#' \dontrun{
#' # Basic referee analysis without play-by-play data
#' schedule <- nflreadr::load_schedules(2020:2023)
#' referee_analysis <- analyze_referee_performance(schedule)
#' 
#' # Enhanced analysis with penalty patterns
#' pbp <- nflfastR::load_pbp(2020:2023)
#' referee_analysis <- analyze_referee_performance(schedule, pbp)
#' 
#' # View top-quality referees
#' head(referee_analysis[order(-referee_analysis$referee_quality), ], 10)
#' 
#' # Check for bias patterns
#' biased_refs <- referee_analysis[referee_analysis$bias_category != "neutral", ]
#' 
#' # Analyze consistency
#' consistent_refs <- referee_analysis[
#'   referee_analysis$consistency_level %in% c("very_consistent", "consistent"), ]
#' }
#'
#' @seealso
#' \code{\link{prepare_games}} for integration with full game preparation
#' \code{\link{analyze_coaching_performance}} for coaching analysis
#'
#' @import dplyr purrr stringr
#' @export
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
      referee_clean = stringr::str_trim(referee),
      referee_clean = stringr::str_to_title(referee_clean),
      referee_clean = stringr::str_replace_all(referee_clean, "\\s+", " "),
      # Fix common name variations
      referee_clean = stringr::str_replace_all(referee_clean, "Mcaulay", "McAulay"),
      referee_clean = stringr::str_replace_all(referee_clean, "Mcdaniels", "McDaniels")
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
    
    # Join pbp with referee info - rename to avoid conflicts
    pbp_with_ref <- pbp_data %>%
      dplyr::left_join(
        clean_schedule %>%
          dplyr::select(season, week, game_id, referee_clean, home_team, away_team) %>%
          dplyr::rename(sched_home_team = home_team, sched_away_team = away_team),
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
          penalty_on_home = penalty_team == sched_home_team,
          penalty_on_away = penalty_team == sched_away_team,
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
      # Apply penalty for very limited experience
      limited_experience_penalty = ifelse(games_officiated < 5, 10, 0),
      
      # Total quality score (0-100)
      referee_quality = pmax(30, experience_score + consistency_score + bias_score + 
                                 game_count_score + playoff_score + penalty_rate_score - limited_experience_penalty)
    ) %>%
    dplyr::arrange(desc(referee_quality)) %>%
    dplyr::rename(referee = referee_clean)
  
  cat("Referee analysis complete for", nrow(final_analysis), "referees\n")
  cat("Quality score breakdown:\n")
  print(table(cut(final_analysis$referee_quality, breaks = c(0,50,70,85,100), 
                  labels = c("Below Average","Average","Good","Excellent"))))
  
  return(final_analysis)
}
