#' Analyze Coaching Performance and Create Tiers
#'
#' Comprehensive analysis of NFL head coach performance that creates coaching tiers,
#' analyzes matchup advantages, and tracks special situation performance. Only
#' includes coaches with sufficient game sample sizes (16+ games) for reliable
#' statistical analysis.
#'
#' @param schedule_data Data frame containing NFL schedule information with the
#'   following required columns:
#'   \itemize{
#'     \item season - NFL season year
#'     \item week - Week number within season
#'     \item home_team, away_team - Team abbreviations
#'     \item home_score, away_score - Final game scores
#'     \item home_coach, away_coach - Head coach names
#'     \item div_game - Logical or integer indicating divisional games
#'   }
#'   Optional columns:
#'   \itemize{
#'     \item gameday - Game date (for filtering completed games only)
#'   }
#'
#' @return A list containing five comprehensive coaching analysis components:
#'   \describe{
#'     \item{coach_tiers}{
#'       Data frame with coaching performance tiers including:
#'       \itemize{
#'         \item coach_name - Head coach name
#'         \item coach_tier - Performance tier (elite, very_good, good, average, below_average, poor)
#'         \item coaching_score - Composite coaching quality score (0-100)
#'         \item experience_level - Experience category (veteran, experienced, developing, emerging, rookie)
#'         \item win_percentage - Overall career win rate
#'         \item seasons_active - Number of seasons as head coach
#'         \item teams_coached - Number of different teams coached
#'         \item coaching_stability - Tenure stability measure
#'       }
#'     }
#'     \item{matchup_analysis}{
#'       Summary of coaching matchup types and their outcomes, showing how
#'       coaching advantages translate to game results
#'     }
#'     \item{special_situations}{
#'       Performance in special game contexts (divisional games, late season)
#'       by coach
#'     }
#'     \item{coaching_trends}{
#'       Career trajectory analysis showing improving/declining/stable coaches
#'       over multiple seasons
#'     }
#'     \item{starting_qbs}{
#'       Placeholder QB stability analysis (requires separate QB data for full implementation)
#'     }
#'   }
#'
#' @details
#' The coaching analysis employs several key methodologies:
#'
#' **Performance Tiers**: Coaches are classified into six tiers based on win percentage:
#' \itemize{
#'   \item Elite (≥65.0%) - Historically great coaches
#'   \item Very Good (58.0-64.9%) - Consistently strong coaches  
#'   \item Good (52.0-57.9%) - Above average coaches
#'   \item Average (45.0-51.9%) - League average coaches
#'   \item Below Average (40.0-44.9%) - Struggling coaches
#'   \item Poor (<40.0%) - Coaches with significant difficulties
#' }
#'
#' **Coaching Score**: Composite 0-100 metric incorporating:
#' \itemize{
#'   \item Win percentage (60% weight)
#'   \item Recent performance - last 3 seasons (25% weight)
#'   \item Experience bonus (coaching years * 0.8)
#'   \item Stability bonus (5 points for stable tenure)
#'   \item Loyalty bonus (5 points for single-team coaches)
#' }
#'
#' **Experience Levels**: Based on seasons as head coach:
#' \itemize{
#'   \item Veteran (15+ seasons)
#'   \item Experienced (8-14 seasons)
#'   \item Developing (4-7 seasons)
#'   \item Emerging (2-3 seasons)
#'   \item Rookie (1 season)
#' }
#'
#' **Sample Size Filter**: Only coaches with 16+ games are included to ensure
#' statistical reliability and exclude interim/emergency coaches.
#'
#' @examples
#' \dontrun{
#' # Basic coaching analysis
#' schedule <- nflreadr::load_schedules(2020:2023)
#' coaching_analysis <- analyze_coaching_performance(schedule)
#' 
#' # View top coaches
#' head(coaching_analysis$coach_tiers, 10)
#' 
#' # Analyze coaching matchups
#' print(coaching_analysis$matchup_analysis)
#' 
#' # Check coaching trends
#' improving_coaches <- coaching_analysis$coaching_trends %>%
#'   filter(career_trajectory == "improving")
#' }
#'
#' @seealso
#' \code{\link{prepare_games}} for integration with full game preparation
#'
#' @import dplyr tidyr purrr stringr
#' @export
analyze_coaching_performance <- function(schedule_data) {
  cat("Analyzing coaching performance across all seasons...\n")
  
  req_cols <- c(
    "season","week","home_team","away_team",
    "home_score","away_score","home_coach","away_coach","div_game"
  )
  missing <- setdiff(req_cols, names(schedule_data))
  if (length(missing) > 0) {
    stop("analyze_coaching_performance(): schedule_data missing columns: ",
         paste(missing, collapse = ", "))
  }
  
  # Completed games only; if gameday exists, keep past-dated rows only
  sched_done <- schedule_data %>%
    dplyr::filter(!is.na(home_score), !is.na(away_score)) %>%
    { if ("gameday" %in% names(.))
      dplyr::filter(., as.Date(gameday) <= Sys.Date()) else . }
  
  # ---- Career-level coach performance ----
  coach_performance <- sched_done %>%
    tidyr::pivot_longer(
      c(home_coach, away_coach),
      names_to = "coach_type",
      values_to = "coach_name"
    ) %>%
    dplyr::mutate(
      team       = if_else(coach_type == "home_coach", home_team, away_team),
      opponent   = if_else(coach_type == "home_coach", away_team, home_team),
      team_score = if_else(coach_type == "home_coach", home_score, away_score),
      opp_score  = if_else(coach_type == "home_coach", away_score, home_score),
      win        = team_score > opp_score,
      is_home    = coach_type == "home_coach"
    ) %>%
    dplyr::filter(!is.na(coach_name), coach_name != "", coach_name != "TBD") %>%
    dplyr::group_by(coach_name) %>%
    dplyr::summarise(
      total_games          = dplyr::n(),
      total_wins           = sum(win),
      home_games           = sum(is_home),
      home_wins            = sum(win & is_home),
      away_games           = sum(!is_home),
      away_wins            = sum(win & !is_home),
      win_percentage       = total_wins / total_games,
      home_win_percentage  = ifelse(home_games > 0, home_wins / home_games, NA_real_),
      away_win_percentage  = ifelse(away_games > 0, away_wins / away_games, NA_real_),
      seasons_active       = dplyr::n_distinct(season),
      teams_coached        = dplyr::n_distinct(team),
      first_season         = min(season, na.rm = TRUE),
      last_season          = max(season, na.rm = TRUE),
      recent_performance   = {
        s_max <- max(season, na.rm = TRUE)
        num   <- sum(win[season >= s_max - 2], na.rm = TRUE)
        den   <- sum(season >= s_max - 2, na.rm = TRUE)
        ifelse(den > 0, num / den, NA_real_)
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(total_games >= 16) %>%
    dplyr::mutate(
      coaching_experience = last_season - first_season + 1,
      avg_tenure_per_team = ifelse(teams_coached > 0,
                                   coaching_experience / teams_coached,
                                   NA_real_),
      coach_tier = dplyr::case_when(
        win_percentage >= 0.650 ~ "elite",
        win_percentage >= 0.580 ~ "very_good",
        win_percentage >= 0.520 ~ "good",
        win_percentage >= 0.450 ~ "average",
        win_percentage >= 0.400 ~ "below_average",
        TRUE                    ~ "poor"
      ),
      experience_level = dplyr::case_when(
        coaching_experience >= 15 ~ "veteran",
        coaching_experience >=  8 ~ "experienced",
        coaching_experience >=  4 ~ "developing",
        coaching_experience >=  2 ~ "emerging",
        TRUE                      ~ "rookie"
      ),
      coaching_stability = dplyr::case_when(
        !is.na(avg_tenure_per_team) & avg_tenure_per_team >= 8 ~ "very_stable",
        !is.na(avg_tenure_per_team) & avg_tenure_per_team >= 5 ~ "stable",
        !is.na(avg_tenure_per_team) & avg_tenure_per_team >= 3 ~ "moderate",
        TRUE                                                   ~ "unstable"
      ),
      coaching_score = pmin(
        100,
        pmax(
          0,
          (win_percentage * 60) +
            (coalesce(recent_performance, 0.50) * 25) +
            (coaching_experience * 0.8) +
            (ifelse(coaching_stability %in% c("stable","very_stable"), 5, 0)) +
            (ifelse(teams_coached == 1, 5, 0))
        )
      )
    ) %>%
    dplyr::arrange(dplyr::desc(coaching_score))
  
  cat("Processed", nrow(coach_performance), "coaches with 16+ games\n")
  
  # ---- Matchups & edges ----
  coaching_matchups <- sched_done %>%
    dplyr::filter(!is.na(home_coach), !is.na(away_coach),
                  home_coach != "", away_coach != "") %>%
    dplyr::left_join(coach_performance %>%
                       dplyr::select(coach_name, coach_tier,
                                     coaching_score, experience_level),
                     by = c("home_coach" = "coach_name")) %>%
    dplyr::rename(
      coach_tier_home = coach_tier,
      coaching_score_home = coaching_score,
      experience_level_home = experience_level
    ) %>%
    dplyr::left_join(coach_performance %>%
                       dplyr::select(coach_name, coach_tier,
                                     coaching_score, experience_level),
                     by = c("away_coach" = "coach_name")) %>%
    dplyr::rename(
      coach_tier_away = coach_tier,
      coaching_score_away = coaching_score,
      experience_level_away = experience_level
    ) %>%
    dplyr::mutate(
      home_won      = home_score > away_score,
      coaching_edge = coalesce(coaching_score_home, 50) -
        coalesce(coaching_score_away, 50),
      matchup_type  = dplyr::case_when(
        abs(coaching_edge) <= 5  ~ "even_matchup",
        coaching_edge >  15      ~ "home_coaching_edge",
        coaching_edge < -15      ~ "away_coaching_edge",
        coaching_edge >   5      ~ "home_slight_edge",
        TRUE                     ~ "away_slight_edge"
      )
    )
  
  matchup_analysis <- coaching_matchups %>%
    dplyr::filter(!is.na(matchup_type)) %>%
    dplyr::group_by(matchup_type) %>%
    dplyr::summarise(
      games                 = dplyr::n(),
      home_win_rate         = mean(home_won),
      avg_home_coaching_edge= mean(coaching_edge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Placeholder QB stability ----
  starting_qbs <- coaching_matchups %>%
    dplyr::transmute(
      season, week, home_team, away_team,
      home_qb_stability = 0.85, away_qb_stability = 0.85,
      home_qb_change_flag = FALSE, away_qb_change_flag = FALSE,
      home_qb_experience = 5, away_qb_experience = 5
    ) %>%
    tidyr::pivot_longer(
      c(home_team, away_team), names_to = "location", values_to = "posteam"
    ) %>%
    dplyr::mutate(
      qb_stability   = if_else(location == "home_team", home_qb_stability,
                               away_qb_stability),
      qb_change_flag = if_else(location == "home_team", home_qb_change_flag,
                               away_qb_change_flag),
      qb_experience  = if_else(location == "home_team", home_qb_experience,
                               away_qb_experience)
    ) %>%
    dplyr::select(season, week, posteam, qb_stability, qb_change_flag,
                  qb_experience) %>%
    dplyr::distinct()
  
  # ---- Special situations ----
  special_situations <- coaching_matchups %>%
    dplyr::group_by(home_coach) %>%
    dplyr::summarise(
      divisional_games    = sum(div_game %in% c(TRUE,1), na.rm = TRUE),
      divisional_wins     = sum(home_won & (div_game %in% c(TRUE,1)), na.rm = TRUE),
      divisional_win_rate = ifelse(divisional_games > 0,
                                   divisional_wins / divisional_games,
                                   NA_real_),
      late_season_games    = sum(week >= 15, na.rm = TRUE),
      late_season_wins     = sum(home_won & week >= 15, na.rm = TRUE),
      late_season_win_rate = ifelse(late_season_games > 0,
                                    late_season_wins / late_season_games,
                                    NA_real_),
      total_games          = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(total_games >= 16) %>%
    dplyr::arrange(dplyr::desc(divisional_win_rate))
  
  # ---- Trends ----
  coaching_trends <- sched_done %>%
    tidyr::pivot_longer(c(home_coach, away_coach),
                        names_to = "coach_type", values_to = "coach_name") %>%
    dplyr::mutate(
      team             = if_else(coach_type == "home_coach", home_team, away_team),
      team_score       = if_else(coach_type == "home_coach", home_score, away_score),
      opp_score        = if_else(coach_type == "home_coach", away_score, home_score),
      win              = team_score > opp_score,
      point_diff       = team_score - opp_score
    ) %>%
    dplyr::filter(!is.na(coach_name), coach_name != "", coach_name != "TBD") %>%
    dplyr::group_by(coach_name, season) %>%
    dplyr::summarise(
      games          = dplyr::n(),
      wins           = sum(win),
      win_rate       = wins / games,
      avg_point_diff = mean(point_diff),
      .groups        = "drop"
    ) %>%
    dplyr::group_by(coach_name) %>%
    dplyr::filter(dplyr::n() >= 3) %>%
    dplyr::mutate(
      career_trajectory = dplyr::case_when(
        dplyr::last(win_rate) > dplyr::first(win_rate) + 0.10 ~ "improving",
        dplyr::last(win_rate) < dplyr::first(win_rate) - 0.10 ~ "declining",
        TRUE                                                 ~ "stable"
      )
    ) %>%
    dplyr::ungroup()
  
  list(
    coach_tiers        = coach_performance,
    matchup_analysis   = matchup_analysis,
    special_situations = special_situations,
    coaching_trends    = coaching_trends,
    starting_qbs       = starting_qbs
  )
}
