# ==================== COACH_ANALYSIS.R ====================
# Comprehensive coaching analysis and QB tracking
# Professional module for coaching performance evaluation

#' Analyze coaching performance and create tiers
#' 
#' @param schedule_data Complete schedule data with coach names and game results
#' @return List with: coach_tiers, matchup_analysis, special_situations,
#'         coaching_trends, starting_qbs
#' @import dplyr purrr stringr tidyr
#' @export
analyze_coaching_performance <- function(schedule_data) {
  cat("Analyzing coaching performance across all seasons...\n")
  
  req_cols <- c("season","week","home_team","away_team",
                "home_score","away_score","home_coach","away_coach","div_game")
  missing <- setdiff(req_cols, names(schedule_data))
  if (length(missing) > 0) {
    stop("analyze_coaching_performance(): schedule_data missing columns: ",
         paste(missing, collapse = ", "))
  }
  
  # Completed games only; if gameday exists, keep past-dated rows only
  sched_done <- schedule_data %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    { if ("gameday" %in% names(.)) filter(., as.Date(gameday) <= Sys.Date()) else . }
  
  # ---- Career-level coach performance ----
  coach_performance <- sched_done %>%
    pivot_longer(c(home_coach, away_coach), names_to = "coach_type", values_to = "coach_name") %>%
    mutate(
      team       = if_else(coach_type == "home_coach", home_team, away_team),
      opponent   = if_else(coach_type == "home_coach", away_team, home_team),
      team_score = if_else(coach_type == "home_coach", home_score, away_score),
      opp_score  = if_else(coach_type == "home_coach", away_score, home_score),
      win        = team_score > opp_score,
      is_home    = coach_type == "home_coach"
    ) %>%
    filter(!is.na(coach_name), coach_name != "", coach_name != "TBD") %>%
    group_by(coach_name) %>%
    summarise(
      total_games          = n(),
      total_wins           = sum(win),
      home_games           = sum(is_home),
      home_wins            = sum(win & is_home),
      away_games           = sum(!is_home),
      away_wins            = sum(win & !is_home),
      win_percentage       = total_wins / total_games,
      home_win_percentage  = ifelse(home_games > 0, home_wins / home_games, NA_real_),
      away_win_percentage  = ifelse(away_games > 0, away_wins / away_games, NA_real_),
      seasons_active       = n_distinct(season),
      teams_coached        = n_distinct(team),
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
    filter(total_games >= 16) %>%
    mutate(
      coaching_experience = last_season - first_season + 1,
      avg_tenure_per_team = ifelse(teams_coached > 0, coaching_experience / teams_coached, NA_real_),
      coach_tier = case_when(
        win_percentage >= 0.650 ~ "elite",
        win_percentage >= 0.580 ~ "very_good",
        win_percentage >= 0.520 ~ "good",
        win_percentage >= 0.450 ~ "average",
        win_percentage >= 0.400 ~ "below_average",
        TRUE                    ~ "poor"
      ),
      experience_level = case_when(
        coaching_experience >= 15 ~ "veteran",
        coaching_experience >=  8 ~ "experienced",
        coaching_experience >=  4 ~ "developing",
        coaching_experience >=  2 ~ "emerging",
        TRUE                      ~ "rookie"
      ),
      coaching_stability = case_when(
        !is.na(avg_tenure_per_team) & avg_tenure_per_team >= 8 ~ "very_stable",
        !is.na(avg_tenure_per_team) & avg_tenure_per_team >= 5 ~ "stable",
        !is.na(avg_tenure_per_team) & avg_tenure_per_team >= 3 ~ "moderate",
        TRUE                                                   ~ "unstable"
      ),
      coaching_score = pmin(100, pmax(0,
                                      (win_percentage * 60) +
                                        (coalesce(recent_performance, 0.50) * 25) +
                                        (coaching_experience * 0.8) +
                                        (ifelse(coaching_stability %in% c("stable","very_stable"), 5, 0)) +
                                        (ifelse(teams_coached == 1, 5, 0))
      ))
    ) %>%
    arrange(desc(coaching_score))
  
  cat("Processed", nrow(coach_performance), "coaches with 16+ games\n")
  
  # ---- Matchups & edges ----
  coaching_matchups <- sched_done %>%
    filter(!is.na(home_coach), !is.na(away_coach), home_coach != "", away_coach != "") %>%
    left_join(coach_performance %>% select(coach_name, coach_tier, coaching_score, experience_level),
              by = c("home_coach" = "coach_name")) %>%
    rename(coach_tier_home = coach_tier, coaching_score_home = coaching_score, experience_level_home = experience_level) %>%
    left_join(coach_performance %>% select(coach_name, coach_tier, coaching_score, experience_level),
              by = c("away_coach" = "coach_name")) %>%
    rename(coach_tier_away = coach_tier, coaching_score_away = coaching_score, experience_level_away = experience_level) %>%
    mutate(
      home_won     = home_score > away_score,
      coaching_edge = coalesce(coaching_score_home, 50) - coalesce(coaching_score_away, 50),
      matchup_type = case_when(
        abs(coaching_edge) <= 5  ~ "even_matchup",
        coaching_edge >  15      ~ "home_coaching_edge",
        coaching_edge < -15      ~ "away_coaching_edge",
        coaching_edge >   5      ~ "home_slight_edge",
        TRUE                     ~ "away_slight_edge"
      )
    )
  
  matchup_analysis <- coaching_matchups %>%
    filter(!is.na(matchup_type)) %>%
    group_by(matchup_type) %>%
    summarise(
      games = n(),
      home_win_rate = mean(home_won),
      avg_home_coaching_edge = mean(coaching_edge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Placeholder QB stability (kept for compatibility) ----
  starting_qbs <- coaching_matchups %>%
    transmute(
      season, week, home_team, away_team,
      home_qb_stability = 0.85, away_qb_stability = 0.85,
      home_qb_change_flag = FALSE, away_qb_change_flag = FALSE,
      home_qb_experience = 5, away_qb_experience = 5
    ) %>%
    pivot_longer(c(home_team, away_team), names_to = "location", values_to = "posteam") %>%
    mutate(
      qb_stability   = if_else(location == "home_team", home_qb_stability,   away_qb_stability),
      qb_change_flag = if_else(location == "home_team", home_qb_change_flag, away_qb_change_flag),
      qb_experience  = if_else(location == "home_team", home_qb_experience,  away_qb_experience)
    ) %>%
    select(season, week, posteam, qb_stability, qb_change_flag, qb_experience) %>%
    distinct()
  
  # ---- Special situations ----
  special_situations <- coaching_matchups %>%
    group_by(home_coach) %>%
    summarise(
      divisional_games     = sum(div_game %in% c(TRUE,1), na.rm = TRUE),
      divisional_wins      = sum(home_won & (div_game %in% c(TRUE,1)), na.rm = TRUE),
      divisional_win_rate  = ifelse(divisional_games > 0, divisional_wins / divisional_games, NA_real_),
      late_season_games    = sum(week >= 15, na.rm = TRUE),
      late_season_wins     = sum(home_won & week >= 15, na.rm = TRUE),
      late_season_win_rate = ifelse(late_season_games > 0, late_season_wins / late_season_games, NA_real_),
      total_games          = n(),
      .groups = "drop"
    ) %>%
    filter(total_games >= 16) %>%
    arrange(desc(divisional_win_rate))
  
  # ---- Trends ----
  coaching_trends <- sched_done %>%
    pivot_longer(c(home_coach, away_coach), names_to = "coach_type", values_to = "coach_name") %>%
    mutate(
      team            = if_else(coach_type == "home_coach", home_team, away_team),
      team_score      = if_else(coach_type == "home_coach", home_score, away_score),
      opp_score       = if_else(coach_type == "home_coach", away_score, home_score),
      win             = team_score > opp_score,
      point_differential = team_score - opp_score
    ) %>%
    filter(!is.na(coach_name), coach_name != "", coach_name != "TBD") %>%
    group_by(coach_name, season) %>%
    summarise(
      games       = n(),
      wins        = sum(win),
      win_rate    = wins / games,
      avg_point_diff = mean(point_differential),
      .groups = "drop"
    ) %>%
    group_by(coach_name) %>%
    filter(n() >= 3) %>%
    mutate(
      career_trajectory = case_when(
        last(win_rate) > first(win_rate) + 0.10 ~ "improving",
        last(win_rate) < first(win_rate) - 0.10 ~ "declining",
        TRUE                                    ~ "stable"
      )
    ) %>%
    ungroup()
  
  return(list(
    coach_tiers        = coach_performance,
    matchup_analysis   = matchup_analysis,
    special_situations = special_situations,
    coaching_trends    = coaching_trends,
    starting_qbs       = starting_qbs
  ))
}

#' Enhanced QB analysis with play-by-play integration
#'
#' @param pbp_data Play-by-play data (nflfastR)
#' @param roster_data Optional weekly roster/depth chart
#' @return List with qb_stability, qb_performance, qb_snaps
#' @export
analyze_starting_qbs <- function(pbp_data, roster_data = NULL) {
  if (is.null(pbp_data) || !all(c("season","week","posteam","passer_player_id") %in% names(pbp_data))) {
    cat("No/invalid pbp_data provided for QB analysis\n")
    return(list(qb_stability = tibble(), qb_performance = tibble(), qb_snaps = tibble()))
  }
  
  cat("Analyzing starting QB performance and stability...\n")
  
  qb_snaps <- pbp_data %>%
    filter(!is.na(passer_player_id), !is.na(posteam)) %>%
    group_by(season, week, posteam, passer_player_id, passer_player_name) %>%
    summarise(
      snaps          = n(),
      pass_attempts  = sum(pass_attempt, na.rm = TRUE),
      completions    = sum(complete_pass, na.rm = TRUE),
      passing_yards  = sum(passing_yards, na.rm = TRUE),
      passing_tds    = sum(pass_touchdown, na.rm = TRUE),
      interceptions  = sum(interception, na.rm = TRUE),
      qb_epa         = sum(qb_epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(season, week, posteam) %>%
    mutate(
      snap_percentage = snaps / sum(snaps),
      is_primary_qb   = snaps == max(snaps, na.rm = TRUE),
      qb_rating = suppressWarnings(pmin(158.3, pmax(0,
                                                    ((completions/pmax(pass_attempts,1) - 0.3) * 5 +
                                                       (passing_yards/pmax(pass_attempts,1) - 3) * 0.25 +
                                                       (passing_tds/pmax(pass_attempts,1)) * 20 +
                                                       2.375 - (interceptions/pmax(pass_attempts,1) * 25)) / 6 * 100)))
    ) %>%
    ungroup()
  
  qb_stability <- qb_snaps %>%
    group_by(season, week, posteam) %>%
    summarise(
      primary_qb_id       = first(passer_player_id[is_primary_qb]),
      primary_qb_name     = first(passer_player_name[is_primary_qb]),
      primary_qb_snap_pct = max(snap_percentage, na.rm = TRUE),
      num_qbs_used        = sum(snap_percentage > 0.1, na.rm = TRUE),
      qb_committee        = num_qbs_used > 1,
      .groups = "drop"
    ) %>%
    group_by(season, posteam) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      qb_change_from_previous = primary_qb_id != lag(primary_qb_id),
      qb_changes_season       = cumsum(replace_na(qb_change_from_previous, FALSE)),
      qb_stability = case_when(
        primary_qb_snap_pct >= 0.9 & !qb_committee ~ 1.0,
        primary_qb_snap_pct >= 0.75                ~ 0.8,
        primary_qb_snap_pct >= 0.6                 ~ 0.6,
        TRUE                                       ~ 0.4
      ),
      qb_change_flag = qb_changes_season > 0 | qb_committee
    ) %>%
    ungroup() %>%
    select(season, week, posteam, qb_stability, qb_change_flag) %>%
    mutate(qb_experience = NA_real_)  # placeholder
  
  qb_performance <- qb_snaps %>%
    filter(is_primary_qb, pass_attempts >= 10) %>%
    group_by(season, passer_player_id, passer_player_name) %>%
    summarise(
      games_started       = n(),
      total_attempts      = sum(pass_attempts),
      completion_percentage = sum(completions) / sum(pass_attempts),
      yards_per_attempt   = sum(passing_yards) / sum(pass_attempts),
      td_rate             = sum(passing_tds) / sum(pass_attempts),
      int_rate            = sum(interceptions) / sum(pass_attempts),
      avg_qb_rating       = mean(qb_rating, na.rm = TRUE),
      total_qb_epa        = sum(qb_epa),
      epa_per_play        = total_qb_epa / sum(snaps),
      .groups = "drop"
    ) %>%
    mutate(
      qb_performance_tier = case_when(
        avg_qb_rating >= 100 & epa_per_play >= 0.15  ~ "elite",
        avg_qb_rating >=  90 & epa_per_play >= 0.05  ~ "very_good",
        avg_qb_rating >=  80 & epa_per_play >= -0.05 ~ "good",
        avg_qb_rating >=  70                         ~ "average",
        TRUE                                         ~ "below_average"
      )
    )
  
  if (!is.null(roster_data) && all(c("season","week","team","position","depth_chart_position","status") %in% names(roster_data))) {
    qb_depth_analysis <- roster_data %>%
      filter(position == "QB", !is.na(depth_chart_position)) %>%
      group_by(season, week, team) %>%
      arrange(depth_chart_position, .by_group = TRUE) %>%
      summarise(
        qb1_available    = any(depth_chart_position == 1 & status %in% c("ACT","A01")),
        qb2_available    = any(depth_chart_position == 2 & status %in% c("ACT","A01")),
        qb_depth_concern = !qb1_available | (!qb2_available & sum(depth_chart_position <= 2, na.rm = TRUE) <= 1),
        .groups = "drop"
      )
    
    qb_stability <- qb_stability %>%
      left_join(qb_depth_analysis, by = c("season","week", "posteam" = "team")) %>%
      mutate(
        qb_depth_concern     = coalesce(qb_depth_concern, FALSE),
        qb_availability_risk = qb_change_flag | qb_depth_concern
      )
  }
  
  cat("QB analysis complete: ", nrow(qb_stability), " team-weeks analyzed\n")
  list(qb_stability = qb_stability, qb_performance = qb_performance, qb_snaps = qb_snaps)
}
