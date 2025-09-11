#' Enhanced Injury Analysis with Performance-Based Proxy
#' 
#' Analyzes injury impacts with performance-based imputation for missing data
#' 
#' @param start_year First season
#' @param end_year Last season  
#' @return Data frame with injury impacts for all team-weeks
#' @import dplyr stringr tidyr purrr tibble
analyze_injury_impacts <- function(start_year, end_year) {
  cat("=== ENHANCED INJURY ANALYSIS WITH PERFORMANCE PROXY ===\n")
  cat("Processing years:", start_year, "to", end_year, "\n")
  
  injury_years <- intersect(start_year:end_year, 2009:2025)
  roster_years <- intersect(start_year:end_year, 2002:2025)
  
  if (length(injury_years) == 0) {
    cat("No injury data available for", start_year, "-", end_year, "\n")
    return(data.frame())
  }
  
  cat("Loading injury data for years:", paste(injury_years, collapse = ", "), "\n")
  
  # Load data with enhanced error handling
  injuries <- tryCatch({
    nflreadr::load_injuries(injury_years)
  }, error = function(e) { 
    cat("Error loading injuries:", e$message, "\n")
    data.frame() 
  })
  
  rosters <- tryCatch({
    nflreadr::load_rosters_weekly(roster_years)
  }, error = function(e) { 
    cat("Error loading rosters:", e$message, "\n")
    data.frame() 
  })
  
  # Position importance weights
  position_weights <- tibble::tibble(
    pos = c("QB","RB","FB","WR","TE","T","G","C","DE","DT","NT",
            "OLB","ILB","MLB","LB","CB","S","FS","SS","K","P","LS"),
    weight = c(1.00,0.70,0.30,0.80,0.60,0.90,0.70,0.80,0.80,0.70,0.70,
               0.60,0.60,0.60,0.60,0.70,0.50,0.50,0.50,0.30,0.20,0.10)
  )
  
  # Injury severity weights
  severity_weights <- tibble::tibble(
    status = c("Out","Doubtful","Questionable","Probable"),
    severity = c(1.00,0.80,0.40,0.10)
  )
  
  # Process actual injury data
  actual_injury_data <- data.frame()
  
  if (nrow(injuries) > 0) {
    cat("Processing", nrow(injuries), "injury records\n")
    
    impact <- injuries %>%
      dplyr::filter(!is.na(report_status)) %>%
      dplyr::mutate(team = map_team_abbreviation(team)) %>%
      dplyr::left_join(
        rosters %>%
          dplyr::select(season, week, team, gsis_id, position, depth_chart_position, status) %>%
          dplyr::rename(roster_pos = position, roster_status = status) %>%
          dplyr::mutate(team = map_team_abbreviation(team)),
        by = c("season","week","team","gsis_id")
      ) %>%
      dplyr::mutate(
        pos_final = dplyr::coalesce(roster_pos, position),
        depth = dplyr::case_when(
          !is.na(depth_chart_position) ~ as.numeric(stringr::str_extract(depth_chart_position, "\\d+")),
          pos_final %in% c("QB", "T", "G", "C") ~ 1,
          TRUE ~ 2
        ),
        depth = dplyr::coalesce(depth, ifelse(pos_final %in% c("QB", "T", "G", "C"), 1, 2)),
        depth_penalty = dplyr::case_when(
          depth == 1 ~ 1.00,
          depth == 2 ~ 0.60,
          depth == 3 ~ 0.30,
          TRUE ~ 0.10
        )
      ) %>%
      dplyr::left_join(position_weights, by = c("pos_final" = "pos")) %>%
      dplyr::left_join(severity_weights, by = c("report_status" = "status")) %>%
      dplyr::mutate(
        weight = dplyr::coalesce(weight, 0.50),
        severity = dplyr::coalesce(severity, 0.50),
        pos_group_penalty = dplyr::case_when(
          pos_final == "QB" ~ 1.20,
          pos_final %in% c("T","C") ~ 1.10,
          TRUE ~ 1.00
        ),
        injury_impact_score = weight * severity * depth_penalty * pos_group_penalty
      ) %>%
      dplyr::filter(injury_impact_score >= 0.05)
    
    # Aggregate to team-week level
    actual_injury_data <- impact %>%
      dplyr::group_by(season, week, team) %>%
      dplyr::summarise(
        total_injury_impact = sum(injury_impact_score, na.rm = TRUE),
        num_key_injuries = sum(injury_impact_score >= 0.30),
        qb_injury_impact = sum(injury_impact_score[pos_final == "QB"], na.rm = TRUE),
        skill_injury_impact = sum(injury_impact_score[pos_final %in% c("RB","WR","TE")], na.rm = TRUE),
        oline_injury_impact = sum(injury_impact_score[pos_final %in% c("T","G","C")], na.rm = TRUE),
        defense_injury_impact = sum(injury_impact_score[pos_final %in% c("DE","DT","NT","OLB","ILB","MLB","LB","CB","S","FS","SS")], na.rm = TRUE),
        .groups = "drop"
      )
    
    cat("Created actual injury data for", nrow(actual_injury_data), "team-weeks\n")
  } else {
    cat("No actual injury data available\n")
  }
  
  # Create performance-based proxy for missing data
  cat("Creating performance-based injury proxy...\n")
  
  # Load weekly performance data (need extra year for lagging)
  weekly_data <- tryCatch({
    prepare_weekly((start_year-1):end_year)
  }, error = function(e) {
    cat("Could not load weekly data for proxy:", e$message, "\n")
    return(data.frame())
  })
  
  if (nrow(weekly_data) > 0) {
    performance_proxy <- weekly_data %>%
      dplyr::filter(season >= start_year, season <= end_year) %>%
      dplyr::arrange(season, posteam, week) %>%
      dplyr::group_by(season, posteam) %>%
      dplyr::mutate(
        # Weighted 3-week performance trends (more recent = higher weight)
        recent_off_epa = (dplyr::lag(off_epa, 1) * 0.5 + 
                          dplyr::lag(off_epa, 2) * 0.3 + 
                          dplyr::lag(off_epa, 3) * 0.2),
        recent_def_epa = (dplyr::lag(def_epa, 1) * 0.5 + 
                          dplyr::lag(def_epa, 2) * 0.3 + 
                          dplyr::lag(def_epa, 3) * 0.2),
        recent_qb_epa = (dplyr::lag(qb_epa, 1) * 0.5 + 
                         dplyr::lag(qb_epa, 2) * 0.3 + 
                         dplyr::lag(qb_epa, 3) * 0.2),
        recent_points = (dplyr::lag(points_scored, 1) * 0.5 + 
                         dplyr::lag(points_scored, 2) * 0.3 + 
                         dplyr::lag(points_scored, 3) * 0.2)
      ) %>%
      dplyr::ungroup()
    
    # Calculate league averages for normalization
    league_norms <- performance_proxy %>%
      dplyr::group_by(season, week) %>%
      dplyr::summarise(
        avg_off_epa = mean(off_epa, na.rm = TRUE),
        avg_def_epa = mean(def_epa, na.rm = TRUE),
        avg_qb_epa = mean(qb_epa, na.rm = TRUE),
        avg_points = mean(points_scored, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Create injury proxy based on performance decline
    performance_proxy <- performance_proxy %>%
      dplyr::left_join(league_norms, by = c("season", "week")) %>%
      dplyr::mutate(
        # Performance decline = potential injury impact
        # Higher values = worse performance = more injury impact
        off_decline = pmax(0, avg_off_epa - dplyr::coalesce(recent_off_epa, off_epa)),
        def_decline = pmax(0, dplyr::coalesce(recent_def_epa, def_epa) - avg_def_epa),
        qb_decline = pmax(0, avg_qb_epa - dplyr::coalesce(recent_qb_epa, qb_epa)),
        points_decline = pmax(0, avg_points - dplyr::coalesce(recent_points, points_scored)),
        
        # Combine into injury impact proxy (scaled to reasonable range)
        total_injury_impact = pmax(0.2, pmin(2.5,
          off_decline * 0.3 + def_decline * 0.2 + qb_decline * 0.3 + 
          points_decline * 0.02 + 0.4
        )),
        
        # Component proxies
        qb_injury_impact = pmax(0, pmin(1.0, qb_decline * 0.5 + 0.1)),
        skill_injury_impact = total_injury_impact * 0.25,
        oline_injury_impact = total_injury_impact * 0.20,
        defense_injury_impact = total_injury_impact * 0.35,
        num_key_injuries = pmax(0, round(total_injury_impact * 0.8))
      ) %>%
      dplyr::select(season, week, posteam, 
                   total_injury_impact, qb_injury_impact, skill_injury_impact,
                   oline_injury_impact, defense_injury_impact, num_key_injuries) %>%
      dplyr::rename(team = posteam)
    
    cat("Created performance proxy for", nrow(performance_proxy), "team-weeks\n")
  } else {
    performance_proxy <- data.frame()
  }
  
  # Create complete grid for all teams and weeks
  all_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
                 "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", 
                 "LV", "LAC", "LA", "MIA", "MIN", "NE", "NO", "NYG", 
                 "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")
  
  complete_grid <- tidyr::expand_grid(
    season = start_year:end_year,
    week = 1:22,
    team = all_teams
  )
  
  # Combine actual data with performance proxy
  complete_data <- complete_grid %>%
    dplyr::left_join(actual_injury_data, by = c("season", "week", "team")) %>%
    dplyr::left_join(performance_proxy, by = c("season", "week", "team"), 
                     suffix = c("", "_proxy")) %>%
    dplyr::mutate(
      # Use actual data where available, performance proxy where missing
      total_injury_impact = dplyr::coalesce(
        total_injury_impact, 
        total_injury_impact_proxy,
        0.6  # Final fallback
      ),
      qb_injury_impact = dplyr::coalesce(
        qb_injury_impact, 
        qb_injury_impact_proxy,
        0.1
      ),
      skill_injury_impact = dplyr::coalesce(
        skill_injury_impact, 
        skill_injury_impact_proxy,
        0.15
      ),
      oline_injury_impact = dplyr::coalesce(
        oline_injury_impact, 
        oline_injury_impact_proxy,
        0.12
      ),
      defense_injury_impact = dplyr::coalesce(
        defense_injury_impact, 
        defense_injury_impact_proxy,
        0.21
      ),
      num_key_injuries = dplyr::coalesce(
        num_key_injuries, 
        num_key_injuries_proxy,
        1L
      )
    ) %>%
    dplyr::select(-dplyr::ends_with("_proxy"))
  
  # Special handling for Week 1 - use prior season carryover
  week1_fixes <- complete_data %>%
    dplyr::filter(week == 1) %>%
    dplyr::group_by(team) %>%
    dplyr::arrange(season) %>%
    dplyr::mutate(
      lag_total = dplyr::lag(total_injury_impact) * 0.7,
      lag_qb = dplyr::lag(qb_injury_impact) * 0.7,
      lag_skill = dplyr::lag(skill_injury_impact) * 0.7,
      lag_oline = dplyr::lag(oline_injury_impact) * 0.7,
      lag_defense = dplyr::lag(defense_injury_impact) * 0.7,
      lag_num = dplyr::lag(num_key_injuries) * 0.7
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total_injury_impact = dplyr::case_when(
        season == start_year ~ 0.5,
        !is.na(lag_total) ~ lag_total,
        TRUE ~ total_injury_impact * 0.8
      ),
      qb_injury_impact = dplyr::case_when(
        season == start_year ~ 0.1,
        !is.na(lag_qb) ~ lag_qb,
        TRUE ~ qb_injury_impact * 0.8
      ),
      skill_injury_impact = dplyr::case_when(
        season == start_year ~ 0.15,
        !is.na(lag_skill) ~ lag_skill,
        TRUE ~ skill_injury_impact * 0.8
      ),
      oline_injury_impact = dplyr::case_when(
        season == start_year ~ 0.12,
        !is.na(lag_oline) ~ lag_oline,
        TRUE ~ oline_injury_impact * 0.8
      ),
      defense_injury_impact = dplyr::case_when(
        season == start_year ~ 0.2,
        !is.na(lag_defense) ~ lag_defense,
        TRUE ~ defense_injury_impact * 0.8
      ),
      num_key_injuries = dplyr::case_when(
        season == start_year ~ 1L,
        !is.na(lag_num) ~ as.integer(lag_num),
        TRUE ~ as.integer(num_key_injuries * 0.8)
      )
    ) %>%
    dplyr::select(-dplyr::starts_with("lag_"))
  
  # Combine Week 1 fixes with other weeks
  final_data <- complete_data %>%
    dplyr::filter(week != 1) %>%
    dplyr::bind_rows(week1_fixes) %>%
    dplyr::arrange(season, week, team)
  
  cat("Injury analysis complete:\n")
  cat("- Team-weeks:", nrow(final_data), "\n") 
  cat("- Seasons:", paste(range(final_data$season), collapse = "-"), "\n")
  cat("- Teams:", length(unique(final_data$team)), "\n")
  cat("- Actual injury data coverage:", 
      round(sum(!is.na(actual_injury_data$total_injury_impact)) / nrow(final_data) * 100, 1), "%\n")
  cat("- Performance proxy usage:", 
      round(sum(is.na(actual_injury_data$total_injury_impact)) / nrow(final_data) * 100, 1), "%\n")
  
  return(final_data)
}
