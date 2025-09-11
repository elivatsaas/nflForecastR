#' Enhanced Coaching Analysis
#' 
#' Analyzes coaching performance with lower thresholds to include interim coaches
#' and better handling of mid-season changes
#' 
#' @param sched Schedule data frame
#' @return List with coach_tiers and coverage stats
enhance_coaching_analysis <- function(sched) {
  cat("Enhanced coaching analysis starting...\n")
  
  # Step 1: Get all unique coach-team-season combinations from schedule
  all_coaches <- sched %>%
    dplyr::select(season, home_team, away_team, home_coach, away_coach) %>%
    tidyr::pivot_longer(cols = c(home_coach, away_coach), 
                names_to = "side", values_to = "coach_name") %>%
    dplyr::mutate(team = ifelse(side == "home_coach", home_team, away_team)) %>%
    dplyr::filter(!is.na(coach_name), coach_name != "", coach_name != "TBD") %>%
    dplyr::select(season, team, coach_name) %>%
    dplyr::distinct()
  
  # Step 2: Count games per coach (including interim coaches)
  coach_games <- sched %>%
    dplyr::select(season, week, home_team, away_team, home_coach, away_coach) %>%
    tidyr::pivot_longer(cols = c(home_coach, away_coach),
                names_to = "side", values_to = "coach_name") %>%
    dplyr::mutate(team = ifelse(side == "home_coach", home_team, away_team)) %>%
    dplyr::filter(!is.na(coach_name), coach_name != "", coach_name != "TBD") %>%
    dplyr::count(coach_name, season, team, name = "games_coached") %>%
    dplyr::arrange(desc(games_coached))
  
  # Step 3: Create coach tiers with lower thresholds and interim handling
  coach_tiers <- coach_games %>%
    dplyr::group_by(coach_name) %>%
    dplyr::summarize(
      total_games = sum(games_coached),
      seasons_active = dplyr::n_distinct(season),
      teams_coached = dplyr::n_distinct(team),
      avg_games_per_season = mean(games_coached),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # More inclusive tiering system
      coach_tier = dplyr::case_when(
        total_games >= 50 ~ "Elite",           # Multiple seasons
        total_games >= 30 ~ "Experienced",    # 2+ seasons typically  
        total_games >= 16 ~ "Established",    # 1 full season
        total_games >= 8 ~ "Developing",      # Half season or interim
        TRUE ~ "New"                          # Very limited experience
      ),
      
      # Scoring system that doesn't penalize interim coaches as much
      coaching_score = dplyr::case_when(
        coach_tier == "Elite" ~ 75,
        coach_tier == "Experienced" ~ 65,
        coach_tier == "Established" ~ 55,
        coach_tier == "Developing" ~ 45,     # Interim coaches get reasonable score
        TRUE ~ 40
      ),
      
      experience_level = dplyr::case_when(
        seasons_active >= 5 ~ "Veteran",
        seasons_active >= 3 ~ "Experienced", 
        seasons_active >= 2 ~ "Developing",
        TRUE ~ "New"
      ),
      
      # Estimate win percentage (placeholder - could be enhanced with actual wins)
      win_percentage = pmax(0.2, pmin(0.8, 0.5 + (coaching_score - 50) * 0.006))
    ) %>%
    dplyr::arrange(desc(total_games))
  
  cat("Enhanced coaching tiers created for", nrow(coach_tiers), "coaches\n")
  cat("Tier breakdown:\n")
  print(table(coach_tiers$coach_tier))
  
  return(list(
    coach_tiers = coach_tiers,
    coach_games = coach_games,
    coverage_stats = list(
      total_coaches = nrow(coach_tiers),
      coaches_with_8plus = sum(coach_tiers$total_games >= 8),
      coaches_with_16plus = sum(coach_tiers$total_games >= 16)
    )
  ))
}

#' Enhanced Prepare NFL Schedule Data with Internal Weekly Data Generation
#'
#' Builds a modeling frame by joining nflreadr schedule with lagged team features.
#' Automatically generates weekly team features using prepare_weekly() and lags them
#' 1 week within (season, posteam) before joining to each game. Includes enhanced
#' coaching, injury, and referee analysis.
#'
#' @param years vector of integers, seasons to include in final dataset
#' @param include_injuries logical, join injury impacts using enhanced analysis
#' @param include_coaching logical, join coaching tiers using enhanced analysis
#' @param include_referee logical, join referee analysis and tendencies
#' @param seed_week1 logical, if TRUE carry prior season's final stats forward
#'   into Week 1 for lagged features
#' @return data.frame of games with home/away lagged features and schedule fields
#' @import dplyr tidyr purrr stringr
#' @export
prepare_games <- function(years,
                          include_injuries = TRUE,
                          include_coaching = TRUE,
                          include_referee = TRUE,
                          seed_week1 = TRUE) {

  cat("=== ENHANCED NFL DATA PREPARATION ===\n")
  cat("Processing years:", paste(years, collapse = ", "), "\n")
  cat("Features: Injuries =", include_injuries,
      "| Coaching =", include_coaching,
      "| Referee =", include_referee,
      "| Seed Week1 =", seed_week1, "\n")
  
  if (!requireNamespace("nflreadr", quietly = TRUE))
    stop("nflreadr is required for prepare_games().")
  
  # ------------------ 1) DETERMINE YEARS FOR WEEKLY DATA ------------------
  # Include prior year for seeding if requested
  weekly_years <- if (seed_week1) {
    (min(years) - 1L):max(years)
  } else {
    years
  }
  
  cat("Preparing weekly data for years:", paste(weekly_years, collapse = ", "), "\n")
  if (seed_week1 && (min(years) - 1L) %in% weekly_years) {
    cat("Including", min(years) - 1L, "data for Week 1", min(years), "seeding\n")
  }
  
  # ------------------ 2) PREPARE WEEKLY DATA ------------------
  weekly_data <- tryCatch({
    prepare_weekly(weekly_years)
  }, error = function(e) {
    stop("Failed to prepare weekly data: ", e$message)
  })
  
  if (nrow(weekly_data) == 0) {
    stop("prepare_weekly() returned no data for years ", paste(weekly_years, collapse = ", "))
  }
  
  cat("Weekly data prepared:", nrow(weekly_data), "team-weeks\n")
  
  # ------------------ 3) LOAD SCHEDULE DATA (TARGET YEARS ONLY) ------------------
  sched <- purrr::map_dfr(
    years,  # Only target years, not including prior year
    ~ nflreadr::load_schedules(.x) %>%
      dplyr::select(dplyr::any_of(c(
        "game_id","season","week","game_type",
        "gameday","weekday","gametime","location",
        "home_team","away_team",
        "home_score","away_score",
        "home_rest","away_rest",
        "home_moneyline","away_moneyline",
        "spread_line","total_line",
        "home_spread_odds","away_spread_odds",
        "under_odds","over_odds",
        "div_game","roof","surface",
        "home_coach","away_coach",
        "referee","stadium_id","stadium"
      )))
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      home_team = map_team_abbreviation(home_team),
      away_team = map_team_abbreviation(away_team)
    )
  
  cat("Schedule loaded:", nrow(sched), "games (target years only)\n")
  
  # ------------------ 4) CLEAN AND STANDARDIZE WEEKLY DATA ------------------
  if ("posteam" %in% names(weekly_data)) {
    weekly_data$posteam <- map_team_abbreviation(weekly_data$posteam)
  }
  
  # Remove columns that will be added from schedule
  weekly_data <- weekly_data %>% 
    dplyr::select(-dplyr::any_of(c("game_id", "posteam_type")))

  # Filter to weeks that exist in our target schedule
  valid_weeks <- unique(sched$week)
  weekly_data_filtered <- weekly_data %>% 
    dplyr::filter(week %in% valid_weeks)
  
  # ------------------ 5) CREATE LAGGED FEATURES ------------------
  key_cols <- c("season","week","posteam","game_id","posteam_type")
  key_cols <- intersect(key_cols, names(weekly_data_filtered))
  
  pregame_cols <- intersect(c(
    "spread_line","total_line","div_game","roof","surface"
  ), names(weekly_data_filtered))
  
  num_cols <- names(dplyr::select(weekly_data_filtered, dplyr::where(is.numeric)))
  lag_cols <- setdiff(num_cols, c(key_cols, pregame_cols))
  
  weekly_lag <- weekly_data_filtered %>%
    dplyr::arrange(.data$season, .data$posteam, .data$week) %>%
    dplyr::group_by(.data$season, .data$posteam) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(lag_cols), ~ dplyr::lag(.x, 1))
    ) %>%
    dplyr::ungroup()
  
  # ------------------ 6) APPLY WEEK 1 SEEDING ------------------
  if (seed_week1) {
    cat("Applying Week 1 seeding for all years...\n")
    
    # Create seeding data for each year from the previous year's final week
    all_carry_data <- data.frame()
    
    for (target_year in years) {
      prior_year <- target_year - 1L
      
      if (prior_year %in% weekly_data$season) {
        cat("Creating seeding data for", target_year, "from", prior_year, "data\n")
        
        carry_year <- weekly_data %>%
          dplyr::filter(season == prior_year) %>%
          dplyr::group_by(posteam, season) %>%
          dplyr::filter(week == max(week)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(season = target_year) %>%  # Set to target year
          dplyr::select(posteam, season, dplyr::all_of(lag_cols))
        
        all_carry_data <- rbind(all_carry_data, carry_year)
      }
    }
    
    if (nrow(all_carry_data) > 0) {
      cat("Found seeding data for", nrow(all_carry_data), "team-year combinations\n")
      
      # Join all seeding data
      weekly_lag <- weekly_lag %>%
        dplyr::left_join(all_carry_data, by = c("posteam","season"), suffix = c("", "_carry"))
      
      # Apply seeding only to Week 1 of target years
      filled_total <- 0
      for (nm in lag_cols) {
        carry_nm <- paste0(nm, "_carry")
        if (carry_nm %in% names(weekly_lag)) {
          filled_count <- sum(
            weekly_lag$week == 1 & 
            weekly_lag$season %in% years &
            is.na(weekly_lag[[nm]]) & 
            !is.na(weekly_lag[[carry_nm]])
          )
          
          weekly_lag[[nm]] <- ifelse(
            weekly_lag$week == 1 & 
            weekly_lag$season %in% years &
            is.na(weekly_lag[[nm]]),
            weekly_lag[[carry_nm]],
            weekly_lag[[nm]]
          )
          weekly_lag[[carry_nm]] <- NULL
          filled_total <- filled_total + filled_count
        }
      }
      cat("Filled", filled_total, "Week 1 lagged values across all years\n")
    } else {
      cat("No seeding data available\n")
    }
  }
  
  # ------------------ 7) FILTER TO TARGET YEARS ONLY ------------------
  # This is the crucial step that was missing - remove prior year data after seeding
  weekly_lag <- weekly_lag %>%
    dplyr::filter(season %in% years)
  
  cat("Filtered weekly data to target years:", nrow(weekly_lag), "team-weeks\n")
  
  # ------------------ 8) BUILD HOME/AWAY DATAFRAMES ------------------
  base_cols <- setdiff(names(weekly_lag), c("season", "week", "posteam"))
  
  home_df <- weekly_lag %>%
    dplyr::select(season, week, posteam, dplyr::all_of(base_cols)) %>%
    dplyr::rename(home_team = posteam) %>%
    dplyr::rename_with(~ paste0("home.", .x), .cols = dplyr::all_of(base_cols))

  away_df <- weekly_lag %>%
    dplyr::select(season, week, posteam, dplyr::all_of(base_cols)) %>%
    dplyr::rename(away_team = posteam) %>%
    dplyr::rename_with(~ paste0("away.", .x), .cols = dplyr::all_of(base_cols))

  # ------------------ 9) JOIN SCHEDULE + LAGGED FEATURES ------------------
  games <- sched %>%
    dplyr::left_join(home_df, by = intersect(c("season","week","home_team"), names(home_df))) %>%
    dplyr::left_join(away_df, by = intersect(c("season","week","away_team"), names(away_df))) %>%
    dplyr::select(-dplyr::any_of(c("home.game_id", "away.game_id", "home.posteam_type", "away.posteam_type")))

  if ("spread_line" %in% names(games)) {
    games <- games %>% dplyr::mutate(away.spread_line = -spread_line)
  }
  
  # ------------------ 10) ENHANCED COACHING ANALYSIS ------------------
  if (include_coaching) {
    cat("Adding enhanced coaching analysis...\n")
    
    coach_res <- tryCatch({
      enhance_coaching_analysis(sched)
    }, error = function(e) {
      cat("Enhanced coaching analysis failed, using defaults:", e$message, "\n")
      NULL
    })
    
    if (!is.null(coach_res) && is.data.frame(coach_res$coach_tiers)) {
      coach_tiers <- coach_res$coach_tiers %>%
        dplyr::select(dplyr::any_of(c("coach_name","coach_tier","coaching_score",
                               "experience_level","win_percentage"))) %>%
        dplyr::distinct()
      
      # Enhanced name matching
      games <- games %>%
        dplyr::mutate(
          home_coach_clean = stringr::str_trim(stringr::str_to_title(home_coach)),
          away_coach_clean = stringr::str_trim(stringr::str_to_title(away_coach))
        ) %>%
        dplyr::left_join(
          coach_tiers %>% 
            dplyr::mutate(coach_name_clean = stringr::str_trim(stringr::str_to_title(coach_name))),
          by = c("home_coach_clean" = "coach_name_clean")
        ) %>%
        dplyr::rename(
          home_coach_tier = coach_tier,
          home_coaching_score = coaching_score,
          home_coach_experience = experience_level,
          home_win_percentage = win_percentage
        ) %>%
        dplyr::left_join(
          coach_tiers %>% 
            dplyr::mutate(coach_name_clean = stringr::str_trim(stringr::str_to_title(coach_name))),
          by = c("away_coach_clean" = "coach_name_clean")
        ) %>%
        dplyr::rename(
          away_coach_tier = coach_tier,
          away_coaching_score = coaching_score,
          away_coach_experience = experience_level,
          away_win_percentage = win_percentage
        ) %>%
        # Fill any remaining NAs with reasonable defaults
        dplyr::mutate(
          home_coaching_score = dplyr::coalesce(home_coaching_score, 45),
          away_coaching_score = dplyr::coalesce(away_coaching_score, 45),
          home_coach_tier = dplyr::coalesce(home_coach_tier, "Developing"),
          away_coach_tier = dplyr::coalesce(away_coach_tier, "Developing"),
          home_coach_experience = dplyr::coalesce(home_coach_experience, "New"),
          away_coach_experience = dplyr::coalesce(away_coach_experience, "New"),
          home_win_percentage = dplyr::coalesce(home_win_percentage, 0.45),
          away_win_percentage = dplyr::coalesce(away_win_percentage, 0.45),
          coaching_advantage = home_coaching_score - away_coaching_score
        ) %>%
        dplyr::select(-home_coach_clean, -away_coach_clean)
      
      cat("Coaching data joined successfully\n")
    } else {
      # Fallback coaching defaults
      cat("Using default coaching values...\n")
      games <- games %>%
        dplyr::mutate(
          home_coach_tier = "Developing",
          home_coaching_score = 45,
          home_coach_experience = "New",
          home_win_percentage = 0.45,
          away_coach_tier = "Developing",
          away_coaching_score = 45,
          away_coach_experience = "New",
          away_win_percentage = 0.45,
          coaching_advantage = 0
        )
    }
  }
  
  # ------------------ 11) ENHANCED INJURY ANALYSIS ------------------
  if (include_injuries) {
    cat("Adding enhanced injury analysis...\n")
    
    inj <- tryCatch({
      analyze_injury_impacts(min(years), max(years))
    }, error = function(e) {
      cat("Injury analysis failed:", e$message, "\n")
      NULL
    })
    
    if (is.data.frame(inj) && nrow(inj)) {
      base_inj <- inj %>%
        dplyr::select(dplyr::any_of(c("season","week","team",
                               "total_injury_impact","qb_injury_impact",
                               "skill_injury_impact","oline_injury_impact", 
                               "defense_injury_impact","num_key_injuries"))) %>%
        dplyr::rename(posteam = team)
      
      games <- games %>%
        dplyr::left_join(
          base_inj %>%
            dplyr::rename(home_team = posteam) %>%
            dplyr::rename_with(~ paste0("home_", .x),
                               -dplyr::any_of(c("season","week","home_team"))),
          by = c("season","week","home_team")
        ) %>%
        dplyr::left_join(
          base_inj %>%
            dplyr::rename(away_team = posteam) %>%
            dplyr::rename_with(~ paste0("away_", .x),
                               -dplyr::any_of(c("season","week","away_team"))),
          by = c("season","week","away_team")
        ) %>%
        dplyr::mutate(
          dplyr::across(dplyr::contains("injury_impact"), ~dplyr::coalesce(.x, 0)),
          dplyr::across(dplyr::contains("num_key_injuries"), ~dplyr::coalesce(.x, 0)),
          injury_advantage = away_total_injury_impact - home_total_injury_impact,
          qb_injury_advantage = away_qb_injury_impact - home_qb_injury_impact
        )
      
      cat("Injury data joined successfully\n")
    } else {
      # Fallback: create minimal injury data
      cat("Creating minimal injury baseline...\n")
      games <- games %>%
        dplyr::mutate(
          home_total_injury_impact = 0.5,
          home_qb_injury_impact = 0.1,
          home_skill_injury_impact = 0.1,
          home_oline_injury_impact = 0.1,
          home_defense_injury_impact = 0.2,
          home_num_key_injuries = 1,
          away_total_injury_impact = 0.5,
          away_qb_injury_impact = 0.1,
          away_skill_injury_impact = 0.1,
          away_oline_injury_impact = 0.1,
          away_defense_injury_impact = 0.2,
          away_num_key_injuries = 1,
          injury_advantage = 0,
          qb_injury_advantage = 0
        )
    }
  }
  
  # ------------------ 12) ENHANCED REFEREE ANALYSIS ------------------
  if (include_referee) {
    cat("Adding enhanced referee analysis...\n")
    
    # Try to get PBP data for penalty analysis
    pbp_data <- tryCatch({
      purrr::map_dfr(years, ~ nflfastR::load_pbp(.x))
    }, error = function(e) {
      cat("Could not load PBP data for referee analysis:", e$message, "\n")
      NULL
    })
    
    ref_analysis <- tryCatch({
      analyze_referee_performance(sched, pbp_data)
    }, error = function(e) {
      cat("Referee analysis failed:", e$message, "\n")
      NULL
    })
    
    if (is.data.frame(ref_analysis) && nrow(ref_analysis)) {
      games <- games %>%
        dplyr::left_join(
          ref_analysis %>%
            dplyr::select(dplyr::any_of(c("referee", "referee_quality", "referee_experience",
                                   "bias_category", "consistency_level", "home_bias_score",
                                   "avg_penalties_per_game", "games_officiated"))),
          by = "referee"
        ) %>%
        dplyr::mutate(
          referee_quality = dplyr::coalesce(referee_quality, 60),
          referee_experience = dplyr::coalesce(referee_experience, "unknown"),
          bias_category = dplyr::coalesce(bias_category, "unknown"),
          consistency_level = dplyr::coalesce(consistency_level, "unknown"),
          home_bias_score = dplyr::coalesce(home_bias_score, 0),
          avg_penalties_per_game = dplyr::coalesce(avg_penalties_per_game, 12.0)
        )
      
      cat("Referee analysis joined successfully\n")
    } else {
      # Fallback referee defaults
      cat("Using default referee values...\n")
      games <- games %>%
        dplyr::mutate(
          referee_quality = 60,
          referee_experience = "unknown",
          bias_category = "unknown",
          consistency_level = "unknown",
          home_bias_score = 0,
          avg_penalties_per_game = 12.0
        )
    }
  }
  
  # ------------------ 13) DERIVED FEATURES ------------------
  games <- games %>%
    dplyr::mutate(
      point_differential = ifelse(!is.na(.data$home_score) & !is.na(.data$away_score),
                                  .data$home_score - .data$away_score, NA_real_),
      total = ifelse(!is.na(.data$home_score) & !is.na(.data$away_score),
                     .data$home_score + .data$away_score, NA_real_),
      rest_advantage = if ("home_rest" %in% names(.)) .data$home_rest - .data$away_rest else NA_real_
    ) %>%
    dplyr::arrange(.data$season, .data$week, .data$home_team)
  
  # ------------------ 14) FINAL QUALITY REPORT ------------------
  cat("\n=== FINAL DATA QUALITY REPORT ===\n")
  quality_report <- games %>%
    dplyr::summarise(
      total_games = dplyr::n(),
      missing_weekly_stats = sum(is.na(home.turnovers)),
      missing_injury = if(include_injuries) sum(is.na(home_total_injury_impact)) else 0,
      missing_referee = if(include_referee) sum(is.na(referee_quality)) else 0,
      pct_complete_weekly = round((1 - missing_weekly_stats/total_games) * 100, 1),
      pct_complete_injury = if(include_injuries) round((1 - missing_injury/total_games) * 100, 1) else 100,
      pct_complete_referee = if(include_referee) round((1 - missing_referee/total_games) * 100, 1) else 100
    )

  cat("Data completeness:\n")
  cat("- Weekly stats:", quality_report$pct_complete_weekly, "%\n")
  if(include_injuries) cat("- Injury data:", quality_report$pct_complete_injury, "%\n")
  if(include_referee) cat("- Referee data:", quality_report$pct_complete_referee, "%\n")

  if (quality_report$missing_weekly_stats > 0) {
    cat("WARNING:", quality_report$missing_weekly_stats, "games missing weekly stats\n")
  }
  
  return(games)
}
