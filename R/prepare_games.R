#' Enhanced Prepare NFL Schedule Data
#'
#' Builds a modeling frame by joining nflreadr schedule with lagged team features
#' from \code{weekly_data}. Team features are lagged 1 week
#' within (season, posteam) before joining to each game. Optionally seed Week 1
#' lagged features from the final game of the prior season.
#'
#' @param start_year integer, first season
#' @param end_year integer, last season
#' @param weekly_data data.frame produced by \code{prepare_weekly()} (team-week features)
#' @param include_injuries logical, join injury impacts if \code{analyze_injury_impacts()} exists
#' @param include_coaching logical, join coaching tiers and lagged QB stability if
#'   \code{analyze_coaching_performance()} exists
#' @param include_referee logical, keep referee metadata (no PBP work here)
#' @param seed_week1 logical, if TRUE carry prior season’s final stats forward
#'   into Week 1 for lagged features
#' @return data.frame of games with home/away lagged features and schedule fields
#' @import dplyr tidyr purrr stringr
#' @export
prepare_games <- function(start_year,
                          end_year,
                          weekly_data,
                          include_injuries = TRUE,
                          include_coaching = TRUE,
                          include_referee  = TRUE,
                          seed_week1       = TRUE) {

  cat("=== ENHANCED NFL DATA PREPARATION ===\n")
  cat("Processing years:", start_year, "to", end_year, "\n")
  cat("Features: Injuries =", include_injuries,
      "| Coaching =", include_coaching,
      "| Referee =", include_referee,
      "| Seed Week1 =", seed_week1, "\n")
  
  years <- seq.int(start_year, end_year)
  if (!requireNamespace("nflreadr", quietly = TRUE))
    stop("nflreadr is required for prepare_games().")
  
  # ------------------ 1) SCHEDULE ------------------
  sched <- purrr::map_dfr(
    years,
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
  
  cat("Available schedule features: ",
      paste(names(sched), collapse = ", "), "\n")
  if ("referee" %in% names(sched)) {
    cat("Referee data coverage:",
        round(mean(!is.na(sched$referee) & sched$referee != "")*100), "% of games\n")
  }

  # Ensure prior season exists for Week 1 seeding
  if (seed_week1 && (start_year - 1L) %in% years && !((start_year - 1L) %in% weekly_data$season)) {
    weekly_data <- dplyr::bind_rows(prepare_weekly(start_year - 1L), weekly_data)
  }

  # Map team abbreviations in weekly data and drop identifier columns
  if ("posteam" %in% names(weekly_data)) {
    weekly_data$posteam <- map_team_abbreviation(weekly_data$posteam)
  }
  weekly_data <- weekly_data %>% dplyr::select(-dplyr::any_of(c("game_id", "posteam_type")))

  # Restrict schedule to seasons/weeks present in weekly_data but keep all teams
  valid_seasons <- unique(weekly_data$season)
  valid_weeks <- unique(weekly_data$week)
  sched <- sched %>%
    dplyr::filter(season %in% valid_seasons,
                  week %in% valid_weeks)
  
  # ------------------ 2) WEEKLY → LAGGED FEATURES ------------------
  # Keys that must NEVER be lagged
  key_cols <- c("season","week","posteam","game_id","posteam_type")
  key_cols <- intersect(key_cols, names(weekly_data))
  
  # Pre-game columns that should NOT be lagged if present in weekly_data
  pregame_cols <- intersect(c(
    "spread_line","total_line","div_game","roof","surface"
  ), names(weekly_data))
  
  # Numeric columns eligible for lag, excluding keys & pregame
  num_cols  <- names(dplyr::select(weekly_data, dplyr::where(is.numeric)))
  lag_cols  <- setdiff(num_cols, c(key_cols, pregame_cols))
  
  weekly_lag <- weekly_data %>%
    dplyr::arrange(.data$season, .data$posteam, .data$week) %>%
    dplyr::group_by(.data$season, .data$posteam) %>%
    dplyr::mutate(
    dplyr::across(dplyr::all_of(lag_cols), ~ dplyr::lag(.x, 1))
    ) %>%
    dplyr::ungroup()
  
  # ---- Week 1 seeding from prior season final game (no leakage) ----
  if (seed_week1) {
    # For each team/season, take last week values and shift them to next season
    carry <- weekly_data %>%
      dplyr::group_by(posteam, season) %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(season = season + 1L) %>%
      dplyr::select(posteam, season, dplyr::all_of(lag_cols))
    
    weekly_lag <- weekly_lag %>%
      dplyr::left_join(carry, by = c("posteam","season"), suffix = c("", "_carry"))
    
    for (nm in lag_cols) {
      carry_nm <- paste0(nm, "_carry")
      if (carry_nm %in% names(weekly_lag)) {
        weekly_lag[[nm]] <- ifelse(
          weekly_lag$week == 1 & is.na(weekly_lag[[nm]]),
          weekly_lag[[carry_nm]],
          weekly_lag[[nm]]
        )
        weekly_lag[[carry_nm]] <- NULL
      }
    }
  }
  
  # Build home/away frames (prefix all non-key columns)
  base_cols <- setdiff(names(weekly_lag), key_cols)
  home_df <- weekly_lag %>%
    dplyr::select(season, week, posteam, dplyr::all_of(base_cols)) %>%
    dplyr::rename(home_team = posteam) %>%
    dplyr::rename_with(~ paste0("home.", .x), .cols = dplyr::all_of(base_cols))

  away_df <- weekly_lag %>%
    dplyr::select(season, week, posteam, dplyr::all_of(base_cols)) %>%
    dplyr::rename(away_team = posteam) %>%
    dplyr::rename_with(~ paste0("away.", .x), .cols = dplyr::all_of(base_cols))

  
  # ------------------ 3) JOIN schedule + lagged team features ------------------
  games <- sched %>%
    dplyr::left_join(
      home_df,
      by = intersect(c("season","week","home_team"), names(home_df))
    ) %>%
    dplyr::left_join(
      away_df,
      by = intersect(c("season","week","away_team"), names(away_df))
    )

  # Remove any residual identifiers from weekly joins
  games <- games %>% dplyr::select(-dplyr::any_of(c("home.game_id","away.game_id",
                                                   "home.posteam_type","away.posteam_type")))
  
  # Ensure away.spread_line exists
  if ("spread_line" %in% names(games)) {
    games <- games %>% dplyr::mutate(away.spread_line = -spread_line)
  }
  
  # ------------------ 4) OPTIONAL: Coaching + lagged QB stability ------------------
  if (include_coaching && exists("analyze_coaching_performance", mode = "function")) {
    cat("Analyzing coaching performance...\n")
    coach_res <- tryCatch(analyze_coaching_performance(sched), error = function(e) NULL)
    
    if (!is.null(coach_res)) {
      # Coach tiers
      if (is.data.frame(coach_res$coach_tiers) && nrow(coach_res$coach_tiers)) {
        coach_tiers <- coach_res$coach_tiers %>%
          dplyr::select(dplyr::any_of(c("coach_name","coach_tier","coaching_score",
                                 "experience_level","win_percentage"))) %>%
          dplyr::distinct()
        
        games <- games %>%
          dplyr::left_join(coach_tiers, by = c("home_coach" = "coach_name")) %>%
          dplyr::rename(
            home_coach_tier       = coach_tier,
            home_coaching_score   = coaching_score,
            home_coach_experience = experience_level,
            home_win_percentage   = win_percentage
          ) %>%
          dplyr::left_join(coach_tiers, by = c("away_coach" = "coach_name")) %>%
          dplyr::rename(
            away_coach_tier       = coach_tier,
            away_coaching_score   = coaching_score,
            away_coach_experience = experience_level,
            away_win_percentage   = win_percentage
          ) %>%
          dplyr::mutate(
            coaching_advantage =
              dplyr::coalesce(.data$home_coaching_score, 50) -
              dplyr::coalesce(.data$away_coaching_score, 50)
          )
      }
      
      # Starting QBs → lag 1wk, then join (and optionally seed Week 1)
      if (is.data.frame(coach_res$starting_qbs) && nrow(coach_res$starting_qbs)) {
        qb_tbl <- coach_res$starting_qbs
        need <- c("season","week","posteam","qb_stability","qb_change_flag","qb_experience")
        if (all(need %in% names(qb_tbl))) {
          qb_lag <- qb_tbl %>%
            dplyr::arrange(.data$season, .data$posteam, .data$week) %>%
            dplyr::group_by(.data$season, .data$posteam) %>%
            dplyr::mutate(
            dplyr::across(dplyr::any_of(c("qb_stability","qb_experience")), ~ dplyr::lag(.x, 1)),
            dplyr::across(dplyr::any_of(c("qb_change_flag")),               ~ dplyr::lag(.x, 1))
            ) %>%
            dplyr::ungroup()
          
          if (seed_week1) {
            qb_carry <- qb_tbl %>%
              dplyr::group_by(posteam, season) %>%
              dplyr::filter(week == max(week)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(season = season + 1L) %>%
              dplyr::select(posteam, season,
                            dplyr::any_of(c("qb_stability","qb_experience","qb_change_flag")))
           
            qb_lag <- qb_lag %>%
              dplyr::left_join(qb_carry, by = c("posteam","season"), suffix = c("", "_carry"))
            
            for (nm in c("qb_stability","qb_experience","qb_change_flag")) {
              carry_nm <- paste0(nm, "_carry")
              if (carry_nm %in% names(qb_lag)) {
                qb_lag[[nm]] <- ifelse(
                  qb_lag$week == 1 & is.na(qb_lag[[nm]]),
                  qb_lag[[carry_nm]],
                  qb_lag[[nm]]
                )
                qb_lag[[carry_nm]] <- NULL
              }
            }
          }
          
          games <- games %>%
            dplyr::left_join(
              qb_lag %>%
                dplyr::rename(home_team = posteam) %>%
                dplyr::rename_with(~ paste0("home_", .x),
                                   -dplyr::any_of(c("season","week","home_team"))),
              by = c("season","week","home_team")
            ) %>%
            dplyr::left_join(
              qb_lag %>%
                dplyr::rename(away_team = posteam) %>%
                dplyr::rename_with(~ paste0("away_", .x),
                                   -dplyr::any_of(c("season","week","away_team"))),
              by = c("season","week","away_team")
            ) %>%
            dplyr::mutate(
              qb_stability_advantage =
                dplyr::coalesce(.data$home_qb_stability, 0.8) -
                dplyr::coalesce(.data$away_qb_stability, 0.8)
            )
        }
      }
    }
  }
  
  # ------------------ 5) OPTIONAL: Injuries ------------------
  if (include_injuries && exists("analyze_injury_impacts", mode = "function")) {
    cat("Analyzing injury impacts...\n")
    inj <- tryCatch(analyze_injury_impacts(start_year, end_year), error = function(e) NULL)
    
    if (is.data.frame(inj) && nrow(inj)) {
      id_col <- dplyr::first(intersect(c("team","posteam","abbrev"), names(inj)))
      if (is.na(id_col)) {
        warning("Injury table lacks team identifier (team/posteam/abbrev). Skipping injury join.")
      } else {
        base_inj <- inj %>%
          dplyr::select(dplyr::any_of(c("season","week", id_col,
                                 "total_injury_impact","qb_injury_impact",
                                 "skill_injury_impact","oline_injury_impact",
                                 "defense_injury_impact","num_key_injuries"))) %>%
          dplyr::rename(posteam = !!id_col)
        
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
            injury_advantage =
              dplyr::coalesce(.data$away_total_injury_impact, 0) -
              dplyr::coalesce(.data$home_total_injury_impact, 0),
            qb_injury_advantage =
              dplyr::coalesce(.data$away_qb_injury_impact, 0) -
              dplyr::coalesce(.data$home_qb_injury_impact, 0)
          )
      }
    }
  }

  # ------------------ 6) Derived outcomes & handy features ------------------
  games <- games %>%
    # consolidate duplicate identifiers and drop unused columns
    dplyr::mutate(game_id = dplyr::coalesce(.data$game_id, .data$game_id.x, .data$game_id.y)) %>%
    dplyr::select(-dplyr::any_of(c("game_id.x","game_id.y",
                                   "home.game_id","away.game_id",
                                   "home.posteam_type","away.posteam_type",
                                   "posteam_type.x","posteam_type.y"))) %>%
    dplyr::mutate(
      point_differential = ifelse(!is.na(.data$home_score) & !is.na(.data$away_score),
                                  .data$home_score - .data$away_score, NA_real_),
      total = ifelse(!is.na(.data$home_score) & !is.na(.data$away_score),
                     .data$home_score + .data$away_score, NA_real_),
      rest_advantage = if ("home_rest" %in% names(.)) .data$home_rest - .data$away_rest else NA_real_
    ) %>%
    dplyr::arrange(.data$season, .data$week, .data$home_team)
  
  return(games)
}
