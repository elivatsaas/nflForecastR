#' Prepare Game-Level Modeling Frame (Leak-Free)
#'
#' Builds a per-game table for modeling and backtesting with **no current-week leakage**:
#' all realized, same-week team stats are suffixed with `*_outcome`, and all predictors
#' are based on prior information only (lags like `_lastweek`, `_last3`, `_std`,
#' schedule lines, coaching, etc.). Raw postgame score columns are removed from the
#' final output.
#'
#' @param years Integer vector of seasons to include, e.g. `2015:2024` or `2024`.
#' @param include_injuries Logical. If `TRUE`, joins injury impact features (may be
#'   spotty historically). For 2025+ predictions these are typically deprecated—set
#'   `FALSE` unless you maintain a reliable source. Default `TRUE`.
#' @param include_coaching Logical. If `TRUE`, joins enhanced coaching features
#'   from `analyze_coaching_performance()`. Default `TRUE`.
#' @param include_referee Logical. If `TRUE`, joins enhanced referee features from
#'   `analyze_referee_performance()`. These can be unstable (single-level factors);
#'   consider setting `FALSE` in production until stabilized. Default `TRUE`.
#' @param seed_week1 Logical. If `TRUE`, seeds each team’s Week 1 lag features
#'   with the team’s **last game of the prior season**. Default `TRUE`.
#' @param injury_missing_strategy One of `"zero"` or `"weight"`. If injury analysis
#'   fails, fills columns via this strategy. Default `"zero"`.
#'
#' @details
#' **No-leak design**
#' - All *same-week realized* per-team metrics are output as `*_outcome` columns.
#' - Lagged predictors are created for each numeric metric: `*_lastweek`, `*_last3`,
#'   `*_std` (a running mean proxy), each **lagged by 1** so they only reflect
#'   information available **before** the current game.
#' - Raw postgame score columns (`home_score`, `away_score`) and legacy pointers
#'   (`point_differential`, `total`) are **dropped**; use `point_differential_outcome`
#'   and `total_outcome` instead.
#'
#' **Inputs expected**
#' - `prepare_weekly(years)` must return per-team, per-week numeric performance
#'   metrics with `season`, `week`, `posteam`, plus any numeric stats.
#' - `analyze_coaching_performance()`, `analyze_referee_performance()`,
#'   `analyze_injury_impacts()` are optional helpers if the corresponding feature
#'   families are enabled.
#' - `map_team_abbreviation()` should normalize team codes. If absent, a no-op is used.
#'
#' @return A tibble with one row per game. Columns include:
#' - Keys & schedule/market: `game_id`, `season`, `week`, `home_team`, `away_team`,
#'   `spread_line`, `total_line`, moneyline/spread odds, `div_game`, `roof`, `surface`, etc.
#' - Home/away **lagged** features like `home.points_scored_lastweek`, `away.yards_last3`,
#'   `home.off_epa_std`, etc. (exact names depend on your \code{prepare_weekly} output).
#' - Realized outcomes: `point_differential_outcome` (home - away), `total_outcome`
#'   (home + away), plus any other realized `*_outcome` variables created from weekly metrics.
#' - Optional families: coaching (e.g., `home_coach_tier`, `coaching_advantage`),
#'   injuries (e.g., `home_total_injury_impact`, `injury_advantage`),
#'   referee (e.g., `referee_quality`, etc.).
#' - Derived pregame: `rest_advantage = home_rest - away_rest`.
#'
#' @examples
#' \dontrun{
#' games <- prepare_games(
#'   years = 2015:2024,
#'   include_injuries = FALSE,   # injuries deprecated for 2025+ predictions
#'   include_coaching = TRUE,
#'   include_referee  = FALSE,   # refs currently unstable; keep off for modeling
#'   seed_week1       = TRUE
#' )
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import zoo
#' @import nflreadr
#' @export
prepare_games <- function(
  years,
  include_injuries = TRUE,
  include_coaching = TRUE,
  include_referee  = TRUE,
  seed_week1       = TRUE,
  injury_missing_strategy = "zero"
) {
  cat("=== ENHANCED NFL DATA PREPARATION ===\n")
  cat("Processing years:", paste(years, collapse = ", "), "\n")
  cat("Features: Injuries =", include_injuries, "(strategy:", injury_missing_strategy, ")",
      "| Coaching =", include_coaching,
      "| Referee =", include_referee,
      "| Seed Week1 =", seed_week1, "\n")

  # --- deps & guards ---
  if (!injury_missing_strategy %in% c("zero","weight")) {
    stop("injury_missing_strategy must be 'zero' or 'weight'")
  }
  for (pkg in c("dplyr","tidyr","purrr","stringr","zoo","nflreadr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(pkg, " is required for prepare_games().")
  }

  # Fallback for team mapping if not provided by your package
  if (!exists("map_team_abbreviation", mode = "function")) {
    map_team_abbreviation <- function(x) x
  }

  # --- determine weekly range (include prior year for Wk1 seeding) ---
  weekly_years <- if (seed_week1) (min(years) - 1L):max(years) else years
  cat("Preparing weekly data for years:", paste(weekly_years, collapse = ", "), "\n")
  if (seed_week1 && (min(years) - 1L) %in% weekly_years)
    cat("Including", min(years) - 1L, "data for Week 1", min(years), "seeding\n")

  # --- per-team weekly realized metrics (same-week) ---
  weekly_data <- tryCatch({
    prepare_weekly(weekly_years)
  }, error = function(e) stop("Failed to prepare weekly data: ", e$message))
  if (nrow(weekly_data) == 0) stop("prepare_weekly() returned no data.")
  cat("Weekly data prepared:", nrow(weekly_data), "team-weeks\n")

  # --- schedules only for target years ---
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
  cat("Schedule loaded:", nrow(sched), "games (target years only)\n")

  # known single-game fixes
  sched <- sched %>%
    dplyr::mutate(
      referee = dplyr::if_else(season == 2021 & week == 15 & away_team == "NE" & home_team == "IND",
                               "Carl Cheffers", referee),
      home_moneyline = dplyr::if_else(season == 2017 & week == 4 & away_team == "CHI" & home_team == "GB" & is.na(home_moneyline),
                                      -350L, home_moneyline),
      away_moneyline = dplyr::if_else(season == 2017 & week == 4 & away_team == "CHI" & home_team == "GB" & is.na(away_moneyline),
                                      280L, away_moneyline),
      home_spread_odds = dplyr::if_else(season == 2017 & week == 4 & away_team == "CHI" & home_team == "GB" & is.na(home_spread_odds),
                                        -110L, home_spread_odds),
      away_spread_odds = dplyr::if_else(season == 2017 & week == 4 & away_team == "CHI" & home_team == "GB" & is.na(away_spread_odds),
                                        -110L, away_spread_odds),
      under_odds = dplyr::if_else(season == 2017 & week == 4 & away_team == "CHI" & home_team == "GB" & is.na(under_odds),
                                  -110L, under_odds),
      over_odds  = dplyr::if_else(season == 2017 & week == 4 & away_team == "CHI" & home_team == "GB" & is.na(over_odds),
                                  -110L, over_odds)
    )
  cat("Fixed specific missing data points.\n")

  # --- actual team-weeks (avoid byes), + prior season teams for seeding ---
  actual_team_weeks <- sched %>%
    dplyr::select(season, week, home_team, away_team) %>%
    tidyr::pivot_longer(c(home_team, away_team), names_to = "side", values_to = "posteam") %>%
    dplyr::select(season, week, posteam) %>%
    dplyr::distinct()

  if (seed_week1) {
    prior_year_teams <- weekly_data %>%
      dplyr::filter(season == min(years) - 1L) %>%
      dplyr::select(season, week, posteam) %>%
      dplyr::distinct()
    actual_team_weeks <- dplyr::bind_rows(actual_team_weeks, prior_year_teams) %>% dplyr::distinct()
  }

  weekly_data$posteam <- map_team_abbreviation(weekly_data$posteam)
  weekly_data <- weekly_data %>% dplyr::select(-dplyr::any_of(c("game_id","posteam_type")))
  weekly_data_complete <- actual_team_weeks %>%
    dplyr::left_join(weekly_data, by = c("season","week","posteam"))
  cat("Complete weekly data (no bye weeks):", nrow(weekly_data_complete), "team-weeks\n")

  # --- create lagged features (no leakage) & mark realized as *_outcome ---
  cat("Creating outcome, last week, last 3 weeks, and season-to-date features...\n")

  key_cols     <- intersect(c("season","week","posteam"), names(weekly_data_complete))
  pregame_cols <- intersect(c("spread_line","total_line","div_game","roof","surface"), names(weekly_data_complete))
  num_cols     <- names(dplyr::select(weekly_data_complete, dplyr::where(is.numeric)))
  perf_cols    <- setdiff(num_cols, c(key_cols, pregame_cols))

  weekly_with_features <- weekly_data_complete %>%
    dplyr::arrange(.data$season, .data$posteam, .data$week) %>%
    dplyr::group_by(.data$season, .data$posteam) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(perf_cols),
        .fns = list(
          lastweek = ~ dplyr::lag(.x, n = 1),
          last3    = ~ dplyr::lag(zoo::rollapplyr(.x, width = 3, FUN = mean, partial = TRUE, fill = NA), n = 1),
          std      = ~ dplyr::lag(cummean(tidyr::replace_na(.x, 0)), n = 1)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    dplyr::ungroup()

  weekly_features <- weekly_with_features %>%
    dplyr::rename_with(~ paste0(.x, "_outcome"), .cols = dplyr::all_of(perf_cols))

  feature_cols <- names(weekly_features)[grepl("(_lastweek|_last3|_std)$", names(weekly_features))]
  cat("Generated", length(perf_cols) * 3, "new features across", length(perf_cols), "performance metrics.\n")

  # --- seed week 1 lags with prior season last game ---
  if (seed_week1) {
    cat("Applying seeding for each team's first game of the season...\n")
    all_carry_data <- dplyr::tibble()

    for (target_year in years) {
      prior_year <- target_year - 1L
      if (prior_year %in% weekly_data$season) {
        target_year_teams <- actual_team_weeks %>%
          dplyr::filter(season == target_year) %>% dplyr::pull(posteam) %>% unique()

        carry_year_raw <- weekly_data %>%
          dplyr::filter(season == prior_year, posteam %in% target_year_teams) %>%
          dplyr::group_by(posteam) %>%
          dplyr::arrange(dplyr::desc(week)) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(posteam, dplyr::all_of(perf_cols))

        carry_year <- carry_year_raw
        for (col in perf_cols) {
          carry_year[[paste0(col, "_lastweek")]] <- carry_year[[col]]
          carry_year[[paste0(col, "_last3")]]    <- carry_year[[col]]
          carry_year[[paste0(col, "_std")]]      <- carry_year[[col]]
        }
        carry_year <- carry_year %>%
          dplyr::select(
            posteam,
            dplyr::all_of(paste0(perf_cols, "_lastweek")),
            dplyr::all_of(paste0(perf_cols, "_last3")),
            dplyr::all_of(paste0(perf_cols, "_std"))
          ) %>%
          dplyr::mutate(season = target_year)

        all_carry_data <- dplyr::bind_rows(all_carry_data, carry_year)
      }
    }

    if (nrow(all_carry_data) > 0) {
      weekly_features <- weekly_features %>%
        dplyr::left_join(all_carry_data, by = c("posteam","season"), suffix = c("", "_carry"))

      team_first_games <- weekly_features %>%
        dplyr::filter(season %in% years) %>%
        dplyr::group_by(season, posteam) %>%
        dplyr::arrange(week) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(season, posteam, week) %>%
        dplyr::rename(first_week = week)

      weekly_features <- weekly_features %>%
        dplyr::left_join(team_first_games, by = c("season","posteam"))

      for (nm in feature_cols) {
        carry_nm <- paste0(nm, "_carry")
        if (carry_nm %in% names(weekly_features)) {
          weekly_features[[nm]] <- ifelse(
            weekly_features$week == weekly_features$first_week &
              weekly_features$season %in% years &
              is.na(weekly_features[[nm]]),
            weekly_features[[carry_nm]],
            weekly_features[[nm]]
          )
          weekly_features[[carry_nm]] <- NULL
        }
      }
      weekly_features <- weekly_features %>% dplyr::select(-first_week)
      cat("Seeding applied to first games for all teams.\n")
    }
  }

  # --- keep target years only for game join ---
  weekly_final <- weekly_features %>% dplyr::filter(season %in% years)
  cat("Filtered weekly data to target years:", nrow(weekly_final), "team-weeks\n")

  # --- home/away splits ---
  base_cols <- setdiff(names(weekly_final), c("season","week","posteam"))
  home_df <- weekly_final %>%
    dplyr::select(season, week, posteam, dplyr::all_of(base_cols)) %>%
    dplyr::rename(home_team = posteam) %>%
    dplyr::rename_with(~ paste0("home.", .x), .cols = dplyr::all_of(base_cols))

  away_df <- weekly_final %>%
    dplyr::select(season, week, posteam, dplyr::all_of(base_cols)) %>%
    dplyr::rename(away_team = posteam) %>%
    dplyr::rename_with(~ paste0("away.", .x), .cols = dplyr::all_of(base_cols))

  games <- sched %>%
    dplyr::left_join(home_df, by = c("season","week","home_team")) %>%
    dplyr::left_join(away_df, by = c("season","week","away_team")) %>%
    dplyr::select(-dplyr::any_of(c("home.game_id","away.game_id",
                                   "home.posteam_type","away.posteam_type")))

  # --- coaching analysis (optional) ---
  if (include_coaching) {
    cat("Adding enhanced coaching analysis...\n")
    coach_res <- tryCatch({
      analyze_coaching_performance(sched)
    }, error = function(e) { cat("Coaching analysis failed:", e$message, "\n"); NULL })

    if (!is.null(coach_res) && is.list(coach_res) && "coach_tiers" %in% names(coach_res)) {
      coach_tiers <- coach_res$coach_tiers %>%
        dplyr::select(dplyr::any_of(c(
          "coach_name","coach_tier","coaching_score","experience_level","win_percentage"
        ))) %>%
        dplyr::distinct() %>%
        dplyr::mutate(coach_name_clean = stringr::str_trim(stringr::str_to_title(coach_name)))

      all_coaches <- unique(c(games$home_coach, games$away_coach))
      all_coaches <- all_coaches[!is.na(all_coaches) & all_coaches != "" & all_coaches != "TBD"]
      missing_coaches <- setdiff(
        stringr::str_trim(stringr::str_to_title(all_coaches)),
        coach_tiers$coach_name_clean
      )

      if (length(missing_coaches) > 0) {
        cat("Adding default values for", length(missing_coaches), "coaches with <16 games\n")
        default_coaches <- dplyr::tibble(
          coach_name = missing_coaches,
          coach_tier = "rookie",
          coaching_score = 45.0,
          experience_level = "new",
          win_percentage = 0.45,
          coach_name_clean = missing_coaches
        )
        coach_tiers <- dplyr::bind_rows(coach_tiers, default_coaches)
      }

      games <- games %>%
        dplyr::mutate(
          home_coach_clean = stringr::str_trim(stringr::str_to_title(home_coach)),
          away_coach_clean = stringr::str_trim(stringr::str_to_title(away_coach))
        ) %>%
        dplyr::left_join(
          coach_tiers %>% dplyr::select(coach_name_clean, coach_tier, coaching_score, experience_level, win_percentage),
          by = c("home_coach_clean" = "coach_name_clean")
        ) %>%
        dplyr::rename(
          home_coach_tier = coach_tier,
          home_coaching_score = coaching_score,
          home_coach_experience = experience_level,
          home_win_percentage = win_percentage
        ) %>%
        dplyr::left_join(
          coach_tiers %>% dplyr::select(coach_name_clean, coach_tier, coaching_score, experience_level, win_percentage),
          by = c("away_coach_clean" = "coach_name_clean")
        ) %>%
        dplyr::rename(
          away_coach_tier = coach_tier,
          away_coaching_score = coaching_score,
          away_coach_experience = experience_level,
          away_win_percentage = win_percentage
        ) %>%
        dplyr::mutate(
          home_coaching_score = dplyr::coalesce(home_coaching_score, 45.0),
          away_coaching_score = dplyr::coalesce(away_coaching_score, 45.0),
          coaching_advantage  = home_coaching_score - away_coaching_score
        ) %>%
        dplyr::select(-home_coach_clean, -away_coach_clean)
      cat("Coaching data joined successfully\n")
    } else {
      cat("Coaching analysis failed - adding default coaching data\n")
      games <- games %>% dplyr::mutate(
        home_coach_tier = "unknown", home_coaching_score = 50.0, home_coach_experience = "unknown",
        home_win_percentage = 0.5, away_coach_tier = "unknown", away_coaching_score = 50.0,
        away_coach_experience = "unknown", away_win_percentage = 0.5,
        coaching_advantage = 0
      )
    }
  }

  # --- injury analysis (optional) ---
  if (include_injuries) {
    cat("Adding enhanced injury analysis (strategy:", injury_missing_strategy, ")...\n")
    inj <- tryCatch({
      analyze_injury_impacts(min(years), max(years), missing_strategy = injury_missing_strategy)
    }, error = function(e) { cat("Injury analysis failed:", e$message, "\n"); NULL })

    if (is.data.frame(inj) && nrow(inj)) {
      base_inj <- inj %>%
        dplyr::select(dplyr::any_of(c(
          "season","week","team","total_injury_impact","qb_injury_impact",
          "skill_injury_impact","oline_injury_impact","defense_injury_impact","num_key_injuries"
        ))) %>%
        dplyr::rename(posteam = team)

      games <- games %>%
        dplyr::left_join(
          base_inj %>% dplyr::rename(home_team = posteam) %>%
            dplyr::rename_with(~ paste0("home_", .x), -dplyr::any_of(c("season","week","home_team"))),
          by = c("season","week","home_team")
        ) %>%
        dplyr::left_join(
          base_inj %>% dplyr::rename(away_team = posteam) %>%
            dplyr::rename_with(~ paste0("away_", .x), -dplyr::any_of(c("season","week","away_team"))),
          by = c("season","week","away_team")
        ) %>%
        dplyr::mutate(
          injury_advantage    = away_total_injury_impact - home_total_injury_impact,
          qb_injury_advantage = away_qb_injury_impact   - home_qb_injury_impact
        )
      cat("Injury data joined successfully\n")
    } else {
      cat("Creating", injury_missing_strategy, "injury baseline (analysis failed)...\n")
      if (injury_missing_strategy == "zero") {
        games <- games %>% dplyr::mutate(
          home_total_injury_impact = 0, home_qb_injury_impact = 0, home_skill_injury_impact = 0,
          home_oline_injury_impact = 0, home_defense_injury_impact = 0, home_num_key_injuries = 0L,
          away_total_injury_impact = 0, away_qb_injury_impact = 0, away_skill_injury_impact = 0,
          away_oline_injury_impact = 0, away_defense_injury_impact = 0, away_num_key_injuries = 0L,
          injury_advantage = 0, qb_injury_advantage = 0
        )
      } else {
        set.seed(1)
        games <- games %>% dplyr::mutate(
          home_total_injury_impact = runif(dplyr::n(), 0, 0.3), home_qb_injury_impact = runif(dplyr::n(), 0, 0.1),
          home_skill_injury_impact = runif(dplyr::n(), 0, 0.1), home_oline_injury_impact = runif(dplyr::n(), 0, 0.1),
          home_defense_injury_impact = runif(dplyr::n(), 0, 0.1), home_num_key_injuries = sample(0:2, dplyr::n(), TRUE),
          away_total_injury_impact = runif(dplyr::n(), 0, 0.3), away_qb_injury_impact = runif(dplyr::n(), 0, 0.1),
          away_skill_injury_impact = runif(dplyr::n(), 0, 0.1), away_oline_injury_impact = runif(dplyr::n(), 0, 0.1),
          away_defense_injury_impact = runif(dplyr::n(), 0, 0.1), away_num_key_injuries = sample(0:2, dplyr::n(), TRUE),
          injury_advantage = away_total_injury_impact - home_total_injury_impact,
          qb_injury_advantage = away_qb_injury_impact - home_qb_injury_impact
        )
      }
    }
  }

  # --- referee analysis (optional) ---
  if (include_referee) {
    cat("Adding enhanced referee analysis...\n")
    pbp_data <- tryCatch({
      purrr::map_dfr(years, ~ nflreadr::load_pbp(.x))
    }, error = function(e) { cat("Could not load PBP data:", e$message, "\n"); NULL })

    ref_analysis <- tryCatch({
      analyze_referee_performance(sched, pbp_data)
    }, error = function(e) { cat("Referee analysis failed:", e$message, "\n"); NULL })

    if (is.data.frame(ref_analysis) && nrow(ref_analysis)) {
      games <- games %>%
        dplyr::left_join(
          ref_analysis %>%
            dplyr::select(dplyr::any_of(c(
              "referee","referee_quality","referee_experience",
              "bias_category","consistency_level","home_bias_score",
              "avg_penalties_per_game","games_officiated"
            ))),
          by = "referee"
        ) %>%
        dplyr::mutate(
          referee_quality        = dplyr::coalesce(referee_quality, 60.0),
          referee_experience     = dplyr::coalesce(referee_experience, "unknown"),
          bias_category          = dplyr::coalesce(bias_category, "unknown"),
          consistency_level      = dplyr::coalesce(consistency_level, "unknown"),
          home_bias_score        = dplyr::coalesce(home_bias_score, 0),
          avg_penalties_per_game = dplyr::coalesce(avg_penalties_per_game, 12.0),
          games_officiated       = dplyr::coalesce(games_officiated, 0L)
        )
      cat("Referee analysis joined successfully\n")
    } else {
      cat("Referee analysis failed - adding default referee data\n")
      games <- games %>% dplyr::mutate(
        referee_quality = 60.0, referee_experience = "unknown",
        bias_category = "unknown", consistency_level = "unknown",
        home_bias_score = 0, avg_penalties_per_game = 12.0,
        games_officiated = 0L
      )
    }
  }

  # --- derive *_outcome only (no legacy pointers), and pregame rest_advantage ---
  games <- games %>%
    dplyr::mutate(
      point_differential_outcome = dplyr::if_else(
        !is.na(.data$home_score) & !is.na(.data$away_score),
        .data$home_score - .data$away_score, NA_real_
      ),
      total_outcome = dplyr::if_else(
        !is.na(.data$home_score) & !is.na(.data$away_score),
        .data$home_score + .data$away_score, NA_real_
      )
    )

  if (all(c("home_rest","away_rest") %in% names(games))) {
    games <- games %>% dplyr::mutate(rest_advantage = .data$home_rest - .data$away_rest)
  } else {
    games <- games %>% dplyr::mutate(rest_advantage = NA_real_)
  }

  # --- DROP raw scores & any legacy pointers so only *_outcome remain for realized stats ---
  games <- games %>%
    dplyr::select(-dplyr::any_of(c("home_score","away_score","point_differential","total"))) %>%
    dplyr::arrange(.data$season, .data$week, .data$home_team)

  # --- quality report ---
  cat("\n=== FINAL DATA QUALITY REPORT ===\n")
  rep_feature <- if (length(feature_cols)) feature_cols[1] else NULL
  missing_counts <- games %>%
    dplyr::summarise(
      total_games = dplyr::n(),
      missing_weekly_stats = if (!is.null(rep_feature) && paste0("home.", rep_feature) %in% names(.))
        sum(is.na(.data[[paste0("home.", rep_feature)]])) else dplyr::n(),
      missing_betting_lines = if (all(c("home_moneyline","away_moneyline") %in% names(.)))
        sum(is.na(home_moneyline) | is.na(away_moneyline)) else 0,
      missing_referee_names = if ("referee" %in% names(.)) sum(is.na(referee)) else 0,
      missing_injury = if (include_injuries && "home_total_injury_impact" %in% names(.))
        sum(is.na(home_total_injury_impact)) else if (include_injuries) dplyr::n() else 0,
      missing_referee = if (include_referee && "referee_quality" %in% names(.))
        sum(is.na(referee_quality)) else if (include_referee) dplyr::n() else 0,
      missing_coaching = if (include_coaching && "coaching_advantage" %in% names(.))
        sum(is.na(coaching_advantage)) else if (include_coaching) dplyr::n() else 0,
      pct_complete_weekly  = round((1 - missing_weekly_stats/total_games) * 100, 1),
      pct_complete_injury  = if (include_injuries) round((1 - missing_injury/total_games) * 100, 1) else 100,
      pct_complete_referee = if (include_referee) round((1 - missing_referee/total_games) * 100, 1) else 100,
      pct_complete_coaching= if (include_coaching) round((1 - missing_coaching/total_games) * 100, 1) else 100
    )

  cat("Data completeness:\n")
  cat("- Weekly stats:", missing_counts$pct_complete_weekly, "%\n")
  cat("- Betting lines: Missing", missing_counts$missing_betting_lines, "games\n")
  cat("- Referee names: Missing", missing_counts$missing_referee_names, "games\n")
  if (include_injuries) cat("- Injury data (", injury_missing_strategy, " strategy): ", missing_counts$pct_complete_injury, "%\n", sep = "")
  if (include_referee)  cat("- Referee analysis:", missing_counts$pct_complete_referee, "%\n")
  if (include_coaching) cat("- Coaching analysis:", missing_counts$pct_complete_coaching, "%\n")
  if (missing_counts$missing_weekly_stats > 0) {
    cat("WARNING:", missing_counts$missing_weekly_stats, "games missing weekly stats\n")
  }

  cat("\n=== DATA PREPARATION COMPLETE ===\n")
  return(games)
}
