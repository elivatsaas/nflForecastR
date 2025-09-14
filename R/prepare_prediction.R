#' Prepare Prediction Frames (Leak-Free) + Optional Training Slice
#'
#' Builds a **future-week** prediction dataframe using only information available
#' **before kickoff** (no same-week leakage). Optionally returns a training frame
#' covering complete prior seasons and the current season up to week `target_week - 1`.
#'
#' Notes:
#' - **Injuries**: for 2025+ workflows these are typically deprecated; this
#'   function disables them automatically when `target_season >= 2025`.
#' - **Referees**: assignments and profiling can be unstable; if you see
#'   single-level factors or noisy joins, call with `include_referee = FALSE`.
#'
#' @param target_week Integer week to predict.
#' @param target_season Integer season to predict.
#' @param training_start Optional integer season. If provided, the return value is a
#'   list with `training` (from `training_start` through week `target_week-1` of
#'   `target_season`) and `prediction` (for the requested week). If `NULL`, only the
#'   `prediction` frame is returned.
#' @param include_coaching Logical, add coaching features via
#'   `analyze_coaching_performance()` with safe fallbacks. Default `TRUE`.
#' @param include_referee Logical, add referee features via
#'   `analyze_referee_performance()` with safe fallbacks. Default `TRUE`.
#' @param include_injuries Logical, add injury impacts (historically spotty).
#'   Automatically disabled for `target_season >= 2025`. Default `FALSE`.
#' @param seed_week1 Logical, when internally calling `prepare_games()` to build
#'   training slices, seed each team’s Week 1 lags with prior season’s last game.
#'   Default `TRUE`.
#'
#' @details
#' **Leak-avoidance design**
#' - This function **never** uses `*_outcome` columns as predictors.
#' - Team features are derived from prior games only by computing lag summaries
#'   (`_lastweek`, `_last3`, `_std`) from past \emph{realized} metrics.
#' - Schedule/market inputs (spread/total/moneylines) are pregame.
#'
#' **What you get**
#' - `prediction`: one row per game in the target week with:
#'   - schedule & market fields (`spread_line`, `total_line`, odds, etc.)
#'   - home/away lag features for every realized metric available historically
#'   - coaching (optional), referee (optional), rest advantage, etc.
#' - `training` (if requested): a multi-season frame you can feed straight into a model.
#'
#' @return If `training_start` is `NULL`, a tibble of games for the target week.
#'   Otherwise a list with elements:
#'   - `training`: tibble of past games (no same-week leakage)
#'   - `prediction`: tibble for the target week
#'
#' @seealso [prepare_games()]
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import nflreadr
#' @export
prepare_predictions <- function(
  target_week,
  target_season,
  training_start = NULL,
  include_coaching = TRUE,
  include_referee  = TRUE,
  include_injuries = FALSE,
  seed_week1 = TRUE
) {
  # ---- validation / deps ----
  if (is.null(target_week) || is.null(target_season)) {
    stop("Both target_week and target_season must be provided.")
  }
  if (!requireNamespace("nflreadr", quietly = TRUE)) stop("nflreadr is required.")
  if (!requireNamespace("dplyr", quietly = TRUE))   stop("dplyr is required.")
  if (!requireNamespace("tidyr", quietly = TRUE))   stop("tidyr is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("stringr is required.")
  if (!requireNamespace("purrr", quietly = TRUE))   stop("purrr is required.")

  # Fallback for team mapping if not provided by your package
  if (!exists("map_team_abbreviation", mode = "function")) {
    map_team_abbreviation <- function(x) x
  }

  cat("=== ENHANCED NFL PREDICTION DATA PREPARATION ===\n")
  cat("Target: Week", target_week, "Season", target_season, "\n")
  if (target_season >= 2025 && include_injuries) {
    cat("Note: Injury data deprecated for 2025+, disabling injury features\n")
    include_injuries <- FALSE
  }

  # ---- helper: compute team features from past games (no leakage) ----
  compute_team_lag_feats <- function(games_df, upto_week = Inf) {
    # Only weeks <= upto_week
    df <- games_df %>%
      dplyr::filter(is.finite(week), week <= upto_week)

    # Identify realized metrics available on the game table
    home_outcomes <- names(df)[grepl("^home\\..*_outcome$", names(df))]
    away_outcomes <- names(df)[grepl("^away\\..*_outcome$", names(df))]
    if (!length(home_outcomes) || !length(away_outcomes)) {
      stop("No *_outcome columns found to build lagged features.")
    }

    strip_metric <- function(nm) {
      nm <- sub("^home\\.|^away\\.", "", nm)
      sub("_outcome$", "", nm)
    }

    home_long <- df %>%
      dplyr::select(season, week, team = home_team, dplyr::all_of(home_outcomes)) %>%
      dplyr::rename_with(~ strip_metric(.x), dplyr::all_of(home_outcomes))

    away_long <- df %>%
      dplyr::select(season, week, team = away_team, dplyr::all_of(away_outcomes)) %>%
      dplyr::rename_with(~ strip_metric(.x), dplyr::all_of(away_outcomes))

    team_week <- dplyr::bind_rows(home_long, away_long)

    team_week_long <- team_week %>%
      tidyr::pivot_longer(
        cols = -c(season, week, team),
        names_to = "metric",
        values_to = "value"
      ) %>%
      dplyr::arrange(team, metric, week)

    feats_long <- team_week_long %>%
      dplyr::group_by(team, metric) %>%
      dplyr::summarise(
        lastweek = {
          v <- value[!is.na(value)]
          if (length(v)) v[length(v)] else NA_real_
        },
        last3 = {
          v <- value[!is.na(value)]
          if (length(v)) mean(utils::tail(v, 3)) else NA_real_
        },
        std = mean(value, na.rm = TRUE),
        .groups = "drop"
      )

    feats_wide <- feats_long %>%
      tidyr::pivot_wider(
        names_from  = metric,
        values_from = c(lastweek, last3, std),
        names_glue  = "{metric}_{.value}"
      ) %>%
      dplyr::rename_with(~ sub("_lastweek$", "_lastweek", .x)) %>%
      dplyr::rename_with(~ sub("_last3$",    "_last3",    .x)) %>%
      dplyr::rename_with(~ sub("_std$",      "_std",      .x))

    feats_wide %>% dplyr::rename(posteam = team)
  }

  # ---- optional training block (no splitting; your backtest does it) ----
  training_data <- NULL
  if (!is.null(training_start)) {
    cat("\nPreparing training data from", training_start, "through Week", target_week - 1, "of", target_season, "\n")

    complete_seasons <- seq(training_start, max(target_season - 1L, training_start - 1L))
    if (length(complete_seasons) && complete_seasons[1] <= complete_seasons[length(complete_seasons)]) {
      cat("Loading complete seasons:", paste(complete_seasons, collapse = ", "), "\n")
      training_complete <- tryCatch(
        prepare_games(
          years = complete_seasons,
          include_injuries = include_injuries && max(complete_seasons) < 2025,
          include_coaching = include_coaching,
          include_referee  = include_referee,
          seed_week1 = seed_week1
        ),
        error = function(e) stop("Failed to prepare training data for complete seasons: ", e$message)
      )
    } else {
      training_complete <- NULL
    }

    current_part <- NULL
    if (target_week > 1) {
      cat("Adding", target_season, "Season Weeks 1-", target_week - 1, "\n")
      current_part <- tryCatch(
        prepare_games(
          years = target_season,
          include_injuries = include_injuries && target_season < 2025,
          include_coaching = include_coaching,
          include_referee  = include_referee,
          seed_week1 = seed_week1
        ) %>% dplyr::filter(week < target_week),
        error = function(e) stop("Failed to prepare current season training slice: ", e$message)
      )
    }

    if (!is.null(training_complete) && !is.null(current_part)) {
      training_data <- dplyr::bind_rows(training_complete, current_part)
    } else if (!is.null(training_complete)) {
      training_data <- training_complete
    } else {
      training_data <- current_part
    }

    cat("Total training games available:", ifelse(is.null(training_data), 0L, nrow(training_data)), "\n")
  }

  # ---- build lag features for the prediction week (no leakage) ----
  cat("\nComputing team features for prediction week...\n")
  if (target_week == 1) {
    cat("Week 1: seeding from prior season", target_season - 1, "\n")
    prior_season_games <- tryCatch(
      prepare_games(
        years = target_season - 1L,
        include_injuries = FALSE,
        include_coaching = include_coaching,
        include_referee  = include_referee,
        seed_week1 = FALSE
      ),
      error = function(e) stop("Failed to prepare prior season for Week 1: ", e$message)
    )
    team_feats <- compute_team_lag_feats(prior_season_games, upto_week = Inf)
  } else {
    current_games <- tryCatch(
      prepare_games(
        years = target_season,
        include_injuries = FALSE,
        include_coaching = include_coaching,
        include_referee  = include_referee,
        seed_week1 = seed_week1
      ) %>% dplyr::filter(week < target_week),
      error = function(e) stop("Failed to prepare current season slice: ", e$message)
    )
    team_feats <- compute_team_lag_feats(current_games, upto_week = target_week - 1L)
  }
  cat("Generated lag features for", nrow(team_feats), "teams\n")

  # ---- target week schedule ----
  cat("\nLoading schedule for Week", target_week, "Season", target_season, "\n")
  target_schedule <- tryCatch({
    nflreadr::load_schedules(target_season) %>%
      dplyr::filter(week == target_week, game_type == "REG") %>%
      dplyr::mutate(
        home_team = map_team_abbreviation(home_team),
        away_team = map_team_abbreviation(away_team)
      )
  }, error = function(e) stop("Failed to load schedule: ", e$message))
  if (nrow(target_schedule) == 0) stop("No games found for Week ", target_week, " Season ", target_season)
  cat("Found", nrow(target_schedule), "games to predict\n")

  # ---- join team features to schedule (home/away) ----
  home_df <- team_feats %>%
    dplyr::rename(home_team = posteam) %>%
    dplyr::rename_with(~ paste0("home.", .x), -home_team)

  away_df <- team_feats %>%
    dplyr::rename(away_team = posteam) %>%
    dplyr::rename_with(~ paste0("away.", .x), -away_team)

  prediction_data <- target_schedule %>%
    dplyr::select(
      game_id, season, week, gameday, weekday, gametime,
      home_team, away_team, location,
      home_rest, away_rest,
      home_moneyline, away_moneyline,
      spread_line, total_line,
      home_spread_odds, away_spread_odds,
      under_odds, over_odds,
      div_game, roof, surface,
      home_coach, away_coach, referee,
      stadium_id, stadium
    ) %>%
    dplyr::left_join(home_df, by = "home_team") %>%
    dplyr::left_join(away_df, by = "away_team") %>%
    dplyr::mutate(
      away.spread_line = -spread_line,
      rest_advantage   = home_rest - away_rest
    )

  # ---- coaching (safe defaults if analysis unavailable) ----
  if (include_coaching) {
    cat("Adding coaching analysis...\n")
    historical_sched <- tryCatch({
      purrr::map_dfr(
        max(2020, target_season - 3):target_season,
        ~ nflreadr::load_schedules(.x)
      )
    }, error = function(e) NULL)

    if (!is.null(historical_sched)) {
      coach_res <- tryCatch(analyze_coaching_performance(historical_sched), error = function(e) NULL)
      if (!is.null(coach_res) && "coach_tiers" %in% names(coach_res)) {
        coach_tiers <- coach_res$coach_tiers %>%
          dplyr::mutate(coach_name_clean = stringr::str_trim(stringr::str_to_title(coach_name)))

        all_coaches <- unique(c(prediction_data$home_coach, prediction_data$away_coach))
        all_coaches <- all_coaches[!is.na(all_coaches) & nzchar(all_coaches)]
        missing_coaches <- setdiff(
          stringr::str_trim(stringr::str_to_title(all_coaches)),
          coach_tiers$coach_name_clean
        )
        if (length(missing_coaches) > 0) {
          coach_tiers <- dplyr::bind_rows(
            coach_tiers,
            data.frame(
              coach_name = missing_coaches,
              coach_tier = "rookie",
              coaching_score = 45.0,
              experience_level = "new",
              win_percentage = 0.45,
              coach_name_clean = missing_coaches,
              stringsAsFactors = FALSE
            )
          )
        }

        prediction_data <- prediction_data %>%
          dplyr::mutate(
            home_coach_clean = stringr::str_trim(stringr::str_to_title(home_coach)),
            away_coach_clean = stringr::str_trim(stringr::str_to_title(away_coach))
          ) %>%
          dplyr::left_join(
            coach_tiers %>% dplyr::select(coach_name_clean, coach_tier, coaching_score, experience_level, win_percentage),
            by = c("home_coach_clean" = "coach_name_clean")
          ) %>%
          dplyr::rename(
            home_coach_tier = coach_tier, home_coaching_score = coaching_score,
            home_coach_experience = experience_level, home_win_percentage = win_percentage
          ) %>%
          dplyr::left_join(
            coach_tiers %>% dplyr::select(coach_name_clean, coach_tier, coaching_score, experience_level, win_percentage),
            by = c("away_coach_clean" = "coach_name_clean")
          ) %>%
          dplyr::rename(
            away_coach_tier = coach_tier, away_coaching_score = coaching_score,
            away_coach_experience = experience_level, away_win_percentage = win_percentage
          ) %>%
          dplyr::mutate(
            home_coaching_score = dplyr::coalesce(home_coaching_score, 45.0),
            away_coaching_score = dplyr::coalesce(away_coaching_score, 45.0),
            coaching_advantage  = home_coaching_score - away_coaching_score
          ) %>%
          dplyr::select(-home_coach_clean, -away_coach_clean)
      } else {
        prediction_data <- prediction_data %>%
          dplyr::mutate(
            home_coach_tier = "unknown", home_coaching_score = 50.0, home_coach_experience = "unknown",
            home_win_percentage = 0.5, away_coach_tier = "unknown", away_coaching_score = 50.0,
            away_coach_experience = "unknown", away_win_percentage = 0.5,
            coaching_advantage = 0
          )
      }
    }
  }

  # ---- referee (safe defaults if unassigned/unavailable) ----
  if (include_referee) {
    cat("Adding referee analysis...\n")
    if (all(is.na(prediction_data$referee))) {
      cat("No referees assigned yet for Week", target_week, "\n")
      prediction_data <- prediction_data %>%
        dplyr::mutate(
          referee_quality = 60.0, referee_experience = "unknown",
          bias_category = "unknown", consistency_level = "unknown",
          home_bias_score = 0, avg_penalties_per_game = 12.0,
          games_officiated = 0L
        )
    } else {
      ref_profile <- tryCatch({
        hist_sched <- purrr::map_dfr(max(2020, target_season - 2):(target_season - 1), nflreadr::load_schedules)
        hist_pbp   <- purrr::map_dfr(max(2020, target_season - 2):(target_season - 1), nflreadr::load_pbp)
        analyze_referee_performance(hist_sched, hist_pbp)
      }, error = function(e) NULL)

      if (is.data.frame(ref_profile) && nrow(ref_profile) > 0) {
        prediction_data <- prediction_data %>%
          dplyr::left_join(
            ref_profile %>%
              dplyr::select(dplyr::any_of(c(
                "referee", "referee_quality", "referee_experience", "bias_category",
                "consistency_level", "home_bias_score", "avg_penalties_per_game", "games_officiated"
              ))),
            by = "referee"
          ) %>%
          dplyr::mutate(
            referee_quality = dplyr::coalesce(referee_quality, 60.0),
            referee_experience = dplyr::coalesce(referee_experience, "unknown"),
            bias_category = dplyr::coalesce(bias_category, "unknown"),
            consistency_level = dplyr::coalesce(consistency_level, "unknown"),
            home_bias_score = dplyr::coalesce(home_bias_score, 0),
            avg_penalties_per_game = dplyr::coalesce(avg_penalties_per_game, 12.0),
            games_officiated = dplyr::coalesce(games_officiated, 0L)
          )
      } else {
        prediction_data <- prediction_data %>%
          dplyr::mutate(
            referee_quality = 60.0, referee_experience = "unknown",
            bias_category = "unknown", consistency_level = "unknown",
            home_bias_score = 0, avg_penalties_per_game = 12.0,
            games_officiated = 0L
          )
      }
    }
  }

  # ---- injuries: keep shape for 2025+ (all zeros) ----
  if (target_season >= 2025) {
    prediction_data <- prediction_data %>%
      dplyr::mutate(
        home_total_injury_impact = 0, home_qb_injury_impact = 0,
        home_skill_injury_impact = 0, home_oline_injury_impact = 0,
        home_defense_injury_impact = 0, home_num_key_injuries = 0L,
        away_total_injury_impact = 0, away_qb_injury_impact = 0,
        away_skill_injury_impact = 0, away_oline_injury_impact = 0,
        away_defense_injury_impact = 0, away_num_key_injuries = 0L,
        injury_advantage = 0, qb_injury_advantage = 0
      )
  }

  # ---- derived placeholders (for compatibility) ----
  prediction_data <- prediction_data %>%
    dplyr::mutate(
      point_differential = NA_real_,
      total = NA_real_
    )

  # ---- optional: live odds override if helpers exist ----
  tryCatch({
    cat("Checking for live betting odds...\n")
    if (exists("get_nfl_odds", mode = "function") && exists("calculate_consensus_odds", mode = "function")) {
      odds_data <- get_nfl_odds()
      if (is.data.frame(odds_data) && nrow(odds_data) > 0) {
        consensus <- calculate_consensus_odds(odds_data) %>% dplyr::filter(week == target_week)
        if (nrow(consensus) > 0) {
          prediction_data <- prediction_data %>%
            dplyr::left_join(
              consensus %>% dplyr::select(home_team, away_team, spread, total) %>%
                dplyr::rename(live_spread = spread, live_total = total),
              by = c("home_team", "away_team")
            ) %>%
            dplyr::mutate(
              spread_line = dplyr::coalesce(live_spread, spread_line),
              total_line  = dplyr::coalesce(live_total, total_line),
              away.spread_line = -spread_line
            ) %>%
            dplyr::select(-live_spread, -live_total)
          cat("Updated with live odds\n")
        }
      }
    } else {
      cat("Live odds helpers not available; using schedule lines\n")
    }
  }, error = function(e) cat("Could not fetch live odds; using schedule lines\n"))

  # ---- final summary ----
  cat("\n=== PREDICTION DATA SUMMARY ===\n")
  cat("Games to predict:", nrow(prediction_data), "\n")
  cat("Columns:", ncol(prediction_data), "\n")
  if (any(grepl("_outcome$", names(prediction_data))))
    warning("Prediction frame contains *_outcome columns; this should not happen.")

  if (!is.null(training_data)) {
    return(list(
      training   = training_data,
      prediction = prediction_data
    ))
  } else {
    return(prediction_data)
  }
}
