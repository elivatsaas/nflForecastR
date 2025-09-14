#' Walk-Forward Backtest for NFL Models (no leakage)
#'
#' Trains strictly on information available **before** each prediction week and
#' evaluates on the completed games of that week. By default, it predicts
#' `point_differential_outcome`, but you can set `target_outcome` to **any**
#' `*_outcome` column produced by `prepare_games()`.
#'
#' The function supports linear models (`lm`) and random forests (`ranger`).
#' If you pass `formula = NULL`, it auto-builds a leak-free formula:
#' \itemize{
#'   \item Drops **all other** `*_outcome` columns (so only the chosen target remains).
#'   \item Drops raw score columns (`home_score`, `away_score`, `point_differential`, `total`).
#'   \item Drops obvious IDs (`season`, `week`, `game_id`, `home_team`, `away_team`) to
#'         avoid factor-level issues across weeks.
#' }
#'
#' @param years Integer vector of seasons to walk-forward over (e.g., `2024` or `2019:2024`).
#' @param start_week Integer, first week to score (default `1`).
#' @param end_week Integer, last week to score (default `18`).
#' @param model_type Character, `"lm"` or `"rf"` (random forest via \pkg{ranger}).
#' @param formula Optional model formula. If `NULL`, a leak-free formula is auto-built.
#' @param target_outcome Character name of the outcome column to predict
#'   (default `"point_differential_outcome"`). May be any `*_outcome` from `prepare_games()`.
#' @param min_training_games Minimum number of training rows required to fit (default `500`).
#' @param parallel Logical; if `TRUE` uses a PSOCK cluster via \pkg{parallel}.
#' @param n_cores Integer cores to use when `parallel = TRUE`. Default `detectCores()-1`.
#' @param all_updated_games Optional prebuilt modeling frame (typically output of `prepare_games()`).
#'
#' @return A list with:
#' \describe{
#'   \item{predictions}{Tibble of per-game predictions with actuals and correctness flags.}
#'   \item{week_summaries}{Data frame of per-week metrics (MAE, accuracy, etc.).}
#'   \item{yearly_summary}{Per-season aggregate metrics (if any predictions).}
#'   \item{overall_metrics}{Overall MAE/median AE and accuracy.}
#'   \item{parameters}{Echo of function inputs for reproducibility.}
#' }
#'
#' @details
#' - Moneyline/spread correctness are only computed when `target_outcome` is
#'   `point_differential_outcome` (margin target).
#' - Numeric features are optionally scaled (z-score) except identifiers, market lines,
#'   and any `*_outcome` columns.
#'
#' @examples
#' \dontrun{
#' games <- prepare_games(2015:2024, include_injuries = FALSE, include_coaching = TRUE,
#'                        include_referee = FALSE, seed_week1 = TRUE)
#' bt <- backtest_model(
#'   years = 2024,
#'   start_week = 1, end_week = 18,
#'   model_type = "lm",
#'   formula = point_differential_outcome ~ . - season - week - game_id - home_team - away_team,
#'   target_outcome = "point_differential_outcome",
#'   all_updated_games = games
#' )
#' }
#'
#' @importFrom stats lm predict median
#' @export
backtest_model <- function(years,
                           start_week = 1,
                           end_week   = 18,
                           model_type = "lm",          # "lm" or "rf"
                           formula    = NULL,          # optional; if NULL we'll build it
                           target_outcome = "point_differential_outcome",
                           min_training_games = 500,
                           parallel   = FALSE,
                           n_cores    = NULL,
                           all_updated_games = NULL) {

  cat("=== NFL MODEL BACKTESTING ===\n")
  cat("Years:", paste(years, collapse = ", "), "\n")
  cat("Weeks:", start_week, "to", end_week, "\n")
  cat("Model:", model_type, "\n")
  cat("Target outcome:", target_outcome, "\n\n")

  # ---------------------------- Load the modeling frame ----------------------------
  if (is.null(all_updated_games)) {
    cat("Loading games via update_games(2015:", max(years), ")...\n")
    all_updated_games <- update_games(2015:max(years))
  }
  if (!target_outcome %in% names(all_updated_games)) {
    stop("Target outcome column not found: ", target_outcome)
  }

  # ---------------------------- Build a no-leakage formula if needed ----------------
  if (is.null(formula)) {
    drop_outcomes <- setdiff(grep("_outcome$", names(all_updated_games), value = TRUE), target_outcome)
    drop_scores   <- c("home_score","away_score","point_differential","total")
    drop_ids      <- intersect(c("season","week","game_id","home_team","away_team"),
                               names(all_updated_games))
    predictors    <- setdiff(names(all_updated_games),
                             c(target_outcome, drop_outcomes, drop_scores, drop_ids))
    formula <- stats::reformulate(predictors, response = target_outcome)
    cat("Auto-built formula:\n  ", deparse(formula), "\n\n")
  } else {
    cat("Using user-supplied formula:\n  ", deparse(formula), "\n\n")
  }

  # ---------------------------- Optional: scale predictors only --------------------
  do_not_scale <- unique(c(
    # identifiers / market lines / money stuff
    "season","week","game_id","home_team","away_team",
    "spread_line","total_line","home_moneyline","away_moneyline",
    "home_spread_odds","away_spread_odds","under_odds","over_odds",
    # raw postgame score summaries (never predictors)
    "home_score","away_score","point_differential","total",
    # ALL outcomes (incl. target) should not be scaled
    grep("_outcome$", names(all_updated_games), value = TRUE)
  ))

  drop_cols <- c("home.posteam_type","away.posteam_type","home.game_id","away.game_id")

  all_scaled_games <- all_updated_games %>%
    dplyr::select(-dplyr::any_of(drop_cols)) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric) & !dplyr::any_of(do_not_scale),
        ~ as.numeric(scale(.)),
        .names = "{.col}"
      )
    )

  # ---------------------------- Walk-forward task grid -----------------------------
  tasks <- expand.grid(year = years, week = seq.int(start_week, end_week), stringsAsFactors = FALSE)
  cat("Total tasks:", nrow(tasks), "\n\n")

  # helper: ATS correctness (only meaningful for margin target)
  spread_win <- function(pred_margin, actual_margin, spread_line) {
    sign(pred_margin - spread_line) == sign(actual_margin - spread_line)
  }
  margin_target <- identical(target_outcome, "point_differential_outcome")

  process_week <- function(task_row) {
    year <- task_row$year; week <- task_row$week

    # training strictly BEFORE this week
    training_data <- if (week == 1) {
      all_scaled_games %>% dplyr::filter(season < year)
    } else {
      all_scaled_games %>% dplyr::filter(season < year | (season == year & week < !!week))
    }

    if (nrow(training_data) < min_training_games) {
      return(list(status="skipped", reason=paste("insufficient_training_data:", nrow(training_data)),
                  year=year, week=week))
    }

    # evaluate only completed games for this week
    prediction_games <- all_scaled_games %>%
      dplyr::filter(season == year, week == !!week) %>%
      dplyr::filter(!is.na(.data[[target_outcome]]))

    if (!nrow(prediction_games)) {
      return(list(status="skipped", reason="no_completed_games", year=year, week=week))
    }

    # fit
    fit <- switch(
      model_type,
      lm = stats::lm(formula, data = training_data),
      rf = {
        if (!requireNamespace("ranger", quietly = TRUE))
          stop("Install 'ranger' to use model_type='rf'.")
        ranger::ranger(formula, data = training_data, num.trees = 500,
                       importance = "permutation",
                       respect.unordered.factors = "order")
      },
      stop("Unsupported model_type: use 'lm' or 'rf'")
    )

    # predict
    preds <- if (model_type == "lm") {
      as.numeric(predict(fit, newdata = prediction_games))
    } else {
      as.numeric(predict(fit, data = prediction_games)$predictions)
    }

    actual <- prediction_games[[target_outcome]]

    # moneyline / spread metrics only for margin target
    ml_correct_vec <- if (margin_target) (preds > 0) == (actual > 0) else rep(NA, length(preds))
    spr_correct_vec <- if (margin_target && "spread_line" %in% names(prediction_games)) {
      spread_win(preds, actual, prediction_games$spread_line)
    } else rep(NA, length(preds))

    week_predictions <- prediction_games %>%
      dplyr::select(season, week, home_team, away_team, spread_line,
                    dplyr::any_of(c("point_differential","total"))) %>%
      dplyr::mutate(
        predicted = preds,
        actual    = actual,
        ml_correct     = ml_correct_vec,
        spread_correct = spr_correct_vec
      )

    week_summary <- data.frame(
      season = year,
      week   = week,
      training_games   = nrow(training_data),
      prediction_games = nrow(prediction_games),
      ml_accuracy      = mean(ml_correct_vec, na.rm = TRUE),
      spread_accuracy  = if (all(is.na(spr_correct_vec))) NA_real_
      else mean(spr_correct_vec, na.rm = TRUE),
      mae              = mean(abs(preds - actual)),
      median_ae        = stats::median(abs(preds - actual))
    )

    list(status="success", predictions=week_predictions, summary=week_summary, year=year, week=week)
  }

  # ---------------------------- Run ----------------------------
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) stop("Install 'parallel' for parallel execution.")
    if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
    cat("Parallel: TRUE (", n_cores, " cores)\n")
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl,
      varlist=c("all_scaled_games","model_type","formula","min_training_games",
                "spread_win","target_outcome","margin_target"),
      envir=environment()
    )
    parallel::clusterEvalQ(cl, { library(dplyr); library(stats) })
    if (identical(model_type,"rf")) parallel::clusterEvalQ(cl, { library(ranger) })
    task_list <- split(tasks, seq_len(nrow(tasks)))
    results <- parallel::parLapply(cl, task_list, function(x) {
      year <- x$year; week <- x$week
      process_week(data.frame(year=year, week=week))
    })
  } else {
    cat("Parallel: FALSE\n")
    results <- vector("list", nrow(tasks))
    for (i in seq_len(nrow(tasks))) {
      yr <- tasks$year[i]; wk <- tasks$week[i]
      cat(sprintf("Processing %d-W%02d (%d/%d)... ", yr, wk, i, nrow(tasks)))
      results[[i]] <- process_week(tasks[i, , drop = FALSE])
      st <- results[[i]]$status
      if (st == "success") {
        s <- results[[i]]$summary
        cat(sprintf("✓ %d games | ML: %s | SPR: %s | MAE: %.2f | MedAE: %.2f\n",
                    s$prediction_games,
                    if (is.na(s$ml_accuracy)) "NA" else sprintf("%.1f%%", 100*s$ml_accuracy),
                    if (is.na(s$spread_accuracy)) "NA" else sprintf("%.1f%%", 100*s$spread_accuracy),
                    s$mae, s$median_ae))
      } else {
        cat(st, if (!is.null(results[[i]]$reason)) paste0(" (", results[[i]]$reason, ")"), "\n")
      }
    }
  }

  # ---------------------------- Aggregate & report ----------------------------
  successes <- results[sapply(results, `[[`, "status") == "success"]
  preds <- if (length(successes)) dplyr::bind_rows(lapply(successes, `[[`, "predictions")) else dplyr::tibble()
  weeks <- if (length(successes)) dplyr::bind_rows(lapply(successes, `[[`, "summary")) else dplyr::tibble()

  cat("\n=== BACKTEST RESULTS ===\n")
  if (nrow(preds)) {
    total_games <- nrow(preds)
    total_ml_correct   <- sum(preds$ml_correct, na.rm = TRUE)
    total_spr_correct  <- sum(preds$spread_correct, na.rm = TRUE)
    spr_den            <- sum(!is.na(preds$spread_correct))

    overall_ml_acc  <- if (spr_den + total_ml_correct >= 0 && any(!is.na(preds$ml_correct)))
      mean(preds$ml_correct, na.rm = TRUE) else NA_real_
    overall_spr_acc <- if (spr_den > 0) total_spr_correct / spr_den else NA_real_
    overall_mae     <- mean(abs(preds$predicted - preds$actual))
    overall_medae   <- stats::median(abs(preds$predicted - preds$actual))

    cat(sprintf("Total games: %d\n", total_games))
    cat(sprintf("ML accuracy: %s\n",
                if (is.na(overall_ml_acc)) "NA (non-margin target)"
                else sprintf("%.1f%%", 100*overall_ml_acc)))
    cat(sprintf("Spread acc.: %s\n",
                if (is.na(overall_spr_acc)) "NA (non-margin target)"
                else sprintf("%.1f%%", 100*overall_spr_acc)))
    cat(sprintf("MAE: %.2f | Median AE: %.2f\n", overall_mae, overall_medae))

    yearly_summary <- preds %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        games = dplyr::n(),
        ml_correct = sum(ml_correct, na.rm = TRUE),
        ml_accuracy = dplyr::if_else(any(!is.na(ml_correct)), 100*ml_correct/games, NA_real_),
        spread_correct_count = sum(spread_correct, na.rm = TRUE),
        spread_games = sum(!is.na(spread_correct)),
        spread_accuracy = dplyr::if_else(spread_games > 0, 100*spread_correct_count/spread_games, NA_real_),
        mae = mean(abs(predicted - actual)),
        median_ae = stats::median(abs(predicted - actual)),
        .groups = "drop"
      )
    print(yearly_summary)
  } else {
    cat("No predictions.\n")
    yearly_summary <- NULL
    overall_ml_acc <- overall_spr_acc <- overall_mae <- overall_medae <- NA
  }

  list(
    predictions = preds,
    week_summaries = weeks,
    yearly_summary = yearly_summary,
    overall_metrics = list(
      total_games = if (nrow(preds)) nrow(preds) else 0,
      ml_accuracy = overall_ml_acc,
      spread_accuracy = overall_spr_acc,
      mae = overall_mae,
      median_ae = overall_medae
    ),
    parameters = list(
      years = years, start_week = start_week, end_week = end_week,
      model_type = model_type, formula = formula, target_outcome = target_outcome
    )
  )
}
