#' Backtest NFL Betting Model with Walk-Forward Analysis
#'
#' Trains strictly on info available BEFORE each prediction week.
#' Works with prepare_games(..., seed_week1=TRUE, include_injuries=TRUE,
#' include_coaching=TRUE, include_referee=TRUE).
#'
#' @param years integer vector, e.g., 2024 or c(2022,2023,2024)
#' @param start_week integer, default 1
#' @param end_week integer, default 18
#' @param model_type "lm" or "rf"
#' @param formula model formula
#' @param min_training_games minimum rows to fit (default 500)
#' @param parallel logical; if TRUE uses PSOCK cluster
#' @param n_cores integer; cores for parallel, default detectCores()-1
#' @param all_updated_games optional prebuilt modeling frame
#' @return list(predictions, week_summaries, yearly_summary, overall_metrics, parameters)
#' @export
backtest_model <- function(years,
                           start_week = 1,
                           end_week   = 18,
                           model_type = "lm",
                           formula    = NULL,
                           min_training_games = 500,
                           parallel   = FALSE,
                           n_cores    = NULL,
                           all_updated_games = NULL) {
  
  cat("=== NFL MODEL BACKTESTING ===\n")
  cat("Years:", paste(years, collapse=", "), "\n")
  cat("Weeks:", start_week, "to", end_week, "\n")
  cat("Model:", model_type, "\n")
  cat("Formula:", if (is.null(formula)) "<NULL>" else deparse(formula), "\n\n")
  
  # ----------------------------
  # Load games (or use provided)
  # ----------------------------
  if (is.null(all_updated_games)) {
    cat("Loading & building modeling frame via update_games()...\n")
    # Make sure update_games internally calls prepare_games(..., seed_week1=TRUE,
    # include_injuries=TRUE, include_coaching=TRUE, include_referee=TRUE)
    all_updated_games <- update_games(2015:max(years))
  }
  
  # ----------------------------
  # Scale numeric predictors only
  # ----------------------------
  # Keep identifiers & outcome intact; keep schedule lines unscaled
  do_not_scale <- c(
    "season","week","game_id","point_differential",
    "home_team","away_team","div_game","total",
    "spread_line","total_line","home_moneyline","away_moneyline",
    "home_spread_odds","away_spread_odds","under_odds","over_odds"
  )
  
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
  
  # ----------------------------------
  # Task grid and single-week runner
  # ----------------------------------
  tasks <- expand.grid(year = years,
                       week = seq.int(start_week, end_week),
                       stringsAsFactors = FALSE)
  cat("Total tasks:", nrow(tasks), "\n\n")
  
  # Helper: compute spread correctness against schedule's spread_line
  spread_win <- function(pred_margin, actual_margin, spread_line) {
    # Correct if (predicted margin - line) and (actual margin - line) have same sign or both == 0
    sign(pred_margin - spread_line) == sign(actual_margin - spread_line)
  }
  
  process_week <- function(task_row) {
    year <- task_row$year; week <- task_row$week
    
    # Train strictly before this week in 'year'
    training_data <- if (week == 1) {
      all_scaled_games %>% dplyr::filter(season < year)
    } else {
      all_scaled_games %>% dplyr::filter(season < year | (season == year & week < !!week))
    }
    
    if (nrow(training_data) < min_training_games) {
      return(list(status="skipped",
                  reason=paste("insufficient_training_data:", nrow(training_data)),
                  year=year, week=week))
    }
    
    # Evaluate only completed games to compute accuracy
    prediction_games <- all_scaled_games %>%
      dplyr::filter(season == year, week == !!week) %>%
      dplyr::filter(!is.na(point_differential))
    
    if (nrow(prediction_games) == 0) {
      return(list(status="skipped", reason="no_completed_games", year=year, week=week))
    }
    
    # Fit & predict
    fit <- switch(
      model_type,
      lm = stats::lm(formula, data = training_data),
      rf = ranger::ranger(formula, data = training_data, num.trees = 500,
                          importance = "permutation",
                          respect.unordered.factors = "order"),
      stop("Unsupported model_type: use 'lm' or 'rf'")
    )
    
    preds <- if (model_type == "lm") {
      as.numeric(predict(fit, newdata = prediction_games))
    } else {
      as.numeric(predict(fit, data = prediction_games)$predictions)
    }
    
    # Metrics
    actual <- prediction_games$point_differential
    ml_correct_vec <- (preds > 0) == (actual > 0)
    
    # Use schedule's spread_line (single column, home-centered)
    if ("spread_line" %in% names(prediction_games)) {
      spr_correct_vec <- spread_win(preds, actual, prediction_games$spread_line)
    } else {
      spr_correct_vec <- rep(NA, length(preds))
    }
    
    week_predictions <- prediction_games %>%
      dplyr::select(season, week, home_team, away_team, spread_line, point_differential) %>%
      dplyr::mutate(
        predicted_margin = preds,
        ml_correct       = ml_correct_vec,
        spread_correct   = spr_correct_vec
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
  
  # ----------------------------------
  # Run sequentially or in parallel
  # ----------------------------------
  if (parallel) {
    if (!requireNamespace("parallel", quietly=TRUE)) {
      stop("Install 'parallel' for parallel execution.")
    }
    if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
    cat("Parallel: TRUE (", n_cores, " cores)\n")
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add=TRUE)
    
    # Ship needed objects/packages
    parallel::clusterExport(cl, varlist=c("all_scaled_games","model_type","formula",
                                          "min_training_games","spread_win"),
                            envir=environment())
    parallel::clusterEvalQ(cl, { library(dplyr); library(ranger); library(stats) })
    
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
      results[[i]] <- process_week(tasks[i, , drop=FALSE])
      st <- results[[i]]$status
      if (st == "success") {
        s <- results[[i]]$summary
        cat(sprintf("✓ %d games | ML: %.1f%% | SPR: %s | MAE: %.2f | MedAE: %.2f\n",
                    s$prediction_games,
                    100*s$ml_accuracy,
                    if (is.na(s$spread_accuracy)) "NA" else sprintf("%.1f%%", 100*s$spread_accuracy),
                    s$mae, s$median_ae))
      } else {
        cat(st, if (!is.null(results[[i]]$reason)) paste0(" (", results[[i]]$reason, ")"), "\n")
      }
    }
  }
  
  # ----------------------------
  # Aggregate results & report
  # ----------------------------
  successes <- results[sapply(results, `[[`, "status") == "success"]
  preds <- if (length(successes)) dplyr::bind_rows(lapply(successes, `[[`, "predictions")) else dplyr::tibble()
  weeks <- if (length(successes)) dplyr::bind_rows(lapply(successes, `[[`, "summary")) else dplyr::tibble()
  
  cat("\n=== BACKTEST RESULTS ===\n")
  if (nrow(preds)) {
    total_games <- nrow(preds)
    total_ml_correct <- sum(preds$ml_correct, na.rm=TRUE)
    total_spr_correct <- sum(preds$spread_correct, na.rm=TRUE)
    spr_den <- sum(!is.na(preds$spread_correct))
    
    overall_ml_acc <- total_ml_correct / total_games
    overall_spr_acc <- if (spr_den > 0) total_spr_correct / spr_den else NA_real_
    overall_mae <- mean(abs(preds$predicted_margin - preds$point_differential))
    overall_medae <- stats::median(abs(preds$predicted_margin - preds$point_differential))
    
    cat(sprintf("Total games: %d\n", total_games))
    cat(sprintf("ML accuracy: %.1f%% (%d/%d)\n", 100*overall_ml_acc, total_ml_correct, total_games))
    cat(sprintf("Spread acc.: %s\n",
                if (is.na(overall_spr_acc)) "NA (no spread_line)" else sprintf("%.1f%% (%d/%d)", 100*overall_spr_acc, total_spr_correct, spr_den)))
    cat(sprintf("MAE: %.2f | Median AE: %.2f\n", overall_mae, overall_medae))
    
    cat("\nYearly breakdown:\n")
    yearly_summary <- preds %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        games = dplyr::n(),
        ml_correct = sum(ml_correct, na.rm=TRUE),
        ml_accuracy = 100*ml_correct/games,
        spread_correct_count = sum(spread_correct, na.rm=TRUE),
        spread_games = sum(!is.na(spread_correct)),
        spread_accuracy = dplyr::if_else(spread_games > 0, 100*spread_correct_count/spread_games, NA_real_),
        mae = mean(abs(predicted_margin - point_differential)),
        median_ae = stats::median(abs(predicted_margin - point_differential)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        ml_record = paste0(ml_correct,"/",games),
        spread_record = paste0(spread_correct_count,"/",spread_games)
      )
    print(yearly_summary[, c("season","games","ml_record","ml_accuracy","spread_record","spread_accuracy","mae","median_ae")])
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
      ml_correct  = if (nrow(preds)) sum(preds$ml_correct, na.rm=TRUE) else 0,
      spread_correct = if (nrow(preds)) sum(preds$spread_correct, na.rm=TRUE) else 0,
      ml_accuracy = overall_ml_acc,
      spread_accuracy = overall_spr_acc,
      mae = overall_mae,
      median_ae = overall_medae
    ),
    parameters = list(
      years = years, start_week = start_week, end_week = end_week,
      model_type = model_type, formula = formula
    )
  )
}
