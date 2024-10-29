#' Compare Multiple Model Results
#'
#' @param ... Named list of model CV results
#' @param round_digits Number of digits to round metrics (default = 3)
#' @return A data frame comparing model metrics
#' @export
compare_models <- function(..., round_digits = 3) {
  # Get all models and their names
  models <- list(...)
  model_names <- names(models)

  # If no names provided, generate default names
  if (is.null(model_names)) {
    model_names <- paste("Model", seq_along(models))
  }

  # Define metrics to extract
  metrics <- c("RMSE", "MAE", "MAD", "WinAccuracy", "SpreadAccuracy")

  # Create comparison data frame with numeric values
  comparison <- data.frame(
    Model = model_names,
    Method = sapply(models, function(x) x$method)
  )

  # Add raw numeric values for each metric
  for (metric in metrics) {
    comparison[[metric]] <- sapply(models, function(x) {
      mean(x$average_metrics[[metric]])
    })
    comparison[[paste0(metric, "_SD")]] <- sapply(models, function(x) {
      x$sd_metrics[[metric]]
    })
  }

  # Add formatted string versions of metrics
  for (metric in metrics) {
    comparison[[paste0(metric, "_Display")]] <- sprintf("%.3f (±%.3f)",
                                                        comparison[[metric]],
                                                        comparison[[paste0(metric, "_SD")]])
  }

  # Determine best models for each metric
  for (metric in metrics) {
    if (metric %in% c("RMSE", "MAE", "MAD")) {
      comparison[[paste0(metric, "_Best")]] <- comparison[[metric]] == min(comparison[[metric]])
    } else {
      comparison[[paste0(metric, "_Best")]] <- comparison[[metric]] == max(comparison[[metric]])
    }
  }

  # Create summary text
  comparison$Summary <- apply(comparison, 1, function(row) {
    best_metrics <- metrics[sapply(metrics, function(m) row[paste0(m, "_Best")]) == TRUE]
    if (length(best_metrics) > 0) {
      paste("Best in:", paste(best_metrics, collapse = ", "))
    } else {
      "No best metrics"
    }
  })

  # Print formatted output
  cat("\nModel Comparison Results:\n")
  cat("========================================\n\n")

  for (i in seq_len(nrow(comparison))) {
    cat(sprintf("Model: %s (%s)\n", comparison$Model[i], comparison$Method[i]))
    cat("----------------------------------------\n")
    for (metric in metrics) {
      cat(sprintf("%s: %s\n", metric, comparison[[paste0(metric, "_Display")]][i]))
    }
    cat(sprintf("Summary: %s\n\n", comparison$Summary[i]))
  }

  # Return data frame with both numeric and display values
  return(comparison)
}
