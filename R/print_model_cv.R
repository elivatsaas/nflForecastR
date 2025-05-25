#' Print method for CV results
#' @param x CV results object
#' @param ... Additional arguments
#' @export
print.model_cv <- function(x, ...) {
  cat("\nModel Cross-Validation Results\n")
  cat("Method:", x$method, "\n")
  cat("\nAverage Metrics:\n")
  metrics_df <- data.frame(
    Metric = names(x$average_metrics),
    Mean = unlist(x$average_metrics),
    SD = unlist(x$sd_metrics)
  )
  print(metrics_df, row.names = FALSE)
}
