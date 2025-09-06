# ==================== utils_parallel.R ====================
# Tiny helper to set/reset parallel plan safely

safe_set_parallel <- function(enable = FALSE, workers = NULL) {
  if (!enable) return(function(){})
  if (!requireNamespace("future", quietly = TRUE)) {
    warning("future not installed; running sequentially.")
    return(function(){})
  }
  old_plan <- future::plan()
  # On Windows, use multisession; else multicore is fine but multisession is safest universally
  if (is.null(workers)) {
    future::plan(future::multisession)
  } else {
    future::plan(future::multisession, workers = workers)
  }
  # Return a restorer you can call in on.exit()
  function() {
    future::plan(old_plan)
  }
}
