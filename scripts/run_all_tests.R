#!/usr/bin/env Rscript

# Ensure suggested packages are available
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required to run tests.")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run tests.")
}

# Optionally set an API key for odds API if needed
# Sys.setenv(ODDS_API_KEY = "<your_key_here>")

# Rebuild documentation and namespace
devtools::document()

# Run package checks (unit tests, examples, etc.)
devtools::test()
devtools::check(args = "--no-manual")
