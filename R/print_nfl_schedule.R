
#' Print method for nfl_schedule objects
#'
#' @param x An object of class nfl_schedule
#' @param ... Additional arguments passed to print
#' @export
print.nfl_schedule <- function(x, ...) {
  cat(sprintf("\nNFL Schedule - Week %d, %d\n", x$week[1], x$year[1]))
  cat(sprintf("Season Type: %s\n\n",
              c("Preseason", "Regular Season", "Postseason")[x$season_type[1]]))

  # Group by date
  dates <- unique(x$game_date)

  for(date in dates) {
    cat(format(date, "%A, %B %d, %Y"), "\n")
    cat(paste(rep("-", 80), collapse = ""), "\n")

    day_games <- x[x$game_date == date,]
    for(i in 1:nrow(day_games)) {
      game <- day_games[i,]
      cat(sprintf("%-8s  %-25s @ %-25s  %s\n",
                  game$game_time,
                  game$away_team,
                  game$home_team,
                  game$network))

      if(!is.na(game$spread_display)) {
        cat(sprintf("%-8s  %-53s\n",
                    "",
                    paste("Spread:", game$spread_display,
                          "  Total:", game$total_display)))
        cat(sprintf("%-8s  %-53s\n",
                    "",
                    paste("ML:",
                          sprintf("%+d", game$away_ml),
                          "at",
                          sprintf("%+d", game$home_ml),
                          sprintf("(Implied: %.1f%% at %.1f%%)",
                                  game$away_implied_prob * 100,
                                  game$home_implied_prob * 100))))
      }
      cat("\n")
    }
    cat("\n")
  }
}
