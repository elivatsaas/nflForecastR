#' Calculate Means by Team History
#' @param data Data frame containing NFL statistics
#' @return Data frame with calculated means
#' @export
calculate_means <- function(data) {
  if("posteam" %in% names(data)) {
    # Posteam format handling stays the same
    return(data %>%
             group_by(season, posteam) %>%
             arrange(season, week) %>%
             mutate(
               across(where(is.numeric),
                      list(
                        last_3 = ~if(length(.) < 3) mean(., na.rm = TRUE)
                        else tail(zoo::rollmean(., k = 3, align = "right", fill = NA), 1),
                        season_mean = ~cummean(.),
                        momentum = ~{
                          current <- .
                          last_3 <- if(length(.) < 3) mean(., na.rm = TRUE)
                          else tail(zoo::rollmean(., k = 3, align = "right", fill = NA), 1)
                          season_mean <- cummean(.)
                          (current - last_3) - (last_3 - season_mean)
                        }
                      )
               )
             ) %>%
             ungroup())
  }
}
