#' Create NFL Prediction Visualization
#'
#' @param results_df Data frame from predict_with_model
#' @param background Character: "black" or "white" (default: "black")
#' @param week Numeric: NFL week number
#' @param layout Character: "vertical" or "horizontal" (default: "vertical")
#' @importFrom ggplot2 ggplot aes theme_void theme geom_rect geom_text
#'   scale_fill_identity coord_fixed element_rect margin
#'   scale_x_continuous scale_y_continuous
#' @importFrom nflplotR geom_nfl_logos
#' @importFrom grid unit
#' @export
create_prediction_plot <- function(results_df, background = "black", week = NULL, layout = "vertical") {
  # Set color scheme based on background
  text_color <- if(background == "black") "white" else "black"

  # Create color palettes with more visible ranges
  blue_palette <- colorRampPalette(c("#1E3F8F", "#4169E1"))(50)  # Navy to royal blue
  red_palette <- colorRampPalette(c("#8B0000", "#DC143C"))(50)   # Dark red to crimson

  # Calculate positions based on layout
  n_games <- nrow(results_df)

  if(layout == "vertical") {
    results_df <- results_df %>%
      mutate(
        y_pos = seq(n_games, 1, -1),
        away_x = 1,
        home_x = 2
      )
  } else {  # horizontal layout
    games_per_row <- ceiling(n_games/2)
    results_df <- results_df %>%
      mutate(
        x_pos = rep(1:2, each = games_per_row)[1:n_games] * 3 - 1,
        y_pos = rep(seq(games_per_row, 1, -1), 2)[1:n_games],
        away_x = x_pos - 0.5,
        home_x = x_pos + 0.5
      )
  }

  results_df <- results_df %>%
    mutate(
      # Calculate color indices based on win probability
      home_intensity = round(home_win_prob * 49) + 1,
      away_intensity = round((1 - home_win_prob) * 49) + 1,
      # Assign colors based on predictions with intensity
      home_color = ifelse(chosen_spread == "Home",
                          blue_palette[home_intensity],
                          red_palette[home_intensity]),
      away_color = ifelse(chosen_spread == "Away",
                          blue_palette[away_intensity],
                          red_palette[away_intensity])
    )

  # Create plot
  p <- ggplot() +
    # Add colored rectangles with higher alpha
    geom_rect(data = results_df,
              aes(xmin = away_x - 0.35, xmax = away_x + 0.35,
                  ymin = y_pos - 0.4, ymax = y_pos + 0.4,
                  fill = away_color),
              alpha = 0.7) +  # Increased alpha
    geom_rect(data = results_df,
              aes(xmin = home_x - 0.35, xmax = home_x + 0.35,
                  ymin = y_pos - 0.4, ymax = y_pos + 0.4,
                  fill = home_color),
              alpha = 0.7) +  # Increased alpha
    # Add darker background rectangles for better contrast
    geom_rect(data = results_df,
              aes(xmin = away_x - 0.35, xmax = away_x + 0.35,
                  ymin = y_pos - 0.4, ymax = y_pos + 0.4),
              fill = "black",
              alpha = 0.3) +
    geom_rect(data = results_df,
              aes(xmin = home_x - 0.35, xmax = home_x + 0.35,
                  ymin = y_pos - 0.4, ymax = y_pos + 0.4),
              fill = "black",
              alpha = 0.3) +
    # Add team logos
    geom_nfl_logos(data = results_df,
                   aes(x = away_x, y = y_pos,
                       team_abbr = away_team),
                   width = 0.3,
                   alpha = 1) +
    geom_nfl_logos(data = results_df,
                   aes(x = home_x, y = y_pos,
                       team_abbr = home_team),
                   width = 0.3,
                   alpha = 1) +
    # Add VS text
    geom_text(data = results_df,
              aes(x = (away_x + home_x)/2, y = y_pos),
              label = "vs",
              color = text_color,
              size = 3) +
    # Styling
    scale_fill_identity() +
    coord_fixed(ratio = if(layout == "vertical") 0.5 else 1) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = background, color = NA),
      plot.margin = margin(30, 30, 30, 30)
    )

  # Add appropriate limits based on layout
  if(layout == "vertical") {
    p <- p + scale_x_continuous(limits = c(0.5, 2.5))
  } else {
    p <- p +
      scale_x_continuous(limits = c(0, 6)) +
      scale_y_continuous(limits = c(0.5, games_per_row + 0.5))
  }

  return(p)
}
