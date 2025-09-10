#' Process NFL Weekly Data
#'
#' @param years Numeric vector of years to process
#' @return A data frame of processed weekly NFL data
#' @import dplyr
#' @importFrom nflfastR load_pbp
#' @importFrom purrr map_df
#' @importFrom stringr str_extract
#' @importFrom tidyr fill
#' @importFrom zoo rollmean
#' @importFrom dplyr filter bind_rows arrange %>%
#' @importFrom data.table :=
#' @examples
#' \dontrun{
#' weekly_data <- prepare_weekly(2024)
#' }
#' @export
prepare_weekly <- function(years) {
    # Load play-by-play data for the specified years
    pbp_data <- map_df(years, function(year) {
      suppressMessages(nflfastR::load_pbp(year))
    })
    # Filter out plays without a valid posteam or defteam and map legacy abbreviations
    pbp_data <- pbp_data %>%
      filter(!is.na(posteam) & !is.na(defteam)) %>%
      mutate(
        posteam = map_team_abbreviation(posteam),
        defteam = map_team_abbreviation(defteam)

      )

    first_appearances <- pbp_data %>%
      group_by(game_id) %>%
      filter(!is.na(game_id)) %>%
      slice(which.min(week)) %>%
      select(game_id, correct_week = week)


    pbp_data <- pbp_data %>%
      left_join(first_appearances, by = "game_id") %>%
      mutate(week = if_else(!is.na(correct_week), correct_week, week)) %>%
      select(-correct_week) %>%
      # Remove duplicates after fixing the week
      distinct()

    # Calculate QB snaps
    qb_snaps <- pbp_data %>%
      filter(!is.na(passer_player_id)) %>%
      group_by(season, week, game_id, posteam, passer_player_id) %>%
      summarize(snaps = n(), .groups = "drop") %>%
      arrange(season, week, game_id, posteam, desc(snaps))

    # Identify the primary QB (majority snaps) and season starter for each team in each week
    primary_qbs <- qb_snaps %>%
      group_by(season, posteam) %>%
      mutate(
        cumulative_snaps = cumsum(snaps),
        total_snaps = sum(snaps),
        primary_qb_majority = passer_player_id == passer_player_id[which.max(cumulative_snaps)],
        season_starter = passer_player_id == first(passer_player_id)
      ) %>%
      ungroup()

    # Calculate the ratio of snaps taken by the primary QB (majority) and season starter
    qb_snap_ratio <- primary_qbs %>%
      group_by(season, week, game_id, posteam) %>%
      summarize(
        primary_qb_majority_snaps = sum(snaps[primary_qb_majority]),
        season_starter_snaps = sum(snaps[season_starter]),
        total_snaps = sum(snaps),
        primary_qb_majority_ratio = primary_qb_majority_snaps / total_snaps,
        season_starter_ratio = season_starter_snaps / total_snaps,
        .groups = "drop"
      )


    # Calculate offensive stats for each team
    off_data <- pbp_data %>%
      group_by(season, week, game_id, posteam, posteam_type) %>%
      summarize(
        points_scored = max(posteam_score, na.rm = TRUE),
        off_epa = sum(epa, na.rm = TRUE),
        pass_epa = sum(epa[pass_attempt == 1], na.rm = TRUE),
        rush_epa = sum(epa[rush_attempt == 1], na.rm = TRUE),
        turnovers = sum(fumble_lost, na.rm = TRUE) + sum(interception, na.rm = TRUE),
        off_plays = n(),
        off_drives = n_distinct(fixed_drive, na.rm = TRUE),
        off_scores = sum((touchdown == 1) | (field_goal_attempt == 1 & field_goal_result == "made"), na.rm = TRUE),
        qb_completions = sum(complete_pass, na.rm = TRUE),
        qb_attempts = sum(pass_attempt, na.rm = TRUE),
        qb_passing_yards = sum(passing_yards, na.rm = TRUE),
        qb_passing_tds = sum(pass_touchdown, na.rm = TRUE),
        qb_interceptions = sum(interception, na.rm = TRUE),
        qb_rushing_attempts = sum(qb_scramble, na.rm = TRUE),
        qb_rushing_yards = sum(rushing_yards[qb_scramble == 1], na.rm = TRUE),
        qb_rushing_tds = sum(rush_touchdown[qb_scramble == 1], na.rm = TRUE),
        qb_epa = sum(qb_epa, na.rm = TRUE),
        qb_cpoe = mean(cpoe, na.rm = TRUE),
        qb_air_yards = sum(air_yards, na.rm = TRUE),
        qb_yac = sum(yards_after_catch, na.rm = TRUE),
        qb_first_downs = sum(first_down_pass, na.rm = TRUE),
        qb_dropbacks = sum(qb_dropback, na.rm = TRUE),
        qb_hits_taken = sum(qb_hit, na.rm = TRUE),
        qb_hurries_taken = sum(qb_hit[!sack], na.rm = TRUE),
        qb_pressures_taken = sum(qb_hit | sack, na.rm = TRUE),
        qb_sacks_taken = sum(sack, na.rm = TRUE),
        qb_sack_yards_taken = sum(yards_gained[sack == 1], na.rm = TRUE),
        qb_deep_passes = sum(pass_attempt & air_yards >= 20, na.rm = TRUE),
        qb_deep_completions = sum(complete_pass & air_yards >= 20, na.rm = TRUE),
        # Add penalty stats
        penalties_for = sum(penalty == 1 & penalty_team == posteam, na.rm = TRUE),
        penalty_yards_for = sum(penalty_yards[penalty == 1 & penalty_team == posteam], na.rm = TRUE),
        penalties_against = sum(penalty == 1 & penalty_team == defteam, na.rm = TRUE),
        penalty_yards_against = sum(penalty_yards[penalty == 1 & penalty_team == defteam], na.rm = TRUE),

        # Penalties by down
        penalties_1st_down = sum(penalty == 1 & down == 1, na.rm = TRUE),
        penalties_2nd_down = sum(penalty == 1 & down == 2, na.rm = TRUE),
        penalties_3rd_down = sum(penalty == 1 & down == 3, na.rm = TRUE),
        penalties_4th_down = sum(penalty == 1 & down == 4, na.rm = TRUE),

        # Conversion rates
        third_down_attempts = sum(down == 3, na.rm = TRUE),
        third_down_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        fourth_down_attempts = sum(down == 4 & !is.na(play_type), na.rm = TRUE),
        fourth_down_conversions = sum(down == 4 & first_down == 1, na.rm = TRUE),
        red_zone_attempts = sum(yardline_100 <= 20 & !is.na(play_type), na.rm = TRUE),
        red_zone_tds = sum(yardline_100 <= 20 & touchdown == 1, na.rm = TRUE),
        spread_line = first(spread_line),
        total_line = first(total_line),
        div_game = first(div_game),
        wp = mean(wp, na.rm = TRUE),
        wpa = sum(wpa, na.rm = TRUE),
        vegas_wpa = sum(vegas_wpa, na.rm = TRUE),
        vegas_home_wpa = sum(vegas_home_wpa, na.rm = TRUE),

        .groups = "drop"
      )

    off_data <- off_data %>%
      left_join(qb_snap_ratio, by = c("season", "week", "game_id", "posteam"))

    # Calculate defensive stats for each team
    def_data <- pbp_data %>%
      group_by(season, week, game_id, defteam) %>%
      summarize(
        points_allowed = max(posteam_score, na.rm = TRUE),
        def_epa = sum(epa, na.rm = TRUE),
        def_plays = n(),
        def.pass_epa = sum(epa[pass_attempt == 1], na.rm = TRUE),
        def.rush_epa = sum(epa[rush_attempt == 1], na.rm = TRUE),
        def.turnovers = sum(fumble_lost, na.rm = TRUE) + sum(interception, na.rm = TRUE),
        def_drives = n_distinct(fixed_drive, na.rm = TRUE),
        def_scores = sum((touchdown == 1) | (field_goal_attempt == 1 & field_goal_result == "made"), na.rm = TRUE),
        def_penalties_for = sum(penalty == 1 & penalty_team == defteam, na.rm = TRUE),
        def_penalty_yards_for = sum(penalty_yards[penalty == 1 & penalty_team == defteam], na.rm = TRUE),
        def_penalties_against = sum(penalty == 1 & penalty_team == posteam, na.rm = TRUE),
        def_penalty_yards_against = sum(penalty_yards[penalty == 1 & penalty_team == posteam], na.rm = TRUE),
        qb_hits_caused = sum(qb_hit, na.rm = TRUE),
        qb_hurries_caused = sum(qb_hit[!sack], na.rm = TRUE),
        qb_pressures_caused = sum(qb_hit | sack, na.rm = TRUE),
        qb_sacks_caused = sum(sack, na.rm = TRUE),
        def_qb_completions = sum(complete_pass, na.rm = TRUE),
        def_qb_attempts = sum(pass_attempt, na.rm = TRUE),
        def_qb_passing_yards = sum(passing_yards, na.rm = TRUE),
        def_qb_passing_tds = sum(pass_touchdown, na.rm = TRUE),
        def_qb_interceptions = sum(interception, na.rm = TRUE),
        def_qb_rushing_attempts = sum(qb_scramble, na.rm = TRUE),
        def_qb_rushing_yards = sum(rushing_yards[qb_scramble == 1], na.rm = TRUE),
        def_qb_rushing_tds = sum(rush_touchdown[qb_scramble == 1], na.rm = TRUE),
        def_qb_epa = sum(qb_epa, na.rm = TRUE),
        def_qb_cpoe = mean(cpoe, na.rm = TRUE),
        # Penalties by down
        def_penalties_1st_down = sum(penalty == 1 & down == 1 & penalty_team == posteam, na.rm = TRUE),
        def_penalties_2nd_down = sum(penalty == 1 & down == 2 & penalty_team == posteam, na.rm = TRUE),
        def_penalties_3rd_down = sum(penalty == 1 & down == 3 & penalty_team == posteam, na.rm = TRUE),
        def_penalties_4th_down = sum(penalty == 1 & down == 4 & penalty_team == posteam, na.rm = TRUE),

        # Defensive conversion rates
        def_third_down_attempts = sum(down == 3, na.rm = TRUE),
        def_third_down_conversions = sum(down == 3 & first_down == 1, na.rm = TRUE),
        def_fourth_down_attempts = sum(down == 4 & !is.na(play_type), na.rm = TRUE),
        def_fourth_down_conversions = sum(down == 4 & first_down == 1, na.rm = TRUE),
        def_red_zone_attempts = sum(yardline_100 <= 20 & !is.na(play_type), na.rm = TRUE),
        def_red_zone_tds = sum(yardline_100 <= 20 & touchdown == 1, na.rm = TRUE),
        def_wp = mean(1 - wp, na.rm = TRUE),
        def_wpa = sum(wpa, na.rm = TRUE),
        def_vegas_wpa = sum(vegas_wpa, na.rm = TRUE),
        # Other relevant defensive statistics can be added here
        .groups = "drop"
      )

    # Merge offensive and defensive stats
    # Combine offensive and defensive stats into game_data
    game_data <- off_data %>%
      left_join(def_data, by = c("season", "week", "game_id", "posteam" = "defteam")) %>%
      mutate(
        # Offensive metrics
        points_per_drive = points_scored / off_drives,
        points_allowed_per_drive = points_allowed / def_drives,
        points_per_play = points_scored / off_plays,
        points_allowed_per_play = points_allowed / def_plays,
        qb_completion_percentage = qb_completions / qb_attempts,
        qb_yards_per_attempt = qb_passing_yards / qb_attempts,
        qb_td_percentage = qb_passing_tds / qb_attempts,
        qb_int_percentage = qb_interceptions / qb_attempts,
        qb_pressure_percentage_taken = qb_pressures_taken / qb_dropbacks,
        qb_sack_percentage_taken = qb_sacks_taken / qb_dropbacks,
        qb_hurry_percentage_taken = qb_hurries_taken / qb_dropbacks,

        # QB pressure percentages for defense
        qb_pressure_percentage_caused = qb_pressures_caused / def_plays,
        qb_sack_percentage_caused = qb_sacks_caused / def_plays,
        qb_hurry_percentage_caused = qb_hurries_caused / def_plays,
        qb_deep_percentage = qb_deep_passes / qb_attempts,
        qb_deep_completion_percentage = qb_deep_completions / qb_deep_passes,
        qb_air_yards_per_attempt = qb_air_yards / qb_attempts,
        qb_yac_per_completion = qb_yac / qb_completions,
        qb_first_down_percentage = qb_first_downs / qb_attempts,
        qb_passer_rating = (
          ((qb_completions / qb_attempts - 0.3) * 5 +
             (qb_passing_yards / qb_attempts - 3) * 0.25 +
             (qb_passing_tds / qb_attempts) * 20 +
             2.375 - (qb_interceptions / qb_attempts * 25)) / 6
        ) * 100,
        qb_approx_qbr = (qb_epa / qb_dropbacks + 5) * 10,
        qb_consistency_majority = 1 - (1 - primary_qb_majority_ratio) * (week / if_else(season >= 2021, 18, 17)),
        qb_consistency_starter = 1 - (1 - season_starter_ratio) * (week / if_else(season >= 2021, 18, 17)),
        third_down_conversion_rate = third_down_conversions / third_down_attempts,
        fourth_down_conversion_rate = fourth_down_conversions / fourth_down_attempts,
        red_zone_conversion_rate = red_zone_tds / red_zone_attempts,

        # Defensive metrics
        points_allowed_per_drive = points_allowed / def_drives,
        def_third_down_conversion_rate = def_third_down_conversions / def_third_down_attempts,
        def_fourth_down_conversion_rate = def_fourth_down_conversions / def_fourth_down_attempts,
        def_red_zone_conversion_rate = def_red_zone_tds / def_red_zone_attempts,

        # Penalty rate calculations
        penalty_rate_for = penalties_for / off_plays,
        penalty_rate_against = penalties_against / def_plays,
        penalty_yards_per_play_for = penalty_yards_for / off_plays,
        penalty_yards_per_play_against = penalty_yards_against / def_plays,

        # Additional defensive metrics
        def_points_per_drive = points_allowed / def_drives,
        def_points_per_play = points_allowed / def_plays,
        def_qb_completion_percentage = (def_qb_completions / def_qb_attempts),  # Assuming you have these metrics in def_data
        def_qb_yards_per_attempt = (def_qb_passing_yards / def_qb_attempts),  # Assuming you have these metrics in def_data
        def_qb_td_percentage = (def_qb_passing_tds / def_qb_attempts),  # Assuming you have these metrics in def_data
        def_qb_int_percentage = (def_qb_interceptions / def_qb_attempts),  # Assuming you have these metrics in def_data

        home_wp = if_else(posteam == str_extract(game_id, "[A-Z]+$"), wp, def_wp),
        away_wp = if_else(posteam == str_extract(game_id, "(?<=)[A-Z]+(?=_[A-Z]+$)"), wp, def_wp)# Assuming you have these metrics in def_data
      )


    weekly_data <- game_data %>%
      group_by(season, posteam) %>%
      arrange(season, week) %>%
      mutate(across(all_of(unique(METRICS)),
                    list(
                      cum = cumsum
                    ))) %>%
      ungroup()

    weekly_data <- fix_all_cumulative_stats(weekly_data)

        # Fill missing values within groups
    weekly_data_filled <- weekly_data %>%
      group_by(season, week) %>%
      mutate(across(
        where(is.numeric),
        ~if(all(is.na(.))) NA else coalesce(., mean(., na.rm = TRUE))
      )) %>%
      ungroup()

    return(weekly_data_filled)
  }
