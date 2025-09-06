# ==================== INJURY_ANALYSIS.R ====================
# Comprehensive injury impact analysis and player availability tracking

library(dplyr)
library(stringr)
library(tidyr)
library(nflreadr)
library(purrr)
library(tibble)

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

# Starter/depth comes from rosters_weekly:
# - depth_chart_position == 1  → starter (QB1, WR1, etc.)
# - If depth_chart_position is missing, we infer a conservative default:
#     QB / OL default to 1 (starter importance), other positions default to 2.
# This is only used when team weekly rosters don't have depth explicitly.
.infer_depth <- function(position, depth_chart_position) {
  d <- suppressWarnings(as.numeric(depth_chart_position))
  if (!is.na(d)) return(d)
  if (position %in% c("QB", "T", "G", "C")) return(1)
  return(2)
}

# Position importance (impact weight baseline)
.position_weights <- tibble(
  pos = c("QB","RB","FB","WR","TE","T","G","C","DE","DT","NT",
          "OLB","ILB","MLB","LB","CB","S","FS","SS","K","P","LS"),
  w   = c(1.00,0.70,0.30,0.80,0.60,0.90,0.70,0.80,0.80,0.70,0.70,
          0.60,0.60,0.60,0.60,0.70,0.50,0.50,0.50,0.30,0.20,0.10)
)

# Injury report severity → availability/penalty proxy
.severity_weights <- tibble(
  status = c("Out","Doubtful","Questionable","Probable"),
  sev    = c(1.00,0.80,0.40,0.10)
)

# ---------------------------------------------------------------------
# 1) Team-week injury impact (for joins in prepare_games)
# ---------------------------------------------------------------------

#' Analyze injury impacts across seasons (team-week summary)
#'
#' @param start_year integer
#' @param end_year integer
#' @return data.frame team-week impacts
analyze_injury_impacts <- function(start_year, end_year) {
  
  injury_years <- intersect(start_year:end_year, 2009:2025)
  roster_years <- intersect(start_year:end_year, 2002:2025)
  
  if (length(injury_years) == 0) {
    cat("No injury data available for", start_year, "-", end_year, "\n")
    return(data.frame())
  }
  
  cat("Loading injury data for years:", paste(injury_years, collapse = ", "), "\n")
  
  injuries <- tryCatch(
    nflreadr::load_injuries(injury_years),
    error = function(e) { cat("Error loading injuries:", e$message, "\n"); data.frame() }
  )
  
  rosters <- tryCatch(
    nflreadr::load_rosters_weekly(roster_years),
    error = function(e) { cat("Error loading rosters:", e$message, "\n"); data.frame() }
  )
  
  if (nrow(injuries) == 0) {
    cat("No injury rows loaded.\n"); return(data.frame())
  }
  
  impact <- injuries %>%
    filter(!is.na(report_status)) %>%
    # attach depth/starter info where available
    left_join(
      rosters %>%
        select(season, week, team, gsis_id, position, depth_chart_position, status) %>%
        rename(roster_pos = position, roster_status = status),
      by = c("season","week","team","gsis_id")
    ) %>%
    mutate(
      pos_final = coalesce(roster_pos, position),
      depth = map2_dbl(pos_final, depth_chart_position, .infer_depth),
      depth_pen = case_when(
        depth == 1 ~ 1.00,  # starter
        depth == 2 ~ 0.60,  # key backup
        depth == 3 ~ 0.30,
        TRUE      ~ 0.10
      )
    ) %>%
    left_join(.position_weights, by = c("pos_final" = "pos")) %>%
    left_join(.severity_weights, by = c("report_status" = "status")) %>%
    mutate(
      w   = coalesce(w,   0.50),
      sev = coalesce(sev, 0.50),
      pos_group_pen = case_when(
        pos_final == "QB"           ~ 1.20,  # QB extra critical
        pos_final %in% c("T","C")   ~ 1.10,  # OL anchor
        TRUE                        ~ 1.00
      ),
      # final impact score per player-week
      injury_impact_score = w * sev * depth_pen * pos_group_pen
    ) %>%
    filter(injury_impact_score >= 0.05)
  
  team_week <- impact %>%
    group_by(season, week, team) %>%
    summarise(
      total_injury_impact  = sum(injury_impact_score, na.rm = TRUE),
      num_key_injuries     = sum(injury_impact_score >= 0.30),
      qb_injury_impact     = sum(injury_impact_score[pos_final == "QB"], na.rm = TRUE),
      skill_injury_impact  = sum(injury_impact_score[pos_final %in% c("RB","WR","TE")], na.rm = TRUE),
      oline_injury_impact  = sum(injury_impact_score[pos_final %in% c("T","G","C")], na.rm = TRUE),
      defense_injury_impact= sum(injury_impact_score[pos_final %in% c("DE","DT","NT","OLB","ILB","MLB","LB","CB","S","FS","SS")], na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Injury analysis complete:\n")
  cat("- Team-weeks:", nrow(team_week), "\n")
  cat("- Seasons:", paste(range(team_week$season), collapse = "-"), "\n")
  
  team_week
}

# ---------------------------------------------------------------------
# 2) How injuries correlate with outcomes
# ---------------------------------------------------------------------

#' Analyze injury impact on game outcomes
#'
#' @param injury_data team-week summary from analyze_injury_impacts
#' @param game_results your prepared games data (needs home/away teams, spreads, point_differential)
#' @return summary frame
analyze_injury_game_impact <- function(injury_data, game_results) {
  
  cat("Analyzing injury impact on game outcomes...\n")
  
  g <- game_results %>%
    left_join(
      injury_data %>%
        select(season, week, team, total_injury_impact, qb_injury_impact,
               skill_injury_impact, oline_injury_impact, defense_injury_impact) %>%
        rename_with(~paste0("home_", .), -c(season, week, team)),
      by = c("season","week","home_team" = "team")
    ) %>%
    left_join(
      injury_data %>%
        select(season, week, team, total_injury_impact, qb_injury_impact,
               skill_injury_impact, oline_injury_impact, defense_injury_impact) %>%
        rename_with(~paste0("away_", .), -c(season, week, team)),
      by = c("season","week","away_team" = "team")
    ) %>%
    mutate(
      injury_advantage = coalesce(away_total_injury_impact, 0) - coalesce(home_total_injury_impact, 0),
      qb_injury_advantage = coalesce(away_qb_injury_impact, 0) - coalesce(home_qb_injury_impact, 0),
      injury_situation = case_when(
        abs(injury_advantage) <= 0.5 ~ "even_health",
        injury_advantage >  1.5      ~ "home_health_advantage",
        injury_advantage < -1.5      ~ "away_health_advantage",
        injury_advantage >  0.5      ~ "home_slight_advantage",
        TRUE                         ~ "away_slight_advantage"
      )
    )
  
  summary <- g %>%
    filter(!is.na(point_differential), !is.na(injury_situation)) %>%
    group_by(injury_situation) %>%
    summarise(
      games                 = n(),
      home_win_rate         = mean(point_differential > 0),
      avg_point_differential= mean(point_differential),
      avg_total_points      = mean(total, na.rm = TRUE),
      home_cover_rate       = mean(point_differential > away.spread_line, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Done.\n")
  summary
}

# ---------------------------------------------------------------------
# 3) Convenience: team week report
# ---------------------------------------------------------------------

#' Get injury report for a specific team & week
get_team_injury_report <- function(team_abbr, season, week, injury_data) {
  row <- injury_data %>% filter(team == team_abbr, season == !!season, week == !!week)
  if (nrow(row) == 0) {
    return(paste("No significant injuries reported for", team_abbr, "in", season, "week", week))
  }
  row <- row[1,]
  
  list(
    team = team_abbr,
    season = season,
    week = week,
    overall_impact = round(row$total_injury_impact, 2),
    impact_level = dplyr::case_when(
      row$total_injury_impact >= 2.0 ~ "Critical",
      row$total_injury_impact >= 1.0 ~ "Major",
      row$total_injury_impact >= 0.5 ~ "Moderate",
      TRUE ~ "Minor"
    ),
    key_areas_affected = dplyr::case_when(
      row$qb_injury_impact >= 0.50   ~ "Quarterback concerns",
      row$oline_injury_impact >= 1.0 ~ "Offensive line depleted",
      row$skill_injury_impact >= 1.0 ~ "Skill positions impacted",
      row$defense_injury_impact >= 1.0 ~ "Defense injuries",
      TRUE ~ "Minor injury concerns"
    )
  )
}

# ---------------------------------------------------------------------
# 4) Injury-adjusted team strength example
# ---------------------------------------------------------------------

#' Apply a simple injury adjustment to team strength stats
#' (You can customize coefficients per your calibration.)
calculate_injury_adjusted_strength <- function(team_stats, injury_impacts) {
  team_stats %>%
    left_join(injury_impacts, by = c("season", "week", "team")) %>%
    mutate(
      adj_off_epa = off_epa_cum * (1 - (coalesce(qb_injury_impact,0) * 0.30 +
                                          coalesce(skill_injury_impact,0) * 0.20 +
                                          coalesce(oline_injury_impact,0) * 0.25)),
      adj_def_epa = def_epa_cum * (1 - (coalesce(defense_injury_impact,0) * 0.20)),
      injury_adjustment_factor = 1 - (coalesce(total_injury_impact,0) * 0.15),
      adj_team_strength = (adj_off_epa - adj_def_epa) * injury_adjustment_factor
    )
}
