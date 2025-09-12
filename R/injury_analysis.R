#' Enhanced NFL Injury Impact Analysis with Multiple Missing Data Strategies
#'
#' Comprehensive analysis of NFL team injury impacts that combines official injury
#' reports with roster data to calculate position-weighted injury impact scores.
#' Provides sophisticated handling of missing data through multiple strategies,
#' including performance-based proxies and zero-default approaches.
#'
#' @param start_year Integer specifying the first NFL season to analyze.
#'   Must be 2009 or later for injury data availability, though roster data
#'   extends back to 2002.
#' @param end_year Integer specifying the last NFL season to analyze.
#'   Cannot exceed current NFL season.
#' @param missing_strategy Character string specifying how to handle missing
#'   injury data. Two strategies available:
#'   \itemize{
#'     \item "zero" - Use zeros as default values for all missing injury impacts.
#'       Conservative approach that assumes no injury impact when data is unavailable.
#'     \item "weight" - Use 3-week weighted performance decline as proxy for injury
#'       impacts. More sophisticated approach that infers injury impacts from
#'       team performance patterns when official data is missing.
#'   }
#'   Default is "zero".
#'
#' @return Data frame containing comprehensive injury analysis with the following columns:
#'   \describe{
#'     \item{season}{NFL season year}
#'     \item{week}{Week number within season (1-22, including playoffs)}
#'     \item{team}{Team abbreviation (standardized to current naming)}
#'     \item{total_injury_impact}{Composite injury impact score across all positions.
#'       Higher values indicate greater injury impact. Typical range: 0-3.0}
#'     \item{qb_injury_impact}{Quarterback-specific injury impact. Range: 0-1.2}
#'     \item{skill_injury_impact}{Impact for skill positions (RB, WR, TE). Range: 0-1.0}
#'     \item{oline_injury_impact}{Offensive line injury impact (T, G, C). Range: 0-1.0}
#'     \item{defense_injury_impact}{Defensive injury impact (all defensive positions). Range: 0-1.5}
#'     \item{num_key_injuries}{Count of injuries with significant impact (≥0.30 threshold).
#'       Integer value representing number of key players affected}
#'   }
#'   The data frame contains all team-week combinations for the specified years,
#'   ensuring no missing observations that could cause joins to fail.
#'
#' @details
#' The injury analysis employs a sophisticated multi-factor scoring system:
#'
#' **Position Importance Weights**: Different positions receive different base weights
#' reflecting their impact on team performance:
#' \itemize{
#'   \item QB: 1.00 (highest impact)
#'   \item Offensive Line (T/C): 0.80-0.90 (critical for protection)
#'   \item Skill Positions (WR/RB): 0.70-0.80 (high offensive impact)
#'   \item Defensive Front: 0.70-0.80 (pressure and run stopping)
#'   \item Secondary: 0.50-0.70 (coverage impact)
#'   \item Special Teams: 0.10-0.30 (limited scope)
#' }
#'
#' **Injury Severity Multipliers**: Based on official injury report status:
#' \itemize{
#'   \item Out: 1.00 (full impact)
#'   \item Doubtful: 0.80 (high likelihood of absence/limitation)
#'   \item Questionable: 0.40 (moderate uncertainty)
#'   \item Probable: 0.10 (minimal expected impact)
#' }
#'
#' **Depth Chart Penalties**: Account for player replaceability:
#' \itemize{
#'   \item Starter (depth 1): 1.00 (full impact)
#'   \item Primary backup (depth 2): 0.60 (reduced impact)
#'   \item Deep backup (depth 3): 0.30 (minimal impact)
#'   \item Scout team/practice squad: 0.10 (negligible impact)
#' }
#'
#' **Position Group Bonuses**: Additional multipliers for critical positions:
#' \itemize{
#'   \item QB: 1.20 (elevated importance)
#'   \item Center/Tackle: 1.10 (line anchors)
#'   \item Other positions: 1.00 (base rate)
#' }
#'
#' **Final Impact Score**: 
#' \code{impact = position_weight × severity × depth_penalty × position_bonus}
#'
#' **Missing Data Strategies**:
#'
#' *Zero Strategy*: Conservative approach suitable for:
#' \itemize{
#'   \item Model training where missing data patterns are consistent
#'   \item Situations where injury impact uncertainty should be minimized
#'   \item Quick analysis without performance data dependencies
#' }
#'
#' *Weight Strategy*: Performance-based inference that:
#' \itemize{
#'   \item Calculates 3-week weighted performance trends (50%, 30%, 20% weights)
#'   \item Compares team performance to league averages
#'   \item Infers injury impacts from offensive/defensive/QB performance declines
#'   \item Provides more realistic estimates when official injury data is incomplete
#'   \item Uses prior season carryover for Week 1 estimation
#' }
#'
#' **Data Quality Considerations**:
#' \itemize{
#'   \item Only injuries with impact scores ≥0.05 are included to filter noise
#'   \item Missing position data is handled with reasonable defaults
#'   \item Team abbreviations are standardized to current naming conventions
#'   \item Complete grid ensures all team-week combinations are present
#' }
#'
#' @examples
#' \dontrun{
#' # Basic analysis with zero defaults for recent seasons
#' injury_data <- analyze_injury_impacts(2020, 2023, missing_strategy = "zero")
#' 
#' # Enhanced analysis with performance-based inference
#' injury_data <- analyze_injury_impacts(2015, 2024, missing_strategy = "weight")
#' 
#' # Historical analysis for specific period
#' injury_data <- analyze_injury_impacts(2009, 2019, missing_strategy = "zero")
#' 
#' # Analyze injury patterns
#' high_impact_weeks <- injury_data %>%
#'   filter(total_injury_impact > 1.5) %>%
#'   arrange(desc(total_injury_impact))
#' 
#' # QB injury analysis
#' qb_injuries <- injury_data %>%
#'   filter(qb_injury_impact > 0.5) %>%
#'   group_by(team, season) %>%
#'   summarise(qb_injury_weeks = n(), .groups = "drop")
#' 
#' # Team injury burden by season
#' team_burden <- injury_data %>%
#'   group_by(team, season) %>%
#'   summarise(
#'     avg_injury_impact = mean(total_injury_impact),
#'     total_key_injuries = sum(num_key_injuries),
#'     .groups = "drop"
#'   )
#' }
#'
#' @section Performance Impact:
#' When using the "weight" strategy, the function requires loading weekly performance
#' data which can be memory and time intensive for large date ranges. Consider:
#' \itemize{
#'   \item Using "zero" strategy for initial exploration
#'   \item Limiting date ranges for "weight" strategy analysis
#'   \item Saving results for reuse in multiple analyses
#' }
#'
#' @section Integration:
#' This function is designed to integrate seamlessly with \code{\link{prepare_games}}:
#' \code{
#' games <- prepare_games(
#'   years = 2020:2023,
#'   include_injuries = TRUE,
#'   injury_missing_strategy = "weight"
#' )
#' }
#'
#' @seealso
#' \code{\link{prepare_games}} for full game preparation with injury integration
#' \code{\link{nflreadr::load_injuries}} for underlying injury data
#' \code{\link{nflreadr::load_rosters_weekly}} for roster/depth chart data
#'
#' @import dplyr tidyr tibble stringr
#' @importFrom nflreadr load_injuries load_rosters_weekly
#' @export
analyze_injury_impacts <- function(start_year, end_year, missing_strategy = "zero") {
    cat("=== ENHANCED INJURY ANALYSIS ===\n")
    cat("Processing years:", start_year, "to", end_year, "\n")
    cat("Missing data strategy:", missing_strategy, "\n")
    
    # Validate strategy parameter
    if (!missing_strategy %in% c("zero", "weight")) {
        stop("missing_strategy must be 'zero' or 'weight'")
    }
    
    injury_years <- intersect(start_year:end_year, 2009:2025)
    roster_years <- intersect(start_year:end_year, 2002:2025)
    
    if (length(injury_years) == 0) {
        cat("No injury data available for", start_year, "-", end_year, "\n")
        return(data.frame())
    }
    
    cat("Loading injury data for years:", paste(injury_years, collapse = ", "), "\n")
    
    # Load data with enhanced error handling
    injuries <- tryCatch({
        nflreadr::load_injuries(injury_years)
    }, error = function(e) { 
        cat("Error loading injuries:", e$message, "\n")
        data.frame() 
    })
    
    rosters <- tryCatch({
        nflreadr::load_rosters_weekly(roster_years)
    }, error = function(e) { 
        cat("Error loading rosters:", e$message, "\n")
        data.frame() 
    })
    
    # Position importance weights
    position_weights <- tibble::tibble(
        pos = c("QB","RB","FB","WR","TE","T","G","C","DE","DT","NT",
                "OLB","ILB","MLB","LB","CB","S","FS","SS","K","P","LS"),
        weight = c(1.00,0.70,0.30,0.80,0.60,0.90,0.70,0.80,0.80,0.70,0.70,
                   0.60,0.60,0.60,0.60,0.70,0.50,0.50,0.50,0.30,0.20,0.10)
    )
    
    # Injury severity weights
    severity_weights <- tibble::tibble(
        status = c("Out","Doubtful","Questionable","Probable"),
        severity = c(1.00,0.80,0.40,0.10)
    )
    
    # Process actual injury data
    actual_injury_data <- data.frame()
    
    if (nrow(injuries) > 0) {
        cat("Processing", nrow(injuries), "injury records\n")
        
        impact <- injuries %>%
            dplyr::filter(!is.na(report_status)) %>%
            dplyr::mutate(team = map_team_abbreviation(team)) %>%
            dplyr::left_join(
                rosters %>%
                    dplyr::select(season, week, team, gsis_id, position, depth_chart_position, status) %>%
                    dplyr::rename(roster_pos = position, roster_status = status) %>%
                    dplyr::mutate(team = map_team_abbreviation(team)),
                by = c("season","week","team","gsis_id"),
                relationship = "many-to-many"  # Handle the warning
            ) %>%
            dplyr::mutate(
                pos_final = dplyr::coalesce(roster_pos, position),
                depth = dplyr::case_when(
                    !is.na(depth_chart_position) ~ as.numeric(stringr::str_extract(depth_chart_position, "\\d+")),
                    pos_final %in% c("QB", "T", "G", "C") ~ 1,
                    TRUE ~ 2
                ),
                depth = dplyr::coalesce(depth, ifelse(pos_final %in% c("QB", "T", "G", "C"), 1, 2)),
                depth_penalty = dplyr::case_when(
                    depth == 1 ~ 1.00,
                    depth == 2 ~ 0.60,
                    depth == 3 ~ 0.30,
                    TRUE ~ 0.10
                )
            ) %>%
            dplyr::left_join(position_weights, by = c("pos_final" = "pos")) %>%
            dplyr::left_join(severity_weights, by = c("report_status" = "status")) %>%
            dplyr::mutate(
                weight = dplyr::coalesce(weight, 0.50),
                severity = dplyr::coalesce(severity, 0.50),
                pos_group_penalty = dplyr::case_when(
                    pos_final == "QB" ~ 1.20,
                    pos_final %in% c("T","C") ~ 1.10,
                    TRUE ~ 1.00
                ),
                injury_impact_score = weight * severity * depth_penalty * pos_group_penalty
            ) %>%
            dplyr::filter(injury_impact_score >= 0.05)
        
        # Aggregate to team-week level
        actual_injury_data <- impact %>%
            dplyr::group_by(season, week, team) %>%
            dplyr::summarise(
                total_injury_impact = sum(injury_impact_score, na.rm = TRUE),
                num_key_injuries = sum(injury_impact_score >= 0.30),
                qb_injury_impact = sum(injury_impact_score[pos_final == "QB"], na.rm = TRUE),
                skill_injury_impact = sum(injury_impact_score[pos_final %in% c("RB","WR","TE")], na.rm = TRUE),
                oline_injury_impact = sum(injury_impact_score[pos_final %in% c("T","G","C")], na.rm = TRUE),
                defense_injury_impact = sum(injury_impact_score[pos_final %in% c("DE","DT","NT","OLB","ILB","MLB","LB","CB","S","FS","SS")], na.rm = TRUE),
                .groups = "drop"
            )
        
        cat("Created actual injury data for", nrow(actual_injury_data), "team-weeks\n")
    } else {
        cat("No actual injury data available\n")
    }
    
    # Create complete grid for all teams and weeks
    all_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
                   "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", 
                   "LV", "LAC", "LA", "MIA", "MIN", "NE", "NO", "NYG", 
                   "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")
    
    complete_grid <- tidyr::expand_grid(
        season = start_year:end_year,
        week = 1:22,
        team = all_teams
    )
    
    # Handle missing data based on strategy
    if (missing_strategy == "zero") {
        cat("Using zero defaults for missing injury data\n")
        
        final_data <- complete_grid %>%
            dplyr::left_join(actual_injury_data, by = c("season", "week", "team")) %>%
            dplyr::mutate(
                total_injury_impact = dplyr::coalesce(total_injury_impact, 0),
                qb_injury_impact = dplyr::coalesce(qb_injury_impact, 0),
                skill_injury_impact = dplyr::coalesce(skill_injury_impact, 0),
                oline_injury_impact = dplyr::coalesce(oline_injury_impact, 0),
                defense_injury_impact = dplyr::coalesce(defense_injury_impact, 0),
                num_key_injuries = dplyr::coalesce(num_key_injuries, 0L)
            )
        
    } else if (missing_strategy == "weight") {
        cat("Using 3-week performance weighting for missing injury data\n")
        
        # Load weekly performance data for weighting
        weekly_data <- tryCatch({
            prepare_weekly((start_year-1):end_year)
        }, error = function(e) {
            cat("Could not load weekly data for weighting:", e$message, "\n")
            return(data.frame())
        })
        
        if (nrow(weekly_data) > 0) {
            performance_proxy <- weekly_data %>%
                dplyr::filter(season >= start_year, season <= end_year) %>%
                dplyr::arrange(season, posteam, week) %>%
                dplyr::group_by(season, posteam) %>%
                dplyr::mutate(
                    # Weighted 3-week performance trends (more recent = higher weight)
                    recent_off_epa = (dplyr::lag(off_epa, 1) * 0.5 + 
                                          dplyr::lag(off_epa, 2) * 0.3 + 
                                          dplyr::lag(off_epa, 3) * 0.2),
                    recent_def_epa = (dplyr::lag(def_epa, 1) * 0.5 + 
                                          dplyr::lag(def_epa, 2) * 0.3 + 
                                          dplyr::lag(def_epa, 3) * 0.2),
                    recent_qb_epa = (dplyr::lag(qb_epa, 1) * 0.5 + 
                                         dplyr::lag(qb_epa, 2) * 0.3 + 
                                         dplyr::lag(qb_epa, 3) * 0.2),
                    recent_points = (dplyr::lag(points_scored, 1) * 0.5 + 
                                         dplyr::lag(points_scored, 2) * 0.3 + 
                                         dplyr::lag(points_scored, 3) * 0.2)
                ) %>%
                dplyr::ungroup()
            
            # Calculate league averages for normalization
            league_norms <- performance_proxy %>%
                dplyr::group_by(season, week) %>%
                dplyr::summarise(
                    avg_off_epa = mean(off_epa, na.rm = TRUE),
                    avg_def_epa = mean(def_epa, na.rm = TRUE),
                    avg_qb_epa = mean(qb_epa, na.rm = TRUE),
                    avg_points = mean(points_scored, na.rm = TRUE),
                    .groups = "drop"
                )
            
            # Create injury proxy based on performance decline
            performance_proxy <- performance_proxy %>%
                dplyr::left_join(league_norms, by = c("season", "week")) %>%
                dplyr::mutate(
                    # Performance decline = potential injury impact
                    off_decline = pmax(0, avg_off_epa - dplyr::coalesce(recent_off_epa, off_epa)),
                    def_decline = pmax(0, dplyr::coalesce(recent_def_epa, def_epa) - avg_def_epa),
                    qb_decline = pmax(0, avg_qb_epa - dplyr::coalesce(recent_qb_epa, qb_epa)),
                    points_decline = pmax(0, avg_points - dplyr::coalesce(recent_points, points_scored)),
                    
                    # Combine into injury impact proxy (scaled to reasonable range)
                    total_injury_impact = pmax(0, pmin(2.5,
                                                       off_decline * 0.3 + def_decline * 0.2 + qb_decline * 0.3 + 
                                                           points_decline * 0.02
                    )),
                    
                    # Component proxies
                    qb_injury_impact = pmax(0, pmin(1.0, qb_decline * 0.5)),
                    skill_injury_impact = total_injury_impact * 0.25,
                    oline_injury_impact = total_injury_impact * 0.20,
                    defense_injury_impact = total_injury_impact * 0.35,
                    num_key_injuries = pmax(0L, as.integer(round(total_injury_impact * 0.8)))
                ) %>%
                dplyr::select(season, week, posteam, 
                              total_injury_impact, qb_injury_impact, skill_injury_impact,
                              oline_injury_impact, defense_injury_impact, num_key_injuries) %>%
                dplyr::rename(team = posteam)
            
            cat("Created performance proxy for", nrow(performance_proxy), "team-weeks\n")
            
            # Combine actual data with performance proxy
            final_data <- complete_grid %>%
                dplyr::left_join(actual_injury_data, by = c("season", "week", "team")) %>%
                dplyr::left_join(performance_proxy, by = c("season", "week", "team"), 
                                 suffix = c("", "_proxy")) %>%
                dplyr::mutate(
                    # Use actual data where available, performance proxy where missing
                    total_injury_impact = dplyr::coalesce(total_injury_impact, total_injury_impact_proxy, 0),
                    qb_injury_impact = dplyr::coalesce(qb_injury_impact, qb_injury_impact_proxy, 0),
                    skill_injury_impact = dplyr::coalesce(skill_injury_impact, skill_injury_impact_proxy, 0),
                    oline_injury_impact = dplyr::coalesce(oline_injury_impact, oline_injury_impact_proxy, 0),
                    defense_injury_impact = dplyr::coalesce(defense_injury_impact, defense_injury_impact_proxy, 0),
                    num_key_injuries = dplyr::coalesce(num_key_injuries, num_key_injuries_proxy, 0L)
                ) %>%
                dplyr::select(-dplyr::ends_with("_proxy"))
            
        } else {
            cat("Warning: Could not load weekly data for weighting, falling back to zeros\n")
            final_data <- complete_grid %>%
                dplyr::left_join(actual_injury_data, by = c("season", "week", "team")) %>%
                dplyr::mutate(
                    total_injury_impact = dplyr::coalesce(total_injury_impact, 0),
                    qb_injury_impact = dplyr::coalesce(qb_injury_impact, 0),
                    skill_injury_impact = dplyr::coalesce(skill_injury_impact, 0),
                    oline_injury_impact = dplyr::coalesce(oline_injury_impact, 0),
                    defense_injury_impact = dplyr::coalesce(defense_injury_impact, 0),
                    num_key_injuries = dplyr::coalesce(num_key_injuries, 0L)
                )
        }
    }
    
    # Special handling for Week 1 based on strategy
    if (missing_strategy == "weight") {
        # Use prior season carryover for weight strategy
        week1_fixes <- final_data %>%
            dplyr::filter(week == 1) %>%
            dplyr::group_by(team) %>%
            dplyr::arrange(season) %>%
            dplyr::mutate(
                lag_total = dplyr::lag(total_injury_impact) * 0.7,
                lag_qb = dplyr::lag(qb_injury_impact) * 0.7,
                lag_skill = dplyr::lag(skill_injury_impact) * 0.7,
                lag_oline = dplyr::lag(oline_injury_impact) * 0.7,
                lag_defense = dplyr::lag(defense_injury_impact) * 0.7,
                lag_num = dplyr::lag(num_key_injuries) * 0.7
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                total_injury_impact = dplyr::case_when(
                    season == start_year ~ 0,
                    !is.na(lag_total) ~ lag_total,
                    TRUE ~ total_injury_impact * 0.8
                ),
                qb_injury_impact = dplyr::case_when(
                    season == start_year ~ 0,
                    !is.na(lag_qb) ~ lag_qb,
                    TRUE ~ qb_injury_impact * 0.8
                ),
                skill_injury_impact = dplyr::case_when(
                    season == start_year ~ 0,
                    !is.na(lag_skill) ~ lag_skill,
                    TRUE ~ skill_injury_impact * 0.8
                ),
                oline_injury_impact = dplyr::case_when(
                    season == start_year ~ 0,
                    !is.na(lag_oline) ~ lag_oline,
                    TRUE ~ oline_injury_impact * 0.8
                ),
                defense_injury_impact = dplyr::case_when(
                    season == start_year ~ 0,
                    !is.na(lag_defense) ~ lag_defense,
                    TRUE ~ defense_injury_impact * 0.8
                ),
                num_key_injuries = dplyr::case_when(
                    season == start_year ~ 0L,
                    !is.na(lag_num) ~ as.integer(lag_num),
                    TRUE ~ as.integer(num_key_injuries * 0.8)
                )
            ) %>%
            dplyr::select(-dplyr::starts_with("lag_"))
        
        # Combine Week 1 fixes with other weeks
        final_data <- final_data %>%
            dplyr::filter(week != 1) %>%
            dplyr::bind_rows(week1_fixes)
    }
    
    final_data <- final_data %>%
        dplyr::arrange(season, week, team)
    
    cat("Injury analysis complete:\n")
    cat("- Team-weeks:", nrow(final_data), "\n") 
    cat("- Seasons:", paste(range(final_data$season), collapse = "-"), "\n")
    cat("- Teams:", length(unique(final_data$team)), "\n")
    cat("- Strategy used:", missing_strategy, "\n")
    if (nrow(actual_injury_data) > 0) {
        cat("- Actual injury data coverage:", 
            round(nrow(actual_injury_data) / nrow(final_data) * 100, 1), "%\n")
    }
    
    return(final_data)
}
