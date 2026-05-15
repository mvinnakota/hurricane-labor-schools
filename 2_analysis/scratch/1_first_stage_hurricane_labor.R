# ==============================================================================
# Author:      Emileigh Harrison
# Date:        2026-04-04
# Project:     Natural Disasters, Labor Markets, and Economic Mobility
# Script:      2_analysis/1_first_stage_hurricane_labor.R
# Description: First-stage test — do hurricanes increase construction employment
#              and wages?
#
#              Treatment definitions (storm-level; ALL storms, ALL events):
#                Direct   — county directly hit by a storm (SSHS >= 0)
#                           in a given quarter/month; one cohort per storm (SID)
#                Indirect — county in the same CZ as a directly hit county for
#                           that storm, but not itself directly hit
#
#              Each storm (SID) is its own cohort. Counties can appear in
#              multiple stacks (treated in one storm, control in another).
#
#              Outcome data: BLS QCEW, private-sector construction (NAICS 23)
#                Quarterly panel → log_avg_wage, log_tot_wages
#                Monthly panel   → log_emp
#
#              Event time is in the native unit of each panel:
#                Quarterly: ±3 years = ±12 quarters
#                Monthly:   ±3 years = ±36 months
#
#              Specifications per outcome:
#                1. Event study   — i(rel_t, treated, ref=-1) | unit×storm + time×storm FEs
#                2. Stacked DiD   — treated:post | unit×storm + time×storm FEs
#              Both SE clustered at county level.
# ==============================================================================

### 0. Setup ------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = FALSE, scipen = 999)

library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
library(fixest)
library(rstudioapi)

# ── Paths ────────────────────────────────────────────────────────────────────
tryCatch(
  setwd(dirname(getActiveDocumentContext()$path)),
  error = function(e) invisible(NULL)
)
data_dir <- file.path("..", "..", "..", "Data")
fig_dir  <- file.path("..", "..", "..", "Figures")
tbl_dir  <- file.path("..", "..", "..", "Tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir, showWarnings = FALSE, recursive = TRUE)

# ── Parameters ────────────────────────────────────────────────────────────────
# SSHS scale: -1=tropical depression, 0=tropical storm, 1–5=Cat 1–5
# Set to 0 to include all named tropical storms and hurricanes
SSHS_THRESH  <-  0L

WIN_YRS      <-  3L
WIN_QTR_MIN  <- -WIN_YRS * 4L    # −12 quarters
WIN_QTR_MAX  <-  WIN_YRS * 4L    # +12 quarters
# Employment uses 4 custom seasons/year (Jun-Aug, Sep-Nov, Dec-Feb, Mar-May)
# → same count per year as quarters, so the window constant is identical
WIN_SEAS_MIN <- WIN_QTR_MIN      # −12 seasons
WIN_SEAS_MAX <- WIN_QTR_MAX      # +12 seasons

OWN_CODE_PVT <-  5L   # BLS QCEW private-sector


### 1. Load Data --------------------------------------------------------------

load(file.path(data_dir, "intermediates/texascounties_ibtracs.Rda"))
# → texas_ibtracs: sf; cols include fips, SEASON, SID, NAME, ISO_TIME,
#                       USA_SSHS, USA_WIND, cz_2000, geometry

load(file.path(data_dir, "intermediates/bls_wages_employment.Rdata"))
# → qrty_wages:       fips_state_county, year, qtr, own_code,
#                     avg_wkly_wage, total_qtrly_wages
# → mthly_employment: fips_state_county, year, qtr, calendar_month,
#                     own_code, employee_count


### 2. Build All Storm Events -------------------------------------------------
# Each unique storm (SID) that hit a county above the SSHS threshold becomes
# its own treatment cohort. Counties can be treated multiple times.

# Parse storm timestamp → quarter and month indices
hurr_raw <- texas_ibtracs %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  filter(!is.na(ISO_TIME)) %>%
  mutate(
    fips         = as.character(fips),
    hurr_dt      = as.POSIXct(ISO_TIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    hurr_yr      = as.integer(SEASON),
    hurr_mo      = as.integer(format(hurr_dt, "%m")),
    hurr_qtr     = ceiling(hurr_mo / 3L),
    # Monotonic integer index for quarters
    hurr_yrqtr   = hurr_yr * 4L  + hurr_qtr - 1L,
    # Custom season grouping (aligned to hurricane season):
    #   1 = Spring (Mar–May), 2 = Summer (Jun–Aug),
    #   3 = Fall   (Sep–Nov), 4 = Winter (Dec–Feb)
    hurr_season  = case_when(
      hurr_mo %in% 3:5         ~ 1L,
      hurr_mo %in% 6:8         ~ 2L,
      hurr_mo %in% 9:11        ~ 3L,
      hurr_mo %in% c(12L,1L,2L) ~ 4L
    ),
    # Dec stays in its own calendar year; Jan/Feb belong to the previous year's winter
    hurr_season_yr  = if_else(hurr_mo %in% c(1L, 2L), hurr_yr - 1L, hurr_yr),
    hurr_yr_season  = hurr_season_yr * 4L + hurr_season - 1L,
    above_thresh = (!is.na(USA_SSHS) & USA_SSHS >= SSHS_THRESH)
  )

# Full county universe (including never-hit counties from st_join left join)
all_counties <- texas_ibtracs %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  distinct(fips = as.character(fips), cz_2000)

# ── Direct events: one row per storm (SID) × directly hit county ──────────────
# Treatment time = earliest quarter/month the storm was in the county above threshold
storm_county_direct <- hurr_raw %>%
  filter(above_thresh) %>%
  group_by(SID, fips) %>%
  summarise(
    storm_name      = first(NAME),
    storm_season    = first(hurr_yr),
    treat_yrqtr     = min(hurr_yrqtr),
    treat_yr_season = min(hurr_yr_season),
    max_sshs        = max(USA_SSHS, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  left_join(select(all_counties, fips, cz_2000), by = "fips")

cat("\n=== Direct storm-county events ===\n")
storm_county_direct %>%
  distinct(SID, storm_name, storm_season) %>%
  arrange(storm_season, storm_name) %>%
  print(n = 60)

cat("\nDirect treated counties per storm:\n")
storm_county_direct %>% count(SID, storm_name, storm_season, name = "n_counties") %>%
  arrange(storm_season) %>% print(n = 60)

# ── Indirect events: counties in the same CZ, not themselves directly hit ─────
# CZ-level treatment time = earliest treat time among directly hit counties in that CZ
storm_county_indirect <- storm_county_direct %>%
  group_by(SID, cz_2000) %>%
  summarise(
    storm_name          = first(storm_name),
    storm_season        = first(storm_season),
    cz_treat_yrqtr      = min(treat_yrqtr),
    cz_treat_yr_season  = min(treat_yr_season),
    .groups             = "drop"
  ) %>%
  # Expand to all counties in each affected CZ
  left_join(all_counties, by = "cz_2000") %>%
  # Remove counties that were directly hit by this same storm
  anti_join(storm_county_direct %>% select(SID, fips), by = c("SID", "fips")) %>%
  filter(!is.na(fips))   # drop CZs with no county crosswalk match

cat("\nIndirect treated counties per storm (sample):\n")
storm_county_indirect %>%
  count(SID, storm_name, storm_season, name = "n_indirect") %>%
  arrange(storm_season) %>% print(n = 30)


### 3. Quarterly Wages Panel --------------------------------------------------
# Plain outcome panel — treatment info added at stacking time

wages_qtr <- qrty_wages %>%
  filter(own_code == OWN_CODE_PVT) %>%
  mutate(
    fips  = str_pad(as.character(fips_state_county), 5, pad = "0"),
    year  = as.integer(year),
    qtr   = as.integer(qtr),
    yrqtr = year * 4L + qtr - 1L
  ) %>%
  # Aggregate across any NAICS sub-industries present (guard against double-count)
  group_by(fips, year, qtr, yrqtr) %>%
  summarise(
    avg_wkly_wage     = mean(avg_wkly_wage,    na.rm = TRUE),
    total_qtrly_wages = sum(total_qtrly_wages, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_avg_wage  = log(avg_wkly_wage     + 1),
    log_tot_wages = log(total_qtrly_wages + 1)
  )

cat("\n=== Quarterly wage panel ===\n")
cat("County-quarters with data:", nrow(wages_qtr), "\n")
cat("Unique counties:", n_distinct(wages_qtr$fips), "\n")
cat("yrqtr range:", min(wages_qtr$yrqtr), "to", max(wages_qtr$yrqtr), "\n")


### 4. Seasonal Employment Panel -----------------------------------------------
# Monthly employment is collapsed to 4 custom seasons per year:
#   1 = Spring (Mar–May)   2 = Summer (Jun–Aug)
#   3 = Fall   (Sep–Nov)   4 = Winter (Dec–Feb)
# Each season has exactly 3 months; outcome is average monthly employment
# within the season. The season index (yr_season) is a monotonic integer
# analogous to yrqtr: season_yr * 4 + season - 1.
# Winter convention: Dec is assigned to its own calendar year
# (Jan/Feb of year Y belong to the Dec of year Y-1 winter group).

emp_mo <- mthly_employment %>%
  filter(own_code == OWN_CODE_PVT) %>%
  mutate(
    fips           = str_pad(as.character(fips_state_county), 5, pad = "0"),
    year           = as.integer(year),
    calendar_month = as.integer(calendar_month),
    season         = case_when(
      calendar_month %in% 3:5          ~ 1L,   # Spring
      calendar_month %in% 6:8          ~ 2L,   # Summer
      calendar_month %in% 9:11         ~ 3L,   # Fall
      calendar_month %in% c(12L,1L,2L) ~ 4L    # Winter
    ),
    season_yr      = if_else(calendar_month %in% c(1L, 2L), year - 1L, year),
    yr_season      = season_yr * 4L + season - 1L
  ) %>%
  group_by(fips, season_yr, season, yr_season) %>%
  summarise(
    employee_count = mean(employee_count, na.rm = TRUE),  # avg monthly employment within season
    .groups        = "drop"
  ) %>%
  mutate(log_emp = log(employee_count + 1))

cat("\n=== Seasonal employment panel ===\n")
cat("County-seasons with data:", nrow(emp_mo), "\n")
cat("Unique counties:", n_distinct(emp_mo$fips), "\n")
cat("yr_season range:", min(emp_mo$yr_season), "to", max(emp_mo$yr_season), "\n")


### 5. Build Stacked Datasets -------------------------------------------------
# One sub-dataset per storm (SID). For each storm:
#   treated = counties hit by that storm (direct or indirect)
#   control = counties with NO storm hit within the event window,
#             AND (for indirect) not in any CZ touched by this storm
# Counties can appear in multiple stacks.

build_stacked <- function(treat_type,          # "direct" or "indirect"
                           time_col,            # "yrqtr" or "yr_season"
                           treat_time_col,      # col in event table with treatment time
                           direct_time_col,     # col in storm_county_direct for contamination check
                           win_min, win_max) {

  # Select the right event table
  if (treat_type == "direct") {
    event_tbl <- storm_county_direct %>%
      rename(treat_t = all_of(treat_time_col))
  } else {
    event_tbl <- storm_county_indirect %>%
      rename(treat_t = all_of(treat_time_col))
  }

  storm_ids <- unique(event_tbl$SID)

  map_dfr(storm_ids, function(sid) {

    sid_events   <- event_tbl %>% filter(SID == sid)
    if (nrow(sid_events) == 0) return(NULL)

    treat_time   <- min(sid_events$treat_t)
    treated_fips <- sid_events$fips
    t_range      <- seq(treat_time + win_min, treat_time + win_max)

    # Counties contaminated by ANY direct storm hit within this event window
    contaminated <- storm_county_direct %>%
      filter(.data[[direct_time_col]] %in% t_range) %>%
      pull(fips) %>%
      unique()

    if (treat_type == "indirect") {
      # Also exclude counties that share a CZ with this storm's direct hit counties
      sid_czs         <- storm_county_direct %>% filter(SID == sid) %>% pull(cz_2000) %>% unique()
      cz_contaminated <- all_counties %>% filter(cz_2000 %in% sid_czs) %>% pull(fips) %>% unique()
      contaminated    <- unique(c(contaminated, cz_contaminated))
    }

    ctrl_fips <- setdiff(all_counties$fips, c(treated_fips, contaminated))
    if (length(ctrl_fips) == 0) return(NULL)   # skip storm if no clean controls

    # Build balanced fips × time grid for this cohort
    crossing(
      fips   = c(treated_fips, ctrl_fips),
      !!time_col := t_range
    ) %>%
      mutate(
        sid        = sid,
        treated    = fips %in% treated_fips,
        post       = .data[[time_col]] >= treat_time,
        rel_t      = as.integer(.data[[time_col]] - treat_time),
        unit_storm = paste0(fips, "_", sid),
        time_storm = paste0(.data[[time_col]], "_", sid)
      )
  })
}

cat("\nBuilding stacked datasets...\n")

stacked_qtr_d <- build_stacked("direct",   "yrqtr", "treat_yrqtr",    "treat_yrqtr",    WIN_QTR_MIN, WIN_QTR_MAX) %>%
  left_join(wages_qtr %>% select(fips, yrqtr, log_avg_wage, log_tot_wages), by = c("fips", "yrqtr")) %>%
  mutate(qoy = (yrqtr %% 4L) + 1L)   # quarter-of-year: 1 (Q1) … 4 (Q4)

stacked_qtr_i <- build_stacked("indirect", "yrqtr", "cz_treat_yrqtr", "treat_yrqtr",    WIN_QTR_MIN, WIN_QTR_MAX) %>%
  left_join(wages_qtr %>% select(fips, yrqtr, log_avg_wage, log_tot_wages), by = c("fips", "yrqtr")) %>%
  mutate(qoy = (yrqtr %% 4L) + 1L)

stacked_mo_d  <- build_stacked("direct",   "yr_season", "treat_yr_season",    "treat_yr_season", WIN_SEAS_MIN, WIN_SEAS_MAX) %>%
  left_join(emp_mo %>% select(fips, yr_season, log_emp), by = c("fips", "yr_season")) %>%
  mutate(soy = (yr_season %% 4L) + 1L)   # season-of-year: 1=Spring, 2=Summer, 3=Fall, 4=Winter

stacked_mo_i  <- build_stacked("indirect", "yr_season", "cz_treat_yr_season", "treat_yr_season", WIN_SEAS_MIN, WIN_SEAS_MAX) %>%
  left_join(emp_mo %>% select(fips, yr_season, log_emp), by = c("fips", "yr_season")) %>%
  mutate(soy = (yr_season %% 4L) + 1L)

cat("\n=== Stacked dataset sizes ===\n")
cat("Quarterly direct  :", nrow(stacked_qtr_d), "rows |",
    n_distinct(stacked_qtr_d$sid), "storms |",
    sum(stacked_qtr_d$treated & stacked_qtr_d$rel_t == 0), "treated county-quarters at t=0\n")
cat("Quarterly indirect:", nrow(stacked_qtr_i), "rows |",
    n_distinct(stacked_qtr_i$sid), "storms\n")
cat("Seasonal direct   :", nrow(stacked_mo_d),  "rows |",
    n_distinct(stacked_mo_d$sid),  "storms\n")
cat("Seasonal indirect :", nrow(stacked_mo_i),  "rows |",
    n_distinct(stacked_mo_i$sid),  "storms\n")


### 6. Event Studies ----------------------------------------------------------
# Specification:
#   outcome ~ i(rel_t, treated, ref = -1) | unit_storm + time_storm + season_fe
#
# season_fe is qoy (quarter-of-year, 1–4) for quarterly models and
#            soy (season-of-year, 1–4) for seasonal employment models.
#
# The cohort-specific time_storm FE already absorbs common shocks at each
# calendar period within a cohort. The season_fe additionally absorbs
# systematic within-year cycles that are shared across cohorts — e.g. the
# tendency for construction activity to be higher in summer regardless of
# hurricane timing. Because hurricanes almost always hit in Q3–Q4, without
# season FEs the event-study coefficients at rel_t = 0 and +1 could reflect
# seasonal patterns rather than genuine reconstruction effects.

# ── Coefficient extraction helper ────────────────────────────────────────────
extract_es_coefs <- function(mod, treat_type, outcome) {
  ct <- as.data.frame(coeftable(mod)) %>%
    rownames_to_column("term") %>%
    rename(estimate = Estimate, se = `Std. Error`)

  # Coefficients from i(rel_t, treated): terms contain "rel_t::"
  ct_es <- ct %>%
    filter(str_detect(term, "rel_t::")) %>%
    mutate(
      # Extract the event-time integer from the term name
      rel_t     = as.integer(str_match(term, "rel_t::(-?\\d+)")[, 2]),
      conf_low  = estimate - 1.96 * se,
      conf_high = estimate + 1.96 * se,
      treat_type = treat_type,
      outcome    = outcome
    ) %>%
    select(rel_t, estimate, se, conf_low, conf_high, treat_type, outcome)

  # Reference row: rel_t = -1, estimate = 0 by construction
  ref_row <- tibble(
    rel_t = -1L, estimate = 0, se = 0,
    conf_low = 0, conf_high = 0,
    treat_type = treat_type, outcome = outcome
  )

  bind_rows(ct_es, ref_row) %>% arrange(rel_t)
}

# ── Run event studies ─────────────────────────────────────────────────────────
# season_fe: name of the season fixed-effect column already in stacked_df
#            ("qoy" for quarterly models, "soy" for seasonal employment models)
run_es <- function(stacked_df, outcome, treat_type, season_fe) {
  df  <- stacked_df %>% filter(!is.na(.data[[outcome]]))
  fe  <- paste("unit_storm + time_storm", season_fe, sep = " + ")
  fml <- as.formula(paste0(outcome, " ~ i(rel_t, treated, ref = -1L) | ", fe))
  mod <- feols(fml, data = df, cluster = ~fips, warn = FALSE)
  extract_es_coefs(mod, treat_type, outcome)
}

cat("\nRunning event studies...\n")

es_avgwage_d  <- run_es(stacked_qtr_d, "log_avg_wage",  "direct",   "qoy")
es_avgwage_i  <- run_es(stacked_qtr_i, "log_avg_wage",  "indirect", "qoy")
es_totwage_d  <- run_es(stacked_qtr_d, "log_tot_wages", "direct",   "qoy")
es_totwage_i  <- run_es(stacked_qtr_i, "log_tot_wages", "indirect", "qoy")
es_emp_d      <- run_es(stacked_mo_d,  "log_emp",       "direct",   "soy")
es_emp_i      <- run_es(stacked_mo_i,  "log_emp",       "indirect", "soy")

cat("\n=== Event study preview: avg wage (direct) ===\n")
print(es_avgwage_d)


### 7. Stacked DiD ------------------------------------------------------------
# Specification: outcome ~ treated:post | unit_storm + time_storm + season_fe
# One pooled ATT estimate per model.

run_did <- function(stacked_df, outcome, season_fe) {
  df  <- stacked_df %>% filter(!is.na(.data[[outcome]]))
  fe  <- paste("unit_storm + time_storm", season_fe, sep = " + ")
  fml <- as.formula(paste0(outcome, " ~ treated:post | ", fe))
  feols(fml, data = df, cluster = ~fips, warn = FALSE)
}

cat("\nRunning stacked DiD models...\n")

did_avgwage_d  <- run_did(stacked_qtr_d, "log_avg_wage",  "qoy")
did_avgwage_i  <- run_did(stacked_qtr_i, "log_avg_wage",  "qoy")
did_totwage_d  <- run_did(stacked_qtr_d, "log_tot_wages", "qoy")
did_totwage_i  <- run_did(stacked_qtr_i, "log_tot_wages", "qoy")
did_emp_d      <- run_did(stacked_mo_d,  "log_emp",       "soy")
did_emp_i      <- run_did(stacked_mo_i,  "log_emp",       "soy")

cat("\n=== Stacked DiD: Quarterly Wages ===\n")
etable(did_avgwage_d, did_totwage_d, did_avgwage_i, did_totwage_i,
       headers = c("Avg Wage\nDirect", "Tot Wages\nDirect",
                   "Avg Wage\nIndirect", "Tot Wages\nIndirect"))

cat("\n=== Stacked DiD: Seasonal Employment ===\n")
etable(did_emp_d, did_emp_i,
       headers = c("Employment\nDirect", "Employment\nIndirect"))

# ── Save LaTeX tables ─────────────────────────────────────────────────────────
did_note_wages <- paste0(
  "Unit of observation: county \\texttimes{} quarter. ",
  "Private-sector construction (NAICS 23). ",
  "All named tropical storms and hurricanes (SSHS $\\geq$ 0). ",
  "Direct treatment: county directly hit; indirect: county in same CZ as hit county, not itself hit. ",
  "Each storm (SID) is a separate cohort; counties can appear in multiple cohort stacks. ",
  "Clean controls exclude counties with any storm hit within $\\pm", WIN_YRS, "$ years of the cohort storm. ",
  "Stacked DiD with cohort-specific unit and quarter FEs. ",
  "Window: $\\pm", WIN_YRS, "$ years ($\\pm", WIN_YRS * 4L, "$ quarters). ",
  "Standard errors clustered by county."
)
etable(
  did_avgwage_d, did_totwage_d, did_avgwage_i, did_totwage_i,
  headers = c("Avg Wage (Direct)", "Total Wages (Direct)",
              "Avg Wage (Indirect)", "Total Wages (Indirect)"),
  title   = "First Stage: Hurricane Effects on Construction Wages (Stacked DiD, Quarterly)",
  notes   = did_note_wages,
  file    = file.path(tbl_dir, "did_first_stage_wages_qtrly.tex"),
  replace = TRUE
)

did_note_emp <- paste0(
  "Unit of observation: county \\texttimes{} season. ",
  "Seasons: Spring (Mar--May), Summer (Jun--Aug), Fall (Sep--Nov), Winter (Dec--Feb); ",
  "4 seasons per year; outcome is average monthly employment within the season. ",
  "Private-sector construction (NAICS 23). ",
  "All named tropical storms and hurricanes (SSHS $\\geq$ 0). ",
  "Stacked DiD with cohort-specific unit and season FEs plus season-of-year FE. ",
  "Window: $\\pm", WIN_YRS, "$ years ($\\pm", WIN_SEAS_MAX, "$ seasons). ",
  "Standard errors clustered by county."
)
etable(
  did_emp_d, did_emp_i,
  headers = c("Employment (Direct)", "Employment (Indirect)"),
  title   = "First Stage: Hurricane Effects on Construction Employment (Stacked DiD, Seasonal)",
  notes   = did_note_emp,
  file    = file.path(tbl_dir, "did_first_stage_emp_seasonal.tex"),
  replace = TRUE
)

cat("\nDiD tables saved to:", tbl_dir, "\n")


### 8. Plotting ---------------------------------------------------------------

COL_DIRECT   <- "#D55E00"
COL_INDIRECT <- "#0072B2"

# x-axis: ticks every year, labeled as −3yr … +3yr
qtr_breaks <- seq(WIN_QTR_MIN, WIN_QTR_MAX, by = 4L)
qtr_labels <- ifelse(qtr_breaks == 0, "0\n(hit qtr)",
                     paste0(ifelse(qtr_breaks > 0, "+", ""), qtr_breaks / 4, "yr"))

# Seasonal x-axis: 4 seasons/year → ticks every 4 seasons (= 1 year), same as qtr_breaks
seas_breaks <- seq(WIN_SEAS_MIN, WIN_SEAS_MAX, by = 4L)
seas_labels <- ifelse(seas_breaks == 0, "0\n(hit season)",
                      paste0(ifelse(seas_breaks > 0, "+", ""), seas_breaks / 4, "yr"))

plot_es <- function(es_direct, es_indirect,
                    y_label, plot_title, x_label, x_breaks, x_labels) {
  bind_rows(
    es_direct   %>% mutate(treat_label = "Direct (county hit)"),
    es_indirect %>% mutate(treat_label = "Indirect (CZ neighbor hit)")
  ) %>%
    ggplot(aes(x = rel_t, y = estimate,
               color = treat_label, fill = treat_label,
               ymin = conf_low, ymax = conf_high)) +
    geom_hline(yintercept = 0,    linetype = "dashed", color = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40", linewidth = 0.5) +
    geom_ribbon(alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.5, shape = 21, stroke = 0.7, fill = "white") +
    scale_color_manual(values = c("Direct (county hit)"       = COL_DIRECT,
                                  "Indirect (CZ neighbor hit)" = COL_INDIRECT)) +
    scale_fill_manual( values = c("Direct (county hit)"       = COL_DIRECT,
                                  "Indirect (CZ neighbor hit)" = COL_INDIRECT)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    labs(
      title    = plot_title,
      subtitle = paste0(
        "i(rel_t, treated, ref=\u22121) | unit\u00d7storm + time\u00d7storm + season FEs | ",
        "SE clustered by county | 95% CI | Window: \u00b1", WIN_YRS, " years"
      ),
      x        = x_label,
      y        = y_label,
      color    = NULL, fill = NULL,
      caption  = paste0(
        "BLS QCEW, private-sector construction (NAICS 23). ",
        "All storms SSHS \u2265 0. Each storm is a separate cohort. ",
        "Controls: counties with no storm hit within \u00b1", WIN_YRS, " years of the cohort storm."
      )
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      legend.key.width = unit(1.5, "cm"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9, color = "grey40"),
      plot.caption     = element_text(size = 8, color = "grey50")
    )
}

p_avgwage <- plot_es(es_avgwage_d, es_avgwage_i,
  y_label    = "Log avg. weekly wage (construction)",
  plot_title = "First Stage: Hurricane \u2192 Construction Avg. Weekly Wage (Quarterly)",
  x_label    = paste0("Quarters relative to storm (\u00b1", WIN_YRS, " years)"),
  x_breaks   = qtr_breaks, x_labels = qtr_labels)

p_totwage <- plot_es(es_totwage_d, es_totwage_i,
  y_label    = "Log total quarterly wages (construction)",
  plot_title = "First Stage: Hurricane \u2192 Construction Total Wages (Quarterly)",
  x_label    = paste0("Quarters relative to storm (\u00b1", WIN_YRS, " years)"),
  x_breaks   = qtr_breaks, x_labels = qtr_labels)

p_emp <- plot_es(es_emp_d, es_emp_i,
  y_label    = "Log avg. monthly employment within season (construction)",
  plot_title = "First Stage: Hurricane \u2192 Construction Employment (Seasonal)",
  x_label    = paste0("Seasons relative to storm (\u00b1", WIN_YRS, " years)"),
  x_breaks   = seas_breaks, x_labels = seas_labels)

print(p_avgwage)
print(p_totwage)
print(p_emp)

ggsave(file.path(fig_dir, "es_first_stage_avg_wage.png"),
       p_avgwage, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(fig_dir, "es_first_stage_tot_wages.png"),
       p_totwage, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(fig_dir, "es_first_stage_employment.png"),
       p_emp,     width = 9, height = 5.5, dpi = 300)

cat("\nFigures saved to:", fig_dir, "\n")
cat("Done.\n")
