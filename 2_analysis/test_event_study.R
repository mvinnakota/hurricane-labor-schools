# ==============================================================================
# Author:      Emileigh Harrison
# Date:        2026-04-04
# Project:     Natural Disasters, Labor Markets, and Economic Mobility
# Script:      2_analysis/first_stage_hurricane_labor.R
# Description: First-stage test — does getting hit by a hurricane increase the
#              total wages spent on construction labor?
#
#              Design: stacked difference-in-differences. Each storm (sid) is its
#              own cohort; counties can appear in multiple stacks. Treatment is
#              already encoded in county_did (direct / indirect), switched on only
#              in the post period. Identification:
#                outcome ~ direct + indirect | unit_storm + time_storm
#              with SEs clustered by county.
# ==============================================================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
source("../0_helper_functions/graph_themes.R")
setwd("../../../")

library(fixest)
library(tibble)
library(fastDummies)


# =============================================================================
# Functions
# =============================================================================
## Stacked difference-in-differences -------------------------------------------
Diff_in_Diff <- function(dep_var   = "yearly_wages",
                         treat_vars = c("direct", "indirect"),
                         controls   = NULL,
                         fixed_fx   = c("event_time^sid", "tea_school_id^sid"),
                         df         = test_data,
                         unit_var = "tea_school_id"){
  # Get total number of event years
  total_years <- n_distinct(data$event_time)
  
  # dropping missingness in the dependent variable
  data %<>% subset(!is.na(get(dep_var)))
  
  # make sure panel is balanced by dropping schools that cannot
  # be observed in all years
  data %<>%
    group_by_at(c("sid", unit_var)) %>%
    mutate(n_years = n_distinct(event_time)) %>%
    subset(n_years == total_years) %>%
    ungroup()
  
  # Build the estimating formula: outcome ~ treatment + controls 
  rhs  <- paste0("`", c(treat_vars, controls), "`")
  fmla <- paste(dep_var, "~", paste(rhs, collapse = " + "))
  # Add fixed effects
  fmla %<>% paste(" | ", paste(fixed_fx, collapse = " + ")) 
  # Convert from string to formula
  fmla %<>% as.formula()
  # Cluster on school year
  model <- feols(fmla, df, cluster = paste0(unit_var,"^sid"))
  return(model)
}


# =============================================================================
#  STACKED EVENT STUDY (Wing / Cengiz et al.): explicit dummies + stack FEs
# =============================================================================
# Builds the treated x event-time dummies as columns and passes them into the
# formula (rather than i()). Design follows the stacked event-study slides:
#   * unit x sub-experiment FE  = tea_school_id^sid
#   * time x sub-experiment FE  = panel_year^sid
#   * SEs clustered at the SCHOOL level (tea_school_id) -- the level at which
#     treatment is assigned -- even though the data are student-level. This
#     also absorbs the stacked-design duplication of a school across stacks.
Event_Study <- function(dep_var  = "yearly_wages",
                              controls     = NULL,
                              treat_vars = c("ever_direct", "ever_indirect"),
                              fixed_fx     = c("tea_school_id^sid", "event_time^sid"),
                              df           = test_data){

  # Drop missing outcomes
  df %<>% subset(!is.na(get(dep_var)))
  

  # --- Build event-time dummies 
  df %<>% dummy_cols(select_columns = "event_time", remove_selected_columns = FALSE)
  event_time_cols <- grep("^event_time_", names(df), value = TRUE)
  event_time_cols %<>% setdiff(paste0("event_time_-1")) # omit reference period

  
  # --- Build event-time x treatment dummies 
  # this code interacts each of the treatment variables with each of the 
  # event time dummy variables
  treat_var_dummies <- c()
  # for each event time dummy
  for(col in event_time_cols){
    # for each treatment variable
    for(var in treat_vars){
      # Name a new column
    dn <- paste0(var, "_", col)
    # interact treatment and event time dummy
    df[[dn]] <- df[[col]] * df[[var]] 
    # add name to the list of treatment event time dummies
    treat_var_dummies %<>% c(dn)
    }}

  
  # --- Estimate --------------------------------------------------------------
  model <- Diff_in_Diff(dep_var, treat_vars = treat_var_dummies, controls = controls,
                        fixed_fx = fixed_fx, df = df)

  # --- Extract event-study coefficients --------------------------------------
  model$data <- summary(model)$coeftable %>% data.frame() %>% clean_names()
  
  # --- Subset to only treatment coefficients
  model$data %<>% 
    rownames_to_column(var = "treatment_event") %>%
    # remove quotations from variable names
    mutate(treatment_event = str_remove_all(treatment_event,"\\`")) %>%
    filter(str_detect(treatment_event, paste0("^",treat_var_dummies, collapse = "|")))
  
  # --- Clean labels 
  model$data %<>% 
    # extract event time and treatment type
    mutate(
      event_time     = as.integer(str_extract(treatment_event, "-?\\d+")),
      treatment_type = ifelse(str_detect(treatment_event, "indirect"), "Indirect", "Direct")
    )

  # Create zeros for reference period (t = -1, coef = 0)
  ref_rows <- data.frame(
    treatment_event = c("dir_ref", "ind_ref"),
    estimate = 0, std_error = 0, t_value = 0, pr_t = 0,
    treatment_type = c("Direct", "Indirect"), event_time = -1)

  model$data %<>% bind_rows(ref_rows) %>% arrange(treatment_type, event_time)

  # Pointwise 95% intervals from the clustered SEs
  model$data %<>% mutate(
    low = estimate - qnorm(1 - 0.05/2) * std_error,
    high = estimate + qnorm(1 - 0.05/2) * std_error
  )

return(model)
}


Plot_Event_Study <- function(model){
  model$data %>%
    ggplot(aes(x = event_time, y = estimate, color = treatment_type)) +
    geom_hline(yintercept = 0,    linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.1, position = position_dodge(width = 0.3)) +
    geom_line(position = position_dodge(width = 0.3)) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    scale_x_continuous(breaks = model$data$event_time) +
    scale_color_manual(values = point_colors) +
    xlab("Years since storm") + ylab("Coefficient") +
    facet_grid(cols = vars(treatment_type)) +
    theme(legend.position = "none") 
}

# =============================================================================
# LOAD DATA
# =============================================================================
load("intermediates/school_storm_unique.Rda")
source("Code/hurricane-labor-schools/1_data_prep/5_build_stacked_did_panel.R")


### Build the stacked panel -----------------------------------------------------
# Treatment is defined inside Build_Panel():
#   direct   = 50-kt wind exposure (wind_50kt)
#   indirect = commuting-zone (cz_2000) spillover from a directly-hit school

stacked_did <- Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_64kt",
  indirect_geo="cz",
  pre_years=4, post_years=3,
  never_treated=T,
  years_since=7,
  donut="wind_50kt",
  radius_miles=10000,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1997:2017)
  )

# =============================================================================
#  WIND EVENT STUDY
# =============================================================================
# Create Fake Data
schools <- stacked_did %>% pull(tea_school_id) %>% unique()
test_data <- data.frame(
  "tea_school_id" = rep(rep(schools, 27) %>% sort(), 10),
  "cohort_year" = rep(rep(c(1994:2020), length(schools)), 10),
  "yearly_wages" = rnorm(length(schools)*270),
  "student_id" = c(1:(length(schools)*270))
)

test_data <- stacked_did %>% 
  left_join(test_data, by=c("tea_school_id", "panel_year"="cohort_year")) %>%
  select("student_id","sid","tea_school_id","storm_name","panel_year",
         "event_time","direct","indirect","ever_direct","ever_indirect","yearly_wages")


stacked_did %>%  
  select(tea_school_id, storm_name, sid, ever_direct, ever_indirect) %>%
  distinct() %>%
  group_by(storm_name, sid) %>%
  summarize(n_direct = sum(ever_direct), n_indirect = sum(ever_indirect))
  
  

# =============================================================================
#  WIND EVENT STUDY
# =============================================================================
### Event studies ---------------------------------------------------------------
Diff_in_Diff(dep_var = "yearly_wages")
Event_Study(dep_var = "yearly_wages") %>% Plot_Event_Study()




