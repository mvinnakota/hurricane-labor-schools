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


# =============================================================================
# Functions
# =============================================================================
## Stacked difference-in-differences -------------------------------------------
Diff_in_Diff <- function(dep_var   = "total_yearly_wages",
                         treat_vars = c("direct", "indirect"),
                         ind_vars   = NULL,
                         fixed_fx   = c("panel_year", "county_fips",
                                        "ever_direct", "ever_indirect"),
                         df         = county_did){
  
  # Cluster variable and cohort-specific fixed-effect keys
  fmla <- paste(dep_var, "~", paste(c(treat_vars, ind_vars), collapse = " + "))
  
  if(!is.null(fixed_fx)){ fmla %<>% paste(" | ", paste(fixed_fx, collapse = " + ")) }
  
  fmla %<>% as.formula()
  
  model <- feols(fmla, df, cluster = fixed_fx)
  return(model)
}

Plot_Event_Study <- function(dep_var = "total_yearly_wages",
                             ind_vars         = NULL,
                             fixed_fx         = c("panel_year", "county_fips"),
                             point_colors      = c("#D55E00","goldenrod"),
                             df               = county_did){
  
  # Drop missing observations in the dependent variable
  df %<>% subset(!is.na(get(dep_var)))
  
  # Treatment x event-time dummies, with event time -1 as the reference period.
  # ever_treated * event_time leaves never-treated (control) rows at 0, and folds
  # the hit period (event_time 0) into the baseline, exactly as in the template.
  df %<>% mutate(
    event_time   = ifelse(ever_direct == 1 | ever_indirect == 1, event_time, 0),
    D            = factor(ever_direct * event_time),
    D            = relevel(D, ref = "-1"),
    I            = factor(ever_indirect * event_time),
    I            = relevel(I, ref = "-1"))
  
  # Run the regression of the form outcome ~ D | FEs
  model <- Diff_in_Diff(dep_var, treat_vars = c("D","I"), ind_vars, fixed_fx, df)
  
  # Extract the D (treatment x event-time) coefficients.
  # summary() returns the SEs clustered by county (set in Diff_in_Diff at
  # estimation); do not pass se= here or it would re-cluster on the first FE.
  coef <- summary(model)$coeftable %>%
    data.frame() %>%
    rownames_to_column(var = "treatment_event") %>%
    filter(str_detect(treatment_event, "^D|^I"))
  
  # Add a blank row for the omitted reference period (event time -1, coef = 0)
  minus_one <- data.frame(treatment_event = c("D-1", "I-1"),
                          Estimate = 0, Std..Error = 0, t.value = 0, Pr...t.. = 0)
  coef <- bind_rows(coef, minus_one)
  
  # Recover numeric event time from the term name and order on it
  coef %<>%
    mutate(event_time = as.integer(str_extract(treatment_event, "-?\\d+$"))) %>%
    arrange(event_time)
  
  # Get Dummy for treatment type
  coef %<>%
    mutate(treatment_type = ifelse(grepl("D", treatment_event), "Direct", "Indirect"))
  
  # Pointwise 95% intervals from the clustered SEs
  # (swap Std..Error for another vcov column to use a different variance estimator)
  coef$low  <- coef$Estimate - qnorm(1 - 0.05/2) * coef$Std..Error
  coef$high <- coef$Estimate + qnorm(1 - 0.05/2) * coef$Std..Error
  
  coef %>%
    ggplot(aes(x = event_time, y = Estimate, color=treatment_type)) +
    geom_hline(yintercept = 0,    linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.1, position = position_dodge(width = 0.3)) +
    geom_line(position = position_dodge(width = 0.3)) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    scale_x_continuous(breaks = coef$event_time) +
    scale_color_manual(values=point_colors) +
    xlab("Years since storm") +
    ylab("Coefficient") +
    facet_grid(cols=vars(treatment_type)) +
    theme(legend.position = "none") +
    ggtitle(str_to_title(str_replace_all(dep_var, "_", " ")))
}






# =============================================================================
# LOAD DATA
# =============================================================================
load("intermediates/bls_wages_employment.Rdata")
load("intermediates/county_building_permits.Rds")
load("intermediates/school_storm_unique.Rda")
load("intermediates/texas_storms.Rda")
load("intermediates/texas_disasters.Rda")
source("Code/hurricane-labor-schools/1_data_prep/5_build_stacked_did_panel.R")



### Clean Construction Data ----------------------------------------------------
# Total construction wages by county-year.
yearly_wages <- qrty_wages %>%
  mutate(calendar_year = year,
         year = ifelse(qtr <= 1, year - 1, year)
         ) %>%
  group_by(fips_state_county, year) %>%
  summarise(total_yearly_wages = sum(total_qtrly_wages), .groups = "drop") %>%
  mutate(log_total_yearly_wages = log(total_yearly_wages))

yearly_units <- census_permits %>%
    mutate(calendar_year = year,
           year = ifelse(month <= 4, as.numeric(year) - 1, as.numeric(year))) %>%
  group_by(fips_state_county, year) %>%
    summarise(total_yearly_unit_permits = 
                sum(units_1_units_imputed) + 
                sum(units_2_units_imputed) + 
                sum(units_3_4_units_imputed) + 
                sum(units_5_plus_units_imputed), 
              
              total_yearly_value_permits = 
                sum(value_1_units_imputed) + 
                sum(value_2_units_imputed) + 
                sum(value_3_4_units_imputed) + 
                sum(value_5_plus_units_imputed), 
              .groups = "drop") %>%
  mutate(log_total_yearly_unit_permits = log(total_yearly_unit_permits),
         log_total_yearly_value_permits = log(total_yearly_unit_permits))
  



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
  radius_miles=100,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1989:2019)
  )


# Collapse to county treatment
county_did <- stacked_did %>%
  group_by(county_fips, cz_2000, sid, storm_year, panel_year, event_time) %>%
  summarise(
    direct = as.integer(any(direct==1)), 
    indirect = as.integer(any(indirect==1)),
    ever_direct = as.integer(any(ever_direct==1)), 
    ever_indirect = as.integer(any(ever_indirect==1)),
    ) %>%
  ungroup()

county_did$indirect <- ifelse(county_did$direct == 1, 0, county_did$indirect)
county_did$ever_indirect <- ifelse(county_did$ever_direct == 1, 0, county_did$ever_indirect)

# Merge the outcome on the observation year (panel_year).
county_did %<>%
  left_join(yearly_wages,
            by = c("county_fips" = "fips_state_county", "panel_year" = "year")) %>%
  left_join(yearly_units,
            by = c("county_fips" = "fips_state_county", "panel_year" = "year"))


# =============================================================================
#  WIND EVENT STUDY
# =============================================================================
### Event studies ---------------------------------------------------------------
Plot_Event_Study(dep_var          = "log_total_yearly_wages") 



# =============================================================================
# DID
# =============================================================================
### Stacked DiD: effect of a hurricane hit on total construction wages -----------
did_model <- Diff_in_Diff(dep_var    = "log_total_yearly_wages",
                          treat_vars = c("direct", "indirect"),
                          df         = county_did)
etable(did_model)




