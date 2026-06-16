# ==================================
# Author: EH
# Date: Updated 05/26/26
# Project: Hurricanes + Schools
# Description: Write function to create stacked did panel
# ==================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")

### Load Data -------------------------------------------------------------------
load("intermediates/school_storm_unique.Rda")


### Function ---------------------------------------------------------------
Build_Panel <- function(df=school_storm_unique, direct_var="wind_50kt", indirect_geo="cz", 
                        years_since=7, pre_years=4, post_years=4, never_treated=T){
  # create direct and indirect vars
  df %<>% ungroup() %>% mutate(direct = df[[direct_var]])
  
  # build indirect treatment var
  if(indirect_geo == "county"){
    df %<>% group_by(sid, county_fips) %>% mutate(indirect = as.integer(any(direct == 1)))
  }
  if(indirect_geo == "cz"){
    df %<>% group_by(sid, cz_2000) %>% mutate(indirect = as.integer(any(direct == 1)))
  }
  
  # Make sure panel is storm by school and not storm by point in time by school
  df %<>%
    group_by(nces_school_id, sid) %>%
    summarise(
      year      = first(year),
      county_fips = first(county_fips),
      cz_2000     = first(cz_2000),
      # treated if the condition held at ANY point on the track
      direct = as.integer(any(direct == 1, na.rm = TRUE)),
      indirect  = as.integer(any(indirect == 1, na.rm = TRUE)),
      .groups = "drop"
    )

  # Enforce mutual exclusivity: if a unit is directly treated, it cannot also be indirectly treated
  df$indirect <- ifelse(df$direct == 1, 0, df$indirect)
  
  # 1 if unit was ever directly or indirectly treated
  df$ever_direct_or_indirect <- df$direct == 1 | df$indirect == 1
  
  # Create never_treated flag 
  df %<>%
    group_by(nces_school_id) %>%
    mutate(never_treated = !any(ever_direct_or_indirect))
  
  # Drop never treated if specified
  if(!never_treated){df %<>% filter(!never_treated)}
  
  # Create time_since_hit for each school-year observation.
  # time_since_hit measures years elapsed since the school's most recent prior hit:
  # time_since_hit measures. Inf if the school has never been hit or this is their first hit.
  df %<>%
    arrange(nces_school_id, year, sid) %>%
    group_by(nces_school_id) %>%
    mutate(
      # most recent hit year in any prior row (NA if no prior hit)
      # locf stands for Last Observation Carried Forward
      year_if_hit = na.locf(ifelse(ever_direct_or_indirect, year, NA_real_), na.rm=F),
      lag_year = dplyr::lag(year_if_hit),
      # years since last hit (Inf if no prior hit)
      time_since_hit = ifelse(is.na(lag_year), Inf, year - lag_year)
    ) %>%
    select(-lag_year, -year_if_hit) %>%
    ungroup()
  
  
  # Now create a panel of treatment and control schools for a given storm
  storm_panels <- list()
  
  for(s in unique(df$sid)){
    # subset to this storm
    storm_df  <- df %>% filter(sid == s)
    # subset to schools eligible for this storm panel
    # (hit or not treated within years_since prior years)
    storm_df %<>% filter(ever_direct_or_indirect | time_since_hit > years_since)
    # Save year of the storm
    s_year <- unique(storm_df$year)
    
    # expand eligible schools to one row per year in the panel window
    panel_df <- storm_df %>%
      crossing(panel_year = (s_year - pre_years):(s_year + post_years)) %>%
      mutate(
        event_time = panel_year - s_year,
        # zero out treatment in pre-period; hold constant at storm-year values post-period
        direct   := ifelse(panel_year < s_year, 0, direct),
        indirect := ifelse(panel_year < s_year, 0, indirect)
      )
    storm_panels %<>% c(list(panel_df))
  }
  
  # Stack into a single dataframe
  storm_panels %<>% bind_rows()
  return(storm_panels)
}


# Psuedo-Code Examples
# Build Hurricane Panel
stacked_did <- Build_Panel(never_treated = T)



