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
# load("intermediates/school_storm_unique.Rda")


### Function ---------------------------------------------------------------
# Defaults
# subset to 64kts (main treatment)
# not been hit with 50kt storm
# 50kts as a buffer
# less than 50kts and in same commuting zone as 64 kt winds
# Check never treated

Build_Panel <- function(df=school_storm_unique, 
                        direct_var="wind_64kt",
                        indirect_var="wind_64kt",
                        indirect_geo="cz", 
                        pre_years=4, post_years=3, 
                        never_treated=T, 
                        years_since=7, 
                        donut="wind_50kt",
                        radius_miles=100){

  # create direct and indirect vars
  df %<>% ungroup() %>% mutate(direct = get(direct_var))
  if(is.null(indirect_var)){
    df %<>% mutate(indirect = get(direct_var))
  } else {
    df %<>% mutate(indirect = get(indirect_var))
  }
  
  # build indirect treatment var
  if(indirect_geo == "county"){
    df %<>% group_by(sid, county_fips) %>% mutate(indirect = as.integer(any(indirect == 1)))
  }
  if(indirect_geo == "cz"){
    df %<>% group_by(sid, cz_2000) %>% mutate(indirect = as.integer(any(indirect == 1)))
  }
  
  # Enforce mutual exclusivity: if a unit is directly treated, it cannot also be indirectly treated
  df$indirect <- ifelse(df$direct == 1, 0, df$indirect)
  
  # Create never_treated flag 
  df %<>%
    group_by(nces_school_id) %>%
    mutate(direct_or_indirect = direct == 1 | indirect == 1) %>%
    mutate(never_treated = !any(direct_or_indirect))
  
  # Drop never treated if specified
  if(!never_treated){df %<>% filter(!never_treated)}
  
  # Create time_since_hit for each school-year observation.
  # time_since_hit measures years elapsed since the school's most recent prior hit:
  # time_since_hit measures. Inf if the school has never been hit or this is their first hit.
  df %<>%
    arrange(nces_school_id, date, sid) %>%
    group_by(nces_school_id) %>%
    mutate(
      # most recent hit year in any prior row (NA if no prior hit)
      # locf stands for Last Observation Carried Forward
      year_if_hit = na.locf(ifelse(direct_or_indirect, storm_year, NA_real_), na.rm=F),
      lag_year = dplyr::lag(year_if_hit),
      # years since last hit (Inf if no prior hit)
      time_since_hit = ifelse(is.na(lag_year), Inf, storm_year - lag_year)
    ) %>%
    select(-lag_year, -year_if_hit) %>%
    ungroup()
  
  
  # Now create a panel of treatment and control schools for a given storm
  storm_panels <- list()
  
  for(s in unique(df$sid)){
    # subset to this storm
    storm_df  <- df %>% filter(sid == s)
    # subset to schools eligible for this storm panel
    # (not treated within years_since prior years)
    storm_df %<>% filter(time_since_hit > years_since)
    
    # If donut, then drop schools that were hit by any wind from the indirect treatment group
    if(!is.null(donut)){
      storm_df %<>% mutate(donut_var = get(donut))
      storm_df %<>% filter(!(donut_var & !direct))
      }
    
    # Drop schools outside the radius
    if(!is.null(radius_miles)){storm_df %<>% filter(dist_to_miles <= radius_miles)}
    
    # # skip if no eligible schools, or no variation in direct-hit status
    if(nrow(storm_df) == 0 || all(storm_df$direct) || !any(storm_df$direct)) next
    
    # Save year of the storm
    s_year <- unique(storm_df$storm_year)
    
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
stacked_did <- Build_Panel(donut=NULL)



