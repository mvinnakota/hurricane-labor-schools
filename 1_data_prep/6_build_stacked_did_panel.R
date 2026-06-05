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
load("intermediates/school_storm_treatment.Rda")


### Function ---------------------------------------------------------------
Build_Panel <- function(df, direct_var="hit_school", indirect_var="hit_cz", 
                        years_since=7, pre_years=4, post_years=4, never_treated=T){
  
  # create direct and indirect vars
  df %<>% 
    mutate(
    direct = df[[direct_var]], 
    indirect = df[[indirect_var]]
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
  
  # Create time_since_hit for each school-season observation.
  # time_since_hit measures years (seasons) elapsed since the school's most recent prior hit:
  # time_since_hit measures. Inf if the school has never been hit or this is their first hit.
  df %<>%
    arrange(nces_school_id, season, sid) %>%
    group_by(nces_school_id) %>%
    mutate(
      # most recent hit season in any prior row (NA if no prior hit)
      # locf stands for Last Observation Carried Forward
      season_if_hit = na.locf(ifelse(ever_direct_or_indirect, season, NA_real_), na.rm=F),
      lag_season = dplyr::lag(season_if_hit),
      # seasons since last hit (Inf if no prior hit)
      time_since_hit = ifelse(is.na(lag_season), Inf, season - lag_season)
    ) 

    select(-lag_season, -season_if_hit) %>%
    ungroup()
  
  
  # Now create a panel of treatment and control schools for a given storm
  storm_panels <- list()
  
  for(s in unique(df$sid)){
    # subset to this storm
    storm_df  <- df %>% filter(sid == s)
    # subset to schools eligible for this storm panel
    # (hit or not treated within years_since prior seasons)
    storm_df %<>% filter(ever_direct_or_indirect | time_since_hit > years_since)
    # Save year of the storm
    storm_season <- unique(storm_df$season)
    
    # expand eligible schools to one row per year in the panel window
    panel_df <- storm_df %>%
      crossing(panel_season = (storm_season - pre_years):(storm_season + post_years)) %>%
      mutate(
        event_time = panel_season - storm_season,
        # zero out treatment in pre-period; hold constant at storm-year values post-period
        direct   := ifelse(panel_season < storm_season, 0, direct),
        indirect := ifelse(panel_season < storm_season, 0, indirect)
      )
    
    storm_panels %<>% c(list(panel_df))
  }
  
  # Stack into a single dataframe
  storm_panels %<>% bind_rows()
  return(storm_panels)
}

# Create Test Panel
stacked_did <- school_storm %>% subset(high_cedp == 1 & cat %in% c(1:5)) %>% Build_Panel()


# # Psuedo-Code Examples
# # Build Hurricane Panel
# hurricane_panel  <- df %>% subset(only_hurricanes) %>% Build_Panel
# 
# 
# # No never treated
# ever_treat_panel <- df %>% Build_Panel(never_treated=F)
# 
# 
# # Only include schools ever within 100 miles of a hurricane
# close_by_panel   <- df %>% subset(within_100) %>% Build_Panel()
# 


