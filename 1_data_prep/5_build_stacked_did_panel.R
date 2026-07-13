# ==================================
# Author: EH
# Date Updated 06/19/26
# Project: Hurricanes + Schools
# Description: Write function to create stacked did panel
# ==================================

### Function ---------------------------------------------------------------
Build_Panel <- function(df=school_storm_unique, 
                        direct_var="wind_64kt",
                        indirect_var="wind_64kt",
                        indirect_geo="cz", 
                        pre_years=4, post_years=3, 
                        never_treated=T, 
                        years_since=7, 
                        donut="wind_50kt",
                        radius_miles=300,
                        radius_var="dist_to_64kt_miles",
                        sample_years = c(1989:2019),
                        keep_previously_hit = FALSE){

  # drop storms that were not hurricanes when they touched texas
  df %<>% subset(max_cat_tx>=1)
  
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
    if(!keep_previously_hit){
      storm_df %<>% filter(time_since_hit > years_since)
    } else {
      storm_df %<>% mutate(
        previously_hit = time_since_hit <= years_since,
        direct = ifelse(time_since_hit <= years_since, FALSE, direct),
        indirect = ifelse(time_since_hit <= years_since, FALSE, indirect)) 
      }
    
    # If donut, then drop schools that were hit by any wind from the indirect or control group
    if(!is.null(donut)){
      storm_df %<>% mutate(donut_var = get(donut))
      storm_df %<>% filter(!(donut_var & !direct))
      }
    
    # Drop schools outside the radius
    if(!is.null(radius_miles)){
      storm_df %<>% mutate(radius = get(radius_var))
      storm_df %<>% filter(radius <= radius_miles)
      }
    
    # # skip if no eligible schools, or no variation in direct-hit status
    if(nrow(storm_df) == 0 || !any(storm_df$direct)) next
    
    # Save year of the storm
    s_year <- unique(storm_df$storm_year)
    
    # expand eligible schools to one row per year in the panel window
    panel_df <- storm_df %>%
      crossing(panel_year = (s_year - pre_years):(s_year + post_years)) %>%
      mutate(
        event_time = panel_year - s_year,
        # zero out treatment in pre-period; hold constant at storm-year values post-period
        ever_direct = direct, 
        ever_indirect = indirect,
        direct   := ifelse(panel_year < s_year, 0, direct),
        indirect := ifelse(panel_year < s_year, 0, indirect)
      )
    storm_panels %<>% c(list(panel_df))
  }
  
  # Stack into a single dataframe
  storm_panels %<>% bind_rows()
  
  # subset to sample years
  storm_panels %<>% filter(storm_year %in% sample_years)
  return(storm_panels)
}




