# ==================================
# Author: MV 
# Date: Updated 11/6/25
# Project: Hurricanes + Schools
# Description: merge school x/y coords with hurricane tracks
# ==================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")

### Load Data -------------------------------------------------------------------
load("inputs/CCD/school_xy.Rds")
load("intermediates/ibtracs_texas.Rda")

### Convert Schools to SF -------------------------------------------------------
school_sf <- school_xy %>%
  filter(!is.na(x), y > -900) %>% # drop missing/bad coords
  st_as_sf(coords = c("y", "x"), crs = st_crs(ibtracs_tx)) %>%
  ungroup()

### Distance Function -----------------------------------------------------------
sid_list <- unique(ibtracs_tx$sid)

processIbtracs <- function(i) {
  print(sid_list[i])
  
  ibtracs_sid <- ibtracs_tx %>% filter(sid == sid_list[i])
  dist_matrix  <- st_distance(school_sf, ibtracs_sid)
  nearest_idx  <- apply(dist_matrix, 1, which.min)
  nearest_hurr <- ibtracs_sid[nearest_idx, ]
  
  data.frame(dist_to  = apply(dist_matrix, 1, min)) %>%
    rename_with(~ paste0(.x, "_", sid_list[i]))
}

### Calculate Distances ---------------------------------------------------------
distances_to_hurrs <- lapply(1:length(sid_list), processIbtracs) %>% bind_cols()
school_sf %<>% bind_cols(distances_to_hurrs)


### Wide to Long ----------------------------------------------------------------
school_storm_distance <- school_sf %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols          = matches("^(dist_to)_"),
    names_to      = c(".value", "sid"),
    names_pattern = "(dist_to)_(.*)"
  ) 

# Clean variables
school_storm_distance %<>%
  select(sid, nces_school_id, tea_school_id, dist_to) %>%
  rename(dist_to_meters = dist_to) %>%
  mutate(dist_to_miles = dist_to_meters / 1609.34)

### Export ----------------------------------------------------------------------
save(school_storm_distance,   file = "intermediates/school_storm_distance.Rda")

