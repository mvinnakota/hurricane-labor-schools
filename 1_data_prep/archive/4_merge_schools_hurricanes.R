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
load("intermediates/texascounties_ibtracs.Rda")

ibtracs <- sf::read_sf("inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp")

### Clean IBTrACS ---------------------------------------------------------------
texas <- tigris::states() %>%
  filter(STUSPS == "TX") %>%
  st_transform(crs = st_crs(ibtracs))

ibtracs_tx <- ibtracs %>%
  st_filter(texas) %>%
  filter(year >= 1989) %>%
  dplyr::select(SID, SEASON, NAME, NATURE, LAT, LON, ISO_TIME,
                USA_LAT, USA_LON, USA_WIND, USA_SSHS, USA_RMW)


### Convert Schools to SF -------------------------------------------------------
school_sf <- school_xy %>%
  filter(!is.na(x), y > -900) %>% # drop missing/bad coords
  st_as_sf(coords = c("y", "x"), crs = st_crs(ibtracs_tx)) %>%
  ungroup()


### Distance Function -----------------------------------------------------------
sid_list <- unique(ibtracs_tx$SID)

processIbtracs <- function(i) {
  print(sid_list[i])
  
  ibtracs_sid <- ibtracs_tx %>% filter(SID == sid_list[i])
  dist_matrix  <- st_distance(school_sf, ibtracs_sid)
  nearest_idx  <- apply(dist_matrix, 1, which.min)
  nearest_hurr <- ibtracs_sid[nearest_idx, ]
  
  data.frame(
    dist_to  = apply(dist_matrix, 1, min),
    cat      = nearest_hurr$USA_SSHS,
    wind     = nearest_hurr$USA_WIND,
    rmw      = nearest_hurr$USA_RMW,
    date     = nearest_hurr$ISO_TIME,     # date of nearest track point
    season   = ibtracs_sid$SEASON[1],     # storm-level (same for all points)
    name     = ibtracs_sid$NAME[1],
    nature   = ibtracs_sid$NATURE[1]
  ) %>%
    rename_with(~ paste0(.x, "_", sid_list[i]))
}

### Calculate Distances ---------------------------------------------------------
distances_to_hurrs <- lapply(1:length(sid_list), processIbtracs) %>% bind_cols()
school_sf %<>% bind_cols(distances_to_hurrs)


### Wide to Long ----------------------------------------------------------------
school_storm <- school_sf %>%
  pivot_longer(
    cols          = matches("^(dist_to|cat|wind|rmw|date|season|name|nature)_"),
    names_to      = c(".value", "sid"),
    names_pattern = "(dist_to|cat|wind|rmw|date|season|name|nature)_(.*)"
  ) 

# Clean variables
school_storm %<>%
  rename(dist_to_meters = dist_to) %>%
  mutate(
    dist_to_miles = dist_to_meters / 1609.34,
    date          = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

# calculate max category
school_storm %<>% group_by(sid) %>% mutate(max_cat = max(cat, na.rm=T)) %>% ungroup()
  

### Export ----------------------------------------------------------------------
save(school_storm,   file = "intermediates/school_storm.Rda")

