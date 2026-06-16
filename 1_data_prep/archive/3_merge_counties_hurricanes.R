# ==================================
# Author: MV
# Date: Updated 11/6/25
# Project: Hurricanes + Counties
# Description: merge counties with hurricane tracks 
# ==================================

### Setup --------------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")

# load in ibtracs 
ibtracs <- sf::read_sf("inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp")

# get the shapefile of texas counties 
txcntys <- tigris::counties() %>% filter(STATEFP == "48")
txcntys %<>% dplyr::select(fips = GEOID, geometry)

# filter ibtracs again (to ever overlapped with texas)
## first, filter to the hurricanes that intersect with texas at all 
st_crs(txcntys)
st_crs(ibtracs)
txcntys %<>% st_transform(crs = st_crs(ibtracs))
ibtracs_tx <- ibtracs %>% st_filter(txcntys)

## next, filter to post 1989 hurricanes 
ibtracs_tx %<>% filter(year >= 1989)

## keep specific columns 
ibtracs_tx %<>% dplyr::select(SID, SEASON, NAME, ISO_TIME, 
                              NATURE, LAT, LON,USA_LAT, USA_LON, 
                              USA_WIND, USA_SSHS, USA_RMW,
                              geometry) %>%
  janitor::clean_names()

# Create storm level data
storm_tx <- ibtracs_tx %>%
  st_drop_geometry() %>%
  group_by(sid) %>%
  summarise(max_cat = max(usa_sshs),
            year = first(season),
            name = first(name),
            date = first(date(iso_time)))

# save
save(storm_tx, file = "intermediates/storm_texas.Rda")
save(ibtracs_tx, file = "intermediates/ibtracs_texas.Rda")




