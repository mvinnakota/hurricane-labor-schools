# ==================================
# Author: MV
# Date: Updated 11/6/25
# Project: Hurricanes + Schools
# Description: merge school x/y coords with hurricane tracks
# ==================================

### Setup -----------------------------------------------------------------------
gc()
rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

package_list <- c("dplyr", "magrittr", "foreign", "lmtest", "tmap", "nlme",
                  "plm", "zoo", "AER", "tidyr", "data.table", "systemfit",
                  "haven", "ggplot2", "stargazer", "lubridate", "clubSandwich",
                  "sandwich", "lfe", "readstata13", "locpol", "parallel",    # BUG FIX: removed duplicate "lfe"
                  "stringr", "sf", "rstudioapi")

new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if (length(new.packages)) invisible(install.packages(new.packages))
invisible(lapply(package_list, library, character.only = TRUE))
rm(package_list, new.packages)

setwd(dirname(getActiveDocumentContext()$path))
setwd("../../Data/")

### Load Data -------------------------------------------------------------------
load("inputs/CCD/district_sf.Rds")
load("inputs/CCD/school_info_ccd.Rds")
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
# NOTE: USA_RMW is sparse pre-2004 — use as robustness check only

### Convert Schools to SF -------------------------------------------------------
school_sf <- school_xy %>%
  filter(!is.na(x), y > -900) %>%                          # drop missing/bad coords
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
school_sf_long <- school_sf %>%
  pivot_longer(
    cols          = matches("^(dist_to|cat|wind|rmw|date|season|name|nature)_"),
    names_to      = c(".value", "sid"),
    names_pattern = "(dist_to|cat|wind|rmw|date|season|name|nature)_(.*)"
  ) 

# Clean variables
school_sf_long %<>%
  rename(dist_to_meters = dist_to) %>%
  mutate(
    dist_to_miles = dist_to_meters / 1609.34,
    date          = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

### Merge School County + CZ ----------------------------------------------------
school_cnty <- school_info_ccd %>%
  select(nces_school_id, fips = county_fips, school_cz = cz_2000) %>%
  distinct()


### Merge Storm × County --------------------------------------------------------
storms <- texas_ibtracs %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  select(fips, sid) %>%
  distinct() %>%
  mutate(hit_county = 1)

# Which CZs were hit by each storm?
cz_hit <- storms %>%
  left_join(school_cnty %>% select(fips, school_cz) %>% distinct(), by = "fips") %>%
  filter(!is.na(school_cz)) %>%
  select(sid, school_cz) %>%
  distinct() %>%
  mutate(hit_cz = 1)

### Build Treatment Definitions -------------------------------------------------
schools <- school_sf_long %>%
  st_drop_geometry() %>%
  left_join(school_cnty, by = "nces_school_id")

school_storm <- schools %>%
  left_join(storms,  by = c("fips", "sid")) %>%
  left_join(cz_hit,  by = c("sid", "school_cz")) %>%
  mutate(
    hit_county = replace_na(hit_county, 0),
    hit_cz     = replace_na(hit_cz,     0),
    
    treatment_cnty = case_when(
      dist_to_miles <= 10  ~ "Direct",
      hit_county == 1      ~ "Indirect",
      TRUE                 ~ "Control"
    ),
    
    treatment_cz = case_when(
      dist_to_miles <= 10  ~ "Direct",
      hit_cz == 1          ~ "Indirect",
      TRUE                 ~ "Control"
    )
  )

### Export ----------------------------------------------------------------------
save(school_sf_long, file = "intermediates/school_sf_with_hurricane_info.Rda")
save(school_storm,   file = "intermediates/school_storm_treatment.Rda")







save(school_, file = "school_sf_with_hurricane_info.Rda")