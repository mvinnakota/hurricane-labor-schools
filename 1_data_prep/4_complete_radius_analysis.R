# ==================================
# Author: MV
# Date: 6/1/2026
# Project: Hurricanes
# Description: Merge wind speed and schools
# ==================================

### Setup --------------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")

### Load in data ---------------------------------------------------
# load in extended best track archive data - hurricane radii
load("intermediates/ebtrak_df.Rda")
# load in school data 
load("inputs/CCD/school_xy.Rds")
# load in ibtracs data
ibtracs <- sf::read_sf("inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp")



# ==================================================================
### Clean IBTRAK Data
# ==================================================================
ibtracs %<>% janitor::clean_names() 

# Create storm level data
storms <- ibtracs %>%
  st_drop_geometry() %>%
  group_by(sid) %>%
  summarise(max_cat = max(usa_sshs),
            year = first(season),
            name = first(toupper(trimws(name))),
            date = min(date(iso_time)))


# Filter ibtracs to ever overlapped with texas
# First, get the shapefile of texas counties 
txcntys <- tigris::counties() %>% filter(STATEFP == "48")
txcntys %<>% dplyr::select(fips = GEOID, geometry)
## Next, filter to the hurricanes that intersect with texas at all 
st_crs(txcntys)
st_crs(ibtracs)
txcntys %<>% st_transform(crs = st_crs(ibtracs))
ibtracs_tx <- ibtracs %>% st_filter(txcntys)

# Get SIDs of storms whose trajectory touched texas
traj_sids <- ibtracs_tx %>% pull(sid)





# ==================================================================
### Clean EBTRAK Data
# ==================================================================
target_crs <- st_crs(school_xy)

#  Convert ebtrak_df to sf (WGS84 first, then reproject) 
# EBTRK lon is already converted to degrees East (negative = west) in code 5
ebtrak_sf <- ebtrak_df %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)  # raw numbers are WGS84 degrees

# Reproject to match school_hurr
ebtrak_sf <- st_transform(ebtrak_sf, crs = target_crs)





# ==================================================================
### Build Wind Sectos from EBTRAK Data
# ==================================================================
# Helper: build one quadrant sector polygon 
# Creates a wedge polygon on the sphere given:
#   center_lon/lat : decimal degrees (WGS84)
#   radius_nm      : radius in nautical miles
#   bearing_from/to: clockwise bearings from north (e.g. NE = 0 to 90)
#   n_pts          : number of arc points (more = smoother)
#
# Returns an sfg POLYGON, or NULL if radius is NA/0.

make_sector <- function(center_lon, center_lat, radius_nm,
                        bearing_from, bearing_to, n_pts = 60) {
  if (is.na(radius_nm) || radius_nm <= 0) return(NULL)
  
  radius_m  <- radius_nm * 1852  # nm -> metres
  bearings  <- seq(bearing_from, bearing_to, length.out = n_pts)
  arc_pts   <- destPoint(c(center_lon, center_lat), bearings, radius_m)
  
  coords <- rbind(
    c(center_lon, center_lat),
    arc_pts,
    c(center_lon, center_lat)   # close the polygon
  )
  
  st_polygon(list(coords))
}

# Quadrant bearing ranges (clockwise from north)
quadrants <- list(
  NE = c(  0,  90),
  SE = c( 90, 180),
  SW = c(180, 270),
  NW = c(270, 360)
)

# Wind speed thresholds available in ebtrak_df
wind_speeds <- c(34, 50, 64)

# Build sector polygons for every observation 
# Output: one row per (observation × wind_speed × quadrant) with the sector polygon.
# Rows where radius is NA or 0 are dropped.

build_sectors <- function(df) {
  results <- vector("list", nrow(df) * length(wind_speeds) * length(quadrants))
  idx <- 1L
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    clon <- row$lon
    clat <- row$lat
    
    for (ws in wind_speeds) {
      for (q in names(quadrants)) {
        col_name <- paste0("r", ws, "_", q, "_nm")
        if (!col_name %in% names(df)) next
        
        radius_nm <- row[[col_name]]
        brg       <- quadrants[[q]]
        sector    <- make_sector(clon, clat, radius_nm, brg[1], brg[2])
        
        if (!is.null(sector)) {
          results[[idx]] <- tibble(
            storm_id  = row$storm_id,
            name      = row$name,
            obs_time  = row$obs_time,
            year      = row$year,
            vmax_kt   = row$vmax_kt,
            wind_speed_kt = as.integer(ws),
            quadrant  = q,
            radius_nm = radius_nm,
            geometry  = st_sfc(sector, crs = 4326)
          )
          idx <- idx + 1L
        }
      }
    }
  }
  
  results[seq_len(idx - 1L)] %>%
    bind_rows() %>%
    st_as_sf() %>%
    st_transform(crs = target_crs)
}

# Building wind sectors
wind_sectors_sf <- build_sectors(ebtrak_df)





# ==================================================================
### Subset Wind sectors to texas storms
# ==================================================================
# first merge with IBTRACKS storm meta data
# build storm vars to merge 
wind_sectors_sf %<>%
  group_by(storm_id) %>%
  mutate(begin_pad = date(min(obs_time)) - 14,
         end_pad   = date(min(obs_time)) + 14,
         name = toupper(trimws(name))) %>%
  ungroup()

# Merge on date, name, and year
wind_sectors_sf %<>% 
  left_join(storms, join_by(begin_pad <= date, end_pad >= date, name, year)) 

# figure out which storms overlap with Texas 
texas <- tigris::states(cb = TRUE) %>%
  filter(NAME == "Texas") %>%
  st_transform(crs = st_crs(wind_sectors_sf))

# Subset wind sectors to those overlapping Texas 
wind_sectors_texas <- wind_sectors_sf %>%
  filter(st_intersects(geometry, st_union(texas), sparse = FALSE)[, 1] | 
           sid %in% traj_sids) 

# Get SIDs of storms whose winds or trajectories touch texas
wind_sids <- wind_sectors_texas %>% 
  st_drop_geometry() %>% 
  pull(sid) %>% unique()


# ==================================================================
### Clean disaster declaration data
# ==================================================================
# Load FEMA disaster declarations
dd_df <- read.csv("inputs/DisasterDeclarationsSummaries.csv", stringsAsFactors = FALSE)

# Clean variables
dd_df %<>% 
  mutate(
    # Convert date string to date
    incidentBeginDate = as.Date(substr(incidentBeginDate, 1, 10)),
    incidentEndDate = as.Date(substr(incidentEndDate, 1, 10)),
    # Build full 5-digit county FIPS: state (2) + county (3, zero-padded)
    county_fips = paste0(
      str_pad(fipsStateCode, 2, pad = "0"),
      str_pad(fipsCountyCode, 3, pad = "0")
    )) %>% janitor::clean_names()

# subset to the years we have storm data for
dd_df %<>% filter(incident_end_date <= max(storms$date))

# Subset to Texas hurricane/tropical storm declarations only
dd_tx_storms <- dd_df %>% 
  filter(state == "TX") %>%
  filter(str_detect(declaration_title, "HURRICANE|TROPICAL STORM|FLOOD|EXCESSIVE RAIN")) 

# Extract storm names from IBTRAKs
# First get list of all storms
storm_names_regex <- paste0("\\b(", paste0(unique(ibtracs$name), collapse = "|"), ")\\b")
# Then search for those names in the declaration title
dd_tx_storms %<>%
  mutate(name = str_extract_all(declaration_title, storm_names_regex)) %>%
  unnest(name, keep_empty = TRUE)

# merge in storm information
dd_tx_storms %<>%
  mutate(
    begin_pad = incident_begin_date - 14,
    end_pad   = incident_end_date + 14
  ) %>%
  inner_join(
    # only merge with storms that hit texas
    subset(storms, sid %in% c(wind_sids, traj_sids)),
    # merge on date of disaster
    join_by(begin_pad <= date, end_pad >= date)
  ) %>%
  # only keep merges that have matching storm names 
  # (or has the same date, but is missing a storm name in disaster data)
  filter((is.na(name.x)) | name.x == name.y)

  
# Select only necessary variables to make flag
dd_tx_storms %<>% 
  select(county_fips, incident_type, sid) %>% 
  group_by(county_fips, sid) %>%
  summarise(incident_type = paste(unique(incident_type), collapse = ", ")) %>%
  ungroup() %>%
  mutate(dd_flag = 1)

# Get SIDs of storms who cause a declared disaster in texas
dd_sids <- dd_tx_storms %>% pull(sid)

# Save just texas disasters
dd_tx <- dd_df %>% filter(state == "TX") %>%
  select(county_fips, contains("declaration"), contains("incident"), contains("_code")) %>%
  distinct()




# ==================================================================
### Create storm level data for all Texas storms
# ==================================================================
# subset to only storms in our sample
storms_tx <- storms %>% subset(sid %in% c(wind_sids, traj_sids, dd_sids))






# ==================================================================
### Create school_storm level Data
# ==================================================================
# all unique schools
schools_unique <- school_xy %>% 
  st_drop_geometry() %>%
  distinct(nces_school_id, tea_school_id, county_fips, cz_2000)

# all unique storms
storms_unique <- storms_tx %>% distinct(sid, name, year)

# cross join: every school × every storm
school_storm_grid <- crossing(schools_unique, storms_unique)



# ==================================================================
### Merge school_storm data with wind sectors
# ==================================================================
# Spatial join: each school point against Texas wind sector polygons.
# left = TRUE keeps all schools even if they fall outside every sector.
# Result has one row per school × sector match; a school hit by multiple
# quadrants or obs_times of the same storm will appear more than once here.
school_sectors_raw <- st_join(
  school_xy,
  wind_sectors_texas %>% dplyr::select(name, year, sid, "ebtrak_sid"="storm_id", wind_speed_kt),
  join = st_within,
  left = TRUE
)

# collapse observed exposures (no NAs since we filtered)
school_storm_exposed <- school_sectors_raw %>%
  st_drop_geometry() %>%
  filter(!is.na(sid)) %>%
  group_by(nces_school_id, tea_school_id, county_fips, cz_2000, name, year, sid, ebtrak_sid) %>%
  summarise(
    wind_34kt = as.integer(any(wind_speed_kt == 34, na.rm = TRUE)),
    wind_50kt = as.integer(any(wind_speed_kt == 50, na.rm = TRUE)),
    wind_64kt = as.integer(any(wind_speed_kt == 64, na.rm = TRUE)),
    .groups = "drop"
  )

# join onto grid, fill unmatched with 0
school_storm_unique <- school_storm_grid %>%
  left_join(school_storm_exposed, 
            by = c("nces_school_id", "tea_school_id", "county_fips", 
                   "cz_2000", "sid", "name", "year")) %>%
  mutate(across(c(wind_34kt, wind_50kt, wind_64kt), ~replace_na(.x, 0L)))




# ==================================================================
### Merge school_storm data with storm metadata 
# ==================================================================
# Merge into school storm data
school_storm_unique %<>% 
  mutate(year = as.integer(year)) %>% 
  left_join(storms)




# ==================================================================
### Merge school_storm data with declared disaster flag 
# ==================================================================
school_storm_unique %<>% 
  left_join(dd_tx_storms) %>% 
  mutate(dd_flag = replace_na(dd_flag, 0L))




# ==================================================================
### Merge IBTRAKS data with school_storm data
### and calculate distance to eye of storm
# ==================================================================
# Reproject to match ibtracs
school_ibtracs_sf <- school_xy %>% st_transform(crs = st_crs(ibtracs_tx))

### Calculate Distances 
sid_list <- unique(ibtracs_tx$sid)

processIbtracs <- function(i) {
  print(sid_list[i])
  
  ibtracs_sid <- ibtracs_tx %>% filter(sid == sid_list[i])
  dist_matrix  <- st_distance(school_ibtracs_sf, ibtracs_sid)
  
  data.frame(dist_to  = apply(dist_matrix, 1, min)) %>%
    rename_with(~ paste0(.x, "_", sid_list[i]))
}

distances_to_hurrs <- lapply(1:length(sid_list), processIbtracs) %>% bind_cols()
school_ibtracs_sf %<>% bind_cols(distances_to_hurrs)

### Wide to long 
school_storm_distance <- school_ibtracs_sf %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols          = matches("^(dist_to)_"),
    names_to      = c(".value", "sid"),
    names_pattern = "(dist_to)_(.*)") 

# Clean variables
school_storm_distance %<>%
  select(nces_school_id, tea_school_id, sid, dist_to) %>%
  rename(dist_to_meters = dist_to) %>%
  mutate(dist_to_miles = dist_to_meters / 1609.34)

### Merge into storm_school data
school_storm_unique %<>% left_join(school_storm_distance) 




### Save -----------------------------------------------------------
school_storm_unique %<>% rename("storm_name"="name", "storm_year"="year")
save(school_storm_unique, file = "intermediates/school_storm_unique.Rda")

storms_tx %<>% rename("storm_name"="name", "storm_year"="year")
save(storms_tx, file = "intermediates/texas_storms.Rda")

save(dd_tx, file = "intermediates/texas_disasters.Rda")


# ==================================================================
### Save wind-field footprint for plotting
# ==================================================================

# Dissolve the per-observation wedges into one footprint per storm x wind speed
# (the total area ever exposed to >= that wind speed). Keep only the Texas-sample
# storms -- wind_sectors_sf spans the whole Atlantic basin and unmatched EBTRK
# storms have sid = NA, so filtering here both restricts to our storms and drops
# the NA group. Use planar geometry (sf_use_s2(FALSE)) to match how the plotting
# code clips the field, then restore the previous setting.
.old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)
wind_footprint <- wind_sectors_sf %>%
  filter(sid %in% storms_tx$sid) %>%
  group_by(sid, wind_speed_kt) %>%
  summarise(.groups = "drop") %>%
  sf::st_make_valid()
sf::sf_use_s2(.old_s2)
save(wind_footprint, file = "intermediates/wind_footprint.Rda")


