# ==================================
# Author: MV
# Date: 
# Project: 
# Description: 
# ==================================

### Setup --------------------------------------------------------

options(stringsAsFactors = FALSE)

# set up environment 
setwd("~/")
gc()
rm(list = ls())

# options
options(stringsAsFactors = FALSE, scipen = 999)

# load in packages
package_list <- c("dplyr", "magrittr", "foreign", "lmtest",
"tmap", "nlme", "plm", "zoo", "AER", "tidyr", "data.table",
"systemfit", "haven", "ggplot2", "stargazer", "lubridate",
"clubSandwich", "sandwich", "lfe", "Synth", "readstata13",
"locpol", "parallel", "stringr", "lfe", "sf")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) invisible(install.packages(new.packages))
invisible(lapply(package_list, library, character.only = TRUE))
rm(package_list, new.packages)

### Load in data ---------------------------------------------------

# load in extended best track archive data - hurricane radii
load("intermediates/ebtrak_df.Rda")

# load in school-storm data 
load("intermediates/school_hurr_treatment.Rda")

### Clean data -----------------------------------------------------

# Inspect CRS of school_hurr
target_crs <- st_crs(school_hurr)
message("school_hurr CRS: ", target_crs$input)

# ── 2. Convert ebtrak_df to sf (WGS84 first, then reproject) -----------------
# EBTRK lon is already converted to degrees East (negative = west) in read_ebtrk.R
ebtrak_sf <- ebtrak_df %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Reproject to match school_hurr
ebtrak_sf <- st_transform(ebtrak_sf, crs = target_crs)

# ── 3. Helper: build one quadrant sector polygon ------------------------------

# Creates a wedge polygon on the sphere given:
#   center_lon/lat : decimal degrees (WGS84)
#   radius_nm      : radius in nautical miles
#   bearing_from/to: clockwise bearings from north (e.g. NE = 0 to 90)
#   n_pts          : number of arc points (more = smoother)
#
# Returns an sfg POLYGON, or NULL if radius is NA/0.

library(geosphere)

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

# ── 4. Build sector polygons for every observation ---------------------------
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

message("Building wind sectors (this may take a few minutes for large datasets)...")
wind_sectors_sf <- build_sectors(ebtrak_df)

message(sprintf("Built %d quadrant sectors across %d storms",
                nrow(wind_sectors_sf),
                n_distinct(wind_sectors_sf$storm_id)))

# ── 5. Quick sanity check: CRS match -----------------------------------------
stopifnot(st_crs(wind_sectors_sf) == st_crs(school_hurr))
message("CRS check passed — wind_sectors_sf and school_hurr are compatible.")



# ── 7. Spatial join: schools × wind sectors ----------------------------------

# Normalise storm names in both datasets to upper-case for matching
school_hurr <- school_hurr %>%
  mutate(.storm_name_key = toupper(trimws(.data[["name"]])))

wind_sectors_sf <- wind_sectors_sf %>%
  mutate(.sector_name_key = toupper(trimws(name)))

# Spatial join: for each school point, find every wind sector it falls inside.
# Using distinct column names on each side avoids suffix-collision ambiguity.
message("Running spatial join...")
schools_in_sectors <- st_join(
  school_hurr,
  wind_sectors_sf %>% dplyr::select(.sector_name_key, wind_speed_kt, quadrant, obs_time),
  join = st_within,
  left = TRUE
)

# Filter to rows where the storm name in school_hurr matches the sector's storm,
# or where the school had no spatial overlap at all (wind_speed_kt is NA).
schools_matched <- schools_in_sectors %>%
  filter(is.na(wind_speed_kt) | .storm_name_key == .sector_name_key) %>%
  dplyr::select(-.sector_name_key)

# ── 8. Aggregate to one row per school-storm pair ----------------------------
# For each school × storm, flag the maximum wind speed it was ever inside,
# and create binary indicators for each wind speed threshold.
#

group_vars <- c("nces_school_id", ".storm_name_key")

exposure_flags <- schools_matched %>%
  st_drop_geometry() %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    exposed_34kt = any(wind_speed_kt == 34, na.rm = TRUE),
    exposed_50kt = any(wind_speed_kt == 50, na.rm = TRUE),
    exposed_64kt = any(wind_speed_kt == 64, na.rm = TRUE),
    .groups = "drop"
  )

# Join indicators back onto school_hurr
school_hurr_exposure <- school_hurr %>%
  left_join(exposure_flags, by = group_vars) %>%
  mutate(
    exposed_34kt = replace_na(exposed_34kt, FALSE),
    exposed_50kt = replace_na(exposed_50kt, FALSE),
    exposed_64kt = replace_na(exposed_64kt, FALSE)
  ) %>%
  dplyr::select(-.storm_name_key)

message(sprintf(
  "Exposure summary:\n  34kt: %d schools exposed\n  50kt: %d\n  64kt: %d",
  sum(school_hurr_exposure$exposed_34kt),
  sum(school_hurr_exposure$exposed_50kt),
  sum(school_hurr_exposure$exposed_64kt)
))

### Analysis -------------------------------------------------------

### Export results -------------------------------------------------