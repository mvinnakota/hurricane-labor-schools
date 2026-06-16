# ==================================
# Author: MV
# Date: 
# Project: 
# Description: 
# ==================================

### Setup --------------------------------------------------------

options(stringsAsFactors = FALSE)

# set up environment 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")
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

library(geosphere)

### Load in data ---------------------------------------------------

# load in extended best track archive data - hurricane radii
load("intermediates/ebtrak_df.Rda")

# load in school data 
load("inputs/CCD/school_xy.Rds")

### Clean data -----------------------------------------------------

# make school sf
school_sf <- school_xy %>%
  filter(!is.na(x), y > -900) %>% # drop missing/bad coords
  st_as_sf(coords = c("y", "x")) %>%
  ungroup()

target_crs <- st_crs(school_sf)

#  Convert ebtrak_df to sf (WGS84 first, then reproject) 
# EBTRK lon is already converted to degrees East (negative = west) in code 5
ebtrak_sf <- ebtrak_df %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = target_crs, remove = FALSE)

# Reproject to match school_hurr
ebtrak_sf <- st_transform(ebtrak_sf, crs = target_crs)

# ── 3. Helper: build one quadrant sector polygon 

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

# ── 4. Build sector polygons for every observation 
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

# ── 5. Quick sanity check: CRS match 
stopifnot(st_crs(wind_sectors_sf) == st_crs(school_sf))
message("CRS check passed — wind_sectors_sf and school_sf are compatible.")

# figure out which storms overlap with Texas 
options(tigris_use_cache = TRUE)

texas <- tigris::states(cb = TRUE, resolution = "5m") %>%
  filter(NAME == "Texas") %>%
  st_transform(crs = st_crs(wind_sectors_sf))

# ── 2. Subset wind sectors to those overlapping Texas 
wind_sectors_texas <- wind_sectors_sf %>%
  filter(st_intersects(geometry, st_union(texas), sparse = FALSE)[, 1])

message(sprintf(
  "%d of %d sectors overlap Texas (%d unique storms)",
  nrow(wind_sectors_texas),
  nrow(wind_sectors_sf),
  n_distinct(wind_sectors_texas$name)
))

# build storm unique ID 
wind_sectors_texas %<>%
  mutate(storm_year_id = paste(toupper(trimws(name)), year, sep = "_"))

### Merge schools with wind sectors --------------------------------
# Spatial join: each school point against Texas wind sector polygons.
# left = TRUE keeps all schools even if they fall outside every sector.
# Result has one row per school × sector match; a school hit by multiple
# quadrants or obs_times of the same storm will appear more than once here.
school_sectors_raw <- st_join(
  school_sf,
  wind_sectors_texas %>% dplyr::select(storm_year_id, wind_speed_kt),
  join = st_within,
  left = TRUE
)

# Collapse to unique school × storm_year_id rows.
# For each pair, flag whether any 34/50/64kt sector covered that school.
# Schools with no overlap for a storm appear with all three flags as 0.
school_storm_unique <- school_sectors_raw %>%
  group_by(nces_school_id, storm_year_id, geometry) %>%        
  summarise(
    wind_34kt = as.integer(any(wind_speed_kt == 34, na.rm = TRUE)),
    wind_50kt = as.integer(any(wind_speed_kt == 50, na.rm = TRUE)),
    wind_64kt = as.integer(any(wind_speed_kt == 64, na.rm = TRUE)),
    .groups = "drop"
  )

# # Confirm uniqueness
# stopifnot(nrow(school_storm_unique) == nrow(distinct(school_storm_unique, ncessch, storm_year_id)))
# message(sprintf("school_storm_unique: %d unique school-storm pairs", nrow(school_storm_unique)))
# 
# message(sprintf(
#   "Wind exposure summary:\n  34kt: %d school-storm pairs\n  50kt: %d\n  64kt: %d",
#   sum(school_storm_unique$wind_34kt),
#   sum(school_storm_unique$wind_50kt),
#   sum(school_storm_unique$wind_64kt)
# ))

# clean disaster declaration data --------------------------------------------------------


# Load FEMA disaster declarations
dd_df <- read.csv("inputs/DisasterDeclarationsSummaries.csv", stringsAsFactors = FALSE)

# subset to the years that we had a hurricane 
dd_df %<>% mutate(year = year(dd_df$declarationDate) %>% as.character())
school_storm_unique %<>% mutate(year = str_extract(storm_year_id,pattern = "[0-9]{4}"))
dd_df %<>% filter(year %in% school_storm_unique$year)

# Subset to Texas hurricane/tropical storm declarations only
dd_tx <- dd_df %>%
  filter(
  #  incidentType %in% c("Hurricane", "Tropical Storm", "Typhoon"),
    state == "TX"
  ) 

# subset to hurricanes/flooding/tropical storm/typhoon
dd_tx %<>%
  filter(str_detect(declarationTitle, "HURRICANE|TROPICAL STORM|FLOOD|EXCESSIVE RAIN")) %>%
  mutate(
    # Extract storm name from declarationTitle (e.g. "HURRICANE RITA" -> "RITA")
    storm_name_raw = toupper(trimws(
      str_remove(declarationTitle, regex("^(HURRICANE|TROPICAL STORM|FLOOD|EXCESSIVE RAIN)\\s+", ignore_case = TRUE))
    )),
    incidentBeginDate = as.Date(substr(incidentBeginDate, 1, 10)),
    # Build full 5-digit county FIPS: state (2) + county (3, zero-padded)
    fips_county = paste0(
      str_pad(fipsStateCode, 2, pad = "0"),
      str_pad(fipsCountyCode, 3, pad = "0")
    )
  )

# get the storm name in a unique column 
school_storm_unique %<>% mutate(hurr_name = str_replace(storm_year_id, 
                                                        pattern = "\\_[0-9]{4}", 
                                                        replacement = "") %>% trimws()) 
# create giant regex
regex_hurr <- paste0(unique(school_storm_unique$hurr_name)[-1], collapse = "|")

# find the storm names in dd_tx
dd_tx %<>% mutate(storm_name_found = str_extract(declarationTitle, pattern = regex_hurr))


# Load Texas county shapefile via tigris
tx_counties <- tigris::counties(state = "TX", cb = TRUE, resolution = "5m") %>%
  st_transform(crs = target_crs) %>%
  mutate(fips_county = paste0(STATEFP, COUNTYFP))

# Spatial join: assign each school its county FIPS
# (schools already have point geometry in school_sf)
 school_storm_unique %<>%
  st_join(tx_counties %>% dplyr::select(fips_county),
    join = st_within) 

# Flag: was the school's county a declared disaster county for that storm?
school_storm_unique %<>% mutate(dd_flag = ifelse(hurr_name %in% dd_tx$storm_name_found[!is.na(dd_tx$storm_name_found)],
                                                 1,0)) 

### Save -----------------------------------------------------------
save(school_storm_unique, file = "intermediates/school_storm_unique 2026 06 08.Rda")
