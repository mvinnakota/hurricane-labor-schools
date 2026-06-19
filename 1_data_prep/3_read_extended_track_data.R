# ==================================
# Author: Mythili Vinnakota
# Date: 5/19/26
# Project: Texas Hurricanes + Schools 
# Description: Read HURDAT data that has wind radii 
# ==================================

### Setup --------------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")


# Load in data -----------------------------------------------------------------
# load in school-hurr data
load("intermediates/school_hurr_treatment.Rda")

# # EBTRAK data for pre-2004 guesses 
# ebtrak_df <- read.delim("~/Downloads/EBTRK_AL_final_1851-2021_new_format_02-Sep-2022-1.txt",
#                         sep = " ", row.names = NULL)

# ── Column positions (1-indexed, inclusive) for the EBTRK new format ----------
# All lines are exactly 124 characters.
# Longitude is stored as degrees West (positive = west), per HURDAT convention.
# This script converts lon to standard degrees East (negative = west).
#
# Wind radii are packed 3 chars per quadrant with no separator between quadrants
# within a group, so fixed-width parsing is required (read.table won't work).

col_pos <- readr::fwf_positions(
  start = c(
    1,   # storm_id
    10,  # name
    22,  # datetime (MMDDHH)
    29,  # year
    33,  # lat
    39,  # lon_W
    46,  # vmax_kt
    50,  # mslp_hpa
    55,  # rmw_nm
    59,  # eye_diam_nm
    63,  # roci_press_hpa
    68,  # roci_rad_nm
    # 34-kt radii (NE/SE/SW/NW), packed 3 chars each; group starts at 73
    73, 76, 79, 82,
    # 50-kt radii; group starts at 86
    86, 89, 92, 95,
    # 64-kt radii; group starts at 99
    99, 102, 105, 108,
    112, # storm_type
    113, # dist_land_km
    119  # source_code (4-digit code indicating data source for structure fields)
  ),
  end = c(
    8,   # storm_id
    21,  # name
    27,  # datetime
    32,  # year
    38,  # lat
    45,  # lon_W
    49,  # vmax_kt
    54,  # mslp_hpa
    58,  # rmw_nm
    62,  # eye_diam_nm
    67,  # roci_press_hpa
    71,  # roci_rad_nm
    75, 78, 81, 84,   # r34
    88, 91, 94, 97,   # r50
    101, 104, 107, 110, # r64
    112, # storm_type
    118, # dist_land_km
    124  # source_code
  ),
  col_names = c(
    "storm_id", "name", "datetime", "year", "lat", "lon_W",
    "vmax_kt", "mslp_hpa", "rmw_nm", "eye_diam_nm",
    "roci_press_hpa", "roci_rad_nm",
    "r34_NE_nm", "r34_SE_nm", "r34_SW_nm", "r34_NW_nm",
    "r50_NE_nm", "r50_SE_nm", "r50_SW_nm", "r50_NW_nm",
    "r64_NE_nm", "r64_SE_nm", "r64_SW_nm", "r64_NW_nm",
    "storm_type", "dist_land_km", "source_code"
  )
)

# read in the raw fixth width file 
ebtrk_raw <- readr::read_fwf(
  "~/Downloads/EBTRK_AL_final_1851-2021_new_format_02-Sep-2022-1.txt",
  col_positions = col_pos,
  col_types = readr::cols(
    storm_id       = col_character(),
    name           = col_character(),
    datetime       = col_character(),
    year           = col_integer(),
    lat            = col_double(),
    lon_W            = col_double(),
    vmax_kt        = col_integer(),
    mslp_hpa       = col_integer(),
    rmw_nm         = col_integer(),
    eye_diam_nm    = col_integer(),
    roci_press_hpa = col_integer(),
    roci_rad_nm    = col_integer(),
    r34_NE_nm = col_integer(), r34_SE_nm = col_integer(),
    r34_SW_nm = col_integer(), r34_NW_nm = col_integer(),
    r50_NE_nm = col_integer(), r50_SE_nm = col_integer(),
    r50_SW_nm = col_integer(), r50_NW_nm = col_integer(),
    r64_NE_nm = col_integer(), r64_SE_nm = col_integer(),
    r64_SW_nm = col_integer(), r64_NW_nm = col_integer(),
    storm_type   = col_character(),
    dist_land_km = col_integer(),
    source_code  = col_character()
  )
)

# filter to 1990 onwards 
ebtrak_df <- ebtrk_raw %>% filter(year >= 1990) 

# clean up some columns like date, remove the -99s 
ebtrak_df %<>%
  mutate(
    name        = trimws(name),
    storm_type  = trimws(storm_type),
    source_code = trimws(source_code),
    # Parse MMDDHH into components
    month = as.integer(substr(datetime, 1, 2)),
    day   = as.integer(substr(datetime, 3, 4)),
    hour  = as.integer(substr(datetime, 5, 6)),
    lon = -lon_W,
    # Assemble UTC datetime
    obs_time = make_datetime(year, month, day, hour, tz = "UTC"),
    # Replace EBTRK missing flag (-99) with NA across meteorological fields.
    # Note: 0 in wind radii means winds don't reach that speed/distance; it is
    # NOT a missing flag. Wind radii are only available from 1988 onwards.
    across(
      c(vmax_kt, mslp_hpa, rmw_nm, eye_diam_nm,
        roci_press_hpa, roci_rad_nm,
        r34_NE_nm:r64_NW_nm),
      ~ na_if(., -99L)
    )
  ) %>%
  dplyr::select(-datetime) %>%
  relocate(
    storm_id, name, obs_time, year, month, day, hour,
    lat, lon, vmax_kt, mslp_hpa, storm_type,
    rmw_nm, eye_diam_nm, roci_press_hpa, roci_rad_nm,
    starts_with("r34"), starts_with("r50"), starts_with("r64"),
    dist_land_km, source_code
  )


# check that the ibtracs data and the hurdat2 data overlap
table(unique(school_hurr$name %in% ebtrak_df$name)) # they are all in there 

# # filter EBTRAK to only hurricanes we are studying 
# ebtrak_df %<>% filter(name %in% school_hurr$name)


save(ebtrak_df, file = "intermediates/ebtrak_df.Rda")

