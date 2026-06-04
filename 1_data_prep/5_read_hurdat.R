# ==================================
# Author: Mythili Vinnakota
# Date: 5/19/26
# Project: Texas Hurricanes + Schools 
# Description: Read HURDAT data that has wind radii 
# ==================================

### Setup --------------------------------------------------------

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

# set up environment 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../../")



# --- Read raw lines and strip HTML wrapper ---
filepath <- "inputs/HURDAT/hurdat2.html"
raw <- readLines(filepath, warn = FALSE)

# Remove HTML tags and keep only non-blank lines
raw <- gsub("<[^>]+>", "", raw)       # strip any HTML tags
raw <- trimws(raw)
raw <- raw[nchar(raw) > 0]

# --- Identify header vs. data lines and drop anything else ---
# Header lines start with "AL" (Atlantic) or "EP" (East Pacific), etc.
# Data lines start with a date (digit). Drop stray lines (e.g. HTML title text).
is_header <- grepl("^[A-Z]{2}[0-9]", raw)
is_data   <- grepl("^[0-9]", raw)
raw <- raw[is_header | is_data]
is_header <- grepl("^[A-Z]{2}[0-9]", raw)

# --- Parse everything into a list of data frames ---
results <- vector("list", sum(is_header))
storm_idx <- 0

for (i in seq_along(raw)) {
  if (is_header[i]) {
    # Parse header: "AL092021,            IDA,     40,"
    parts <- trimws(strsplit(raw[i], ",")[[1]])
    storm_idx <- storm_idx + 1
    current_id   <- parts[1]
    current_name <- parts[2]
    data_rows    <- list()
    row_idx      <- 0
  } else {
    # Parse data line — split on comma
    vals <- trimws(strsplit(raw[i], ",")[[1]])
    row_idx <- row_idx + 1
    
    # Date (YYYYMMDD) and Time (HHMM)
    date_str <- vals[1]
    time_str <- vals[2]
    
    # Record identifier and Status
    record_id  <- vals[3]
    status     <- vals[4]
    
    # Latitude: "29.1N" -> +29.1, "10.5S" -> -10.5
    lat_raw <- vals[5]
    lat_val <- as.numeric(sub("[NS]$", "", lat_raw))
    if (grepl("S$", lat_raw)) lat_val <- -lat_val
    
    # Longitude: "90.2W" -> +90.2 (deg W), "30.0E" -> -30.0
    # Convention: store as signed with West positive (standard for Atlantic)
    #   ... OR store as standard geographic (West negative).
    # We'll use standard geographic: West = negative, East = positive.
    lon_raw <- vals[6]
    lon_val <- as.numeric(sub("[EW]$", "", lon_raw))
    if (grepl("W$", lon_raw)) lon_val <- -lon_val
    
    # Remaining numeric fields
    max_wind     <- as.integer(vals[7])
    min_pressure <- as.integer(vals[8])
    r34_ne <- as.integer(vals[9])
    r34_se <- as.integer(vals[10])
    r34_sw <- as.integer(vals[11])
    r34_nw <- as.integer(vals[12])
    r50_ne <- as.integer(vals[13])
    r50_se <- as.integer(vals[14])
    r50_sw <- as.integer(vals[15])
    r50_nw <- as.integer(vals[16])
    r64_ne <- as.integer(vals[17])
    r64_se <- as.integer(vals[18])
    r64_sw <- as.integer(vals[19])
    r64_nw <- as.integer(vals[20])
    rmw    <- as.integer(vals[21])
    
    data_rows[[row_idx]] <- data.frame(
      storm_id         = current_id,
      storm_name       = current_name,
      date             = date_str,
      time             = time_str,
      record_id        = record_id,
      status           = status,
      lat              = lat_val,
      lon              = lon_val,
      max_wind_kt      = max_wind,
      min_pressure_mb  = min_pressure,
      r34_ne_nm        = r34_ne,
      r34_se_nm        = r34_se,
      r34_sw_nm        = r34_sw,
      r34_nw_nm        = r34_nw,
      r50_ne_nm        = r50_ne,
      r50_se_nm        = r50_se,
      r50_sw_nm        = r50_sw,
      r50_nw_nm        = r50_nw,
      r64_ne_nm        = r64_ne,
      r64_se_nm        = r64_se,
      r64_sw_nm        = r64_sw,
      r64_nw_nm        = r64_nw,
      rmw_nm           = rmw,
      stringsAsFactors  = FALSE
    )
    
    # Store when we've collected all rows for this storm
    results[[storm_idx]] <- data_rows
  }
}

# --- Combine into one data frame ---
hurdat2 <- do.call(rbind, unlist(results, recursive = FALSE))

# --- Replace missing-value sentinels with NA ---
# -999 is used for pressure, wind radii, and RMW
# -99  is used for wind speed (rare, 1967 non-developing TDs)
hurdat2[hurdat2 == -999] <- NA
hurdat2[hurdat2 == -99]  <- NA

# --- Parse date/time into useful columns ---
hurdat2$year  <- as.integer(substr(hurdat2$date, 1, 4))
hurdat2$month <- as.integer(substr(hurdat2$date, 5, 6))
hurdat2$day   <- as.integer(substr(hurdat2$date, 7, 8))
hurdat2$hour  <- as.integer(substr(hurdat2$time, 1, 2))
hurdat2$min   <- as.integer(substr(hurdat2$time, 3, 4))

# --- Optional: create a proper datetime column ---
hurdat2$datetime <- as.POSIXct(
  paste0(hurdat2$date, sprintf("%04s", hurdat2$time)),
  format = "%Y%m%d%H%M", tz = "UTC"
)

# get year from date 
hurdat2 %<>% mutate(year = substr(date,1,4))

# filter to hurricanes only 
hurdat2 %<>% filter(status == "HU")

# load in the school hurricane treatment data
load("intermediates/school_hurr_treatment.Rda")


# check that the ibtracs data and the hurdat2 data overlap
table(unique(school_hurr$name %in% hurdat2$storm_name)) # they are all in there 

# filter hurdat2 to only hurricanes we are studying 
hurdat_df <- hurdat2 %>% filter(storm_name %in% school_hurr$name)
hurdat_df %<>% filter(year > 1989)



# EBTRAK data for pre-2004 guesses 
ebtrak_df <- read.delim("~/Downloads/EBTRK_AL_final_1851-2021_new_format_02-Sep-2022-1.txt",
                        sep = " ", row.names = NULL)

# ── Column positions (1-indexed, inclusive) for the EBTRK new format ----------
# All lines are exactly 124 characters.
# Longitude is stored as degrees West (positive = west), per HURDAT convention.
# This script converts lon to standard degrees East (negative = west).
#
# Wind radii are packed 3 chars per quadrant with no separator between quadrants
# within a group, so fixed-width parsing is required (read.table won't work).

library(readr)

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
    "storm_id", "name", "datetime", "year", "lat", "lon",
    "vmax_kt", "mslp_hpa", "rmw_nm", "eye_diam_nm",
    "roci_press_hpa", "roci_rad_nm",
    "r34_NE_nm", "r34_SE_nm", "r34_SW_nm", "r34_NW_nm",
    "r50_NE_nm", "r50_SE_nm", "r50_SW_nm", "r50_NW_nm",
    "r64_NE_nm", "r64_SE_nm", "r64_SW_nm", "r64_NW_nm",
    "storm_type", "dist_land_km", "source_code"
  )
)


ebtrk_raw <- readr::read_fwf(
  "~/Downloads/EBTRK_AL_final_1851-2021_new_format_02-Sep-2022-1.txt",
  col_positions = col_pos,
  col_types = readr::cols(
    storm_id       = col_character(),
    name           = col_character(),
    datetime       = col_character(),
    year           = col_integer(),
    lat            = col_double(),
    lon            = col_double(),
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

ebtrak_df <- ebtrk_raw %>% filter(year > 1990) 


ebtrak_df %<>%
  mutate(
    name        = trimws(name),
    storm_type  = trimws(storm_type),
    source_code = trimws(source_code),
    # Parse MMDDHH into components
    month = as.integer(substr(datetime, 1, 2)),
    day   = as.integer(substr(datetime, 3, 4)),
    hour  = as.integer(substr(datetime, 5, 6)),
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

# filter EBTRAK to only hurricanes we are studying 
ebtrak_df %<>% filter(name %in% school_hurr$name)


