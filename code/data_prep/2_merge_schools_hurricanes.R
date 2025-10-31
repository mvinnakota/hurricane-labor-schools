# ==================================
# Author: Mythili Vinnakota
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

# pull in the school + district location information from code 1
load("~/Downloads/CCD-selected/district_sf.Rds")
load("~/Downloads/CCD-selected/school_info_ccd.Rds")
load("~/Downloads/CCD-selected/school_xy.Rds")

# pull in the ibtracs location data from MV other project - just from the ibtracs folder
ibtracs <- sf::read_sf("~/Dropbox/Projects/Firms Project/Analysis/inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp")

# get the shapefile of Texas 
texas <- tigris::states() %>% filter(STUSPS == "TX")

### CLEAN DATA ---------------------------------------------------------------

## first, filter to the hurricanes that intersect with texas at all 
st_crs(texas)
st_crs(ibtracs)
texas %<>% st_transform(crs = st_crs(ibtracs))
ibtracs_tx <- ibtracs %>% st_filter(texas)

## next, filter to post 1989 hurricanes 
ibtracs_tx %<>% filter(year >= 1989)

## keep specific columns 
ibtracs_tx %<>% dplyr::select(SID, SEASON, NAME, NATURE, LAT, LON,
                              USA_LAT, USA_LON, USA_WIND, USA_SSHS, USA_RMW)

# USA_RMW is pretty sparsely populated, especially prior to 2004 or so. 
# So we can use this as a robustness check later, but maybe ignore for now. 

### convert school data to sf - DROPPING 12% OF DATA DUE TO NO X/Y AND ONE WEIRDO THAT'S PROBABLY TYPO??
school_xy %<>% filter(!is.na(x)) %>% filter(y > -900)
school_sf <- school_xy %>% st_as_sf(coords = c("y", "x"), crs = st_crs(ibtracs_tx)) %>% ungroup()

### Match SCHOOLS WITH HURRICANE DATA  ---------------------------------------

# make list 
sid_list <- ibtracs_tx$SID %>% unique()

# Go through each SID. Calculate min distance of school to SID. 
# Return distance column.
processIbtracs <- function(i) {
  
  df <- data.frame(id = 1:nrow(school_sf))
  
  # get the ith SID
  print(sid_list[i])
  
  # subset to the storm
  ibtracs_sid <- ibtracs_tx %>% filter(SID == sid_list[i])
  
  # from chat gpt >>
  # Compute all distances (rows=points1, cols=points2)
  dist_matrix <- st_distance(school_sf, ibtracs_sid)
  
  # For each point in school_sf, get index of nearest point in ibtracs_sid
  nearest_hurr_point <- apply(dist_matrix, 1, which.min)
  
  # Extract the nearest point geometry
  nearest_hurr <- ibtracs_sid[nearest_hurr_point, ]
  
  # Optional: add distance as a column
  df[,paste0("dist_to_", sid_list[i])] <- apply(dist_matrix, 1, min)
  
  # Combine if you want attributes from nearest point
  df[,paste0("cat_", sid_list[i])] <- nearest_hurr$USA_SSHS
  df[,paste0("wind_", sid_list[i])] <- nearest_hurr$USA_WIND
  df[,paste0("rmw_", sid_list[i])] <- nearest_hurr$USA_RMW
  
  return(df %>% dplyr::select(-id))
}

distances_to_hurrs <- lapply(1:length(sid_list), processIbtracs) %>% bind_cols()

school_sf %<>% bind_cols(distances_to_hurrs)


# make from wide to long 
school_sf_long <- school_sf %>%
  pivot_longer(
    cols = matches("^(dist_to_|cat_|wind_|rmw_)"),  # all storm-related columns
    names_to = c(".value", "sid"),                 # split column names
    names_pattern = "(dist_to_|cat_|wind_|rmw_)?(\\d+[A-Z]\\d+)")

### Export results -------------------------------------------------

save(school_sf_long, file = "school_sf_with_hurricane_info.Rda")



