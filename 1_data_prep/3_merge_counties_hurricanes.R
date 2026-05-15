# ==================================
# Author: MV
# Date: Updated 11/6/25
# Project: Hurricanes + Counties
# Description: merge counties with hurricane tracks 
# ==================================

### Setup --------------------------------------------------------
gc()
rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

# load in packages
package_list <- c("dplyr", "magrittr", "foreign", "lmtest",
"tmap", "nlme", "plm", "zoo", "AER", "tidyr", "data.table",
"systemfit", "haven", "ggplot2", "stargazer", "lubridate",
"clubSandwich", "sandwich", "lfe", "Synth", "readstata13",
"locpol", "parallel", "stringr", "lfe", "sf", "rstudioapi")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) invisible(install.packages(new.packages))
invisible(lapply(package_list, library, character.only = TRUE))
rm(package_list, new.packages)


# set up working directory 
setwd(dirname(getActiveDocumentContext()$path))
setwd("../../Data/")

# load in ibtracs 
ibtracs <- sf::read_sf("inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp")

# get the shapefile of texas counties 
texas_cntys <- tigris::counties() %>% filter(STATEFP == "48")
texas_cntys %<>% dplyr::select(fips = GEOID, geometry)

# filter ibtracs again (to ever overlapped with texas)
## first, filter to the hurricanes that intersect with texas at all 
st_crs(texas_cntys)
st_crs(ibtracs)
texas_cntys %<>% st_transform(crs = st_crs(ibtracs))
ibtracs_tx <- ibtracs %>% st_filter(texas_cntys)

## next, filter to post 1989 hurricanes 
ibtracs_tx %<>% filter(year >= 1989)

## keep specific columns 
ibtracs_tx %<>% dplyr::select(SID, SEASON, NAME, ISO_TIME, 
                              NATURE, LAT, LON,USA_LAT, USA_LON, 
                              USA_WIND, USA_SSHS, USA_RMW,
                              geometry)

## st join the counties with the hurricanes that overlap it: will get n:n data 
texas_ibtracs <- st_join(texas_cntys, ibtracs_tx, join = st_intersects)
# texas_ibtracs_91 <- st_join(texas_cntys, ibtracs_tx_1991, join = st_intersects)
ibtracs_texas <- st_join(ibtracs_tx, texas_cntys, join = st_intersects)


# load in the commuting zone (cz) data 
cz <- readxl::read_excel("inputs/USDA Commuting zones/cz_2000")
cz %<>% 
  filter(substr(FIPS,1,2) == "48") %>% 
  dplyr::select(fips = FIPS, cz_2000 = `Commuting Zone ID, 2000`) 

# merge in cz data
texas_ibtracs %<>% left_join(cz)
ibtracs_texas %<>% left_join(cz)

# save
save(ibtracs_texascnty, file = "intermediates/ibtracs_texascounties.Rda")
save(texas_ibtracs, file = "intermediates/texascounties_ibtracs.Rda")
