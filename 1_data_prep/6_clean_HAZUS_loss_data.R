
# Prep for Loss Regression

# run a tract-level regression of loss values ~ direct hit + indirect hit + year FE + tract FE 
# this will show whether there were a lot of losses in our definition of indirect tracts 
# if we find that direct hit tracts losses >>> indirect tract losses, that validates our research design

### Setup --------------------------------------------------------
library(rstudioapi)
library(tigris)
library(sf)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")

### Load in loss data ----------------------------------------------

file_list <- list.files("inputs/HAZUS Loss Data/")

hazus_loss <- bind_rows(lapply(file_list, function(f) {
  df <- read.csv(file.path("inputs/HAZUS Loss Data/", f),
                 stringsAsFactors = FALSE)
  df$storm <- sub("^Results_", "", tools::file_path_sans_ext(f))
  df
}))

# filter to only texas 
hazus_loss %<>% filter(substr(tract,1,2) == "48")

# add storm year
storm_years <- c(
  Bret = 1999, Claudette = 2003, Rita = 2005,
  Dolly = 2008, Ike = 2008, Harvey = 2017)
hazus_loss <- hazus_loss %>%
  mutate(year = storm_years[storm])

### Load storm and cz data, shapefiles ------------------------------------------------

# texas storms 
load("intermediates/wind_sectors_sf.Rda")
wind_sectors_sf %<>% mutate(storm_year = paste0(name, "_", year)) %>% 
  filter(storm_year %in% c("BRET_1999", "CLAUDETTE_2003", "RITA_2005", 
                           "DOLLY_2008", "IKE_2008", "HARVEY_2017"))

# commuting zone data 
cz_2000 <- readxl::read_excel("inputs/USDA Commuting zones/cz_2000") %>%
  filter(substr(FIPS, 1, 2) == "48") %>%                  # Texas only
  select(county_fips = FIPS,
         cz_2000     = `Commuting Zone ID, 2000`,
         cz_1990     = `Commuting Zone ID, 1990`)

# load in texas shapefiles 
# 2010 matches up with Ike, Rita, Harvey, Dolly
tx_tracts_2010 <- tracts(state = "TX", year = 2010, cb = FALSE)
tx_tracts_2010 %<>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>% 
  rename(tract = GEOID10)

# 2020 matches up with Bret and Claudette
tx_tracts_2020 <- tracts(state = "TX", year = 2020, cb = FALSE)
tx_tracts_2020 %<>% mutate(fips = paste0(STATEFP, COUNTYFP)) %>% 
  rename(tract = GEOID)
                      
# check compatibility with hazus
table(hazus_loss$tract %in% tx_tracts_2010$tract, hazus_loss$storm) %>% prop.table(2)
table(hazus_loss$tract %in% tx_tracts_2020$tract, hazus_loss$storm) %>% prop.table(2)

# create one dissolved 64-kt footprint per storm
wind_sectors_sf %<>% st_make_valid

winds64 <- wind_sectors_sf %>%
  filter(wind_speed_kt == 64) %>%
  group_by(name) %>%
  summarise(.groups = "drop")     # unions all obs_times/quadrants per storm

winds50 <- wind_sectors_sf %>%
  filter(wind_speed_kt == 50) %>%
  group_by(name) %>%
  summarise(.groups = "drop")     # unions all obs_times/quadrants per storm


### figure out which tracts were exposed directly or indirectly -------

# pull out the 2010 and 2020 census tracts we need 
tx_tracts_2010 %<>% dplyr::select(tract, geometry) %>% filter(tract %in% hazus_loss$tract) %>% mutate(tract_year = "2010")
tx_tracts_2020 %<>% dplyr::select(tract, geometry) %>% filter(tract %in% hazus_loss$tract) %>% mutate(tract_year = "2020")
tx_tracts_full <- rbind(tx_tracts_2010, tx_tracts_2020) %>% unique()

# if there's identical tracts keep the 2020
tx_tracts_full %<>% arrange(desc(tract_year)) %>%     
  distinct(tract, .keep_all = TRUE)

# Project everything to an equal-area CRS so area math is valid 
tx_tracts_full %<>% st_transform(crs = st_crs(wind_sectors_sf)) %>% st_make_valid()


# merge with hazus loss 
hazus_loss %<>% merge(tx_tracts_full, by = "tract")
hazus_loss <- st_as_sf(hazus_loss)
hazus_loss %<>% mutate(storm = toupper(storm))


processHurricane <- function(hurr_name){
  
  print(hurr_name)
  
  # get hazus for that one storm
  hazus <- hazus_loss %>% filter(storm == hurr_name)
  
  # get that storm shape file 
  winds64_hurr <- winds64 %>% filter(name == hurr_name) %>% st_make_valid()
  winds50_hurr <- winds50 %>% filter(name == hurr_name)
  
  # calc overlap area if it exists for hazus and 64kt winds 
  sf::sf_use_s2(FALSE)
  hazus_overlap_64 <- st_filter(hazus, winds64_hurr) 
  hazus_overlap_64 %<>% mutate(
    direct_64 = 1,
    overlap_64_area = st_area(hazus_overlap_64, winds64_hurr) %>% as.numeric)
  hazus_overlap_64 %<>% st_drop_geometry %>% dplyr::select(tract, direct_64, overlap_64_area)
  
  hazus_overlap_50 <- st_filter(hazus, winds50_hurr) 
  hazus_overlap_50 %<>% mutate(
    direct_50 = 1,
    overlap_50_area = st_area(hazus_overlap_50, winds50_hurr) %>% as.numeric)
  hazus_overlap_50 %<>% st_drop_geometry %>% dplyr::select(tract, direct_50, overlap_50_area)
  
  # merge back in
  hazus %<>% left_join(hazus_overlap_64, by = "tract") %>% 
    left_join(hazus_overlap_50, by = "tract")
  
  return(hazus)
}

hazus_loss_final <- lapply(unique(hazus_loss$storm), processHurricane) %>% bind_rows()


# add in CZ info
hazus_loss_final %<>% mutate(fips = substr(tract,1,5))
hazus_loss_final %<>% merge(cz_2000, by.x = "fips", by.y = "county_fips", all.x = T)


### Export data -------------------------------------------------------------

save(hazus_loss_final, file = "intermediates/hazus_loss_cleaned.Rda")
