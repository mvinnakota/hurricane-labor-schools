###########################################
# Set up
###########################################

library(sf)
library(tidyverse)
library(educationdata)
library(readr)
library(magrittr)


###########################################
# District Shape Files
###########################################
# Read in School District Data for 2022-2023 SY
districts_geojson_url <- "https://services2.arcgis.com/5MVN2jsqIrNZD4tP/arcgis/rest/services/Current_Districts/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
district_sf <- st_read(districts_geojson_url, quiet = TRUE) %>% janitor::clean_names()
# select relevant variables

district_sf %<>% select("nces_district_id"="nces_distr", "tea_district_id"="district_c", "district_name"="name")


###########################################
# School Locations
###########################################
# Get CCD Data
years <- 1990:2023

tx_hs <- get_education_data(
  level   = "schools",
  source  = "ccd",
  topic   = "directory",
  filters = list(year = years, fips = 48), # 48 = Texas
  add_labels = FALSE
) 

# Filter to only high schools
tx_hs %<>% filter(high_cedp == 1)

# Rename relevant variables
tx_hs %<>% rename("nces_school_id"="ncessch",
                  "nces_district_id"="leaid", 
                  "district_name"="lea_name",
                  "x"="latitude", 
                  "y"="longitude")

# For variables that should be stable over time (like address)
# Find the modal set of values
modal_set <- function(vars, data = tx_hs) {
  data %<>%
    select(nces_school_id, all_of(vars)) %>%
    # drop rows where all vars are missing or not applicable (-2) or blank
    filter(!(if_all(all_of(vars), is.na) | 
               if_all(all_of(vars), ~ .x == -2) |
               if_all(all_of(vars), ~ .x == "")))
  # Group by variables and count the instances
  # then sort by the highest number of instances 
  # and select the most common
  data %<>%
    group_by(nces_school_id, across(all_of(vars))) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(nces_school_id) %>%
    arrange(desc(n)) %>%
    slice(1) %>%
    select(-n)
  
return(data)
}

# Select Stable variables
stable_vars <- list(
  c("x","y"), 
  "nces_district_id",
  "school_name", 
  "school_type",
  "charter",
  "magnet",
  "school_status",
  c("street_location", "city_location", "state_location", "zip_location"),
  "urban_centric_locale") %>%
  lapply(modal_set) %>%
  reduce(full_join, by = "nces_school_id")


# Select Yearly variables
yearly_vars <- tx_hs %>%
  select(nces_school_id, year, enrollment, teachers_fte, 
         contains("title_i_"), contains("lunch"))

# Merge stable and yearly variables
school_info_ccd <- right_join(stable_vars, yearly_vars) %>% select(-x, -y)
school_xy <- stable_vars %>% select(nces_school_id, nces_district_id, x, y)


###########################################
# Save data
###########################################
save(school_info_ccd, file="../Box/Natural_Disasters_and_Human_Capital/Data/CCD/school_info_ccd.Rds")
save(school_xy, file="../Box/Natural_Disasters_and_Human_Capital/Data/CCD/school_xy.Rds")
save(district_sf, file="../Box/Natural_Disasters_and_Human_Capital/Data/CCD/district_sf.Rds")





