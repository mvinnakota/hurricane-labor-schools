###########################################
# Set up
###########################################
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")


###########################################
# Commuting Zones
###########################################
# Load commuting zone crosswalk  
cz_2000 <- readxl::read_excel("inputs/USDA Commuting zones/cz_2000") %>%
  filter(substr(FIPS, 1, 2) == "48") %>%                  # Texas only
  select(county_fips = FIPS,
         cz_2000     = `Commuting Zone ID, 2000`,
         cz_1990     = `Commuting Zone ID, 1990`)


###########################################
# School Locations (TEA)
###########################################
# Read in School Data for 2023-2024 SY
schools_geojson_url <- "https://services2.arcgis.com/5MVN2jsqIrNZD4tP/arcgis/rest/services/Schools_2023_to_2024/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
schools_sf <- st_read(schools_geojson_url, quiet = TRUE) %>% janitor::clean_names()
# select relevant variables
schools_sf %<>% select("nces_school_id"="user_nces_school_id","lat"="y", "long"="x")
# Clean id var
schools_sf %<>% mutate(nces_school_id = str_remove_all(nces_school_id,"\'"))
# Drop bad coordinates
schools_sf %<>% filter(
  !is.na(long) & !is.na(lat), 
  lat > -900 & long > -900, 
  lat != 0 & long != 0, 
  between(long, -106.65, -93.51),  # texas longitude bounds
  between(lat,   25.84,  36.50))   # texas latitude bounds)


###########################################
# School Locations (CCD)
###########################################
# Get CCD Data
years <- 1990:2023

ccd_data <- get_education_data(
  level   = "schools",
  source  = "ccd",
  topic   = "directory",
  filters = list(year = years, fips = 48), # 48 = Texas
  add_labels = FALSE
) 

# Rename relevant variables
ccd_data %<>% rename("nces_school_id"="ncessch",
                  "nces_district_id"="leaid", 
                  "tea_school_id"="seasch",
                  "x"="longitude" , 
                  "y"="latitude")

# clean school id var
ccd_data$tea_school_id %<>% str_remove("^.*?-\\s*")

# For variables that should be stable over time (like address)
stable_vars <- function(vars, data = ccd_data, method = c("modal", "first", "ever")) {
  method <- match.arg(method)
  
  data %<>%
    select(nces_school_id, year, all_of(vars)) %>%
    filter(!(if_any(all_of(vars), is.na) | 
               if_any(all_of(vars), ~ .x == -2) |
               if_any(all_of(vars), ~ .x == "")))
  
  if("x" %in% vars & "y" %in% vars){
    data %<>%
      # Drop bad coordinates
      filter(
        !is.na(y) & !is.na(x), 
        y > -900 & x > -900, 
        y != 0 & x != 0, 
        between(x, -106.65, -93.51),  # texas longitude bounds
        between(y,   25.84,  36.50))   # texas latitude bounds)
  }
  
  if (method == "modal") {
    data %<>%
      group_by(nces_school_id, across(all_of(vars))) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(nces_school_id) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      select(-n)
    
  } else if (method == "first") {
    data %<>%
      group_by(nces_school_id) %>%
      arrange(nces_school_id, year) %>%
      slice(1) %>%
      select(-year)
  }
  
  return(data)
}

# Select stable variables
# First instance
school_xy <- list(c("x", "y")) %>%
  lapply(stable_vars, method="first") %>%
  reduce(full_join, by = "nces_school_id") %>%
  rename("lat"="y", "long"="x") 

# Modal instance
school_vars <- list(
  "tea_school_id", 
  "nces_district_id",
  "school_name", 
  "school_type",
  "urban_centric_locale") %>%
  lapply(stable_vars, method="modal") %>%
  reduce(full_join, by = "nces_school_id")

school_type <- ccd_data %>%
  group_by(nces_school_id) %>%
  summarize(high_cedp = any(high_cedp == 1))

school_vars %<>% left_join(school_type)


###########################################
# Get School X and Y coordinates
###########################################
# imput missing x y with TEA 2023-2024 data
school_xy %<>% full_join(st_drop_geometry(schools_sf), by = "nces_school_id")

school_xy %<>% mutate(
  x = ifelse(!is.na(long.x), long.x, long.y),
  y = ifelse(!is.na(lat.x), lat.x, lat.y)
) %>% select(-starts_with("lat"), -starts_with("long"))

# merge in school vars
school_xy %<>% left_join(school_vars)


###########################################
# Add in County and CZ
###########################################
# # Convert schools to sf using lat/lon 
school_xy %<>% st_as_sf(coords = c("x", "y"), remove=FALSE, crs = 4326)

# Load Texas counties and spatial join to get county FIPS
tx_counties <- tigris::counties(state = "TX", cb = TRUE) %>%
  st_transform(crs = 4326) %>%
  select(county_fips = GEOID, county_name = NAME)

school_xy %<>% st_join(tx_counties, join = st_intersects, left=TRUE)

# Merge in commuting zones via county FIPS 
school_xy %<>% left_join(cz_2000, by = "county_fips")

# Merge in other variables
stop("Check whether this line is needed")
school_xy %<>% left_join(distinct(select(ccd_data, nces_school_id, high_cedp)))

# drop schools with missing ids
school_xy %<>% filter(nces_school_id != "NULL")

###########################################
# Save data
###########################################
save(school_xy, file="inputs/CCD/school_xy.Rds")




