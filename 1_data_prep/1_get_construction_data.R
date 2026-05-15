###########################################
# Set up
###########################################
library(sf)
library(tidyverse)
library(tidycensus)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(jsonlite)
library(magrittr)
library(rvest)
library(rstudioapi)
library(purrr)
library(rstudioapi)
library(parallel)


###########################################
# Load Census Data on Construction Permits
###########################################
# set up working directory 
setwd(dirname(getActiveDocumentContext()$path))
setwd("../../../Data/")

###########################################
# Census Building Permits
###########################################
# Documentation
# https://www2.census.gov/econ/bps/Documentation/

# Create function to read file URLs
Read_Census <- function(url, county=T){
  print(url)
  print(grep(url, file_urls)/length(file_urls))
  
  # Read first two header lines properly as CSV
  headers <- suppressWarnings(read_csv(url, n_max = 3, col_names = FALSE, show_col_types = FALSE))
  
  h1 <- as.character(headers[1, ])
  h2 <- as.character(headers[2, ])
  
  # Combine the two header rows element-wise
  paste_rows <- paste0(str_trim(h1), "_", str_trim(h2)) %>% str_remove("_NA")
  
  # Only keep everything before buildings
  first_rows <- c(1:grep("Bldgs",h2)[1]-1)
  id_vars <- paste_rows[first_rows]
  data_vars <- c(paste0(c("bldgs","units","value"),"_1_units"),
                        paste0(c("bldgs","units","value"),"_2_units"),
                        paste0(c("bldgs","units","value"),"_3_4_units"),
                        paste0(c("bldgs","units","value"),"_5_plus_units"))
  
  if(county){data_vars <- c(paste0(data_vars,"_imputed"), data_vars)}
  
  # Skip the first two header rows (they describe column meanings)
  df <- read_csv(url, skip = 2, col_names = FALSE, show_col_types = FALSE)
  
  # Clean up column names
  colnames(df) <- c(id_vars, data_vars)
  
  # change variable types
  df %<>% mutate(across(any_of(id_vars), as.character),
                 across(any_of(data_vars), as.numeric))
  df %<>% select(any_of(id_vars), any_of(data_vars))
  df %<>% janitor::clean_names()
  
  return(df)
}


###########################################
# County level
###########################################
data_url <- "https://www2.census.gov/econ/bps/County/" # County

# Read the HTML page and extract links ending in .txt
files <- read_html(data_url) %>% html_elements("a") %>% html_attr("href") %>% str_subset("\\.txt$") 

# Convert to full file URLs
file_urls <- paste0(data_url, files)

# Use function to read in all Monthly files
monthly_files <- str_subset(file_urls, "c\\.txt$") 
census_permits <- monthly_files %>% lapply(Read_Census)
census_permits %<>% bind_rows()

# Filter for Texas (state_code == 48)
census_permits %<>% filter(fips_state == 48)

# clean month and year
census_permits$year <- substr(census_permits$survey_date, 1, 4)
census_permits$month <- substr(census_permits$survey_date, 5, 6)

# clean state county fips variable
census_permits %<>% mutate(fips_state_county = paste0(fips_state,fips_county))

# Merge population estimates
tx_pop <- get_acs(
  geography = "county",
  variables = "B01003_001", # total population
  year = 2010,
  state = "TX"
) %>%
  select(fips_state_county = GEOID, population_2010 = estimate)
# Merge with census data
census_permits %<>% left_join(tx_pop)


# ###########################################
# # Place level
# ###########################################
# data_url <- "https://www2.census.gov/econ/bps/Place/South%20Region/" # Place
# # Read the HTML page and extract links ending in .txt
# files <- read_html(data_url) %>% html_elements("a") %>% html_attr("href") %>% str_subset("\\.txt$") 
# 
# # Convert to full file URLs
# file_urls <- paste0(data_url, files)
# 
# # Use function to read in all Monthly files
# monthly_files <- str_subset(file_urls, "c\\.txt$") 
# census_place_permits1 <- monthly_files[1:199] %>% lapply(Read_Census, county=F) 
# census_place_permits2 <- monthly_files[200:349] %>% lapply(Read_Census, county=F)
# census_place_permits3 <- monthly_files[350:452] %>% lapply(Read_Census, county=F) 
# census_place_permits <- bind_rows(bind_rows(census_place_permits1), 
#                                   bind_rows(census_place_permits2), 
#                                   bind_rows(census_place_permits3))
# 
# # Filter for Texas (state_code == 48)
# census_place_permits %<>% filter(state_code == 48)
# 
# # clean month and year
# census_place_permits$year <- substr(census_place_permits$survey_date, 1, 4)
# census_place_permits$month <- substr(census_place_permits$survey_date, 5, 6)
# 
# # clean state county fips variable
# census_place_permits %<>% mutate(fips_state_place = paste0(fips_state, fips_place))
# 
# # Merge population estimates
# tx_pop_place <- get_acs(
#   geography = "place",
#   variables = "B01003_001", # total population
#   year = 2010,
#   state = "TX"
# ) %>%
#   select(fips_state_place = GEOID, population_2010 = estimate)
# 
# # Merge with census data
# census_place_permits %<>% left_join(tx_pop)


###########################################
# Save Census Data
###########################################
save(census_permits, census_place_permits, file="intermediates/building_permits.Rds")






###########################################
# BLS QCEW Construction Employment and Wages
###########################################
# https://www.bls.gov/cew/downloadable-data-files.htm
# Note: pre-2000, BLS used SIC codes and after they use NAICS codes
# We use BLS-retroclassified NAICS construction series for all years.
# This means fewer counties were available pre 2000
# we may want to subset to counties that were observable across our entire time series

# Also, 1990-2000, we can only observe construction wages for private companies at
# the county level. post-2000, we can observe state and local government wages as well

read_qcew_construction <- function(zipurl) {
  options(timeout = 600)
  
  files <- unzip(zipurl, list = TRUE)$Name
  construction_files <- files[grepl("23 Construction", files, ignore.case = TRUE)]
  
  df <- map_dfr(
    construction_files,
    ~ read_csv(unz(zipurl, .x), show_col_types = FALSE)
  )
 return(df) 
}

# Read Data
bls_files <- list.files("inputs/BLS/", full.names=T)
construction_wages <- bls_files %>% lapply(read_qcew_construction) %>% bind_rows()

# clean variables
construction_wages %<>% rename("fips_state_county"="area_fips")
construction_wages$fips_state = substr(construction_wages$fips_state_county, 1, 2)

# subset to only texas counties
construction_wages %<>% subset(fips_state == 48)
# drop the entire state counts
construction_wages %<>% subset(fips_state_county != 48000)
# drop variables that are constant
construction_wages %<>% select(where(~ n_distinct(., na.rm = F) > 1))
# drop suppressed data
construction_wages %<>% subset(is.na(disclosure_code))




###########################################
# quarterly data
###########################################
qrty_wages <- construction_wages %>% 
  select(fips_state_county, area_title, own_title, own_code, industry_title,
         year, qtr, qtrly_estabs_count, total_qtrly_wages, avg_wkly_wage)



###########################################
# Monthly data
###########################################
mthly_employment <- construction_wages %>% 
  select(
    fips_state_county, area_title,
    own_title, own_code, industry_title,
    year, qtr, month1_emplvl, month2_emplvl, month3_emplvl
  ) %>%
  pivot_longer(
    cols = starts_with("month"),
    names_to = "month",
    values_to = "emplvl"
  ) %>%
  # extract month number (1, 2, 3)
  mutate(
    month = parse_number(month),
    # convert quarter + within-quarter month to calendar month (1–12)
    calendar_month = (qtr - 1) * 3 + month
  ) %>%
  select(fips_state_county, area_title,
         own_title, own_code, industry_title,
         year, qtr, "qrt_month"="month", calendar_month, "employee_count"="emplvl")



###########################################
# Save 
###########################################
save(qrty_wages, mthly_employment, file="intermediates/bls_wages_employment.Rdata")


load("intermediates/bls_wages_employment.Rdata")





