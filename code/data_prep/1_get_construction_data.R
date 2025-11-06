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


###########################################
# Load Census Data on Construction Permits
###########################################
setwd("~/")
setwd("../Box/Research/Natural_Disasters_and_Human_Capital/Data/inputs/")

###########################################
# 1. Census Building Permits (County level)
###########################################
# Documentation
# https://www2.census.gov/econ/bps/Documentation/placeasc.pdf
# data_url <- "https://www2.census.gov/econ/bps/Place/South%20Region/" # Place
data_url <- "https://www2.census.gov/econ/bps/County/" # County

# Read the HTML page and extract links ending in .txt
files <- read_html(data_url) %>% html_elements("a") %>% html_attr("href") %>% str_subset("\\.txt$") 

# Convert to full file URLs
file_urls <- paste0(data_url, files)

# Create function to read file URLs
Read_Census <- function(url){
  
  # Read first two header lines properly as CSV
  headers <- read_csv(url, n_max = 3, col_names = FALSE, show_col_types = FALSE)
  
  h1 <- as.character(headers[1, ])
  h2 <- as.character(headers[2, ])
  
  # Combine the two header rows element-wise
  paste_rows <- paste0(str_trim(h1), "_", str_trim(h2)) %>% str_remove("_NA")
  
  # Only keep everything before buildings
  first_rows <- c(1:grep("Bldgs",h2)[1]-1)
  combined_names_1 <- paste_rows[first_rows]
  combined_names_2 <- c(paste0(c("bldgs","units","value"),"_1_units"),
                        paste0(c("bldgs","units","value"),"_2_units"),
                        paste0(c("bldgs","units","value"),"_3_4_units"),
                        paste0(c("bldgs","units","value"),"_5_plus_units"))
  
  
  # Skip the first two header rows (they describe column meanings)
  df <- read_csv(url, skip = 2, show_col_types = FALSE)
  
  # Clean up column names
  colnames(df) <- c(combined_names_1, combined_names_2)
  df %<>% janitor::clean_names()
  
  # change variable types
  df %<>% mutate(across(any_of(colnames(df[first_rows])), as.character),
                 across(any_of(combined_names_2), as.numeric))
  
  return(df)
}

# Use function to read in all Monthly files
census_permits <- str_subset(file_urls, "c\\.txt$") %>% lapply(Read_Census)
census_permits %<>% bind_rows()

# Filter for Texas (state_code == 48)
census_permits %<>% filter(state_code == 48)

# select relevant variables
census_permits %<>%
  select(
    all_of(c("survey_date", "state_code", "place_code", "place_name", "county_code", "zip_code")),
    matches("bldgs|units|value")
  )

save(census_permits, file="Census/county_building_permits.Rds")


###########################################
# Population (for per-capita normalization)
###########################################
# You can choose any year’s ACS or Population Estimates.
# For long-run panel, use decennial or interpolate.

tx_pop <- get_acs(
  geography = "county",
  variables = "B01003_001", # total population
  year = 2020,
  state = "TX"
) %>%
  select(county_fips = GEOID, county = NAME, population = estimate)




###########################################
# BLS QCEW Construction Employment and Wages
###########################################
# https://www.bls.gov/cew/downloadable-data-files.htm

get_qcew_tx <- function(y) {
  url <- paste0("https://data.bls.gov/cew/data/api/", y, "/county/48.csv") # 48 = TX
  message("Downloading QCEW for ", y)
  df <- suppressWarnings(read_csv(url, show_col_types = FALSE))
  
  df %>%
    filter(own_title == "Private", industry_code == "23") %>%  # NAICS 23 = Construction
    select(year, area_fips, area_title, annual_avg_emplvl, total_annual_wages)
}

tx_qcew_all <- map_dfr(c(1990:2023), get_qcew_tx)

tx_qcew <- tx_qcew_all %>%
  mutate(county_fips = str_sub(area_fips, 3, 5)) %>%
  group_by(county_fips, year) %>%
  summarize(
    construction_emp = mean(annual_avg_emplvl, na.rm = TRUE),
    construction_wages = mean(total_annual_wages, na.rm = TRUE),
    .groups = "drop"
  )



###########################################
# 4. Merge into a single panel
###########################################
tx_construction_panel <- tx_permits %>%
  left_join(tx_qcew, by = c("county_fips", "year")) %>%
  left_join(tx_pop, by = "county_fips") %>%
  mutate(
    permits_per_1000 = ifelse(!is.na(population) & population > 0,
                              total_units / population * 1000, NA),
    avg_wage_construction = construction_wages / construction_emp,
    log_construction_emp = log1p(construction_emp)
  )

###########################################
# 5. Save for later use
###########################################
write_csv(tx_construction_panel, "tx_construction_panel_1990_2023.csv")

