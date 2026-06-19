# ==================================
# Author: Mythili Vinnakota
# Date: 12/15/25
# Project: 
# Description: First stage- do hurricanes change building permit volume?
# ==================================

### Setup --------------------------------------------------------

options(stringsAsFactors = FALSE)

# set up environment 
setwd("~/Library/CloudStorage/Box-Box/Natural_Disasters_and_Human_Capital/Data/")
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

# load in the permits data 
load("inputs/Census/county_building_permits.Rds")

# load in the county level hurricane data 
load("intermediates/texascounties_ibtracs.Rda")

### Clean data -----------------------------------------------------

# make a month/ year variable 
texas_ibtracs %<>% mutate(date = as.POSIXct(ISO_TIME, 
                            format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                          year = format(date, "%Y"),
                          month = format(date, "%m"))


### Analysis -------------------------------------------------------


### Export results -------------------------------------------------



