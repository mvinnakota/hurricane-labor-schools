# Options
gc()
rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

# packages
package_list <- c("dplyr", "magrittr", "foreign", "lmtest", "tmap", "nlme", "ggridges",
                  "plm", "zoo", "AER", "tidyr", "data.table", "systemfit", "ggrepel",
                  "haven", "ggplot2", "stargazer", "lubridate", "clubSandwich",
                  "sandwich", "lfe", "readstata13", "locpol", "parallel", "forcats",    
                  "stringr", "sf", "rstudioapi", "viridis", "scales", "educationdata",
                  "zipcodeR", "readr", "geosphere")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if (length(new.packages)) invisible(install.packages(new.packages))
invisible(lapply(package_list, library, character.only = TRUE))
rm(package_list, new.packages)

