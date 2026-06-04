# ==================================
# Author: EH
# Date: Updated 05/26/26
# Project: Hurricanes + Schools
# Description: Write function to create stacked did panel
# ==================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
setwd("../../../")

### Load Data -------------------------------------------------------------------
load("intermediates/school_storm_treatment.Rda")


### Function ---------------------------------------------------------------
Build_Panel <- function(df, direct_var, indirect_var, years_since=7, pre_years=4, post_years=4){
  
  # Enforce mutual exclusivity: if a unit is directly treated, it cannot also be indirectly treated
  df[[indirect_var]] <- ifelse(df[[direct_var]] == 1, 0, df[[indirect_var]])
  
  # 1 if unit was ever directly or indirectly treated
  df$ever_direct_or_indirect <- as.integer(df[[direct_var]] == 1 | df[[indirect_var]] == 1)
  
  # For each school storm observation
  
  # Deal with cases where direct and indirect are not mutually exclusive
  
  # Subset to controls schools that have not had a storm in a certain number of years
  
  # Subset to the pre and post years
  
  # Stack
  

  return(stack)
}

# # Psuedo-Code Examples
# # Build Hurricane Panel
# hurricane_panel  <- df %>% subset(only_hurricanes) %>% Build_Panel
# 
# 
# # No never treated
# ever_treat_panel <- df %>% subset(ever_storm) %>% Build_Panel
# 
# 
# # Only include schools ever within 100 miles of a hurricane
# close_by_panel   <- df %>% subset(within_100) %>% Build_Panel
# 



