# ==================================
# Author: EH
# Date: Updated 5/23/26
# Project: Hurricanes + Schools
# Description: summarize storms
# ==================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
source("../0_helper_functions/graph_themes.R")
setwd("../../../")

# =============================================================================
# 0. LOAD DATA
# =============================================================================
load("intermediates/texas_storms.Rda")
load("intermediates/school_storm_unique.Rda")
source("Code/hurricane-labor-schools/1_data_prep/5_build_stacked_did_panel.R")

# Clean Variables
storms_tx %<>%
 mutate(
   month = month(date),
   month_num = as.numeric(month),
   cat_fac = factor(max_cat_tx,
              levels = rev(-5:5),
              labels = rev(c(
                "Unknown",                  # -5 [XX]
                "Post-tropical",            # -4 [EX, ET, PT]
                "Miscellaneous disturbance",# -3 [WV, LO, DB, DS, IN, MD]
                "Subtropical",              # -2 [SS, SD]
                "Tropical Depression",      # -1 W < 34
                "Tropical Storm",           #  0 34 ≤ W < 64
                "Category 1",               #  1 64 ≤ W < 83
                "Category 2",               #  2 83 ≤ W < 96
                "Category 3",               #  3 96 ≤ W < 113
                "Category 4",               #  4 113 ≤ W < 137
                "Category 5"                #  5 W ≥ 137
              )
            )),
            .groups = "drop"
  )

# =============================================================================
# 1. storm_tx PER MONTH
# =============================================================================
storm_tx_by_month <- storms_tx %>%
  count(month, month_num, cat_fac, max_cat_tx) %>%
  arrange(month_num)

storm_tx_by_month %>%
  subset(!is.na(max_cat_tx)) %>%
ggplot(aes(x = month, y = n, fill = cat_fac)) +
  geom_col(color = "white", linewidth = 0.4) +
  labs(x = NULL, y = "Number of storm_tx") + 
  graph$hurr_theme
ggsave("outputs/figures/summary_stats/month_of_storm.png", height=4, width=5, units="in")

storm_tx_by_month %>%
  subset(max_cat_tx >0 & !is.na(max_cat_tx)) %>%
  ggplot(aes(x = month, y = n, fill = cat_fac)) +
  geom_col(color = "white", linewidth = 0.4) +
  labs(x = NULL, y = "Number of storm_tx") + 
  graph$hurr_theme
ggsave("outputs/figures/summary_stats/month_of_hurr.png", height=4, width=6, units="in")


# =============================================================================
# 2. storm_tx PER year (YEAR)
# =============================================================================
storm_tx_by_season <- storms_tx %>%
  count(storm_year, cat_fac, max_cat_tx, storm_name) %>%
  arrange(storm_year) %>%
  mutate(storm_year = as.numeric(as.character(storm_year))) 

storm_tx_by_season %>%
  filter(max_cat_tx > 0) %>%
  ggplot(aes(x = storm_year, y = cat_fac, color = cat_fac, label = storm_name)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text_repel(size = 2.8) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(
    breaks = seq(1989, 2021, 2),
    limits=c(1991, 2019),
    labels = function(x) paste0("\'", substr(x, 3, 4))) +
  labs(x = "Year", y = NULL) +
  graph$hurr_theme + 
  theme(legend.position = "none",
        panel.grid.major = element_line()) 
ggsave("outputs/figures/summary_stats/year_of_hurr_named.png",
       height = 3, width = 7, units = "in")


# hurricanes
storm_tx_by_season %>% 
  subset(max_cat_tx >0 & !is.na(max_cat_tx)) %>%
  ggplot(aes(x = storm_year, y = n, fill=cat_fac)) +
  geom_col(linewidth = 0.4) +
  labs(x = "year", y = "Number of Hurricanes") +
  graph$hurr_theme + 
  scale_y_continuous(breaks=c(0,1,2)) +
  scale_x_continuous(
    breaks=seq(1989, 2021, 2), 
    limits=c(1991, 2019),
    labels = function(x) paste0("\'",substr(x, 3, 4)))
ggsave("outputs/figures/summary_stats/year_of_hurr.png", height=4, width=9, units="in")


# storm_tx
storm_tx_by_season %>% 
  subset(!is.na(max_cat_tx)) %>%
  ggplot(aes(x = storm_year, y = n, fill=cat_fac)) +
  geom_col(linewidth = 0.4) +
  labs(x = "storm_year", y = "Number of storm_tx") +
  graph$hurr_theme + 
  scale_y_continuous(breaks=c(0:4)) +
  scale_x_continuous(breaks=seq(1989, 2021, 2), labels = function(x) paste0("\'",substr(x, 3, 4)))
ggsave("outputs/figures/summary_stats/year_of_storm.png", height=4, width=9, units="in")


# =============================================================================
# 7. STORMS PER COUNTY (high winds)
# =============================================================================
exposures_per_county <- school_storm_unique %>%
  group_by(county_fips, sid) %>%
  summarise(wind_64kt = any(wind_64kt)) %>%
  group_by(county_fips) %>%
  summarise(n_storm_tx = sum(wind_64kt)) %>%
  bind_rows(data.frame("county_fips"="48301", "n_storm_tx"=as.integer(0)))

ggplot(exposures_per_county, aes(x = n_storm_tx)) +
  geom_histogram(binwidth = 1, fill="#0072B2", color="white") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=seq(0, 100, 1)) +
  labs(title    = "Distribution of Storm Hits per County",
       x        = "Number of Storms",
       y        = "Number of Counties\nExposed to Winds >=64 knots") +
  geom_text(
    aes(x = n_storm_tx, label = after_stat(comma(count))),
    stat = "bin", binwidth = 1, vjust = -0.5) +
  graph$theme



# =============================================================================
# 9. SCHOOL EXPOSURE: STORMS PER SCHOOL (distribution)
# =============================================================================
exposures_per_school <- school_storm_unique %>%
  group_by(tea_school_id, sid) %>%
  summarise(wind_64kt = any(wind_64kt)) %>%
  group_by(tea_school_id) %>%
  summarise(n_storm_tx = sum(wind_64kt)) 

ggplot(exposures_per_school, aes(x = n_storm_tx)) +
  geom_histogram(binwidth = 1, fill="#4ec3c9", color="white") +
  # scale_y_continuous(labels = comma, limits = c(0,8800)) +
  scale_x_continuous(breaks=seq(0, 100, 1)) +
  labs(title    = "Distribution of Storm Hits per School",
    x        = "Number of Storms",
    y        = "Number of Schools\nExposed to Winds >=50 knots") +
  geom_text(
    aes(x = n_storm_tx, label = after_stat(comma(count))),
    stat = "bin", binwidth = 1, vjust = -0.5) +
  graph$theme









