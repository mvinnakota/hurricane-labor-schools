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
load("intermediates/school_storm_treatment.Rda")

# Ensure date is parsed and derive time fields
school_storm %<>%
  mutate(
    date      = as.Date(date),
    month     = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date),
    year      = year(date),
    season    = factor(season),
    cat_fac = factor(
      max(cat),
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
      ))
  )

# Deduplicate to one row per storm (for storm-level counts)
storms <- school_storm %>%
  group_by(name, season, month, month_num, year) %>%
  summarize(cat = max(cat),
            cat_fac = factor(
              max(cat),
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
# 1. STORMS PER MONTH
# =============================================================================
storms_by_month <- storms %>%
  count(month, month_num, cat_fac, cat) %>%
  arrange(month_num)

storms_by_month %>%
ggplot(aes(x = month, y = n, fill = cat_fac)) +
  geom_col(color = "white", linewidth = 0.4) +
  labs(x = NULL, y = "Number of Storms") + 
  graph$hurr_theme
ggsave("outputs/figures/summary_stats/month_of_storm.png", height=4, width=5, units="in")


storms_by_month %>%
  subset(cat>0) %>%
  ggplot(aes(x = month, y = n, fill = cat_fac)) +
  geom_col(color = "white", linewidth = 0.4) +
  labs(x = NULL, y = "Number of Storms") + 
  graph$hurr_theme
ggsave("outputs/figures/summary_stats/month_of_hurr.png", height=4, width=6, units="in")


# =============================================================================
# 2. STORMS PER SEASON (YEAR)
# =============================================================================
storms_by_season <- storms %>%
  count(season, cat_fac, cat, name) %>%
  arrange(season) %>%
  mutate(season = as.numeric(as.character(season))) 

storms_by_season %>%
  filter(cat > 0) %>%
  mutate(season = ifelse(name == "JERRY", season - 0.25, season),
         season = ifelse(name == "CHANTAL", season + 0.25, season)) %>%
  ggplot(aes(x = season, y = cat_fac, color = cat_fac, label = name)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text_repel(size = 2.8) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(
    breaks = seq(1989, 2021, 2),
    labels = function(x) paste0("\'", substr(x, 3, 4))) +
  labs(x = "Season", y = NULL) +
  graph$hurr_theme + 
  theme(legend.position = "none",
        panel.grid.major = element_line()) 
ggsave("outputs/figures/summary_stats/year_of_hurr_named.png",
       height = 4, width = 7, units = "in")


# hurricanes
storms_by_season %>% 
  subset(cat>0) %>%
  ggplot(aes(x = season, y = n, fill=cat_fac)) +
  geom_col(linewidth = 0.4) +
  labs(x = "Season", y = "Number of Hurricanes") +
  graph$hurr_theme + 
  scale_y_continuous(breaks=c(0,1,2)) +
  scale_x_continuous(breaks=seq(1989, 2021, 2), labels = function(x) paste0("\'",substr(x, 3, 4)))
ggsave("outputs/figures/summary_stats/year_of_hurr.png", height=4, width=9, units="in")


# storms
storms_by_season %>% 
  ggplot(aes(x = season, y = n, fill=cat_fac)) +
  geom_col(linewidth = 0.4) +
  labs(x = "Season", y = "Number of Storms") +
  graph$hurr_theme + 
  scale_y_continuous(breaks=c(0:4)) +
  scale_x_continuous(breaks=seq(1989, 2021, 2), labels = function(x) paste0("\'",substr(x, 3, 4)))
ggsave("outputs/figures/summary_stats/year_of_storm.png", height=4, width=9, units="in")


# =============================================================================
# 7. STORMS PER COUNTY
# =============================================================================
school_storm %>%
  filter(hit_county == 1) %>%
  group_by(fips) %>%
  summarize(n_storms = n_distinct(sid)) %>%
  ggplot(aes(x = n_storms)) +
  geom_histogram() + 
  scale_x_continuous(breaks=c(0:10))

school_storm %>%
  filter(hit_county == 1 & cat > 0) %>%
  group_by(fips) %>%
  summarize(n_storms = n_distinct(sid)) %>%
  ggplot(aes(x = n_storms)) +
  geom_histogram() + 
  scale_x_continuous(breaks=c(0:10))

# =============================================================================
# 8. STORMS PER COMMUTING ZONE 
# =============================================================================
school_storm %>%
  filter(hit_cz == 1) %>%
  group_by(school_cz) %>%
  summarize(n_storms = n_distinct(sid)) %>%
  ggplot(aes(x = n_storms)) +
  geom_histogram() + 
  scale_x_continuous(breaks=c(0:20))


school_storm %>%
  filter(hit_cz == 1 & cat > 0) %>%
  group_by(school_cz) %>%
  summarize(n_storms = n_distinct(sid)) %>%
  ggplot(aes(x = n_storms)) +
  geom_histogram() + 
  scale_x_continuous(breaks=c(0:20))


# =============================================================================
# 9. SCHOOL EXPOSURE: STORMS PER SCHOOL (distribution)
# =============================================================================
exposures_per_school <- school_storm %>%
  filter(hit_school == 1) %>%
  count(nces_school_id, name = "n_storms")

ggplot(exposures_per_school, aes(x = n_storms)) +
  geom_histogram(binwidth = 1, fill = "#2171b5", color = "white") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Distribution of Storm Hits per School",
    subtitle = "Schools with at least one direct hit",
    x        = "Number of Storms",
    y        = "Number of Schools"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))









