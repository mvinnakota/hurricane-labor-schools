# ==================================
# Author: EH
# Date: Updated 06/04/26
# Project: Hurricanes + Schools
# Description: Map one storm's stacked-DiD experiment. Plot Texas county
#              boundaries (uncolored) and overlay schools color-coded as
#              direct / indirect / control. The band of direct-hit schools
#              traces the storm's path across the state.
# ==================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)

# Load Build_Panel(), packages, and graph themes.
# (Recompute the active-doc path each time: packages.R runs rm(list=ls()).)
source(file.path(dirname(getActiveDocumentContext()$path),
                 "../1_data_prep/6_build_stacked_did_panel.R"))   # defines Build_Panel(); sets wd to project root
source(file.path(dirname(getActiveDocumentContext()$path),
                 "../0_helper_functions/graph_themes.R"))
options(tigris_use_cache = TRUE)

### Load data -------------------------------------------------------------------
load("intermediates/school_storm_treatment.Rda")   # school_storm (sf, school x storm x track-point)

### Build the stacked panel -----------------------------------------------------
# Drop geometry before Build_Panel (not needed for the panel, much faster).
# Restrict to hurricanes (max_cat 1-5) BEFORE Build_Panel. max_cat is storm-level
# (constant across a storm's track points), so this keeps/drops whole storms and
# never loses a school's closest-approach point.
stacked_did <- school_storm %>%
  mutate(direct = dist_to_miles < 10 & cat %in% c(1:5)) %>%
  subset(high_cedp == 1)  %>%
  filter(max_cat %in% 1:5) %>%
  Build_Panel(never_treated = T)

### Shared inputs (built once, reused for every storm) --------------------------
# sid -> storm name / year (name was not carried into the panel)
storm_meta <- school_storm %>%
  sf::st_drop_geometry() %>%
  distinct(sid, name, season)

# school geometry, rejoined from school_storm by id
school_geo <- school_storm %>%
  sf::st_as_sf() %>%
  select(nces_school_id) %>%
  distinct(nces_school_id, .keep_all = TRUE)

# Texas county boundaries (basemap)
tx_counties <- tigris::counties(state = "TX", cb = TRUE) %>%
  st_transform(st_crs(school_geo))

dir.create("outputs/figures/maps", recursive = TRUE, showWarnings = FALSE)

### Plot one storm --------------------------------------------------------------
# Classify schools for the storm, then map county boundaries + color-coded schools.
# direct   = within 10 mi of the track            (direct == 1)
# indirect = in a hit commuting zone, not direct  (indirect == 1)
# control  = eligible comparison school           (neither)
plot_storm <- function(focal_sid) {
  storm_name <- storm_meta$name[match(focal_sid, storm_meta$sid)]
  storm_year <- storm_meta$season[match(focal_sid, storm_meta$sid)]
  if (is.na(storm_name) || storm_name == "") storm_name <- focal_sid   # unnamed storms

  storm_schools <- stacked_did %>%
    filter(sid == focal_sid, event_time == 0) %>%      # storm-year values (un-zeroed)
    distinct(nces_school_id, .keep_all = TRUE) %>%
    mutate(
      group = case_when(
        direct   == 1 ~ "Direct",
        indirect == 1 ~ "Indirect",
        TRUE          ~ "Control"
      ),
      group = factor(group, levels = c("Control", "Indirect", "Direct"))  # Direct drawn last
    ) %>%
    select(nces_school_id, group)

  storm_sf <- school_geo %>%
    inner_join(storm_schools, by = "nces_school_id") %>%
    arrange(group)

  p <- ggplot() +
    geom_sf(data = tx_counties, fill = "white", color = "grey75", linewidth = 0.25) +
    geom_sf(data = storm_sf, aes(color = group), size = 0.7, alpha = 0.85) +
    scale_color_manual(
      values = c("Direct" = "#D55E00", "Indirect" = "#0072B2", "Control" = "#B2B2B2"),
      name   = NULL, drop = FALSE
    ) +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
    labs(
      title    = paste0(tools::toTitleCase(tolower(storm_name)), " (", storm_year, ")"),
      subtitle = "Texas schools by treatment status in the stacked DiD"
    ) +
    coord_sf(expand = FALSE) +
    theme(
      axis.text   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      legend.position = "bottom"
    )

  fname <- paste0("trajectory_", storm_year, "_",
                  gsub("[^A-Za-z0-9]", "", tolower(storm_name)), ".png")
  ggsave(file.path("outputs/figures/maps", fname),
         plot = p, height = 6, width = 5, units = "in", dpi = 300)
  invisible(p)
}

### Loop over every storm in the panel ------------------------------------------
all_sids <- sort(unique(stacked_did$sid))
for (s in all_sids) plot_storm(s)

# # To map just one storm:           plot_storm(all_sids[1])
# # To map a specific named storm:   plot_storm(storm_meta$sid[match("IKE", storm_meta$name)])
