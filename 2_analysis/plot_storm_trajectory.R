# ==================================
# Author: EH
# Date: Updated 06/04/26
# Project: Hurricanes + Schools
# Description: Map one storm's stacked-DiD experiment. Plot Texas county
#              boundaries (uncolored), the schools colored by treatment status
#              (direct / indirect / control), and the hurricane's actual track
#              from the IBTrACS line shapefile (matched on SID).
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
stacked_did <- school_storm %>%
  # Drop geometry before Build_Panel (not needed for the panel, much faster).
  sf::st_drop_geometry() %>%
  # Drop storms that are not hurricanes
  subset(max_cat %in% c(1:5)) %>%
  # drop schools that were never closer than 100 miles from a storm
  group_by(nces_school_id) %>% 
  mutate(min_dist = min(dist_to_miles)) %>%
  filter(min_dist < 100) %>%
  ungroup() %>%
  # define treatment as being 10 miles from storm while it is a hurricane
  mutate(direct = dist_to_miles < 10 & cat %in% c(1:5)) %>%
  # Only keep high schools
  subset(high_cedp == 1)  %>%
  # Build panel and keep never treated
  Build_Panel(never_treated = T)

### Shared inputs (built once, reused for every storm) --------------------------
# sid -> storm name / year (not carried into the panel)
storm_meta <- school_storm %>%
  sf::st_drop_geometry() %>%
  distinct(sid, name, season)

# Treatment status for each school-storm in the DiD sample.
# (event_time == 0 holds the un-zeroed storm-year values of direct / indirect.)
groups <- stacked_did %>%
  sf::st_drop_geometry() %>%
  filter(event_time == 0) %>%
  distinct(sid, nces_school_id, direct, indirect) %>%
  mutate(group = factor(
    case_when(direct == 1 ~ "Direct", indirect == 1 ~ "Indirect", TRUE ~ "Control"),
    levels = c("Control", "Indirect", "Direct")        # Direct drawn last
  ))

# School locations (the geometry in school_storm), one point per school-storm,
# joined to that school-storm's treatment status in the DiD sample.
points <- school_storm %>%
  sf::st_as_sf() %>%
  select(sid, nces_school_id, date) %>%
  inner_join(groups, by = c("sid", "nces_school_id"))
points <- points[!sf::st_is_empty(points), ]      # drop schools with missing coords

# Texas county boundaries (basemap)
tx_counties <- tigris::counties(state = "TX", cb = TRUE) %>%
  st_transform(st_crs(points))

# Texas state outline (counties dissolved) used to clip tracks to the state
sf::sf_use_s2(FALSE)
tx_state <- tx_counties %>% summarise() %>% sf::st_make_valid()

# Saffir-Simpson category factor + palette (matches graph_themes$hurr_theme)
hurr_cols <- c(
  "Unknown" = "#cccccc", "Post-tropical" = "#aaaaaa",
  "Miscellaneous disturbance" = "#888888", "Subtropical" = "#66c2a5",
  "Tropical Depression" = "#ffffb2", "Tropical Storm" = "#fecc5c",
  "Category 1" = "#fd8d3c", "Category 2" = "#f03b20", "Category 3" = "#bd0026",
  "Category 4" = "#800026", "Category 5" = "#4d0013"
)
cat_labels <- rev(c("Unknown", "Post-tropical", "Miscellaneous disturbance",
                    "Subtropical", "Tropical Depression", "Tropical Storm",
                    "Category 1", "Category 2", "Category 3", "Category 4", "Category 5"))

# Actual hurricane tracks: IBTrACS line segments for the storms we plot (matched
# on SID), clipped to Texas, with a storm-category label per segment.
ibtracs_lines <- sf::read_sf(
    "inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp"
  ) %>%
  filter(SID %in% unique(stacked_did$sid)) %>%
  st_transform(st_crs(points)) %>%
  st_intersection(tx_state) %>%                       # clip to Texas boundary
  sf::st_collection_extract("LINESTRING") %>%
  mutate(cat_fac = factor(USA_SSHS, levels = rev(-5:5), labels = cat_labels))

# Texas view window for framing
tx_bb <- sf::st_bbox(tx_counties)



### Plot one storm --------------------------------------------------------------
plot_storm <- function(focal_sid) {
  storm_name <- storm_meta$name[match(focal_sid, storm_meta$sid)]
  storm_year <- storm_meta$season[match(focal_sid, storm_meta$sid)]
  if (is.na(storm_name) || storm_name == "") storm_name <- focal_sid   # unnamed storms

  # Schools' closest-approach points for this storm, colored by treatment.
  storm_pts <- points %>% filter(sid == focal_sid) %>% arrange(group)

  # Hurricane track for this storm: actual IBTrACS positions (line segments).
  track_line <- ibtracs_lines %>% filter(SID == focal_sid)

  p <- ggplot() +
    geom_sf(data = tx_counties, fill = "white", color = "grey80", linewidth = 0.25)
  if (nrow(track_line) > 0)
    p <- p + geom_sf(data = track_line, aes(color = cat_fac), linewidth = 0.9)
  if (nrow(storm_pts) > 0)
    p <- p + geom_sf(data = storm_pts, aes(fill = group), shape = 21,
                     color = "white", stroke = 0.1, size = 1.3, alpha = 0.9)
  p <- p +
    scale_fill_manual(
      values = c("Direct" = "#D55E00", "Indirect" = "#0072B2", "Control" = "#B2B2B2"),
      name   = NULL, drop = FALSE
    ) +
    scale_color_manual(values = hurr_cols, name = "Category") +
    guides(
      fill  = guide_legend(override.aes = list(size = 3, shape = 21), order = 1),
      color = guide_legend(override.aes = list(linewidth = 1.5), order = 2)
    ) +
    labs(
      title    = paste0(tools::toTitleCase(tolower(storm_name)), " (", storm_year, ")"),
      subtitle = "Schools by treatment status; track colored by storm category"
    ) +
    coord_sf(xlim = c(tx_bb[["xmin"]], tx_bb[["xmax"]]),
             ylim = c(tx_bb[["ymin"]], tx_bb[["ymax"]]), expand = FALSE) +
    theme(
      axis.text   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      legend.position = "bottom",
      legend.box  = "vertical"
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
