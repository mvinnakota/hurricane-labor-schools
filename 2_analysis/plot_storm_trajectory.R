# ==================================
# Author: EH
# Date: Updated 06/16/26
# Project: Hurricanes + Schools
# Description: Map one storm's stacked-DiD experiment.
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

### Load data -------------------------------------------------------------------
# School point geometry (one row per school). school_storm_unique carries no
# geometry, so the map coordinates come from school_xy.
load("inputs/CCD/school_xy.Rds")

### Build the stacked panel -----------------------------------------------------
# Treatment is defined inside Build_Panel():
#   direct   = 34-kt wind exposure (wind_34kt)
#   indirect = commuting-zone (cz_2000) spillover from a directly-hit school
stacked_did <- Build_Panel()

# Treatment status for each school-storm in the DiD sample.
# (event_time == 0 holds the un-zeroed storm-year values of direct / indirect.)
groups <- stacked_did %>%
  filter(event_time == 0) %>%
  distinct(sid, nces_school_id, direct, indirect) %>%
  mutate(group = factor(
    case_when(direct == 1 ~ "Direct", indirect == 1 ~ "Indirect", TRUE ~ "Control"),
    levels = c("Control", "Indirect", "Direct")        # Direct drawn last
  ))

# School locations joined to each school-storm's treatment status in the DiD
# sample. School geometry is constant across storms, so join school_xy (one
# point per school) to groups (one row per school-storm) by school.
points <- school_xy %>%
  sf::st_as_sf() %>%
  distinct(nces_school_id, .keep_all = TRUE) %>%
  select(nces_school_id) %>%
  inner_join(groups, by = "nces_school_id")


# Texas county boundaries (basemap)
tx_counties <- tigris::counties(state = "TX", cb = TRUE) %>%
  st_transform(st_crs(points))

# Map view window: Texas plus a margin into the Gulf of Mexico, so a storm's
# offshore (over-water) approach is shown, not just the part over land. The
# Gulf margin is defined in lon/lat (easy to tweak) and projected to the map
# CRS; the frame is the bounding box of Texas + this margin.
sf::sf_use_s2(FALSE)
gulf_margin <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = -96, ymin = 23, xmax = -86, ymax = 30), crs = 4326)) %>%
  sf::st_transform(st_crs(points))
view_bb   <- sf::st_bbox(c(sf::st_as_sfc(sf::st_bbox(tx_counties)), gulf_margin))
view_rect <- sf::st_as_sfc(view_bb)                   # rectangle used to trim tracks

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

# Categories shown in the legend on EVERY map, regardless of whether a given
# storm reached them. Matches the track filter below (USA_SSHS >= 0, i.e.
# Tropical Storm and Categories 1-5). Edit this vector to change the legend.
cat_show <- c("Category 5", "Category 4", "Category 3", "Category 2", "Category 1",
              "Tropical Storm")

# Actual hurricane tracks: IBTrACS line segments for the storms we plot (matched
# on SID), clipped to the Texas + Gulf view window (keeps the offshore track),
# with a storm-category label per segment.
ibtracs_lines <- sf::read_sf(
    "inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp"
  ) %>%
  filter(SID %in% unique(stacked_did$sid)) %>%
  st_transform(st_crs(points)) %>%
  st_intersection(view_rect) %>%                      # clip to Texas + Gulf window
  sf::st_collection_extract("LINESTRING") %>%
  mutate(cat_fac = factor(USA_SSHS, levels = rev(-5:5), labels = cat_labels))



### Plot one storm --------------------------------------------------------------
plot_storm <- function(focal_sid) {
  storm_name <- storm_tx$storm_name[match(focal_sid, storm_tx$sid)]
  storm_year <- storm_tx$storm_year[match(focal_sid, storm_tx$sid)]
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
      limits = c("Control", "Indirect", "Direct"),     # always show all groups
      name   = NULL, drop = FALSE
    ) +
    scale_color_manual(values = hurr_cols, limits = cat_show, drop = FALSE,
                       name = "Category") +             # always show all kept categories
    guides(
      fill  = guide_legend(override.aes = list(size = 3, shape = 21), order = 1),
      color = guide_legend(override.aes = list(linewidth = 1.5), order = 2)
    ) +
    labs(
      title    = paste0(tools::toTitleCase(tolower(storm_name)), " (", storm_year, ")"),
    ) +
    coord_sf(xlim = c(view_bb[["xmin"]], view_bb[["xmax"]]),
             ylim = c(view_bb[["ymin"]], view_bb[["ymax"]]), expand = FALSE) +
    theme(
      axis.text   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      legend.position = "bottom",
      legend.box  = "vertical"
    )

  fname <- paste0("trajectory_", storm_year, "_",
                  gsub("[^A-Za-z0-9]", "", tolower(storm_name)), ".png")
  ggsave(file.path("outputs/figures/maps/trajectories", fname),
         plot = p, height = 6, width = 5, units = "in", dpi = 300)
  print(p)
  p
}

### Loop over every storm in the panel ------------------------------------------
all_sids <- sort(unique(stacked_did$sid))
for (s in all_sids) plot_storm(s)

# # To map just one storm:           plot_storm(all_sids[1])
# # To map a specific named storm:   plot_storm(storm_tx$sid[match("IKE", storm_tx$storm_name)])
