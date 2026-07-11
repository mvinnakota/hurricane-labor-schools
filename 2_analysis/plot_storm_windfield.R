# ==================================
# Author: EH
# Date: 06/16/26
# Project: Hurricanes + Schools
# Description: Map one storm's stacked-DiD experiment using the WIND FIELD
#              instead of the IBTrACS track. Plot Texas county boundaries
#              (basemap), the EBTRK wind-radii footprint (>=34 / >=50 / >=64 kt),
#              and the schools colored by treatment status (direct / indirect /
#              control). The 50-kt footprint is the area that defines
#              direct = wind_50kt in Build_Panel(), so direct schools should
#              fall inside it.
# ==================================

### Setup -----------------------------------------------------------------------
library(rstudioapi)
# set working directory
setwd(dirname(getActiveDocumentContext()$path))
source("../0_helper_functions/packages.R")
source("../0_helper_functions/graph_themes.R")
setwd("../../../")
options(tigris_use_cache = TRUE)

# =============================================================================
# 0. LOAD DATA
# =============================================================================
# All wind-field objects are built in 1_data_prep/4_complete_radius_analysis.R
# and loaded here, so the sector-building logic lives in exactly one place.
# Everything is keyed on `sid`.
load("intermediates/texas_storms.Rda")              # storms_tx (sid, storm_name, storm_year)
load("intermediates/school_storm_unique.Rda")       # school x storm panel inputs
load("intermediates/wind_footprint.Rda")            # wind_footprint: dissolved field per sid x wind speed
source("Code/hurricane-labor-schools/1_data_prep/5_build_stacked_did_panel.R")  # Build_Panel()
load("inputs/CCD/school_xy.Rds")                    # school_xy (sf, school points)
ibtraks <- sf::read_sf("inputs/IBTrACS.since1980.list.v04r00.lines/IBTrACS.since1980.list.v04r00.lines.shp")



### Build the stacked panel -----------------------------------------------------
# Treatment is defined inside Build_Panel():
stacked_did <- Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_50kt",
  indirect_geo="cz",
  pre_years=4, post_years=3,
  never_treated=T,
  years_since=7,
  donut=NULL,
  radius_miles=300,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1989:2017)
  ) %>%
  filter(high_cedp == 1)

# Treatment status for each school-storm in the DiD sample.
# (event_time == 0 holds the un-zeroed storm-year values of direct / indirect.)
groups <- stacked_did %>%
  filter(event_time == 0) %>%
  distinct(sid, nces_school_id, direct, indirect) %>%
  mutate(group = factor(
    case_when(direct == 1 ~ "Direct", indirect == 1 ~ "Indirect", TRUE ~ "Control"),
    levels = c("Control", "Indirect", "Direct")        # Direct drawn last
  ))

# School locations joined to each school-storm's treatment status. School
# geometry is constant across storms, so join school_xy (one point per school)
# to groups (one row per school-storm) by school.
points <- school_xy %>%
  sf::st_as_sf() %>%
  distinct(nces_school_id, .keep_all = TRUE) %>%
  select(nces_school_id) %>%
  inner_join(groups, by = "nces_school_id")



### Basemap + view window -------------------------------------------------------
# Texas county boundaries (basemap)
tx_counties <- tigris::counties(state = "TX", cb = TRUE) %>%
  st_transform(st_crs(points))

# Map view window: Texas plus a margin into the Gulf of Mexico, so a storm's
# offshore (over-water) wind field is shown, not just the part over land. The
# Gulf margin is defined in lon/lat (easy to tweak) and projected to the map
# CRS; the frame is the bounding box of Texas + this margin.
sf::sf_use_s2(FALSE)
gulf_margin <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = -97, ymin = 23, xmax = -88, ymax = 30), crs = 4326)) %>%
  sf::st_transform(st_crs(points))
view_bb   <- sf::st_bbox(c(sf::st_as_sfc(sf::st_bbox(tx_counties)), gulf_margin))
view_rect <- sf::st_as_sfc(view_bb)                   # rectangle used to trim the wind field

# =============================================================================
# PREP THE WIND FIELD 
# =============================================================================
# Clip wind footprint to the Texas + Gulf view window, and label thresholds.
wind_footprint <- wind_footprint %>% st_transform(st_crs(points))

wind_footprint <- sf::st_intersection(wind_footprint, view_rect) %>%
  sf::st_collection_extract("POLYGON")

wind_footprint %<>%
  mutate(wind_fac = factor(wind_speed_kt, levels = c(34, 50, 64),
                           labels = c(">=34 kt", ">=50 kt", ">=64 kt")))

# Saffir-Simpson category factor + palette (matches plot_storm_trajectory.R /
# graph_themes$hurr_theme), used to color the storm track by intensity.
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

# Storm track: IBTrACS line segments for each panel storm, colored by storm
# category. Clip to the same Texas + Gulf view window so the offshore part stays
# in frame, and tag each segment with its Saffir-Simpson category.
track_lines <- ibtraks %>%
  filter(SID %in% unique(stacked_did$sid)) %>%
  filter(USA_SSHS >= 0) %>%
  st_transform(st_crs(points)) %>%
  st_intersection(view_rect) %>%                      # clip to Texas + Gulf window
  sf::st_collection_extract("LINESTRING") %>%
  mutate(cat_fac = factor(USA_SSHS, levels = rev(-5:5), labels = cat_labels))

# Palettes: wind field on FILL (purples); the track category and the school
# treatment groups each get their own COLOR scale (via ggnewscale) in the plot.
wind_cols <- c(">=34 kt" = "#cbc9e2", ">=50 kt" = "#9e9ac8", ">=64 kt" = "#6a51a3")
grp_cols  <- c("Direct" = "#D55E00", "Indirect" = "#0072B2", "Control" = "#B2B2B2")



# =============================================================================
# 2. PLOT ONE STORM
# =============================================================================
plot_storm <- function(focal_sid) {
  # Everything keys on sid: storms_tx for the title; wind_footprint / points /
  # track for the geometry.
  storm_name <- storms_tx$storm_name[match(focal_sid, storms_tx$sid)]
  storm_year <- storms_tx$storm_year[match(focal_sid, storms_tx$sid)]
  if (is.na(storm_name) || storm_name == "") storm_name <- focal_sid   # unnamed storms

  # Wind footprint for this storm (largest threshold drawn first / on the bottom).
  wind_f    <- wind_footprint %>% filter(sid == focal_sid) %>% arrange(wind_speed_kt)
  # Schools' locations for this storm, colored by treatment.
  storm_pts <- points %>% filter(sid == focal_sid) %>% arrange(group)
  # Storm track for this storm, colored by Saffir-Simpson category.
  track_line <- track_lines %>% filter(SID == focal_sid)

  p <- ggplot() +
    geom_sf(data = tx_counties, fill = "white", color = "grey80", linewidth = 0.25)

  # Wind field (FILL scale).
  if (nrow(wind_f) > 0)
    p <- p + geom_sf(data = wind_f, aes(fill = wind_fac), color = NA, alpha = 0.55)
  p <- p + scale_fill_manual(values = wind_cols, limits = names(wind_cols), drop = FALSE,
                             name = "Wind field",
                             guide = guide_legend(override.aes = list(alpha = 0.7), order = 3))

  # Storm track, colored by category (1st COLOR scale).
  if (nrow(track_line) > 0)
    p <- p + geom_sf(data = track_line, aes(color = cat_fac), linewidth = 0.8)
  # drop = TRUE: show only the categories this storm actually reached.
  p <- p + scale_color_manual(values = hurr_cols, drop = TRUE,
                              name = "Category",
                              guide = guide_legend(override.aes = list(linewidth = 1.5), order = 2))

  # Schools, colored by treatment (2nd COLOR scale, via ggnewscale).
  p <- p + ggnewscale::new_scale_color()
  if (nrow(storm_pts) > 0)
    p <- p + geom_sf(data = storm_pts, aes(color = group), size = 1.1, alpha = 0.9)
  p <- p + scale_color_manual(values = grp_cols, limits = c("Control", "Indirect", "Direct"),
                              drop = FALSE, name = NULL,
                              guide = guide_legend(override.aes = list(size = 3), order = 1))

  p <- p +
    labs(
      title    = paste0(tools::toTitleCase(tolower(storm_name)), " (", storm_year, ")")
    ) +
    coord_sf(xlim = c(view_bb[["xmin"]], view_bb[["xmax"]]),
             ylim = c(view_bb[["ymin"]], view_bb[["ymax"]]), expand = FALSE) +
    theme(
      axis.text   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      legend.position = "right",
      legend.box  = "vertical"
    )

  fname <- paste0("windfield_", storm_year, "_",
                  gsub("[^A-Za-z0-9]", "", tolower(storm_name)), ".png")
  ggsave(file.path("outputs/figures/maps/winds/main/", fname),
         plot = p, height = 6, width = 5, units = "in", dpi = 300)
  print(p)
  p
}

# =============================================================================
# 3. LOOP OVER EVERY STORM IN THE PANEL
# =============================================================================
all_sids <- sort(unique(stacked_did$sid))
for (s in all_sids) plot_storm(s)


# =============================================================================
# 4. PLOT ALL STORMS IN ONE GRAPH (facet_wrap, 3 rows × 2 cols)
# =============================================================================

# Storm labels (storm name + year) in chronological order for facet ordering.
storm_labels <- storms_tx %>%
  filter(sid %in% all_sids) %>%
  mutate(
    name_clean  = ifelse(is.na(storm_name) | storm_name == "", sid, storm_name),
    storm_label = paste0(tools::toTitleCase(tolower(name_clean)), " (", storm_year, ")")
  ) %>%
  select(sid, storm_label, storm_year) %>%
  arrange(storm_year)

facet_levels <- storm_labels$storm_label   # chronological order for factor

# --- Combine layers across all storms, adding storm_label for faceting --------

# Wind footprint (fill scale): largest threshold at bottom.
wind_all <- wind_footprint %>%
  filter(sid %in% all_sids) %>%
  arrange(wind_speed_kt) %>%
  left_join(storm_labels, by = "sid") %>%
  mutate(storm_label = factor(storm_label, levels = facet_levels))

# Storm tracks (1st color scale): cat_fac already attached in track_lines.
track_all <- track_lines %>%
  filter(SID %in% all_sids) %>%
  left_join(storm_labels, by = c("SID" = "sid")) %>%
  mutate(storm_label = factor(storm_label, levels = facet_levels))

# School points (2nd color scale via ggnewscale).
points_all <- points %>%
  filter(sid %in% all_sids) %>%
  arrange(group) %>%
  left_join(storm_labels, by = "sid") %>%
  mutate(storm_label = factor(storm_label, levels = facet_levels))

# Basemap: replicate tx_counties for every facet.
counties_all <- bind_rows(lapply(facet_levels, function(lbl) {
  tx_counties %>% mutate(storm_label = factor(lbl, levels = facet_levels))
}))

# --- Build plot ---------------------------------------------------------------
p_all <- ggplot() +
  # Basemap
  geom_sf(data = counties_all, fill = "white", color = "grey80", linewidth = 0.2) +
  # Wind field (fill scale)
  geom_sf(data = wind_all, aes(fill = wind_fac), color = NA, alpha = 0.55) +
  scale_fill_manual(
    values = wind_cols, limits = names(wind_cols), drop = FALSE,
    name   = "Wind field",
    guide  = guide_legend(override.aes = list(alpha = 0.7), order = 3)
  ) +
  # Storm track (1st color scale)
  geom_sf(data = track_all, aes(color = cat_fac), linewidth = 0.7) +
  scale_color_manual(
    values = hurr_cols, drop = TRUE,
    name   = "Category",
    guide  = guide_legend(override.aes = list(linewidth = 1.5), order = 2)
  ) +
  # Schools (2nd color scale via ggnewscale)
  ggnewscale::new_scale_color() +
  geom_sf(data = points_all, aes(color = group), size = 0.7, alpha = 0.9) +
  scale_color_manual(
    values = grp_cols, limits = c("Control", "Indirect", "Direct"),
    drop   = FALSE, name = "School group",
    guide  = guide_legend(override.aes = list(size = 3), order = 1)
  ) +
  # Facet: 3 rows × 2 cols, chronological left-to-right, top-to-bottom
  facet_wrap(~storm_label, nrow = 2) +
  coord_sf(
    xlim   = c(view_bb[["xmin"]], view_bb[["xmax"]]),
    ylim   = c(view_bb[["ymin"]], view_bb[["ymax"]]),
    expand = FALSE
  ) +
  theme(
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank(),
    # legend.text = element_text(size = 7),
    # strip.text      = element_text(size = 8, face = "bold"),
    legend.position = "right",
    legend.box      = "vertical"
  )

ggsave(
  "outputs/figures/maps/winds/all_storms_windfield_wide.png",
  plot = p_all, height = 6, width = 10, units = "in", dpi = 300
)
print(p_all)

