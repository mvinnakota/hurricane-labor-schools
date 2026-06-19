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
stacked_did <- Build_Panel(df=school_storm_unique, 
                           direct_var="wind_64kt",
                           indirect_var="wind_64kt",
                           indirect_geo="cz", 
                           pre_years=4, post_years=3, 
                           never_treated=T, 
                           years_since=7, 
                           donut="wind_50kt",
                           radius_miles=100)

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
  c(xmin = -98, ymin = 20, xmax = -87, ymax = 30), crs = 4326)) %>%
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

# Storm track: IBTrACS line for each panel storm, drawn as a plain black path.
# Clip to the same Texas + Gulf view window so the offshore part stays in frame.
track_lines <- ibtraks %>%
  filter(SID %in% unique(stacked_did$sid)) %>%
  st_transform(st_crs(points)) %>%
  st_intersection(view_rect) %>%                      # clip to Texas + Gulf window
  sf::st_collection_extract("LINESTRING")

# Palettes: wind field on FILL (purples), treatment points on COLOR (so the two
# legends don't collide on a single fill scale).
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
  # Storm track for this storm (plain black line).
  track_line <- track_lines %>% filter(SID == focal_sid)

  p <- ggplot() +
    geom_sf(data = tx_counties, fill = "white", color = "grey80", linewidth = 0.25)
  if (nrow(wind_f) > 0)
    p <- p + geom_sf(data = wind_f, aes(fill = wind_fac), color = NA, alpha = 0.55)
  if (nrow(track_line) > 0)
    p <- p + geom_sf(data = track_line, color = "black", linewidth = 0.6)
  if (nrow(storm_pts) > 0)
    p <- p + geom_sf(data = storm_pts, aes(color = group), size = 1.1, alpha = 0.9)
  p <- p +
    scale_fill_manual(values = wind_cols, limits = names(wind_cols), drop = FALSE,
                      name = "Wind field") +             # always show all thresholds
    scale_color_manual(values = grp_cols, limits = c("Control", "Indirect", "Direct"),
                       drop = FALSE, name = NULL) +       # always show all groups
    guides(
      color = guide_legend(override.aes = list(size = 3), order = 1),
      fill  = guide_legend(override.aes = list(alpha = 0.7), order = 2)
    ) +
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

# # To map just one storm:           plot_storm(all_sids[1])
# # To map a specific named storm:   plot_storm(storms_tx$sid[match("IKE", storms_tx$storm_name)])
