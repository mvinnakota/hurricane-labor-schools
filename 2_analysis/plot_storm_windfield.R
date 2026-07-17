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
school_storm_unique %<>% subset(high_cedp == 1 | tea_school_id == "084906002")

# =============================================================================
### Basemap + view window 
# =============================================================================
# Texas county boundaries (basemap)
tx_counties <- tigris::counties(state = "TX", cb = TRUE) %>%
  st_transform(4326)

# Map view window: Texas plus a margin into the Gulf of Mexico, so a storm's
# offshore (over-water) wind field is shown, not just the part over land. The
# Gulf margin is defined in lon/lat (easy to tweak) and projected to the map
# CRS; the frame is the bounding box of Texas + this margin.
sf::sf_use_s2(FALSE)
gulf_margin <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = -97, ymin = 23, xmax = -88, ymax = 30), crs = 4326)) %>%
  sf::st_transform(4326)
view_bb   <- sf::st_bbox(c(sf::st_as_sfc(sf::st_bbox(tx_counties)), gulf_margin))
view_rect <- sf::st_as_sfc(view_bb)                   # rectangle used to trim the wind field

# Commuting-zone boundaries (basemap): dissolve Texas counties into their USDA
# 2000 commuting zones, matching the cz_2000 grouping used to define indirect
# treatment in Build_Panel(). s2 is off (set above), so the dissolve uses planar
# geometry; st_make_valid cleans any slivers left by the union.
cz_xwalk <- readxl::read_excel("inputs/USDA Commuting zones/cz_2000") %>%
  filter(substr(FIPS, 1, 2) == "48") %>%              # Texas only
  select(county_fips = FIPS, cz_2000 = `Commuting Zone ID, 2000`)
tx_cz <- tx_counties %>%
  left_join(cz_xwalk, by = c("GEOID" = "county_fips")) %>%
  group_by(cz_2000) %>%
  summarise(.groups = "drop") %>%
  sf::st_make_valid()

# =============================================================================
# CREATE GRAPH THEMES
# =============================================================================

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

# Palettes: wind field on FILL (purples); the track category and the school
# treatment groups each get their own COLOR scale (via ggnewscale) in the plot.
wind_cols <- c(">=34 kt" = "#cbc9e2", ">=50 kt" = "#9e9ac8", ">=64 kt" = "#6a51a3")
grp_cols  <- c("Direct" = "#D55E00", "Indirect" = "#0072B2", "Control" = "grey75", 
               "Previously Hit" = "grey30")



# =============================================================================
# 4. CUSTOM FUNCTION TO PLOT ALL STORMS IN ONE GRAPH 
# =============================================================================
# Wrap the all-storms facet map in a function so it can be regenerated for
# different Build_Panel() parameterizations. Everything that depends on the
# panel (treatment groups, sample storms, tracks) is recomputed from the passed
# `stacked_did`; the static basemap / palettes / view window / clipped
# wind_footprint are taken from the global objects built above.
plot_all_storms <- function(stacked_did,
                            title = NULL,
                            out        = "outputs/figures/maps/winds/all_storms_windfield_wide.png",
                            facet_nrow = 2,
                            width      = 10,
                            height     = 6,
                            point_size = 0.7,
                            save       = TRUE) {

  # Treatment status per school-storm (event_time == 0 holds the un-zeroed
  # storm-year values of direct / indirect).
  groups <- stacked_did %>%
    filter(event_time == 0) %>%
    distinct(sid, nces_school_id, direct, indirect, previously_hit) %>%
    mutate(group = factor(
      case_when(
        previously_hit ~ "Previously Hit",
        direct == 1 ~ "Direct",
        indirect == 1 ~ "Indirect",
        TRUE ~ "Control"),
      levels = c("Control", "Previously Hit", "Indirect", "Direct")
    ))

  # School locations joined to each school-storm's treatment status.
  points <- school_xy %>%
    sf::st_as_sf() %>%
    distinct(nces_school_id, .keep_all = TRUE) %>%
    select(nces_school_id) %>%
    inner_join(groups, by = "nces_school_id")

  all_sids <- sort(unique(stacked_did$sid))

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

  # Storm tracks for this panel's storms, colored by Saffir-Simpson category.
  track_lines <- ibtraks %>%
    filter(SID %in% all_sids) %>%
    filter(USA_SSHS >= 0) %>%
    st_transform(st_crs(points)) %>%
    st_intersection(view_rect) %>%                      # clip to Texas + Gulf window
    sf::st_collection_extract("LINESTRING") %>%
    mutate(cat_fac = factor(USA_SSHS, levels = rev(-5:5), labels = cat_labels))

  # --- Combine layers across all storms, adding storm_label for faceting ------

  # Wind footprint (fill scale): largest threshold at bottom. Rebuild wind_fac
  # here so the function doesn't depend on the global wind_footprint already
  # carrying it.
  wind_all <- wind_footprint %>%
    filter(sid %in% all_sids) %>%
    arrange(wind_speed_kt) %>%
    mutate(wind_fac = factor(wind_speed_kt, levels = c(34, 50, 64),
                             labels = c(">=34 kt", ">=50 kt", ">=64 kt"))) %>%
    left_join(storm_labels, by = "sid") %>%
    mutate(storm_label = factor(storm_label, levels = facet_levels))

  # Storm tracks (1st color scale): cat_fac already attached in track_lines.
  track_all <- track_lines %>%
    left_join(storm_labels, by = c("SID" = "sid")) %>%
    mutate(storm_label = factor(storm_label, levels = facet_levels))

  # School points (2nd color scale via ggnewscale).
  points_all <- points %>%
    filter(sid %in% all_sids) %>%
    arrange(group) %>%
    left_join(storm_labels, by = "sid") %>%
    mutate(storm_label = factor(storm_label, levels = facet_levels))

  # Basemap: replicate the commuting-zone boundaries for every facet.
  cz_all <- bind_rows(lapply(facet_levels, function(lbl) {
    tx_cz %>% mutate(storm_label = factor(lbl, levels = facet_levels))
  }))

  # --- Build plot -------------------------------------------------------------
  p_all <- ggplot() +
    # Basemap (commuting zones)
    geom_sf(data = cz_all, fill = "white", color = "grey70", linewidth = 0.2) +
    # Wind field (fill scale)
    geom_sf(data = wind_all, aes(fill = wind_fac), color = NA, alpha = 0.55) +
    scale_fill_manual(
      values = wind_cols, limits = names(wind_cols), drop = FALSE,
      name   = "Wind field",
      guide  = guide_legend(override.aes = list(alpha = 0.7), order = 3)
    ) +
    # Schools (1st color scale). Drawn BEFORE the track so the track is on top.
    geom_sf(data = points_all, aes(color = group), size = point_size, alpha = 0.9) +
    scale_color_manual(
      values = grp_cols, limits = c("Direct","Control","Indirect","Previously Hit"),
      drop   = FALSE, name = "School group",
      guide  = guide_legend(override.aes = list(size = 3), order = 1)
    ) +
    # Storm track on top (2nd color scale via ggnewscale)
    ggnewscale::new_scale_color() +
    geom_sf(data = track_all, aes(color = cat_fac), linewidth = 0.7) +
    scale_color_manual(
      values = hurr_cols, drop = TRUE,
      name   = "Category",
      guide  = guide_legend(override.aes = list(linewidth = 1.5), order = 2)
    ) +
    # Facet: chronological left-to-right, top-to-bottom
    facet_wrap(~storm_label, nrow = facet_nrow) +
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

  if(!is.null(title)){p_all = ggtitle(title)}
  
  if (save) {
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    ggsave(out, plot = p_all, height = height, width = width, units = "in", dpi = 300)
  }
  print(p_all)
  invisible(p_all)
}





# =============================================================================
# PLOT STORMS
# =============================================================================
# Default
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_64kt",
  indirect_geo="cz",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
plot_all_storms(out = "outputs/figures/maps/winds/d64_i64_gcz_dtNA_r200.png")

# Using county instead
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_64kt",
  indirect_geo="county",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d64_i64_gcounty_dtNA_r200.png")


# Expanding indirect to wind50kt and adding a buffer
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_50kt",
  indirect_geo="county",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d64_i50_gcounty_dtNA_r200.png")




# Adding a buffer
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_50kt",
  indirect_geo="county",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut="wind_50kt",
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d64_i50_gcounty_dt50_r200.png")



# Using only 50kt as indirect and not county
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_50kt",
  indirect_var="wind_64kt",
  indirect_geo="cz",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=10,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d50_i64_gcz_dtNA_r200.png")


# Using only 50kt as indirect and not county
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_64kt",
  indirect_var="wind_50kt",
  indirect_geo=NULL,
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=10,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d64_i50_gNA_dtNA_r200.png")



# Expanding Directly hit to 50kt
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_50kt",
  indirect_var="wind_50kt",
  indirect_geo="cz",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d50_i50_gcz_dtNA_r200.png")



# Expanding Directly hit to 50kt
Build_Panel(
  df=school_storm_unique,
  direct_var="wind_50kt",
  indirect_var="wind_50kt",
  indirect_geo="county",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut=NULL,
  radius_miles=200,
  radius_var="dist_to_64kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d50_i50_gcounty_dtNA_r200.png")



# add a 10 mile buffer and removing indirect
Build_Panel(
  df=school_storm_unique %>% mutate(buffer_miles = dist_to_50kt_miles <= 25),
  direct_var="wind_50kt",
  indirect_var=NULL,
  indirect_geo="county",
  pre_years=4, post_years=1,
  never_treated=T,
  years_since=7,
  donut="buffer_miles",
  radius_miles=200,
  radius_var="dist_to_50kt_miles",
  sample_years = c(1990:2019),
  keep_previously_hit = T
) %>%
  plot_all_storms(out = "outputs/figures/maps/winds/d50_iNA_gNA_dt25_r200.png")





















