### ================================================================
# Author: Emileigh Harrison
# Description: This is where we store all our graph themes 
### ================================================================

### ==========================================================================================================
### Set the Graph Themes and Colors
### ==========================================================================================================
library("RColorBrewer")
theme_set(theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), # Center Graph Title
                  # Text
                  axis.text = element_text(size=11),
                  text = element_text(size=11, color="black"), # Font
                  # axis.title = element_text(size=11, face="bold"),
                  # Set Facet Theme
                  strip.background = element_rect(fill="grey90", colour = "grey90"),
                  strip.text = element_text(face="bold", size=12),
                  # Remove Axis Lines
                  panel.border = element_blank(), 
                  # Remove panel grid lines
                  # panel.grid.minor = element_blank(),
                  panel.grid.minor = element_line(colour = "grey65"), # Grid Color
                  panel.grid.minor.x = element_blank() ,
                  # explicitly set the horizontal lines (or they will disappear too)
                  panel.grid.minor.y = element_line( size=.25, color="gray75", linetype = "longdash"),
                  panel.grid.major = element_blank(),
                  # Remove panel background
                  panel.background = element_blank(),
                  # Change legend 
                  legend.position = "bottom",
                  legend.text = element_text(size=11),
                  legend.title = element_blank()))

graph <- list()
                              
graph$color <- c("#4ec3c9", #teal
                 "#0072B2", #blue
                 "#B2B2B2", #grey
                 "#9999CC", #periwinkle
                 "#009E73", #green
                 "#e5d919", #dark yellow
                 "#CC79A7", #pink
                 "#000000", #black
                 "#CC6666",
                 "#E69F00", #light orange
                 "#D55E00", #orange
                 "#661100" #maroon

)

graph$theme <- list(scale_color_manual(values=graph$color),
                   scale_fill_manual(values=graph$color),
                   guides(fill=guide_legend(title=NULL)))
graph$hurr_theme <- list(
  theme(legend.position = "right"),
  scale_fill_manual(
    values = c(
      "Unknown"                   = "#cccccc",
      "Post-tropical"             = "#aaaaaa",
      "Miscellaneous disturbance" = "#888888",
      "Subtropical"               = "#66c2a5",
      "Tropical Depression"       = "#ffffb2",
      "Tropical Storm"            = "#fecc5c",
      "Category 1"                = "#fd8d3c",
      "Category 2"                = "#f03b20",
      "Category 3"                = "#bd0026",
      "Category 4"                = "#800026",
      "Category 5"                = "#4d0013"
    ),
    name = "Category"
  ),
  scale_color_manual(
    values = c(
      "Unknown"                   = "#cccccc",
      "Post-tropical"             = "#aaaaaa",
      "Miscellaneous disturbance" = "#888888",
      "Subtropical"               = "#66c2a5",
      "Tropical Depression"       = "#ffffb2",
      "Tropical Storm"            = "#fecc5c",
      "Category 1"                = "#fd8d3c",
      "Category 2"                = "#f03b20",
      "Category 3"                = "#bd0026",
      "Category 4"                = "#800026",
      "Category 5"                = "#4d0013"
    ),
    name = "Category"
  )
)



