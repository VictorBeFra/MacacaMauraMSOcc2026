library(rnaturalearth)
library(rnaturalearthhires)
library(cowplot)
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(ggplot2)
library(patchwork)
library(cowplot)

##FIGURE 1 INDONESIA + DISTRIBUTION MAP####
# 1. Get Country and Province Data
indo_map <- ne_states(country = "Indonesia", returnclass = "sf")
sulsel_map <- subset(indo_map, iso_3166_2 == "ID-SN")
MMdist <- st_read("XXXX") # Sulawesi Selatan
ProtectedMM <- st_read("XXXX")


forest <- rast("XXXX")
agriculture <- rast("XXXX")
urban <- rast("XXXX")

# --- A. Pre-process Forest Raster ---
# Reclassify matrix: <10 to NA, 10-14 = 1, 15-19 = 2, >19 = 3
m_forest <- matrix(c(
  -Inf,  9, NA,
  9, 14,  1,
  14, 19,  2,
  19, Inf, 3
), ncol = 3, byrow = TRUE)

forest_class <- classify(forest, m_forest)
forest_class <- as.factor(forest_class)
# Assign labels for the legend
levels(forest_class) <- data.frame(ID = 1:3, Forest = c("10-14", "15-19", ">19"))

# --- B. Pre-process Agriculture Raster ---
agri_class <- ifel(agriculture == 1, 1, NA)
agri_class <- as.factor(agri_class)
levels(agri_class) <- data.frame(ID = 1, Class = "Agriculture")

# --- C. Pre-process Urban Raster ---
urban_class <- ifel(urban == 2, 1, NA) # Assumed '+' meant '=' here
urban_class <- as.factor(urban_class)
levels(urban_class) <- data.frame(ID = 1, Class = "Urban")

# 2. Main Map (Indonesia) - Unchanged
p_indo <- ggplot() +
  
  geom_sf(data = indo_map, fill = "lightgrey", color = "lightgrey", linewidth = 0.2) +
  
  geom_rect(aes(xmin = 119, xmax = 121, ymin = -6, ymax = -3), 
            
            fill = NA, color = "red", linewidth = 0.5) +
  
  coord_sf(xlim = c(95, 141), ylim = c(-11, 6), expand = FALSE) +
  
  theme_void() +
  theme(
    # 1. Paint the white background underneath
    panel.background = element_rect(fill = "white", color = NA), 
    # 2. Paint the black frame ON TOP of the maps
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    plot.background = element_blank() 
  )

# 3. Main Map (Sulawesi Selatan) - Fixed Legends and Labels
p_zoom <- ggplot() +
  # 1. Base Map (Bottom layer)
  geom_sf(data = sulsel_map, fill = "grey95", color = "grey95", linewidth = 0.5) +
  
  # 2 & 3. Unified Land Use Layers
  geom_spatraster(data = agri_class) +
  geom_spatraster(data = urban_class) +
  scale_fill_manual(
    name = "Land Use",
    values = c("Agriculture" = "#E69F00", "Urban" = "grey30"),
    na.translate = FALSE,
    guide = guide_legend(order = 3) # Force Land Use to the very top
  ) +
  
  new_scale_fill() + 
  
  # 4. Forest Layer
  geom_spatraster(data = forest_class) +
  scale_fill_manual(
    name = "Forest by height",
    values = c("10-14" = "lightgreen", "15-19" = "green", ">19" = "darkgreen"),
    labels = c("10-14" = "10-14 m", "15-19" = "15-19 m", ">19" = ">19 m"),
    na.translate = FALSE,
    guide = guide_legend(order = 4) # Force Forest to the very bottom
  ) +
  
  # 5. District Boundaries
  geom_sf(data = MMdist, aes(linetype = "Geographic distribution"), 
          color = "black", linewidth = 1, fill = NA) +
  scale_linetype_manual(
    name = NULL,
    values = c("Geographic distribution" = "dashed"),
    guide = guide_legend(order = 1,
                         override.aes = list(linetype = "22", linewidth = 0.5)) # Place under Land Use
  ) +
  
  # 6. Point Locations
  new_scale_fill() +
  geom_sf(data = locations, aes(color = type),  # Use color aesthetic for outline
          shape = 22,          # Shape 22 = square with fill (but we'll make fill transparent)
          fill = NA,           # No fill inside the squares
          size = 0.65) +      # Thicker stroke to make outline colors visible
  scale_color_manual(  # Using scale_color_manual for outline colors
    name = "Sampling Location",
    values = c("PAM" = "#CC29A7", "CT" = "#0015B2"), # Blue and Reddish Purple
    labels = c("PAM" = "Passive Acoustic Monitoring", 
               "CT" = "Camera Trap"),
    guide = guide_legend(order = 2) 
  ) +
  # 7. Scale Bar (Placed in the bottom-right corner)
  annotation_scale(
    location = "br",                # "br" = bottom right
    width_hint = 0.3,               # Tells it to span roughly 40% of the map width
    text_size = 10,                 # Font size for the numbers/units
    pad_x = unit(0.5, "cm"),        # Distance from the right edge
    pad_y = unit(0.5, "cm")         # Distance from the bottom edge
  ) +
  
  # 8. North Arrow (Placed in the top-right corner to avoid your inset map on the left)
  annotation_north_arrow(
    location = "br",                # "tr" = top right
    which_north = "true",           # Aligns to true north
    pad_x = unit(0.5, "cm"),        # Distance from the right edge
    pad_y = unit(1, "cm"),        # Distance from the top edge
    style = north_arrow_orienteering(
      fill = c("black", "white"),   # Clean, high-contrast crisp look
      text_col = "black",
      text_size = 8
    )
  ) +
  # Coordinates and Theme
  coord_sf(xlim = c(119.25, 121.25), ylim = c(-5.8, -3.55), expand = FALSE) +
  scale_x_continuous(breaks = c(119, 120, 121)) +  
  scale_y_continuous(breaks = c(-6, -5, -4, -3)) +  
  theme_minimal() +  
  theme(
    # 1. Paint the white background underneath
    panel.background = element_rect(fill = "white", color = NA),
    # 2. Paint the black frame ON TOP of the maps
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),  
    axis.title = element_blank(),
    legend.position = c(0.98, 0.8),          # Coordinates inside panel (Top Right)
    legend.justification = c("right", "top"), # Aligns top-right corner of legend to the coordinates
    
    # Text sizing adjustments
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7.5),
    
    # Graphic Icon layout shrinking 
    legend.key.size = unit(0.4, "cm"),       # Shrinks sizes of checkboxes/lines/dots altogether
    legend.spacing.y = unit(0.05, "cm")       # Eliminates dead vertical air between legend keys
  )



# --- Layout Math Configuration ---
# We will treat the final canvas as a 0 to 1 space.
# To keep the inset border from clipping on the far left, we start it at x = 0.02.
# Inset width is set to 0.30. Its midpoint is therefore at x = 0.17.
# By starting the main map exactly at x = 0.17, the inset is perfectly split:
# half hangs out (0.02 to 0.17) and half overlaps (0.17 to 0.32).

inset_w <- 0.245
inset_h <- 0.245  # Adjust height based on your preferred aspect ratio
main_x   <- 0.15  # The exact midpoint of the inset map

combined_maps <- ggdraw() +
  # 1. Draw the main map (shifted right to allow for the overhang)
  draw_plot(
    p_zoom, 
    x = 0, 
    y = 0, 
    width = 1.0, 
    height = 1.0
  ) +
  # 2. Draw the zoom-out inset map in the top-right corner
  draw_plot(
    p_indo, 
    x = 0.61,  # Pushes it to the right edge
    y = 0.8,  # Pushes it to the top edge
    width = inset_w, 
    height = inset_h
  )

# --- Save for Conservation Biology Submission ---
# The journal prefers standard metric widths: 
# 84 mm (single column) or 171-180 mm (double column). 
# This layout requires a double-column spread.
ggsave(
  filename    = "Manuscript_Map.png",
  plot        = combined_maps,
  device      = "png",
  width       = 180,         # Standard double-column width in mm
  height      = 150,         # Adjust based on your preferred final shape
  units       = "mm",
  dpi         = 600         # 300 DPI is standard for color; use 600 if text is tiny
)

ggsave(
  filename    = "Manuscript_Map.tif",
  plot        = combined_maps,
  device      = "tiff",
  width       = 180,         # Standard double-column width in mm
  height      = 150,         # Adjust based on your preferred final shape
  units       = "mm",
  dpi         = 600,         # 300 DPI is standard for color; use 600 if text is tiny
  compression = "lzw"        # LZW compression is strictly requested by Wiley/Journals
)




##FIGURE 2 OCCUPANCY MODEL EFFECTS####

library(ggplot2)
library(patchwork)
library(cowplot)

# PANEL A: Forest Cover (Top Left)
Effect_Forest$covariateValue <- Effect_Forest$covariateValue * 
  attr(scale(Covariates$Suitable2km), "scaled:scale") +
  attr(scale(Covariates$Suitable2km), "scaled:center")

plotForest <- ggplot(Effect_Forest, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Habitat Quality"), alpha = 0.2) +
  geom_line(aes(color = "Habitat Quality"), linewidth = 1) +
  
  # Y-axis label removed to make room for the global title
  labs(x = "Forest cover (%)", y = NULL) +
  
  scale_y_continuous(limits = c(0, 1)) + 
  scale_color_manual(name = NULL, 
                     values = c("Habitat Quality" = "forestgreen", "Anthropogenic Activity" = "coral1"),
                     labels = c("Forest Quality", "Anthropogenic Activity")) +
  scale_fill_manual(name = NULL, 
                    values = c("Habitat Quality" = "forestgreen", "Anthropogenic Activity" = "coral1"),
                    labels = c("Forest Quality", "Anthropogenic Activity")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )


# PANEL B: Human Footprint Index (Top Right - No Y-Axis)
Effect_HFI$covariateValue <- Effect_HFI$covariateValue* attr(scale(Covariates$hfi),"scaled:scale")+
  attr(scale(Covariates$hfi), "scaled:center")

plotHFI <- ggplot(Effect_HFI, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Anthropogenic Activity"), alpha = 0.2) +
  geom_line(aes(color = "Anthropogenic Activity"), linewidth = 1) +
  
  # # ADD THIS: An invisible point that forces the "Habitat Quality" legend entry to appear
  # geom_ribbon(aes(ymin = NA, ymax = NA, fill = "Habitat Quality"), alpha = 0.2)+
  # geom_line(aes(x = NA, y = NA,color = "Habitat Quality"), linewidth = 1) +
  
  labs(x = "Human Footprint Index (HFI)", y = NULL) +
  scale_y_continuous(limits = c(0, 1)) + 
  
  # Now both will appear because we've included both in the scale
  scale_color_manual(name = NULL, 
                     values = c("Habitat Quality" = "forestgreen", 
                                "Anthropogenic Activity" = "coral1"),
                     breaks = c("Habitat Quality", "Anthropogenic Activity"),
                     labels = c("Forest Quality", "Anthropogenic Activity"))+ # Order them
  scale_fill_manual(name = NULL, 
                    values = c("Habitat Quality" = "forestgreen", 
                               "Anthropogenic Activity" = "coral1"),
                    breaks = c("Habitat Quality", "Anthropogenic Activity"),
                    labels = c("Forest Quality", "Anthropogenic Activity")) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top",
    
    # ADD THESE LINES TO FIX THE BOLDING:
    axis.text.x = element_text(color = "black", size = 11),
    axis.title.x = element_text(size = 12)
  )


# PANEL C: Interaction (Bottom Left)
EdgeDensiti10 <- quantile(Covariates$EdgeDensity, probs = 0.10, na.rm = TRUE)
EdgeDensiti90 <- quantile(Covariates$EdgeDensity, probs = 0.90, na.rm = TRUE)
newdata <- expand.grid(
  TreeHeight2km_orig = seq(min(Covariates$TreeHeight), max(Covariates$TreeHeight), length.out = 100),
  hfi_orig = c(EdgeDensiti10, mean(Covariates$EdgeDensity), EdgeDensiti90),  # Low, mean, high hfi
  Slope = mean(OFC@siteCovs$Slope),     # Hold other detection covariates constant
  Ta = mean(OFC@siteCovs$Ta),
  Rain = mean(OFC@siteCovs$Rain),
  Model = names(which.max(table(OFC@siteCovs$Model))),
  # Suitable250DL = mean(OFC@siteCovs$Suitable250DL),  # Most common camera model
  hfi = mean(OFC@siteCovs$hfi),
  Suitable2km = mean(OFC@siteCovs$Suitable2km)
)

plotInteraction <- ggplot(newdata, aes(x = TreeHeight2km_orig, y = psi, 
                                       color = factor(hfi_orig, labels = c("Low", "Mean", "High")))) +
  geom_ribbon(aes(ymin = psi_lower, ymax = psi_upper, fill = factor(hfi_orig, labels = c("Low", "Mean", "High"))), 
              alpha = 0.2, color = NA) + 
  geom_line(linewidth = 1) + 
  
  # Y-axis label removed to make room for the global title
  labs(
    x = "Tree Height (m)", 
    y = NULL, 
    color = "Edge Density", 
    fill = "Edge Density",
    title = ""
  ) +
  scale_y_continuous(limits = c(0, 1)) + 
  scale_color_manual(values = c("Low" = "yellow", "Mean" = "orange", "High" = "red3")) +
  scale_fill_manual(values = c("Low" = "yellow", "Mean" = "orange", "High" = "red3")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )


# LAYOUT ASSEMBLY & SHARED TITLE

# 1. DEFINE THE GRID MATRIX (Same as before)

layout_design <- "
  AB
  C#
"


# 3. ASSEMBLY (Remove 'guides = "collect"')

combined_panels <- plotForest + plotHFI + plotInteraction + 
  plot_layout(design = layout_design) +
  plot_annotation(
    theme = theme(plot.margin = margin(t = 15, r = 5, b = 5, l = 35))
  )



# 4. DRAW LABELS & SHARED Y-TITLE (Your specific coordinates)

final_plot <- ggdraw(combined_panels) +
  
  # POSITION 1: Panel Labels 
  draw_label("A", x = 0.16, y = 0.93, fontface = "bold", size = 14, hjust = 0) +
  draw_label("B", x = 0.59, y = 0.93, fontface = "bold", size = 14, hjust = 0) + 
  draw_label("C", x = 0.16, y = 0.48, fontface = "bold", size = 14, hjust = 0) +
  
  # POSITION 2: Shared Y-axis Title 
  draw_label(
    "Occupancy Probability (ψ)", 
    x = 0.02,           
    y = 0.5,            
    angle = 90,         
    vjust = 0.5, 
    size = 13,
    fontface = "plain"
  )


# EXPORT

ggsave("Final_Effects_Plot.tif", final_plot, device = "tiff", 
       width = 180, height = 180, units = "mm", dpi = 600, compression = "lzw")

ggsave("Final_Effects_Plot.png", final_plot, device = "png", 
       width = 180, height = 180, units = "mm", dpi = 600)


##FIGURE 3 OCCUPANCY MODEL MAP####
library(ggplot2)
library(sf)
library(ggspatial)
library(cowplot)

p_map <- ggplot() +
  # 1. Base Map (Bottom layer)
  geom_sf(data = sulsel_map, fill = "grey95", color = "grey95", linewidth = 0.5) +
  
  # 2. OCCUPANCY RASTER
  geom_raster(data = current_df, aes(x = x, y = y, fill = Predicted)) +
  scale_fill_distiller(
    name = "Occupancy probability", 
    palette = "YlGnBu",                         
    direction = 1,                              
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    na.value = NA,
    guide = guide_colorbar(
      order = 1,
      barheight = unit(4.5, "cm"),   # STRETCHES THE GRADIENT TALLER
      barwidth = unit(0.6, "cm")     # MAKES THE GRADIENT WIDER
    )
  ) +
  
  # 3. PROTECTED AREAS 
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = ProtectedMM, 
    aes(color = "Protected Areas"), 
    fill = NA,          
    linewidth = 0.7                
  ) + 
  scale_color_manual(
    name = NULL, 
    values = c("Protected Areas" = "darkorange"), 
    guide = guide_legend(order = 3) 
  ) +
  
  # 4. DISTRIBUTION 2020 IUCN
  geom_sf(data = MMdist, aes(linetype = "Geographic distribution IUCN"), 
          color = "black", linewidth = 1, fill = NA) +
  scale_linetype_manual(
    name = NULL,
    values = c("Geographic distribution IUCN" = "dashed"),
    guide = guide_legend(
      order = 2,                    
      override.aes = list(linetype = "22", linewidth = 0.5)
    ) 
  ) +
  
  # 7. Scale Bar (Bottom-Right)
  annotation_scale(
    location = "br",                
    width_hint = 0.3,               
    text_cex = 0.9,                 
    pad_x = unit(0.5, "cm"),        
    pad_y = unit(0.5, "cm")         
  ) +
  
  # 8. North Arrow (Bottom-Right, above Scale Bar)
  annotation_north_arrow(
    location = "br",                
    which_north = "true",           
    pad_x = unit(0.5, "cm"),        
    pad_y = unit(1.2, "cm"),        
    style = north_arrow_orienteering(
      fill = c("black", "white"),   
      text_col = "black",
      text_size = 8
    )
  ) +
  
  # Coordinates and Theme
  coord_sf(xlim = c(119, 121.25), ylim = c(-5.9, -3.5), expand = FALSE) +
  scale_x_continuous(breaks = c(119, 120, 121)) +  
  scale_y_continuous(breaks = c(-6, -5, -4, -3)) +  
  theme_minimal() +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),  
    axis.title = element_blank(),
    
    
    # IN-FRAME COMPACT LEGEND SETTINGS
    
    legend.position = c(0.98, 0.98),          # Inside top-right
    legend.justification = c("right", "top"),
    
    # Creates one solid, semi-transparent box around all legends
    legend.box.background = element_rect(fill = NA, color = NA, linewidth = 0.5),
    legend.background = element_blank(),      # Removes individual legend backgrounds
    
    # Adds huge padding on the LEFT (l = 45) to create a blank column for the monkeys
    legend.margin = margin(t = 5, r = 5, b = 5, l = 45), 
    legend.title = element_text(size = 8, face = "bold", margin = margin(b = 8)),
    # Compresses vertical spacing between different legends
    legend.spacing.y = unit(0.4, "cm"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 7.5)
  )


# COMPOSE FINAL PLOT WITH SCALED MONKEYS

final_map <- ggdraw(p_map) +
  
  # LARGE MACAQUE (High Occupancy - Top of colorbar)
  draw_image(
    image = "macacamaura.png", 
    x = 0.77,                # Positioned in the left-margin of the legend box
    y = 0.83,                # Aligned near the top (1.0)
    width = 0.12,            # Large size
    height = 0.12,           
    hjust = 0, vjust = 0
  ) +
  
  # SMALL MACAQUE (Low Occupancy - Bottom of colorbar)
  draw_image(
    image = "macacamaura.png", 
    x = 0.78,                # Positioned slightly further right to align centers
    y = 0.665,                # Aligned near the bottom (0.0)
    width = 0.05,            # Small size
    height = 0.05,           
    hjust = 0, vjust = 0
  )

# Display
final_map




# EXPORT FINAL MAP


# 1. Save as TIFF (Best for journal submission)
ggsave(
  filename = "Macaque_Occupancy_Map.tif",
  plot = final_map,
  device = "tiff",
  width = 180,            # Width in mm (Standard full-page width)
  height = 180,           # Height in mm (Adjust to fit your desired aspect ratio)
  units = "mm",
  dpi = 300,              # High resolution for print
  compression = "lzw",    # Compresses the TIFF file size without losing quality
  bg = "white"            # Forces a solid white background
)

# 2. Save as PNG (Best for presentations and quick sharing)
ggsave(
  filename = "Macaque_Occupancy_Map_PPT30m.png",
  plot = final_map,
  device = "png",
  width = 180, 
  height = 180, 
  units = "mm",
  dpi = 300,
  bg = "white"
)


##FIGURE 4 SOCIOECONOMIC SCENARIOS####
plot_data <- readRDS("plot_data_Figure4.rds")

p_socioec <- ggplot(plot_data, aes(x = Year, y = PercRemaining, 
                                   group = Scenario_Type)) +
  geom_line(linewidth = 1, alpha = 0.6, color = "gray50") +
  geom_point(aes(size = abs(AreaLost_HighOcc_km2), 
                 color = abs(AreaLost_HighOcc_km2)),
             alpha = 0.8) +
  geom_text(aes(label = ifelse(Year != 2023, Scenario_Type, "")), 
            vjust = -1.5, 
            hjust = ifelse(plot_data$Year == 2100, 1, 0.5),  
            nudge_x = ifelse(plot_data$Year == 2100, 0.2, 0), 
            size = 3, 
            check_overlap = TRUE)  +
  scale_color_gradient(low = "lightcoral", high = "darkred", 
                       name = "",
                       guide = "none") +
  scale_size_continuous(name = "Area Lost (km²)",
                        range = c(4, 15),
                        limits = c(2520, 7390),
                        guide = guide_legend(override.aes = list(
                          color = c("lightcoral",  "#c24c44", "#b1392e", "#9d2118", "darkred")
                        ))) +
  scale_x_continuous(breaks = unique(plot_data$Year),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100),
                     expand = c(0, 0)) +
  
 
# 1. ALLOWS CIRCLES TO DRAW OUTSIDE THE BOUNDARY LINE

coord_cartesian(clip = "off") +
  
  labs(title = "",
       x = "Period",
       y = "Remaining Geographic Distribution (%)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.15, "cm"),
        
        # Split axis.text so we can target X and Y independently
        axis.text.y = element_text(color = "black", size = 11),
        
        # 2. PUSHES X-AXIS LABELS DOWN (t = top margin)
        axis.text.x = element_text(color = "black", size = 11, vjust = -5),
        
        axis.title.y = element_text(size = 12),
        
        # 3. PUSHES X-AXIS TITLE DOWN
        axis.title.x = element_text(size = 12, margin = margin(t = 15)),
        
        legend.position = "right",
        legend.box = "vertical",
        
        # 4. ADDS EXTRA BOTTOM MARGIN TO THE WHOLE PLOT SO LOWERED TEXT ISN'T CUT OFF
        plot.margin = margin(t = 10, r = 10, b = 40, l = 10))

p_socioec


ggsave("Figure 5 (MS).png", 
       plot = p_socioec,
       width = 180, 
       height = 120,
       dpi = 600,
       units = "mm")

ggsave("Figure 5 (MS).tif", 
       plot = p_socioec,
       width = 180, 
       height = 120,
       dpi = 600,
       units = "mm",
       compression = "lzw")



##FIGURE SUPLEMENTARY SSP-RCP MAPS####




# 1. LOAD & PREPARE REUSABLE SPATIAL LAYERS 

current    <- rast("XXXX")
MMdist     <- st_read("XXXX") %>% st_transform(crs = crs(current))
sulsel_map <- st_read("XXXX") %>% st_transform(crs = crs(current)) 

current_extent <- st_as_sfc(st_bbox(current)) %>% st_as_sf()

# Process Protected Areas mapping 
Protected_sf <- st_read("XXXX") %>% 
  st_transform(crs = crs(current)) %>% 
  filter(F_SK_362 %in% c("KK", "HL")) %>% 
  st_filter(current_extent, .predicate = st_intersects)

current_pred <- current[["Predicted"]]


# 2. DEFINE SCENARIOS, YEARS, AND LABELS

scenarios <- c('SSP1_RCP26', 'SSP2_RCP45', 'SSP4_RCP34')
years     <- c(2050, 2075, 2100)

col_titles <- c("Low-emissions\nsustainability", 
                "Current development\ntrends", "Moderate-emissions\ninequality")


plot_list <- list()

# 3. LOOP AND GENERATE THE 18 INDIVIDUAL PANEL MAPS

for (y_idx in seq_along(years)) {
  for (s_idx in seq_along(scenarios)) {
    
    scenario <- scenarios[s_idx]
    year     <- years[y_idx]
    panel_id <- paste0(scenario, "_", year)
    
    future_file <- paste0("XXXX", scenario, "_", year, ".tif")
    
    if (!file.exists(future_file)) {
      plot_list[[panel_id]] <- ggplot() + theme_void()
      next
    }
    
    future_raster <- rast(future_file)
    future_raster <- project(future_raster, crs(current))
    future_raster <- resample(future_raster, current, method = "bilinear")
    future_pred   <- future_raster[["Predicted"]]
    
    # --- DYNAMIC AXIS LOGIC ---
    # Only show Y-axis coordinates on the leftmost column
    if (s_idx == 1) {
      y_text <- element_text(size = 8, color = "black")
    } else {
      y_text <- element_blank()
    }
    
    # Only show X-axis coordinates on the bottom row
    if (y_idx == length(years)) {
      x_text <- element_text(size = 8, color = "black")
    } else {
      x_text <- element_blank()
    }
    # ---------------------------
    
    # Generate Panel Map
    p_panel <- ggplot() +
      geom_sf(data = sulsel_map, fill = "grey95", color = NA) + # Removed borders from the landmass itself
      
      # 1. Baseline Background (Firebrick) - Occupancy Part 1
      geom_spatraster(data = current_pred, aes(fill = Predicted), show.legend = FALSE) +
      scale_fill_gradient(low = "firebrick", high = "firebrick", na.value = NA) +
      
      # 2. Future Occupancy - Occupancy Part 2
      ggnewscale::new_scale_fill() +
      geom_spatraster(data = future_pred) +
      scale_fill_distiller(name = "Occupancy probability", palette = "YlGnBu", direction = 1, 
                           limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), na.value = NA, guide = guide_colorbar(order = 1)) +
      
      # 3. Protected Areas (Printed ON TOP of occupancy)
      ggnewscale::new_scale_color() +
      geom_sf(data = ProtectedMM, aes(color = "Protected Areas"), fill = NA, linewidth = 0.25) + 
      scale_color_manual(name = NULL, values = c("Protected Areas" = "darkorange"), guide = guide_legend(order = 2)) +
      
      # 4. Historic Distribution & Limits
      geom_sf(data = MMdist, color = "black", linetype = "dashed", linewidth = 0.5, fill = NA) +
      coord_sf(xlim = c(119.3, 120.5), ylim = c(-5.9, -3.5), expand = FALSE) +
      scale_x_continuous(breaks = c(120)) +
      scale_y_continuous(breaks = c(-3.5, -4.5, -5.5)) +
      
      # Clean Theme
      theme_minimal() +  
      theme(
        panel.background = element_rect(fill = "white", color = NA), # Forces clean white ocean
        panel.grid       = element_blank(), 
        axis.text.y      = y_text,   # Applies dynamic Y axis
        axis.text.x      = x_text,   # Applies dynamic X axis
        axis.title       = element_blank(), 
        legend.position  = "none",
        plot.margin      = margin(2, 2, 2, 2, "pt") # Slight margin so outer coordinates don't clip
      )
    
    # Inject Column Titles (Top Row Only)
    if (y_idx == 1) {
      p_panel <- p_panel + labs(title = col_titles[s_idx]) +
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5, lineheight = 1.1))
    }
    
    # Inject Row Titles/Years (Left Column Only)
    if (s_idx == 1) {
      p_panel <- p_panel + ylab(as.character(year)) +
        theme(axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 5)))
    }
    
    # --- SINGLE SCALE BAR & NORTH ARROW ---
    # Only inject on the absolute bottom-right map
    if (s_idx == length(scenarios) && y_idx == length(years)) {
      p_panel <- p_panel + 
        annotation_scale(
          location = "br", 
          width_hint = 0.25,               
          height = unit(0.1, "cm"),        
          text_size = 8                    
        ) +
        annotation_north_arrow(
          location = "br", 
          which_north = "true", 
          pad_x = unit(0.2, "cm"), 
          pad_y = unit(0.6, "cm"),        
          height = unit(0.4, "cm"),       
          width = unit(0.4, "cm")                    
        )
    }
    
    plot_list[[panel_id]] <- p_panel
  }
}

# 4. COMPILATION AND LAYOUT
combined_grid <- wrap_plots(plot_list, ncol = 3, byrow = TRUE) + 
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "right",
    legend.margin   = margin(t = 5, r = 5, b = 5, l = 15),
    # --- LEGEND SIZE CONTROLS ---
    legend.title      = element_text(size = 12, face = "bold", margin = margin(b = 10, unit = "pt")), 
    legend.text       = element_text(size = 12), 
    legend.key.height = unit(1.5, "cm"),  # Makes the colorbar taller/longer
    legend.key.width  = unit(1.5, "cm"),  # Makes the colorbar wider/thicker
    
    # --- INDEPENDENT SPACING CONTROL ---
    panel.spacing.x = unit(3, "pt"), # Increases horizontal space between columns
    panel.spacing.y = unit(3, "pt"),  # Keeps vertical space tight between rows
         
    plot.margin     = margin(5, 5, 5, 5, "pt")
  )

# Add Illustrations (You may need to slightly adjust x/y coordinates now that axes take up a tiny bit of room)
final_map_with_monkeys <- ggdraw(combined_grid) +
  draw_image("macacamaura.png", x = 0.74, y = 0.57, width = 0.07, height = 0.07) +
  draw_image("macacamaura.png", x = 0.75, y = 0.38, width = 0.03, height = 0.03)

# SAVE FIGURE
ggsave(
  filename = "Figure S4 (SSP-RCP Maps).png",
  plot = final_map_with_monkeys,
  device = "png",
  width = 14,            
  height = 12,           
  units = "in",
  dpi = 600,              
  bg = "white"            
)

ggsave(
  filename = "Figure S4 (SSP-RCP Maps).tif",
  plot = final_map_with_monkeys,
  device = "tiff",
  width = 14,            # Width in mm (Standard full-page width)
  height = 12,           # Height in mm (Adjust to fit your desired aspect ratio)
  units = "in",
  dpi = 600,              # High resolution for print
  compression = "lzw",    # Compresses the TIFF file size without losing quality
  bg = "white"            # Forces a solid white background
)

##PLOT Figure S2 (scale effect) V2.0 ####

library(ggplot2)
library(patchwork)

# 1. DEFINE THE COMMON THEME
my_custom_theme <- theme_minimal() +
  theme(
    # Remove all gridline squares inside the plot
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Add solid black bars for the x and y axes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Add the ticks back to the axes
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    
    # Text and Legend layout
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none" 
  )

# 2. CREATE THE SIX PLOTS

#### FOREST COVER (TOP ROW) ###

# Forest Cover 0.25km (Top Left) - mf.1
Effect_Forest0.25 <- plotEffectsData(mf.1, type = "state", "Suitable250")
Effect_Forest0.25$covariateValue <- Effect_Forest0.25$covariateValue * attr(scale(Covariates$Suitable250), "scaled:scale") +
  attr(scale(Covariates$Suitable250), "scaled:center")

plotForestCover0.25km <- ggplot(Effect_Forest0.25, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "forestgreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "forestgreen") + 
  labs(
    x = "Forest cover (%)", 
    y = "Occupancy Probability (ψ)" # Y-axis labeled for the far-left side
  ) +
  ylim(0, 1) +
  my_custom_theme +
  theme(
    plot.tag = element_text(size = 12, face = "bold", hjust = 0),
    plot.tag.position = c(0.25, 1.02) # PUSHED MORE (Accounts for Y-axis title)
  )

# Forest Cover 2km (Top Middle) - mf.3
Effect_Forest2 <- plotEffectsData(mf.3, type = "state", "Suitable2km")
Effect_Forest2$covariateValue <- Effect_Forest2$covariateValue * attr(scale(Covariates$Suitable2km), "scaled:scale") +
  attr(scale(Covariates$Suitable2km), "scaled:center")

plotForestCover2km <- ggplot(Effect_Forest2, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "forestgreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "forestgreen") + 
  labs(
    x = "Forest cover (%)", 
    y = NULL # Y-axis removed to prevent clutter
  ) +
  ylim(0, 1) +
  my_custom_theme +
  theme(
    plot.tag = element_text(size = 12, face = "bold", hjust = 0),
    plot.tag.position = c(0.2, 1.02) # PUSHED LESS (No Y-axis title conflict)
  )

# Forest Cover 6.25km (Top Right) - mf.5
Effect_Forest6 <- plotEffectsData(mf.5, type = "state", "Suitable1250")
Effect_Forest6$covariateValue <- Effect_Forest6$covariateValue * attr(scale(Covariates$Suitable1250), "scaled:scale") +
  attr(scale(Covariates$Suitable1250), "scaled:center")

plotForestCover6.25km <- ggplot(Effect_Forest6, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "forestgreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "forestgreen") +
  labs(
    x = "Forest cover (%)", 
    y = NULL # Y-axis removed to prevent clutter
  ) +
  ylim(0, 1) +
  my_custom_theme +
  theme(
    plot.tag = element_text(size = 12, face = "bold", hjust = 0),
    plot.tag.position = c(0.2, 1.02) # PUSHED LESS (No Y-axis title conflict)
  )


#### INTER-PATCH DISTANCE (BOTTOM ROW) ###

# Patch Distance 0.25km (Bottom Left) - md.1
Effect_PatchDistance0.25 <- plotEffectsData(md.1, type = "state", "PatchDistance250")
Effect_PatchDistance0.25$covariateValue <- Effect_PatchDistance0.25$covariateValue * attr(scale(Covariates$PatchDistance250), "scaled:scale") +
  attr(scale(Covariates$PatchDistance250), "scaled:center")

plotPatchDistance0.25km <- ggplot(Effect_PatchDistance0.25, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkgreen") +
  ylim(0, 1) +
  labs(
    x = "Forest Inter-Patch Distance (m)", 
    y = "Occupancy Probability (ψ)" # Y-axis labeled for the far-left side
  ) +
  my_custom_theme +
  theme(
    plot.tag = element_text(size = 12, face = "bold", hjust = 0),
    plot.tag.position = c(0.25, 1.02) # PUSHED MORE (Accounts for Y-axis title)
  )

# Patch Distance 2km (Bottom Middle) - md.3
Effect_PatchDistance2 <- plotEffectsData(md.3, type = "state", "PatchDistance2km")
Effect_PatchDistance2$covariateValue <- Effect_PatchDistance2$covariateValue * attr(scale(Covariates$PatchDistance2km), "scaled:scale") +
  attr(scale(Covariates$PatchDistance2km), "scaled:center")

plotPatchDistance2km <- ggplot(Effect_PatchDistance2, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkgreen") +
  ylim(0, 1) +
  labs(
    x = "Forest Inter-Patch Distance (m)", 
    y = NULL # Y-axis removed to prevent clutter
  ) +
  my_custom_theme +
  theme(
    plot.tag = element_text(size = 12, face = "bold", hjust = 0),
    plot.tag.position = c(0.2, 1.02) # PUSHED LESS (No Y-axis title conflict)
  )

# Patch Distance 6.25km (Bottom Right) - md.5
Effect_PatchDistance6 <- plotEffectsData(md.5, type = "state", "PatchDistance1250")
Effect_PatchDistance6$covariateValue <- Effect_PatchDistance6$covariateValue * attr(scale(Covariates$PatchDistance1250), "scaled:scale") +
  attr(scale(Covariates$PatchDistance1250), "scaled:center")

plotPatchDistance6.25km <- ggplot(Effect_PatchDistance6, aes(x = covariateValue, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkgreen") +
  ylim(0, 1) +
  labs(
    x = "Forest Inter-Patch Distance (m)", 
    y = NULL # Y-axis removed to prevent clutter
  ) +
  my_custom_theme +
  theme(
    plot.tag = element_text(size = 12, face = "bold", hjust = 0),
    plot.tag.position = c(0.2, 1.02) # PUSHED LESS (No Y-axis title conflict)
  )

# 3. COMBINE INTO A 2x3 GRID (FOREST TOP, PATCH DISTANCE BOTTOM)

# Row 1: Forest 0.25km + Forest 2km + Forest 6.25km
# Row 2: Patch 0.25km + Patch 2km + Patch 6.25km
final_plot <- (plotForestCover0.25km + plotForestCover2km + plotForestCover6.25km) / 
  (plotPatchDistance0.25km + plotPatchDistance2km + plotPatchDistance6.25km)

# Add overarching annotations and custom tags based on the 6-panel order
final_plot <- final_plot +
  plot_annotation(
    tag_levels = list(c("A) 0.25km²", "B) 2km²", "C) 6.25km²", "D) 0.25km²", "E) 2km²", "F) 6.25km²"))
  ) &
  theme(
    axis.title.y = element_text(margin = margin(r = 10)) # Gives breathing room to the Y-axis title
  )

# Display the layout
print(final_plot)

# Note: You may want to increase the 'width' parameter slightly since you are moving from 2 columns to 3. 
# 220mm to 240mm is usually a good starting point for a 3-column figure.
ggsave("Figure S2test.tif", final_plot, device = "tiff", 
       width = 240, height = 150, units = "mm", dpi = 600, compression = "lzw")
ggsave("Figure S2test.png", final_plot, device = "png",
       width = 240, height = 150, units = "mm", dpi = 600)
