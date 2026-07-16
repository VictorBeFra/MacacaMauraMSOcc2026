library(raster)
library(terra)
library(sf)
library(landscapemetrics)
library(dplyr)
library(tidyr)

forest <- rast("XXXX/ForestClasses30.tif")

forest <- rast("C:/Users/Usuario/Desktop/MacacaMauraSurvey/Landscape Metrics/Forest10m.tif")

points <- st_read("XXXX/SamplingPoints2023.shp")

pointsDL <- st_read("XXXX/Landscape Metrics/DL/DL.shp")

target_crs <- "EPSG:32750"
forest <- project(forest, target_crs, method = "near")
points <- st_transform(pointsDL, crs = crs(forest))

check_landscape(forest)

# Convert 2 sq km to a buffer radius (in meters)
area <- 2 * 1e6  # 2 sq km in sq meters
radius <- sqrt(area)/2  # Radius for a circular buffer

##### EDGE DENSITY ####

resultsED <-scale_sample(forest, y = points, shape = "square", size = radius, what = "lsm_c_ed")

EdgeDensity_2km <- resultsED %>% filter(class != 0)

EdgeDensity_2km <- EdgeDensity_2km %>%
  left_join(st_drop_geometry(points), by = c("plot_id" = "id"))
EdgeDensity_2km <- EdgeDensity_2km[, !(colnames(EdgeDensity_2km) %in% c("PageNumber", "ORIG_FID"))]

mean_EDClass_2km <- EdgeDensity_2km %>%
  group_by(PageName, class) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

# Create a new column for grouped classes
EDGrouped_2km <- EdgeDensity_2km %>%
  mutate(class_group = case_when(
    class %in% c(1, 2, 3, 4) ~ "SuitableForest",  # Classes 1, 2, 3, 4
    class %in% c(5, 6) ~ "NonSuitableForest"          # Classes 5, 6
  ))

# Calculate the mean metric value for each point_name and class group
mean_EDGrouped_2km <- EDGrouped_2km %>%
  group_by(PageName, class_group) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

##### PATCH DISTANCE BUFFERS ####
resultsPDistB <-scale_sample(forest, y = points, shape = "circle", size = c(250, 707.1, 1250), what = "lsm_c_enn_mn")

PatchDistance_B <- resultsPDistB %>% filter(class != 0)

PatchDistance_B <- PatchDistance_B %>%
  left_join(st_drop_geometry(points), by = c("plot_id" = "id"))
#PatchDistance_B <- PatchDistance_B[, !(colnames(PatchDistance_B) %in% c("Name"))]

mean_PDistClass_B <- PatchDistance_B %>%
  group_by(Name, size, class) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

mean_PDistClass_B <- mean_PDistClass_B %>%
  pivot_wider(names_from = size, values_from = mean_value, names_prefix = "size_")

