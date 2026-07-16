library(raster)
library(terra)
library(sf)
library(landscapemetrics)
library(dplyr)
library(tidyr)
library(rasterImage)
library(unmarked)
library(fs)
library(stars)
library(ggplot2)
library(glue)
library(viridis)

#CALCULATE EDGE DENSITY####
# 1. Define all scenarios
scenarios <- c(
  'SSP1_RCP26_2050', 'SSP1_RCP26_2075', 'SSP1_RCP26_2100',
  'SSP2_RCP45_2050', 'SSP2_RCP45_2075', 'SSP2_RCP45_2100',
  'SSP4_RCP34_2050', 'SSP4_RCP34_2075', 'SSP4_RCP34_2100',
  )


# 2. Load base layers (only once)
forest <- rast("XXXX")
points <- st_read("XXXX")

# 3. Define buffer parameters
area <- 2 * 1e6  # 2 sq km in sq meters
radius <- sqrt(area)  # Radius for circular buffer

# 4. Process each scenario
for(scenario in scenarios) {
  cat("\nProcessing", scenario, "...\n")
  
  # A. Load and prepare forest projection
  input_path <- paste0("XXXX", scenario, "_raster.tif")
  forest_proj <- rast(input_path)
  
  # B. Reproject and align with base forest
  forest_proj <- project(forest_proj, target_crs, method = "near")
  forest_proj <- resample(forest_proj, forest, method = "near")
  forest_clean <- forest * (forest_proj == 1)  # Keep only pixels = 1
  
  # C. Calculate edge density
  resultsED <- scale_sample(forest_clean, y = pointsR, shape = "square", 
                            size = radius, what = "lsm_c_ed")
  
  # D. Filter results (keep class=1 if duplicates)
  EdgeDensity_2km <- resultsED %>% filter(class != 0)
  
  # E. Create output raster (WITH WHITE LINE FIX)
  results_with_coords <- EdgeDensity_2km %>%
    left_join(
      points %>%
        mutate(
          x = st_coordinates(.)[,1],
          y = st_coordinates(.)[,2]
        ) %>%
        st_drop_geometry(),
      by = c("plot_id" = "Id")
    ) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(points))
  
  # Calculate pixel size (2 km² area)
  pixel_side <- 0.01274066  # ~1414.214 meters
  
  # Adjust extent to exact multiples of pixel size
  original_ext <- ext(vect(points))
  adjusted_ext <- align(original_ext, pixel_side)
  
  # Create raster with adjusted extent
  r <- rast(adjusted_ext, resolution = pixel_side, crs = "EPSG:4326")
  
  # Rasterize points
  ed_raster <- rasterize(vect(results_with_coords), r, field = "value")
  
  # F. Save output
  output_path <- paste0("XXXX", scenario, ".tif")
  
  writeRaster(ed_raster, filename = output_path, 
              filetype = "GTiff", overwrite = TRUE,
              NAflag = -9999)  # Explicit NA value
  
  cat("Saved to:", output_path, "\n")
  
}

#CALCULATE FOREST PERCENTAGE####
area2km <- read.csv("XXXX", 
                    header = TRUE, sep = ",")

# Pixel size for 2 km² area
pixel_side <- 0.01274066  # ~1414.214 meters

# Process each scenario
for(scenario in scenarios) {
  cat("\nProcessing", scenario, "...\n")
  
  # A. Load forest percentage data
  forest_file <- paste0("C:/Users/Usuario/Desktop/MacacaMauraSurvey/R/Macaca Maura Occupancy Models/Predict/FUTURE PROJECTION/Forest/ForestPercentage_", scenario, ".csv")
  forestperc <- read.csv(forest_file, header = TRUE, sep = ",")
  
  # B. Calculate forest percentage
  forestperc$area <- area2km$area
  forestperc$Class_1 <- forestperc$Class_1 / forestperc$area
  forestperc$Class_3 <- forestperc$Class_3 / forestperc$area
  forestperc$forestperc <- forestperc$Class_1 + forestperc$Class_3
  
  # C. Join with spatial points
  forest_sf <- forestperc %>%
    left_join(
      points %>%
        mutate(
          x = st_coordinates(.)[,1],
          y = st_coordinates(.)[,2]
        ) %>%
        st_drop_geometry(),
      by = c("id" = "Id")
    ) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(points))
  
  # D. Create and align raster (with white line fix)
  original_ext <- ext(vect(points))
  adjusted_ext <- align(original_ext, pixel_side)
  r <- rast(adjusted_ext, resolution = pixel_side, crs = "EPSG:4326")
  
  # E. Rasterize points
  forest_raster <- rasterize(vect(forest_sf), r, field = "forestperc")
  
  # F. Save output
  output_path <- paste0("XXXX", scenario, ".tif")
  
  writeRaster(forest_raster, filename = output_path,
              filetype = "GTiff", overwrite = TRUE,
              NAflag = -9999)
 }

#CALCULATE TREE HEIGHT####
# Pixel size for 2 km² area
pixel_side <- 0.01274066  # ~1414.214 meters

# Process each scenario
for(scenario in scenarios) {
  cat("\nProcessing", scenario, "...\n")
  
  # A. Load forest percentage data
  TH_file <- paste0("XXXX", scenario, ".csv")
  TH <- read.csv(TH_file, header = TRUE, sep = ",")
  
  # C. Join with spatial points
  TH_sf <- TH %>%
    left_join(
      points %>%
        mutate(
          x = st_coordinates(.)[,1],
          y = st_coordinates(.)[,2]
        ) %>%
        st_drop_geometry(),
      by = c("id" = "Id")
    ) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(points))
  
  # D. Create and align raster (with white line fix)
  original_ext <- ext(vect(points))
  adjusted_ext <- align(original_ext, pixel_side)
  r <- rast(adjusted_ext, resolution = pixel_side, crs = "EPSG:4326")
  
  # E. Rasterize points
  TH_raster <- rasterize(vect(TH_sf), r, field = "max")
  
  # F. Save output
  output_path <- paste0("XXXX", scenario, ".tif")
  
  writeRaster(TH_raster, filename = output_path,
              filetype = "GTiff", overwrite = TRUE,
              NAflag = -9999)
  
}

#ESTIMATE OCCUPANCY####
EdgeDensity <- rast("C:/Users/Usuario/Desktop/MacacaMauraSurvey/R/Macaca Maura Occupancy Models/Predict/FUTURE PROJECTION/Edge Density/edge_density_SSP1_RCP19_2050.tif")
EdgeDensity <- project(EdgeDensity, crs("EPSG:4326"))
TreeHeight2km = rast("C:/Users/Usuario/Desktop/MacacaMauraSurvey/R/Macaca Maura Occupancy Models/Predict/FUTURE PROJECTION/Tree Height/MaxTH_SSP1_RCP19_2050.tif")
hfi = rast("./Predict/HFIe.tif")
forest = rast("C:/Users/Usuario/Desktop/MacacaMauraSurvey/R/Macaca Maura Occupancy Models/Predict/FUTURE PROJECTION/Forest/forest_percentage_SSP1_RCP19_2050.tif")
plot(TreeHeight2km)

names(EdgeDensity) <- "EdgeDensity" 
names(TreeHeight2km) <- "TreeHeight2km" 
names(hfi) <- "hfi"
names(forest) <- "forest"

EdgeDensitye <- (EdgeDensity-75.8889)/41.68581

TreeHeight2kme <- (TreeHeight2km-30.34071)/6.458333

foreste <- (forest-0.5118001)/0.2947601

#Obtener betas del modelo para predecir la ocupación
(betas<-coef(best, type="state"))

betas[1] #Intercept
betas[2] #TreeHeight2km
betas[3] #EdgeDensity
betas[4] #hfi
betas[5] #Suitable2km
betas[6] #TreeHeight2km:EdgeDensity

logit.psi <- betas[1] + betas[2]*TreeHeight2kme + betas[4]*hfie + betas[5]*foreste + betas[3]*EdgeDensitye + betas[6]*(TreeHeight2kme+EdgeDensitye)

psifull <- exp(logit.psi) / (1 + exp(logit.psi))

plot(psifull, col=terrain.colors(100))

library(terra)
library(glue)

# Define all scenarios

# Load static HFI layer
hfie <- rast("XXXX")
names(hfie) <- "hfi"

# Standardization parameters (from your model)
edge_mean <- 75.8889
edge_sd <- 41.68581
height_mean <- 30.34071
height_sd <- 6.458333
forest_mean <- 0.5118001
forest_sd <- 0.2947601

# Process each scenario
for(scenario in scenarios) {
  cat("\nProcessing", scenario, "...\n")
  
  # Extract year from scenario name
  year <- substr(scenario, nchar(scenario)-3, nchar(scenario))
  
  # Load scenario-specific rasters
  edge_file <- glue("XXXX{scenario}.tif")
  height_file <- glue("XXXX{scenario}.tif")
  forest_file <- glue("XXXX{scenario}.tif")
  
  EdgeDensity <- rast(edge_file)
  EdgeDensity <- project(EdgeDensity, crs("EPSG:4326"))
  EdgeDensity <- resample(EdgeDensity, forest,method="bilinear")
  hfie <- resample(hfie, forest,method="bilinear")
  TreeHeight2km <- rast(height_file)
  forest <- rast(forest_file)
  
  # Standardize variables
  EdgeDensitye <- (EdgeDensity - edge_mean)/edge_sd
  TreeHeight2kme <- (TreeHeight2km - height_mean)/height_sd
  foreste <- (forest - forest_mean)/forest_sd
  
  
  raster_stack <- c(TreeHeight2kme, hfie, EdgeDensitye, foreste) #stack all rasters in one stacked_raster
  names(raster_stack) <- c("TreeHeight2km", "hfi", "EdgeDensity", "Suitable2km") 
  
  psi <- predict(best, type="state", newdata=raster_stack)
  
  
  # Plot and save results
  plot(psi)
  
  output_file <- glue("XXXX{scenario}.tif")
  writeRaster(psi, filename = output_file, overwrite = TRUE)
  
  cat("Saved occupancy prediction to:", output_file, "\n")
}

###OCCUPANCY CHANGE####
# Install packages if you haven't already
# install.packages("terra")

# Load the package
library(terra)
library(ggplot2)
library(dplyr)


# Read the baseline raster for 2023
base_2023 <- rast("XXXX") # Replace with your actual path

# Define your scenarios (adjust names to match your files)
scenarios <- c("SSP1_RCP26", "SSP2_RCP45", "SSP4_RCP34")
years <- c(2050, 2075, 2100)

# Create an empty list to store the future rasters
future_rasters <- list()

# Loop through scenarios and years to read all files
for (scen in scenarios) {
  for (yr in years) {
    # Create the file name pattern
    file_name <- paste0("XXXX", scen, "_", yr, ".tif")
    # Create a name for the list element
    list_name <- paste(scen, yr, sep = "_")
    # Read the raster and store it in the list
    future_rasters[[list_name]] <- rast(file_name)
  }
}

# Check if the geometry of one future raster matches the base
compareGeom(base_2023, future_rasters[[1]])
# If this returns TRUE, you are good. If not, you may need to resample.
# Let's assume they are all aligned. If not, you can resample the future rasters to the base:
# Apply resample() to all rasters
for (i in 1:length(future_rasters)) {
 
  future_rasters[[i]] <- resample(future_rasters[[i]], base_2023, method = "bilinear")
  
  
  }

# Verify that all future rasters are aligned with the base raster
for (i in 1:length(future_rasters)) {
  check <- compareGeom(base_2023, future_rasters[[i]])
  cat("Raster", i, names(future_rasters)[i], "aligned:", check, "\n")
}

# Create an empty list to store the results for each scenario
results_list <- list()

# Loop through each future scenario in the list
for (i in 1:length(future_rasters)) {
  
  fut_raster <- future_rasters[[i]]     # Get one future raster
  scen_name <- names(future_rasters)[i] # Get its name (e.g., "SSP1_RCP19_2050")
  
  # 0. extract the Predicted layer from both rasters
  base_pred <- base_2023$Predicted
  fut_pred <- fut_raster$Predicted
  
  # 1. Calculate the raw difference raster (Future - 2023)
  diff_raster <- fut_pred - base_pred
  
  # 2. Create a binary raster showing WHERE change occurred (ignoring NA)
  change_mask <- diff_raster
  change_mask[!is.na(change_mask)] <- 1 # Set all non-NA cells to 1 initially
  # Now, set cells to NA where the difference is exactly zero (no change)
  no_change_cells <- abs(diff_raster) < 1e-9 # Using a tiny tolerance for float comparison
  change_mask[no_change_cells] <- NA
  
  # 3. Count Pixels (Number of Cells)
  total_cells <- global(!is.na(base_pred), "sum", na.rm = TRUE)[1,1] # Cells with data in 2023
  
  # Count cells with data in the future projection
  future_cells <- global(!is.na(fut_pred), "sum", na.rm = TRUE)[1,1]
  
  # Count cells that LOST data (were data in 2023 but are NA in the future)
  lost_data_cells <- total_cells - global(!is.na(fut_pred + base_pred), "sum", na.rm = TRUE)[1,1]
  
  # Count cells that GAINED data (were NA in 2023 but have data in the future)
  gained_data_cells <- future_cells - global(!is.na(fut_pred + base_pred), "sum", na.rm = TRUE)[1,1]
  
  # Count cells where change occurred (both have data and values are different)
  changed_cells <- global(change_mask, "sum", na.rm = TRUE)[1,1]
  
  #Mean and SD TOTAL OCCUPANCY
  # For cells that have data in 2023
  mean_occ_2023 <- global(base_pred, "mean", na.rm = TRUE)[1,1]
  sd_occ_2023 <- global(base_pred, "sd", na.rm = TRUE)[1,1]
  
  # For cells that have data in future scenario
  mean_occ_future <- global(fut_pred, "mean", na.rm = TRUE)[1,1]
  max_occ_future <- global(fut_pred, "max", na.rm = TRUE)[1,1]
  min_occ_future <- global(fut_pred, "min", na.rm = TRUE)[1,1]
  sd_occ_future <- global(fut_pred, "sd", na.rm = TRUE)[1,1]
  
  # 4. Calculate Intensity of Change (only on cells that have data in BOTH periods)
  # Create a mask of cells that have data in both rasters (USANDO SOLO LA CAPA Predicted)
  both_data_mask <- !is.na(fut_pred + base_pred)
  # Extraer valores de diferencia SOLO donde ambos rasters tienen datos
  diff_values <- diff_raster[both_data_mask]
  
  mean_change <- mean(diff_values, na.rm = TRUE)
  sd_change <- sd(diff_values, na.rm = TRUE)
  
  # 5. NEW: Calculate cells with occupancy > 0.4 in 2023
  high_occ_mask_2023 <- base_pred > 0.4
  total_cells_high_occ_2023 <- global(high_occ_mask_2023, "sum", na.rm = TRUE)[1,1]
  
  # Count future cells that overlap with high occupancy 2023 areas
  future_cells_high_occ <- global(!is.na(fut_pred) & high_occ_mask_2023, "sum", na.rm = TRUE)[1,1]
  
  # 6. NEW: Calculate occupancy categories for 2023
  low_occ_2023 <- global(base_pred <= 0.4, "sum", na.rm = TRUE)[1,1]
  high_occ_2023 <- global(base_pred > 0.4, "sum", na.rm = TRUE)[1,1]
  
  # NEW: Calculate occupancy categories for future scenario
  low_occ_future <- global(fut_pred <= 0.4, "sum", na.rm = TRUE)[1,1]
  high_occ_future <- global(fut_pred > 0.4, "sum", na.rm = TRUE)[1,1]
  
  # NEW: Calculate percentages for 2023
  perc_low_occ_2023 <- (low_occ_2023 / total_cells) * 100
  perc_high_occ_2023 <- (high_occ_2023 / total_cells) * 100
  
  # NEW: Calculate percentages for future scenario
  perc_low_occ_future <- (low_occ_future / future_cells) * 100
  perc_high_occ_future <- (high_occ_future / future_cells) * 100
  
  # 7. Store the results for this scenario in the list
  results_list[[scen_name]] <- data.frame(
    Scenario = scen_name,
    Total_Cells_2023 = total_cells,
    Total_Cells_Future = future_cells,
    Cells_Lost_Data = lost_data_cells,
    Cells_Gained_Data = gained_data_cells,
    Cells_With_Change = changed_cells,
    Mean_Occupancy_Change = mean_change,
    SD_Occupancy_Change = sd_change,
    
    # Occupancy category counts
    Low_Occ_2023 = low_occ_2023,
    High_Occ_2023 = high_occ_2023,
    Low_Occ_Future = low_occ_future,
    High_Occ_Future = high_occ_future,
    
    # Occupancy category percentages
    Perc_Low_Occ_2023 = perc_low_occ_2023,
    Perc_High_Occ_2023 = perc_high_occ_2023,
    Perc_Low_Occ_Future = perc_low_occ_future,
    Perc_High_Occ_Future = perc_high_occ_future,
    
    Total_Cells_High_Occ_2023 = total_cells_high_occ_2023,
    Future_Cells_High_Occ_Areas = future_cells_high_occ,
    Mean_Occ_2023 = mean_occ_2023,
    SD_Occ_2023 = sd_occ_2023,
    Mean_Occ_Future = mean_occ_future,
    SD_Occ_Future = sd_occ_future,
    Max_Occ_Future = max_occ_future,
    Min_Occ_Future = min_occ_future
  )
  
  # Optional: Plot the difference raster for visual inspection
  plot(diff_raster, main = paste("Occupancy Change:", scen_name))
}

# Combine all results into one data frame
results_df <- do.call(rbind, results_list)
rownames(results_df) <- NULL

# 8. Calculate the percentage columns and area columns
pixel_area_km2 <- 2  # Each pixel is 2 km²

results_df$PercLost <- (1 - (results_df$Total_Cells_Future / results_df$Total_Cells_2023)) * 100
results_df$PercLost_HighOcc <- (1 - (results_df$Future_Cells_High_Occ_Areas / results_df$Total_Cells_High_Occ_2023)) * 100

# Calculate area lost (in km²)
results_df$AreaLost_Total_km2 <- (results_df$Total_Cells_2023 - results_df$Total_Cells_Future) * pixel_area_km2
results_df$AreaLost_HighOcc_km2 <- (results_df$Total_Cells_High_Occ_2023 - results_df$Future_Cells_High_Occ_Areas) * pixel_area_km2

# Calculate area gained (in km²)
results_df$AreaGained_Total_km2 <- results_df$Cells_Gained_Data * pixel_area_km2

# Calculate net area change (in km²)
results_df$NetAreaChange_km2 <- results_df$AreaGained_Total_km2 - results_df$AreaLost_Total_km2

# 9. Calculate differences in mean occupancy
results_df$Mean_Occ_Difference_Total <- results_df$Mean_Occ_Future - results_df$Mean_Occ_2023

results_df$Mean_Occ_Future_Formatted <- paste0(
  round(results_df$Mean_Occ_Future, 3), 
  " (", 
  round(results_df$SD_Occ_Future, 3), 
  ")"
)

results_df$Mean_Occ_Change_Formatted <- paste0(
  round(results_df$Mean_Occupancy_Change, 3), 
  " (", 
  round(results_df$SD_Occupancy_Change, 3), 
  ")"
)

results_df$PercLOst_Formatted <- paste0(
  round(results_df$PercLost, 3), 
  " (", 
  round(results_df$AreaLost_Total_km2, 0), 
  ")"
)

results_df$PercLostHigh_Formatted <- paste0(
  round(results_df$PercLost_HighOcc, 3), 
  " (", 
  round(results_df$AreaLost_HighOcc_km2, 0), 
  ")"
)
# View the results table
print(results_df)

# Write to CSV
write.csv(results_df, file = "occupancy_change_results.csv", row.names = FALSE)


# Extract SSP and RCP from scenario names (assuming format like "SSP1_RCP19_2050")
results_df$SSP <- gsub("_.*", "", results_df$Scenario)  # Get SSP part
results_df$RCP <- gsub(".*_(RCP[0-9]+)_.*", "\\1", results_df$Scenario)  # Get RCP part

# Create more readable scenario type labels
results_df$Scenario_Type <- paste0("SSP", gsub("SSP", "", results_df$SSP), 
                                   " + RCP", gsub("RCP", "", results_df$RCP))

# Calculate percentage remaining (100 - percentage lost)
results_df$PercRemaining <- 100 - results_df$PercLost

# Calculate percentage remaining (100 - percentage lost)
results_df$PercRemaining <- 100 - results_df$PercLost_HighOcc

# Create a data point for 2023 with 100% remaining and 0 area lost for each scenario
baseline_2023 <- data.frame(
  Year = 2023,
  PercRemaining = 100,
  AreaLost_HighOcc_km2 = 0,
  Scenario_Type = unique(results_df$Scenario_Type)
)

# Combine with the original data
plot_data <- bind_rows(baseline_2023, results_df)
