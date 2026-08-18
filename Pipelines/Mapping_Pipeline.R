###############################################################
# DEER MOUSE LANDSCAPE OF FEAR MAPPING PIPELINE FOR 2026 DATA 
###############################################################

# =============================================================
# 1. LOAD PACKAGES
# =============================================================

library(sf)
library(terra)
library(dplyr)
library(gstat)
library(glmmTMB)


# =============================================================
# 2. LOAD / PREPARE DATA
# =============================================================
# ORDER: 
# 1. CoveR_Pipeline.R 
# 2. Full_pipeline_2026.R
# 3. This pipeline! 
# should have: the chm from earlier, master_log_lidar_canopy

# =============================================================
# 3. DEFINE TRYON CREEK STUDY AREA
# =============================================================

# Get bucket locations

tryon_points <- master_log_lidar_canopy |>
  filter(grepl("^BOHO|^SOWE", stationID)) |>
  distinct(
    stationID,
    latitude,
    longitude
  )


# Convert to spatial points

tryon_sf <- st_as_sf(
  tryon_points,
  coords = c("longitude", "latitude"),
  crs = 4326
)


# Transform to LiDAR CRS

tryon_sf <- st_transform(
  tryon_sf,
  crs(chm)
)


# Buffer locations

tryon_buffer <- st_buffer(
  tryon_sf,
  dist = 50
)


# Study area extent

tryon_extent <- ext(tryon_buffer)


# Crop LiDAR

chm_tryon <- crop(
  chm,
  tryon_extent
)
plot(chm_tryon)

# =============================================================
# 4. CREATE LIDAR PREDICTOR RASTERS
# =============================================================

mean_height_raster <- terra::focal(
  chm_tryon,
  w = 61,
  fun = mean,
  na.rm = TRUE
)

sd_height_raster <- terra::focal(
  chm_tryon,
  w = 61,
  fun = sd,
  na.rm = TRUE
)

cover2m_raster <- terra::focal(
  chm_tryon > 2,
  w = 61,
  fun = mean,
  na.rm = TRUE
)


# =============================================================
# 5. PREPARE COVEr DATA
# =============================================================

cover_points <- master_log_lidar_canopy |>
  filter(grepl("^BOHO|^SOWE", stationID)) |>
  group_by(
    stationID,
    latitude,
    longitude
  ) |>
  summarise(
    CC = mean(CC, na.rm = TRUE),
    FC = mean(FC, na.rm = TRUE),
    .groups = "drop"
  )


cover_sf <- st_as_sf(
  cover_points,
  coords = c("longitude", "latitude"),
  crs = 4326
)


cover_sf <- st_transform(
  cover_sf,
  crs(chm_tryon)
)


# =============================================================
# 6. CREATE PREDICTION GRID
# =============================================================

prediction_grid <- as.data.frame(
  terra::xyFromCell(
    mean_height_raster,
    1:ncell(mean_height_raster)
  )
)

names(prediction_grid) <- c(
  "x",
  "y"
)


prediction_grid_sf <- st_as_sf(
  prediction_grid,
  coords = c("x", "y"),
  crs = crs(mean_height_raster)
)


# =============================================================
# 7. INTERPOLATE COVeR CROWN COVER
# =============================================================

cc_idw <- gstat(
  formula = CC ~ 1,
  data = cover_sf,
  nmax = 8,
  set = list(idp = 2)
)

CC_prediction <- predict(
  cc_idw,
  newdata = prediction_grid_sf
)

CC_raster <- mean_height_raster

values(CC_raster) <-
  CC_prediction$var1.pred


# =============================================================
# 8. INTERPOLATE COVeR FOLIAGE COVER
# =============================================================

fc_idw <- gstat(
  formula = FC ~ 1,
  data = cover_sf,
  nmax = 8,
  set = list(idp = 2)
)

FC_prediction <- predict(
  fc_idw,
  newdata = prediction_grid_sf
)

FC_raster <- mean_height_raster

values(FC_raster) <-
  FC_prediction$var1.pred


# =============================================================
# 9. COMBINE ALL PREDICTORS
# =============================================================

predictor_stack <- c(
  mean_height_raster,
  sd_height_raster,
  cover2m_raster,
  CC_raster,
  FC_raster
)

names(predictor_stack) <- c(
  "mean_height",
  "sd_height",
  "cover2m",
  "CC",
  "FC"
)


# =============================================================
# 10. PREPARE MODEL DATA
# =============================================================

model_data <- master_log_lidar_canopy |>
  filter(
    !is.na(PESO),
    !is.na(moonlightModel),
    !is.na(mean_height),
    !is.na(sd_height),
    !is.na(cover2m),
    !is.na(CC),
    !is.na(FC)
  )


# =============================================================
# 11. FIT DEER MOUSE GLMM
# =============================================================

moon_model <- glmmTMB(
  PESO ~
    moonlightModel +
    mean_height +
    sd_height +
    cover2m +
    CC +
    FC +
    (1 | stationID),
  family = binomial,
  data = model_data
)


summary(moon_model)


# =============================================================
# 12. CREATE PREDICTION DATA
# =============================================================

prediction_df <- as.data.frame(
  predictor_stack,
  xy = TRUE,
  na.rm = TRUE
)


# =============================================================
# 13. SET MOONLIGHT CONDITION
# =============================================================

dark_moon <- quantile(
  LidarCanopyMoon_masterLog$moonlightModel[
    LidarCanopyMoon_masterLog$moonlightModel > 0
  ],
  0.10,
  na.rm = TRUE
)

prediction_df$moonlightModel <- dark_moon


# =============================================================
# 14. PREDICT DEER MOUSE PROBABILITY
# =============================================================

prediction_df$PESO_probability_dark <- predict(
  moon_model,
  newdata = prediction_df,
  type = "response",
  re.form = NA
)


# =============================================================
# 15. CREATE PROBABILITY RASTER
# =============================================================

PESO_dark_raster <- rast(
  prediction_df[, c(
    "x",
    "y",
    "PESO_probability_dark"
  )],
  type = "xyz",
  crs = crs(chm_tryon)
)

writeRaster(
  PESO_dark_raster,
  "PESO_dark_probability.tif",
  overwrite = TRUE
)
# =============================================================
# 16. CREATE LANDSCAPE OF FEAR
# =============================================================

LOF_dark <- 1 - PESO_dark_raster


# =============================================================
# 17. PLOT RESULTS
# =============================================================

plot(
  PESO_dark_raster,
  main = "Predicted Deer Mouse Probability - Dark Moon"
)
plot(
  tryon_sf,
  add = TRUE,
  pch = 16,
  cex = 1,
  col = "white"
)

plot(
  tryon_sf,
  add = TRUE,
  pch = 16,
  cex = 0.8,
  col = "darkred"
)

# =============================================================
# 18. WRITE RASTERS FOR QGIS WORK
# =============================================================

writeRaster(
  LOF_dark,
  "PESO_dark_LOF.tif",
  overwrite = TRUE
)
st_write(
  tryon_sf,
  "Tryon_buckets.gpkg",
  delete_dsn = TRUE
)
