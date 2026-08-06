###############################################################
# CANOPY STRUCTURE + CoveR CANOPY EXTRAPOLATION + LOF MAPPING
#
# Purpose:
#   Combine LiDAR-derived canopy structure and CoveR canopy
#   metrics into spatial predictor rasters for deer mouse
#   Landscape of Fear (LOF) predictions.
#
# Outputs:
#   - CC raster (interpolated crown cover)
#   - FC raster (interpolated foliage cover)
#   - Deer mouse probability raster
#   - Landscape of Fear raster
###############################################################


############################
# Load packages
############################

library(sf)
library(terra)
library(dplyr)
library(gstat)
library(glmmTMB)


############################
# 1. Create bucket locations
############################

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


############################
# 2. Match LiDAR projection
############################

tryon_sf <- st_transform(
  tryon_sf,
  crs(chm)
)


############################
# 3. Crop CHM around buckets
############################

tryon_buffer <- st_buffer(
  tryon_sf,
  dist = 50
)

tryon_extent <- ext(tryon_buffer)


chm_tryon <- crop(
  chm,
  tryon_extent
)


plot(
  chm_tryon,
  main = "CHM - Tryon Study Area"
)

plot(
  tryon_sf,
  add = TRUE
)


############################
# 4. Create LiDAR canopy predictors
############################

# Mean canopy height (~30 m neighborhood)

mean_height_raster <- terra::focal(
  chm_tryon,
  w = 61,
  fun = mean,
  na.rm = TRUE
)


# Canopy height variability

sd_height_raster <- terra::focal(
  chm_tryon,
  w = 61,
  fun = sd,
  na.rm = TRUE
)


# Proportion of cells with canopy >2 m

cover2m_raster <- terra::focal(
  chm_tryon > 2,
  w = 61,
  fun = mean,
  na.rm = TRUE
)



############################
# 5. Prepare CoveR canopy data
############################

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


# Convert CoveR points to sf

cover_sf <- st_as_sf(
  cover_points,
  coords = c("longitude", "latitude"),
  crs = 4326
)


# Transform into LiDAR CRS

cover_sf <- st_transform(
  cover_sf,
  crs(chm_tryon)
)


plot(chm_tryon)
plot(
  cover_sf,
  add = TRUE
)



############################
# 6. Create prediction grid
############################

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



############################
# 7. Interpolate Crown Cover (CC)
############################

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


values(CC_raster) <- CC_prediction$var1.pred


plot(
  CC_raster,
  main = "Interpolated Crown Cover"
)

plot(
  cover_sf,
  add = TRUE
)



############################
# 8. Interpolate Foliage Cover (FC)
############################

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


values(FC_raster) <- FC_prediction$var1.pred


plot(
  FC_raster,
  main = "Interpolated Foliage Cover"
)

plot(
  cover_sf,
  add = TRUE
)



############################
# 9. Combine predictor rasters
############################

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


plot(predictor_stack)



############################
# 10. Prepare prediction data
############################

prediction_df <- as.data.frame(
  predictor_stack,
  xy = TRUE,
  na.rm = TRUE
)


############################
# 11. Add moon condition
############################

# Dark moon illumination value

prediction_df$moonlightModel <- dark_moon



############################
# 12. Fit deer mouse model
############################

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



############################
# 13. Predict deer mouse probability
############################

prediction_df$PESO_probability_dark <- predict(
  moon_model,
  newdata = prediction_df,
  type = "response",
  re.form = NA
)



############################
# 14. Create probability raster
############################

PESO_dark_raster <- rast(
  prediction_df[, c(
    "x",
    "y",
    "PESO_probability_dark"
  )],
  type = "xyz",
  crs = crs(chm_tryon)
)


plot(
  PESO_dark_raster,
  main = "Predicted Deer Mouse Presence Probability - Dark Moon"
)

plot(
  tryon_sf,
  add = TRUE
)



############################
# 15. Create Landscape of Fear raster
############################

LOF_dark <- 1 - PESO_dark_raster


plot(
  LOF_dark,
  main = "Landscape of Fear - Dark Moon"
)

plot(
  tryon_sf,
  add = TRUE
)


###############################################################
# END SCRIPT
###############################################################