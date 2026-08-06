# canopy raster extrapolation 
library(sf)
library(terra)
library(dplyr)
install.packages("gstat")
library(gstat)

cover_points <- master_log_lidar_canopy |>
  filter(grepl("^BOHO|^SOWE", stationID)) |>
  distinct(
    stationID,
    latitude,
    longitude,
    CC,
    FC
  )

cover_points

cover_sf <- st_as_sf(
  cover_points,
  coords = c("longitude", "latitude"),
  crs = 4326
)

cover_sf <- st_transform(
  cover_sf,
  crs(chm_tryon)
)

plot(chm_tryon)
plot(cover_sf, add=TRUE)

template <- mean_height_raster
cover_sf

cc_grid <- as.data.frame(
  terra::xyFromCell(
    mean_height_raster,
    1:ncell(mean_height_raster)
  )
)

names(cc_grid) <- c("x","y")

cc_grid_sf <- st_as_sf(
  cc_grid,
  coords = c("x","y"),
  crs = crs(mean_height_raster)
)

cc_idw <- gstat(
  formula = CC ~ 1,
  data = cover_sf,
  nmax = 8,
  set = list(idp = 2)
)

CC_prediction <- predict(
  cc_idw,
  newdata = cc_grid_sf
)

CC_raster <- mean_height_raster

values(CC_raster) <- CC_prediction$var1.pred

plot(CC_raster,
     main="Interpolated Crown Cover (CC)")
plot(cover_sf, add=TRUE)

fc_idw <- gstat(
  formula = FC ~ 1,
  data = cover_sf,
  nmax = 8,
  set = list(idp = 2)
)

FC_prediction <- predict(
  fc_idw,
  newdata = cc_grid_sf
)

FC_raster <- mean_height_raster

values(FC_raster) <- FC_prediction$var1.pred

plot(
  FC_raster,
  main="Interpolated Foliage Cover (FC)"
)

plot(cover_sf, add=TRUE)

predictor_stack <- c(
mean_height_raster,
sd_height_raster,
cover2m_raster
)

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

prediction_df <- as.data.frame(
  predictor_stack,
  xy = TRUE,
  na.rm = TRUE
)

head(prediction_df)

prediction_df$moonlightModel <- dark_moon

moon_model <- glmmTMB(
  PESO ~
    moonlightModel +
    mean_height +
    sd_height +
    cover2m +
    CC +
    FC +
    (1|stationID),
  family = binomial,
  data = model_data
)

prediction_df$PESO_probability_dark <- predict(
  moon_model,
  newdata = prediction_df,
  type = "response",
  re.form = NA
)

PESO_dark_raster <- rast(
  prediction_df[,c(
    "x",
    "y",
    "PESO_probability_dark"
  )],
  type="xyz",
  crs=crs(chm_tryon)
)

plot(
  PESO_dark_raster,
  main="Predicted Deer Mouse Presence Probability - Dark Moon"
)

plot(
  tryon_sf,
  add=TRUE
)

LOF_dark <- 1 - PESO_dark_raster

plot(
  LOF_dark,
  main="Landscape of Fear - Dark Moon"
)

plot(
  tryon_sf,
  add=TRUE
)
