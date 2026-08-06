library(sf)
library(terra)
library(dplyr)

# Get Tryon bucket locations
tryon_points <- master_log_lidar_canopy |>
  filter(grepl("^BOHO|^SOWE", stationID)) |>
  distinct(
    stationID,
    latitude,
    longitude
  )


tryon_sf <- st_as_sf(
  tryon_points,
  coords = c("longitude","latitude"),
  crs = 4326
)

# Transform to LiDAR CRS
tryon_sf <- st_transform(
  tryon_sf,
  crs(chm)
)

tryon_extent <- ext(tryon_sf)

tryon_extent

# Buffer bucket locations by 50 m
tryon_buffer <- st_buffer(
  tryon_sf,
  dist = 50
)

# Convert buffered area to terra extent
tryon_extent <- ext(tryon_buffer)

chm_tryon <- crop(
  chm,
  tryon_extent
)

plot(chm_tryon)

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

# Proportion of canopy >2 m
cover2m_raster <- terra::focal(
  chm_tryon > 2,
  w = 61,
  fun = mean,
  na.rm = TRUE
)

#dark moon 
dark_moon <- quantile(
  final_master_log$moonlightModel[
    final_master_log$moonlightModel > 0
  ],
  0.10,
  na.rm = TRUE
)

#medium moon
medium_moon <- quantile(
  final_master_log$moonlightModel[
    final_master_log$moonlightModel > 0
  ],
  0.50,
  na.rm = TRUE
)

#bright moon 
bright_moon <- quantile(
  final_master_log$moonlightModel,
  0.95,
  na.rm = TRUE
)

#Create a raster stack
predictor_stack <- c(
  mean_height_raster,
  sd_height_raster,
  cover2m_raster
)

names(predictor_stack) <- c(
  "mean_height",
  "sd_height",
  "cover2m"
)

plot(predictor_stack)


##########
#CoveR metrics only exist at bucket locations.

cover_means <- master_log_lidar_canopy |>
filter(grepl("^BOHO|^SOWE", stationID)) |>
distinct(
  stationID,
  CC,
  FC
) |>
summarise(
  CC = mean(CC, na.rm = TRUE),
  FC = mean(FC, na.rm = TRUE)
)
