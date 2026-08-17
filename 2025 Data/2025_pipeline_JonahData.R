###############################################################
# CANOPY STRUCTURE & MOONLIGHT PIPELINE 2025 
#
# Purpose:
#   Build a single analysis dataset for the 2025 data containing:
#
#   • 15-minute camera observations
#   • Astronomical variables from Moonlit
#   • LiDAR-derived canopy structure
#   • CoveR canopy metrics already present in the spreadsheet
#
# Output:
#  2025_master_log_lidar_canopy
###############################################################


###############################################################
# LOAD PACKAGES
###############################################################

library(dplyr)
library(sf)
library(terra)
library(moonlit)
library(purrr)
library(lubridate)
library(tidyr)


###############################################################
# LOAD MASTER LOG
###############################################################

# Change this name if your new spreadsheet has a different
# object name in R.

master_2025_log <- read.csv(
  "/Users/whitneymaxfield/Desktop/Moon_data_202606/Moon_Mice/2025 Data/USE THESE!/2025_FullSheet_with_coords.csv"
)


###############################################################
# CHECK COLUMN NAMES
###############################################################

names(master_2025_log)


###############################################################
# SECTION A: MOONLIGHT DATA
###############################################################

# The new spreadsheet already contains 15-minute bins.
# Therefore, we do NOT need to create detection intervals.
#
# Relevant columns:
#
#   timestamp
#   latitude
#   longitude
#
# Moonlit will calculate astronomical conditions for each
# observation.


###############################################################
# CONVERT TIMESTAMP
###############################################################

master_log_moon <- master_2025_log |>
  mutate(
    timestamp = parse_date_time(
      timestamp,
      orders = c("ymd HMS", "ymd HM", "ymd"),
      tz = "America/Los_Angeles"
    )
  )

#will get error that 8 failed to parse, I believe these times happened during daylight savings 
# and I am unsure what to do with them lol 
master_log_moon <- master_log_moon |>
  filter(!is.na(timestamp))

###############################################################
# CHECK TIMESTAMP
###############################################################
sum(is.na(master_log_moon$timestamp))

###############################################################
# CALCULATE MOONLIGHT
###############################################################

# Moonlit expects scalar inputs, so map_dfr() calculates
# moonlight conditions one observation at a time.
#
# e = 0.28 is appropriate for approximately sea-level
# conditions. Tryon Creek is relatively low elevation.


moon_data <- map_dfr(
  seq_len(nrow(master_log_moon)),
  ~ calculateMoonlightIntensity(
      lat  = master_log_moon$latitude[.x],
      lon  = master_log_moon$longitude[.x],
      date = master_log_moon$timestamp[.x],
      e = 0.28
    )
)


###############################################################
# SELECT MOONLIGHT VARIABLES
###############################################################

moon_data <- moon_data |>
  select(
    night,
    sunAltDegrees,
    moonAltDegrees,
    moonlightModel,
    twilightModel,
    illumination,
    moonPhase
  )


###############################################################
# ADD MOONLIGHT TO MASTER LOG
###############################################################

master_log_moon <- bind_cols(
  master_log_moon,
  moon_data
)


###############################################################
# SECTION B: LiDAR CANOPY STRUCTURE
###############################################################

# LiDAR provides three-dimensional measurements of vegetation.
#
# We calculate canopy height within a 30 m buffer around each
# camera bucket.
#
# CHM = DSM - DEM
#
# DSM = top of vegetation
# DEM = ground surface
# CHM = vegetation height above ground


###############################################################
# LOAD LiDAR RASTERS
###############################################################

dem <- rast(
  "/Users/whitneymaxfield/Desktop/Moon_data_202606/Tryon Lidar Folder/LDQ-45122D6/2014_OLC_Metro/Bare_Earth/bh45122d6"
)

dsm <- rast(
  "/Users/whitneymaxfield/Desktop/Moon_data_202606/Tryon Lidar Folder/LDQ-45122D6/2014_OLC_Metro/Highest_Hit/hh45122d6"
)


###############################################################
# CREATE CANOPY HEIGHT MODEL
###############################################################

chm <- dsm - dem


###############################################################
# REMOVE NEGATIVE CANOPY HEIGHT VALUES
###############################################################

# Small negative values can occur because of raster
# interpolation or alignment.
#
# Vegetation cannot have negative height, so set these values
# to zero.

chm[chm < 0] <- 0


###############################################################
# SECTION C: CREATE ONE LOCATION PER BUCKET
###############################################################

# Each Bucket has many 15-minute observations.
#
# We only need one geographic location per Bucket for the
# LiDAR extraction.

buckets <- master_2025_log |>
  group_by(Bucket) |>
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    .groups = "drop"
  )


###############################################################
# CHECK BUCKET LOCATIONS
###############################################################

print(buckets)

sum(is.na(buckets$latitude))
sum(is.na(buckets$longitude))


###############################################################
# CONVERT BUCKETS TO SF
###############################################################

bucket_sf <- buckets |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |>
  st_transform(crs(chm))


###############################################################
# CREATE 30 m BUFFERS CAN CHANGE THIS 
###############################################################

# All sites in this spreadsheet are Tryon Creek,
# so NO GOME/WARO filtering is necessary.

buffers <- st_buffer(
  bucket_sf,
  30
)


###############################################################
# EXTRACT CANOPY HEIGHT VALUES
###############################################################

vals <- terra::extract(
  chm,
  vect(buffers),
  ID = TRUE
)


###############################################################
# MATCH EXTRACTION IDs TO BUCKET NAMES
###############################################################

vals$Bucket <- bucket_sf$Bucket[vals$ID]


###############################################################
# RENAME LiDAR VALUE COLUMN
###############################################################

names(vals)[2] <- "chm_height"


###############################################################
# SUMMARIZE CANOPY STRUCTURE
###############################################################

canopy_metrics <- vals |>
  group_by(Bucket) |>
  summarise(

    # Average vegetation height within 30 m
    mean_height = mean(
      chm_height,
      na.rm = TRUE
    ),

    # Maximum vegetation height within 30 m
    max_height = max(
      chm_height,
      na.rm = TRUE
    ),

    # Variation in vegetation height
    sd_height = sd(
      chm_height,
      na.rm = TRUE
    ),

    # Proportion of the 30 m buffer with vegetation >2 m
    cover2m = mean(
      chm_height > 2,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


###############################################################
# CHECK LiDAR METRICS
###############################################################

print(canopy_metrics)


###############################################################
# SECTION D: MERGE LiDAR WITH CAMERA DATA
###############################################################

# LiDAR metrics are associated with Bucket.
#
# Therefore, every 15-minute observation from the same Bucket
# will receive the same LiDAR canopy metrics.

analysis_df <- master_log_moon |>
  left_join(
    canopy_metrics,
    by = "Bucket"
  )


###############################################################
# SECTION E: FINAL ANALYSIS DATASET
###############################################################

# The new spreadsheet already contains CoveR metrics:
#
#   mean_FC
#   mean_CC
#   n_canopy_photos
#
# Therefore, no separate CoveR join is needed.


master_log_lidar_canopy <- analysis_df


###############################################################
# SECTION F: CHECK FINAL DATASET
###############################################################

glimpse(master_log_lidar_canopy)


###############################################################
# CHECK NUMBER OF OBSERVATIONS (shoul dbe 176131)
###############################################################

nrow(master_log_lidar_canopy)


###############################################################
# CHECK NUMBER OF BUCKETS (should be 19)
###############################################################

n_distinct(master_log_lidar_canopy$Bucket)


###############################################################
# CHECK MOONLIGHT VARIABLES (see moon spreasheet for more info)
###############################################################

summary(
  master_log_lidar_canopy$moonlightModel
)

summary(
  master_log_lidar_canopy$illumination
)

summary(
  master_log_lidar_canopy$moonPhase
)


###############################################################
# CHECK LiDAR VARIABLES (do they seem reasonable)
###############################################################

summary(
  master_log_lidar_canopy$mean_height
)

summary(
  master_log_lidar_canopy$max_height
)

summary(
  master_log_lidar_canopy$sd_height
)

summary(
  master_log_lidar_canopy$cover2m
)


###############################################################
# CHECK COVeR VARIABLES (do they seem reasonable or correct)
###############################################################

summary(
  master_log_lidar_canopy$mean_FC
)

summary(
  master_log_lidar_canopy$mean_CC
)

summary(
  master_log_lidar_canopy$n_canopy_photos
)


###############################################################
# CHECK FOR MISSING LiDAR VALUES (should be 0)
###############################################################

master_log_lidar_canopy |>
  summarise(
    n_rows = n(),
    n_buckets = n_distinct(Bucket),

    missing_mean_height =
      sum(is.na(mean_height)),

    missing_max_height =
      sum(is.na(max_height)),

    missing_sd_height =
      sum(is.na(sd_height)),

    missing_cover2m =
      sum(is.na(cover2m))
  )


###############################################################
# CHECK FINAL COLUMN NAMES
###############################################################

names(master_log_lidar_canopy)


###############################################################
# EXPORT FINAL DATASET
###############################################################

write.csv(
  master_log_lidar_canopy,
  "2025_LidarCanopyMoon_masterLog.csv",
  row.names = FALSE
)


###############################################################
# END OF PIPELINE
###############################################################
