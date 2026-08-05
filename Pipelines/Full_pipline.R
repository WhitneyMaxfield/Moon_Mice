###############################################################
# CANOPY STRUCTURE & MOONLIGHT PIPELINE
#
# Purpose:
#   Build a single analysis dataset containing:
#
#   • Astronomical variables (Moonlit)
#   • LiDAR-derived canopy structure
#   • CoveR canopy photograph metrics
#
# Output:
#   master_log_lidar_canopy
###############################################################

#load packages (may need to do install.packages if these have not been loaded before!)
library(dplyr)
library(sf)
library(terra)
install.packages("moonlit")
library(moonlit)
library(purrr)
library(lubridate)

# Load Master Scoring Log
master_log <- Scoring_master_log_Master_log

###############################################################
# SECTION A: Moonlight data (moonlit package)
###############################################################

# Moonlit calculates the astronomical conditions present at the
# exact time and location of every camera observation.
#
# Variables include:
#   • Moonlight intensity
#   • Moon phase
#   • Lunar elevation
#   • Solar elevation
#   • Twilight intensity
#
# These variables vary by observation rather than by station.

# Convert timestamp column
# Convert to POSIXct with local timezone — required by calculateMoonlightIntensity()
master_log_moon <- master_log |>
  mutate(
    time_stamp = ymd_hms(
      time_stamp,
      tz = "America/Los_Angeles"
    )
  )

###### Calculate moonlight intensity ###### 
#using e=0.28 which is at sea level (most tryon sites sit at ~50m above sea level
# map_dfr() used because calculateMoonlightIntensity() expects scalar inputs,
moon_data <- map_dfr(
  seq_len(nrow(master_log_moon)),
  ~ calculateMoonlightIntensity(
      lat  = master_log_moon$latitude[.x],
      lon  = master_log_moon$longitude[.x],
      date = master_log_moon$time_stamp[.x],
      e = 0.28
    )
)

#selecting output columns we want
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

master_log_moon <- bind_cols(
  master_log_moon,
  moon_dat
)

###############################################################
# SECTION B: LiDAR Canopy Structure
###############################################################

# LiDAR provides three-dimensional measurements of vegetation.
#
# This section calculates canopy height surrounding each
# camera station using a 30 m buffer.
# SEE CITATIONS FOR EXP OF 30 M BUFFER 

###############################################################

# Load LiDAR raster
# Will have to change the location of the downloaded folders! Whitney can share the correct download
# or add it to mouselab computer :) 
dem <- rast("/Users/whitneymaxfield/Downloads/LDQ-45122D6/2014_OLC_Metro/Bare_Earth/bh45122d6")

dsm <- rast("/Users/whitneymaxfield/Downloads/LDQ-45122D6/2014_OLC_Metro/Highest_Hit/hh45122d6")

# Create Canopy Height Model
# CHM represents vegetation height above ground
# DSM = top of vegetation
# DEM = ground surface
# CHM = DSM - DEM
chm <- dsm - dem

# Occasionally small negative values occur because of raster
# misalignment or interpolation error. Since vegetation cannot
# have negative height, set these values to zero.
chm[chm < 0] <- 0

# Create one point per camera station
# The master log contains many observations for each station.
# But we only need one location per station for extracting habitat variables! 
buckets <- master_log |>
  group_by(stationID) |>
  summarise(
    latitude = first(latitude),
    longitude = first(longitude)
  )

# Convert to an sf object using latitude/longitude coordinates
bucket_sf <- buckets |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |>
  st_transform(crs(chm))

# Remove stations outside LiDAR coverage
# WILL NEED TO UPDATE THIS TO FOR ALL NON TRYON LOCATIONS!!! (IE WARO, BOBS ECT)
bucket_sf_filtered <- bucket_sf |>
  filter(!stationID %in% c("GOME01", "GOME02"))

# Create 30 m habitat buffers (can be changed)
buffers <- st_buffer(bucket_sf_filtered, 30)

# Extract canopy height values
vals <- terra::extract(
  chm,
  vect(buffers),
  ID = TRUE
)

# Match extraction IDs back to station names, Rename raster column for clarity
vals$stationID <- bucket_sf_filtered$stationID[vals$ID]
names(vals)[2] <- "chm_height"

# Summarize canopy structure
canopy_metrics <- vals |>
  group_by(stationID) |>
  summarise(

    mean_height = mean(chm_height, na.rm = TRUE),

    max_height = max(chm_height, na.rm = TRUE),

    sd_height = sd(chm_height, na.rm = TRUE),

    cover2m = mean(chm_height > 2, na.rm = TRUE)

  )

# Add LiDAR metrics to observation dataset
analysis_df <- master_log_moon |>
  left_join(
    canopy_metrics,
    by = "stationID"
  )

###############################################################
# SECTION C: Canopy Photographs (CoveR)
###############################################################

# Canopy photographs provide fine-scale canopy structure that
# complements the LiDAR-derived measurements.
#
# Only upper canopy photographs from Tryon Creek stations are
# used in this workflow.

################################################################
#The canopy photo analysis is performed separately using the CoveR pipeline!!!
# RUN COVER PIPELINE FIRST THEN UPLOAD RESULSTS AS CANOPY_COVER_RESULTS OR RENAME TO MATCH 
###############################################################

# filtering for Tryon sites and Upper images ONLY 
# select variables wanted 
canopy_upper <- Canopy_Cover_Results |>
  filter(
    grepl("^BOHO|^SOWE", Site),
    grepl("Upper", Image)
  ) |>
  select(
    Site,

    CC,   # Crown Cover

    FC,   # Foliage Cover

    CP,   # Crown Porosity

    Le,   # Effective Leaf Area Index

    L,    # Leaf Area Index

    CI    # Clumping Index
  )

###############################################################
# SECTION D: Final Analysis Dataset
###############################################################

# Merge:
#
#   • Camera observations
#   • Moonlit variables
#   • LiDAR canopy metrics
#   • CoveR canopy metrics
#
# Final output:
#
#   master_log_lidar_canopy
#
# Each row represents one camera observation with both
# observation-level (moondata) and station-level (lidar and canopy) environmental variables.

master_log_lidar_canopy <- analysis_df |>
  left_join(
    canopy_upper,
    by = c("stationID" = "Site")
  )

write.csv(
  master_log_lidar_canopy,
  "LidarCanopyMoon_masterLog.csv",
  row.names = FALSE
)
###############################################################
# End of Pipeline
###############################################################
