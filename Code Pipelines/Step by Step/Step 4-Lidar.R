
###############################################################
#       STEP 4: add lidar data to the master log 
###############################################################

# LiDAR provides three-dimensional measurements of vegetation.

# This section calculates canopy height surrounding each
# camera station using a (currently) 30 m buffer.
# Longer explanation for 30m buffer here: ______ ADD LATER 

# ============================================================
# 1. LOAD LiDAR RASTERS
# ============================================================
# Whitney can share the correct download or add it to the
# mouse lab computer.

# Load the Digital Elevation Model (DEM).
# The DEM represents the elevation of the ground surface, without vegetation.

dem <- rast("/Users/whitneymaxfield/Desktop/Moon_data_202606/Tryon Lidar Folder/LDQ-45122D6/2014_OLC_Metro/Bare_Earth/bh45122d6")

# Load the Digital Surface Model (DSM).
# The DSM represents the elevation of the highest surface 
# detected by LiDAR, including vegetation

dsm <- rast("/Users/whitneymaxfield/Desktop/Moon_data_202606/Tryon Lidar Folder/LDQ-45122D6/2014_OLC_Metro/Highest_Hit/hh45122d6")

# ============================================================
# 2. CREATE CANOPY HEIGHT MODEL
# ============================================================

# The Canopy Height Model (CHM) represents vegetation height
# above the ground.

# DSM = top of vegetation
# DEM = ground surface

# Therefore:
# CHM = DSM - DEM

chm <- dsm - dem


# Occasionally, small negative values occur because of raster
# misalignment or interpolation error.
# Because vegetation cannot have a negative height, these
# values are set to zero.

chm[chm < 0] <- 0

# ============================================================
# 3. CREATE ONE POINT PER CAMERA STATION
# ============================================================

# The master log contains many observations for each station.

# However, each station has only one geographic location.
# Therefore, we only need one latitude/longitude coordinate
# per station for extracting habitat variables.

buckets <- master_log |>
  filter(Site == "Tryon") |>
  group_by(stationID) |>
  summarise(
    latitude = first(latitude),
    longitude = first(longitude)
  )


# ============================================================
# 4. CONVERT STATIONS TO SPATIAL OBJECTS
# ============================================================

# Convert the station coordinates into an sf spatial object.

# coords = c("longitude", "latitude"):
#   Identifies the columns containing geographic coordinates.

# crs = 4326:
#   Specifies that the coordinates are in WGS84 latitude/
#   longitude.

# st_transform(crs(chm)):
#   Reprojects the stations into the same coordinate reference
#   system as the LiDAR raster.

# This is necessary so that the station locations and LiDAR
# raster are spatially aligned.

bucket_sf <- buckets |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |>
  st_transform(crs(chm))


# ============================================================
# 5. CREATE 30 M HABITAT BUFFERS
# ============================================================

# Create a 30 m buffer around each camera station.

# This defines the area surrounding each station that will be
# used to calculate local canopy structure.

# Each station has a circular 30 m area from which
# LiDAR canopy height values will be extracted.

# The 30 m buffer can be changed if a different spatial scale
# is desired.

buffers <- st_buffer(bucket_sf, 30)

# ============================================================
# 6. EXTRACT LiDAR CANOPY HEIGHT VALUES
# ============================================================

# Extract the CHM values that fall within each 30 m buffer.

# terra::extract() returns the LiDAR canopy height for the
# raster cells contained within each station's buffer.

# ID = TRUE:
#   Keeps an ID identifying which buffer each extracted value
#   came from.

vals <- terra::extract(
  chm,
  vect(buffers),
  ID = TRUE
)

# ============================================================
# 7. MATCH LiDAR VALUES TO STATION NAMES
# ============================================================

# Match each extracted LiDAR value back to its corresponding
# station using the extraction ID.

vals$stationID <- bucket_sf$stationID[vals$ID]


# Rename the extracted raster-value column for clarity.
# chm_height represents the vegetation height of each
# individual LiDAR raster cell.

names(vals)[2] <- "chm_height"


# ============================================================
# 8. CALCULATE CANOPY METRICS
# ============================================================

# Calculate canopy structure metrics for each station.

# mean_height:
#   Mean vegetation height within the 30 m buffer.

# max_height:
#   Maximum vegetation height within the 30 m buffer.

# sd_height:
#   Variation in vegetation height within the 30 m buffer.

# cover2m:
#   Proportion of LiDAR cells with vegetation taller than 2 m.

# na.rm = TRUE:
#   Removes missing values when calculating each metric.

canopy_metrics <- vals |>
  group_by(stationID) |>
  summarise(

    mean_height = mean(chm_height, na.rm = TRUE),

    max_height = max(chm_height, na.rm = TRUE),

    sd_height = sd(chm_height, na.rm = TRUE),

    cover2m = mean(chm_height > 2, na.rm = TRUE)

  )

# ============================================================
# 9. ADD LiDAR METRICS TO THE MASTER LOG
# ============================================================

# Add the station-level LiDAR canopy metrics to the dataset
# containing the Moonlit variables.

# left_join() matches the LiDAR metrics to observations using
# stationID.

# Because each station has one set of LiDAR metrics, those
# values will be added to every observation from that station.

analysis_df <- master_log_moon |>
  left_join(
    canopy_metrics,
    by = "stationID"
  )
