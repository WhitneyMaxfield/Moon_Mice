
###################################################################
#       STEP 5: Add canopy cover photographs to master log  
###################################################################

# Only upper canopy photographs from Tryon Creek stations are
# used in this workflow.

# ============================================================
# 1. FILTER CANOPY PHOTOS
# ============================================================

# Read the CoveR canopy analysis results.

# The data are filtered to:
#   1. Include only Tryon stations beginning with BOHO or SOWE
#   2. Include only Upper canopy photographs.

# The variables needed for the analysis are then selected.

canopy_upper <- read.csv("/Users/whitneymaxfield/Desktop/Moon_data_202606/Moon_Mice/2026 Data/Labeled_Canopy_Cover_Results.csv") |>
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

# ============================================================
# 2. COMBINE ALL DATA
# ============================================================

# Merge:
# • Camera observations
# • Moonlit variables
# • LiDAR canopy metrics
# • CoveR canopy metrics


master_log_lidar_canopy <- analysis_df |>
  filter(Site == "Tryon") |>
  left_join(
    canopy_upper,
    by = c("stationID" = "Site")
  )

write.csv(
  master_log_lidar_canopy,
  "LidarCanopyMoon_masterLog.csv",
  row.names = FALSE
)

# Save the completed dataset as a CSV file.
# The final dataset contains the camera observations together
# with Moonlit, LiDAR canopy, and CoveR canopy variables.

write.csv(
  master_log_lidar_canopy,
  "LidarCanopyMoon_masterLog.csv",
  row.names = FALSE
)