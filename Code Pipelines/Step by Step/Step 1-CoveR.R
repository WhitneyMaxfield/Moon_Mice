###############################################################
#     STEP 1: Calculate canopy cover using CoveR2
###############################################################
# to clear enviornment: 
# rm(list = ls())

# Load packages
library(coveR2)   # Used to calculate canopy cover metrics from photos
library(dplyr)    # Used for data manipulation
library(purrr)    # Used to efficiently run a function across many images


# ============================================================
# 1. SET UP THE PHOTO DIRECTORY
# ============================================================

# The canopy photos are organized by site and then station.

# For this pipeline, we are ONLY using the Tryon folder.

# Folder structure:
#
# Labeled_CanopyPhotos/
# └── Tryon/
#     ├── BOHO01/
#     │   ├── BOHO01_20260626/
#     │   │   ├── LowerBOHO01_20260626.JPG
#     │   │   └── UpperBOHO01_20260626.JPG
#     │   └── ...
#     ├── BOHO02/
#     └── ...

# Parent directory containing all site folders
parent_dir <- "/Users/whitneymaxfield/Desktop/Moon_data_202606/Moon_Mice/2026 Data/Labeled_CanopyPhotos"

# Path to the Tryon folder
tryon_dir <- file.path(parent_dir, "Tryon")


# ============================================================
# 2. FIND ALL CANOPY PHOTOS
# ============================================================

# Search ONLY inside the Tryon folder for JPG images.

# recursive = TRUE:
#   Search inside all station and date subfolders.

# full.names = TRUE:
#   Return the complete file path for each image.

# ignore.case = TRUE:
#   Find both .JPG and .jpg files.

images <- list.files(
  tryon_dir,
  pattern = "\\.JPG$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


# Check how many images were found
length(images)

# Look at the first few file paths
head(images)

# ============================================================
# 3. CREATE A FUNCTION TO PROCESS ONE IMAGE
# ============================================================

# This function takes one image file path and:
#
#   1. Sets the Site to "Tryon"
#   2. Finds the station folder
#   3. Finds the date folder
#   4. Gets the image filename
#   5. Runs the image through coveR2
#   6. Adds the identifying information to the results


process_image <- function(img){

  # ----------------------------------------------------------
  # Extract information from the folder structure
  # ----------------------------------------------------------

  # Get the name of the folder immediately containing the image.
  
  # Example:
  # .../Tryon/BOHO01/BOHO01_20260626/LowerBOHO01_20260626.JPG
  
  # date_folder will become: "BOHO01_20260626"

  date_folder <- basename(dirname(img))


  # Get the name of the folder containing the date folder.
  
  # Using the example above:
  # station_folder will become: "BOHO01"
  station_folder <- basename(dirname(dirname(img)))


  # Get the filename of the image itself.
  
  # Example: "LowerBOHO01_20260626.JPG"
  image_name <- basename(img)

  # ----------------------------------------------------------
  # Run coveR2 on the image
  # ----------------------------------------------------------

  # Analyze the canopy photo using coveR2.
  
  # coveR2 calculates canopy cover metrics from the image
  # and returns the results as a data frame.
  out <- coveR2(img)

  # ----------------------------------------------------------
  # Add identifying information to the coveR2 results
  # ----------------------------------------------------------

  # Add:
  #   Site = "Tryon"
  #   StationID = station folder name
  #   Date_Folder
  #   Image
  
  # .before = 1 places these columns at the beginning
  # of the resulting data frame.

  out %>%
    mutate(
      Site = "Tryon",
      StationID = station_folder,
      Date_Folder = date_folder,
      Image = image_name,
      .before = 1
    )

} # close function

# ============================================================
# 4. RUN coveR2 ON EVERY IMAGE
# ============================================================

# Apply process_image() to every image found in the Tryon folder.
#
# map_dfr() means:
#   - run process_image() once for every image
#   - combine all resulting data frames
#     into one large data frame

results1 <- map_dfr(
  images,
  process_image
)


# ============================================================
# 5. CHECK THE RESULTS
# ============================================================

# View the first few rows
head(results1)

# See how many rows and columns were produced
dim(results1)

# Check the Site column
table(results1$Site)

# Check the StationID column
unique(results1$StationID)


# ============================================================
# 6. SAVE THE RESULTS
# ============================================================

# Write the complete coveR2 results to a CSV file.
# row.names = FALSE prevents R from adding an extra
# row-number column to the CSV.

write.csv(
  results1,
  "Tryon_Canopy_Cover_Results.csv",
  row.names = FALSE
)
