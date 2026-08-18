# ============================================================
# CANOPY PHOTO ANALYSIS USING coveR2
# ============================================================

# Load packages
library(coveR2)   # Used to calculate canopy cover metrics from photos
library(dplyr)    # Used for data manipulation
library(purrr)    # Used to efficiently run a function across many images

# ============================================================
# 1. SET UP THE PHOTO DIRECTORY
# ============================================================

# IMPORTANT:
# On the lab computer, you will need to create one parent
# directory containing ALL canopy photos.
#
# The folder structure should remain organized by site/date,
# for example:
#
# Labeled_CanopyPhotos/
# ├── BOHO01/
# │   ├── BOHO01_20260626/
# │   │   ├── LowerBOHO01_20260626.JPG
# │   │   ├── UpperBOHO01_20260626.JPG
# │   └── ...
# ├── BOHO02/
# └── SOWE01/
# etc. 
# This organization allows the code below to recover the
# site and date information from the folder names.

# Parent directory containing all canopy photo folders
parent_dir <- "/Users/whitneymaxfield/Downloads/Labeled_CanopyPhotos"

# ============================================================
# 2. FIND ALL CANOPY PHOTOS
# ============================================================

# Search the parent directory for every JPG image.
#
# recursive = TRUE:
#   Search inside all subfolders, not just the parent folder.
#
# full.names = TRUE:
#   Return the complete file path for each image.
#
# ignore.case = TRUE:
#   Find both .JPG and .jpg files.
images <- list.files(
  parent_dir,
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
#   1. Finds the date folder
#   2. Finds the site folder
#   3. Gets the image filename
#   4. Runs the image through coveR2
#   5. Adds the identifying information to the results
#
# The function will then be applied to every image below.
process_image <- function(img){

  # ----------------------------------------------------------
  # Extract information from the folder structure
  # ----------------------------------------------------------

  # Get the name of the folder immediately containing the image.
  #
  # For example:
  # .../BOHO01/BOHO01_20260626/100_0044.JPG
  #
  # date_folder will become:
  # "BOHO01_20260626"
  date_folder <- basename(dirname(img))


  # Get the name of the folder containing the date folder.
  #
  # Using the same example, site_folder will become:
  # "BOHO01"
  site_folder <- basename(dirname(dirname(img)))


  # Get the filename of the image itself.
  #
  # Example:
  # "100_0044.JPG"
  image_name <- basename(img)


  # ----------------------------------------------------------
  # Run coveR2 on the image
  # ----------------------------------------------------------

  # Analyze the canopy photo using coveR2.
  #
  # coveR2 calculates the canopy cover metrics from the image
  # and returns the results as a data frame.
  out <- coveR2(img)


  # ----------------------------------------------------------
  # Add identifying information to the coveR2 results
  # ----------------------------------------------------------

  # Add the site, date-folder, and image filename to the
  # coveR2 output.
  #
  # .before = 1 places these identifying columns at the
  # beginning of the resulting data frame.
  out %>%
    mutate(
      Site = site_folder,
      Date_Folder = date_folder,
      Image = image_name,
      .before = 1
    )
}


# ============================================================
# 4. RUN coveR2 ON EVERY IMAGE
# ============================================================

# Apply process_image() to every image found above.
#
# map_dfr() means:
#   - run process_image() once for every image
#   - combine all of the resulting data frames
#     into one large data frame
#
# The final result contains the coveR2 measurements for every canopy photo.
results1 <- map_dfr(
  images,
  process_image
)


# Check the resulting dataset
head(results1)

# See how many rows and columns were produced
dim(results1)


# ============================================================
# 5. SAVE THE RESULTS
# ============================================================

# Write the complete coveR2 results to a CSV file.
#
# row.names = FALSE prevents R from adding an extra
# row-number column to the CSV.
write.csv(
  results1,
  "Canopy_Cover_Results.csv",
  row.names = FALSE
)