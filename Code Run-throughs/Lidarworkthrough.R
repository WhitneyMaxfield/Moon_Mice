###### Trying out LiDAR ######
# Workflow: build a canopy height model (CHM) from bare-earth (DEM) and 
# highest-hit (DSM) LiDAR rasters, then summarize and visualize canopy 
# structure at a specific point of interest ("bucket trap" site).

library(terra)

# --- Load LiDAR-derived surfaces ---
# DEM = Digital Elevation Model (bare earth / ground surface)
# DSM = Digital Surface Model (highest hit, i.e. top of canopy/objects)
dem <- rast("/Users/whitneymaxfield/Downloads/LDQ-45122D6/2014_OLC_Metro/Bare_Earth/bh45122d6")
dsm <- rast("/Users/whitneymaxfield/Downloads/LDQ-45122D6/2014_OLC_Metro/Highest_Hit/hh45122d6")

# Inspect raster metadata (extent, resolution, CRS) before proceeding
dem
dsm
plot(dem)
plot(dsm)

# --- Compute Canopy Height Model (CHM) ---
# CHM = DSM - DEM, i.e. height of surface features above bare ground
chm <- dsm - dem
chm[chm < 0] <- 0   # clip negative values (artifacts from noise/misalignment) to 0
plot(chm)

# --- Extract canopy stats at a point of interest ---
library(sf)

# Define bucket trap location (lon/lat, WGS84) and reproject to match CHM's CRS
trap <- st_sfc(st_point(c(-122.684142, 45.441706)), crs = 4326)
trap <- st_transform(trap, crs(chm))

# Create a 30 m buffer around the point to sample a local neighborhood
buf <- st_buffer(trap, 30)
buf_v <- vect(buf)

# Extract CHM values within the buffer
vals <- extract(chm, buf_v)

# Extract CHM value at the exact point (single pixel)
extract(chm, vect(trap))

# Summary stats of canopy height within the 30 m buffer
mean_height <- mean(vals[,2], na.rm = TRUE)      # average canopy height
max_height  <- max(vals[,2], na.rm = TRUE)       # tallest canopy feature
sd_height   <- sd(vals[,2], na.rm = TRUE)        # variability in height
cover_2m    <- mean(vals[,2] > 2, na.rm = TRUE)  # proportion of buffer with canopy > 2m

# --- Plot CHM with point overlay ---
pal <- terrain.colors(50)
plot(chm, col = pal, main = "Canopy Height Model (LiDAR)")

# (Re-declaring trap here is redundant since it's unchanged from above — 
# kept for clarity/standalone reproducibility of this plotting block)
trap <- st_sfc(st_point(c(-122.684142, 45.441706)), crs = 4326)
trap <- st_transform(trap, crs(chm))

plot(chm, col = pal)
plot(trap, add = TRUE, pch = 19, cex = 1.5, col = "red")  # overlay trap location

plot(chm, col = pal)
plot(buf, add = TRUE, border = "white", lwd = 2)  # overlay 30 m buffer

# --- Full-extent styled plot: CHM + point + buffer ---
plot(chm,
     col = hcl.colors(100, "YlGn"),
     axes = FALSE,
     box = FALSE,
     main = "LiDAR-Derived Canopy Structure at Bucket Trap Site")
# NOTE: typo below — "lot(" should be "plot(" or this line won't run
plot(trap, add = TRUE, pch = 19, col = "red", cex = 1.5)
plot(buf, add = TRUE, border = "white", lwd = 2)

# --- Zoom in on the area immediately around the trap ---
library(terra)
library(sf)

trap <- st_sfc(st_point(c(-122.684142, 45.441706)), crs = 4326)
trap <- st_transform(trap, crs(chm))

# Wider 75 m buffer used just to define the zoomed viewing window
zoom_buf <- st_buffer(trap, 75)
zoom_v <- vect(zoom_buf)

# Crop and mask CHM to the zoom window
chm_zoom <- crop(chm, zoom_v)
chm_zoom <- mask(chm_zoom, zoom_v)

plot(chm_zoom,
     col = hcl.colors(100, "YlGn"),
     main = "LiDAR Canopy Structure (Zoomed to Bucket Trap)",
     axes = FALSE,
     box = FALSE)
plot(zoom_v, add = TRUE, border = "white", lwd = 2)
plot(trap, add = TRUE, col = "red", pch = 19, cex = 1.5)

# --- Reverse color ramp (dark green = taller canopy) ---
pal <- hcl.colors(100, "Greens", rev = TRUE)
plot(chm_zoom,
     col = pal,
     main = "LiDAR-Derived Canopy Height (Zoomed)",
     axes = FALSE,
     box = FALSE)

# --- Final version: zoomed CHM with legend and labeled units ---
plot(chm_zoom,
     col = hcl.colors(100, "Greens", rev = TRUE),
     axes = FALSE,
     box = FALSE,
     legend = TRUE,
     plg = list(title = "Canopy height (m)"))  # legend title clarifies units
plot(trap, add = TRUE, col = "red", pch = 19, cex = 1.5)
