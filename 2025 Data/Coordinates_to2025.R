# Old data
old_data <- X2025_LidarCanopyMoon_masterLog

# New data
new_data <- Tracy_Data

# Make a BKTID_ROW -> coordinate lookup
coordinates <- old_data |>
  select(
    BKTID_ROW,
    latitude,
    longitude
  ) |>
  distinct()

# Add coordinates to the NEW data using BKTID_ROW
new_data_with_coords <- new_data |>
  left_join(
    coordinates,
    by = "BKTID_ROW"
  )

write.csv(
  new_data_with_coords,
  "2025_FullSheet_with_coords",
  row.names = FALSE
)
