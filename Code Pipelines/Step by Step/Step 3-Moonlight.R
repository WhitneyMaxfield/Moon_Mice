
###############################################################
#       STEP 3: add moonlight data to the master log 
###############################################################

# Moonlit calculates the astronomical conditions present at the
# exact time and location of each camera observation.

#  Variables include:
#   • Moonlight intensity
#   • Moon phase
#   • Lunar elevation
#   • Solar elevation
#   • Twilight intensity

# Unlike variables such as site or elevation, these variables
# can change from one observation to the next. Therefore, they
# are calculated using the latitude, longitude, and timestamp
# of each individual observation.


# ============================================================
# 1. CONVERT THE TIMESTAMP
# ============================================================

# calculateMoonlightIntensity() requires the observation time
# to be in POSIXct date-time format.

# ymd_hms() converts the existing time_stamp column into a
# proper date-time object.

# tz = "America/Los_Angeles":
#   Tells R that the timestamps are recorded in Pacific Time.
#   This is important because moon and sun positions depend
#   on the exact local time of each observation.

# The converted timestamps are stored in a new object called
# master_log_moon so that the original master_log is not changed.

master_log_moon <- master_log |>
  mutate(
    time_stamp = ymd_hms(
      time_stamp,
      tz = "America/Los_Angeles"
    )
  )


# Check that the timestamp was successfully converted.
# The expected output should indicate that time_stamp is a
# POSIXct/POSIXt object.

class(master_log_moon$time_stamp)


# ============================================================
# 2. CALCULATE MOONLIGHT CONDITIONS
# ============================================================

# calculateMoonlightIntensity() calculates lunar and solar
# conditions for a specific latitude, longitude, and time.

# The function expects one latitude, longitude, and timestamp
# at a time, so we need to run it once for every observation.

# map_dfr() is used to:
#   1. Go through every row of master_log_moon
#   2. Run calculateMoonlightIntensity() for that row
#   3. Combine all of the individual results into one
#      data frame by row.

# seq_len(nrow(master_log_moon)):
#   Creates a sequence from 1 to the total number of rows.
#   Each number represents one observation in the master log.

# .x:
#   Represents the current row number being processed.
#
# e = 0.28:
#   Represents the atmospheric extinction coefficient used by
#   moonlit when calculating moonlight intensity.

#   We use 0.28 because it represents approximately sea-level
#   conditions. Most Tryon Creek sites are only ~50 m above sea
#   level, so 0.28 is a reasonable approximation for this.

moon_data <- map_dfr(
  seq_len(nrow(master_log_moon)),
  ~ calculateMoonlightIntensity(
      lat  = master_log_moon$latitude[.x],
      lon  = master_log_moon$longitude[.x],
      date = master_log_moon$time_stamp[.x],
      e = 0.28
    )
)


# Check the calculated moonlight data.
# This allows us to verify that moonlit successfully returned
# values for each observation (just checking first 20 rows).

head(moon_data, n = 20)

# ============================================================
# 3. KEEP THE MOONLIGHT VARIABLES WE NEED
# ============================================================

# calculateMoonlightIntensity() returns several variables.
#
# Here, we keep only the variables needed for our analysis 
  #currently all variables but this is useful if we want to quickly 
  #filter out specific variables in the future!

# night:
#   Indicates whether the observation occurred during the night.

# sunAltDegrees:
#   The sun's elevation above/below the horizon in degrees.

# moonAltDegrees:
#   The moon's elevation above/below the horizon in degrees.

# moonlightModel:
#   Categorical estimate of moonlight conditions based on the
#   calculated lunar conditions.

# twilightModel:
#   Categorical estimate of twilight conditions.

# illumination:
#   Estimated proportion of the moon's visible disk that is
#   illuminated.

# moonPhase:
#   The lunar phase associated with the observation.

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


# ============================================================
# 4. ADD MOONLIGHT DATA TO THE MASTER LOG
# ============================================================

# bind_cols() combines the original master log with the
# calculated moonlight variables.

# Because moon_data was generated in the exact same row order
# as master_log_moon, each row of moon_data corresponds to the
# same observation in master_log_moon.

master_log_moon <- bind_cols(
  master_log_moon,
  moon_data
)

#check first 20 rows 
head(master_log_moon, n=20)

