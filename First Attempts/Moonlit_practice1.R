####### Moonlit attempt with practice master log 6/25/2026 #######
install.packages("moonlit")
library(Moonlit)
library(purrr)
library(dplyr)
library(purrr)
library(lubridate)

# Convert timestamp column
# Convert to POSIXct with local timezone — required by calculateMoonlightIntensity()
practice_log$time_stamp <- ymd_hms(
  practice_log$time_stamp,
  tz = "America/Los_Angeles"
)

###### Calculate moonlight intensity ###### 
#using e=0.24 because tryon is roughly 200m above sea level but we could also consider using 
#e=0.28 which is aat sea level. We could also potentially calculate our own specific value for this! 
# map_dfr() used because calculateMoonlightIntensity() expects scalar inputs,
# not vectorized columns. Results are row-bound in practice_log row order.

moon_data_practice <- map_dfr(
  seq_len(nrow(practice_log)),
  ~ calculateMoonlightIntensity(
      lat = practice_log$latitude[.x],
      lon = practice_log$longitude[.x],
      date = practice_log$time_stamp[.x],
      e = 0.28
    )
)

#selecting output columns we want
moon_data_practice <- moon_data_practice %>%
  select(
    night,
    sunAltDegrees,
    moonAltDegrees,
    moonlightModel,
    twilightModel,
    illumination,
    moonPhase
  )

#re-join and export 
practice_log_final <- bind_cols(practice_log, moon_data_practice)

write.csv(
  practice_log_final,
  "practice_log_with_moonlight.csv",
  row.names = FALSE
)
