#attempting bins 

library(lubridate)
library(dplyr)
library(tidyr)

binned_PESO <- practice_log_final %>%
  filter(night == TRUE) %>%
  mutate(
    date      = as.Date(time_stamp),
    bin       = floor((hour(time_stamp) * 60 + minute(time_stamp)) / 15) + 1,
    bin_start = floor_date(time_stamp, "15 minutes")
  ) %>%
  group_by(stationID, date, bin, bin_start) %>%  # add bin_start here
  summarise(
    detected = as.integer(any(PESO > 0)),
    .groups = "drop"
  )

all_bins <- binned_PESO %>%
  distinct(stationID, date) %>%
  cross_join(tibble(bin = 1:96))

binned_PESO_complete <- all_bins %>%
  left_join(binned_PESO, by = c("stationID", "date", "bin")) %>%
  replace_na(list(detected = 0))
