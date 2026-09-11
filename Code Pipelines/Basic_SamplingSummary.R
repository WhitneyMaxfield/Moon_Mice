library(tidyverse)

#### JUST TRYON!! #### 

data <- read.csv("/Users/whitneymaxfield/Desktop/Moon_data_202606/Moon_Mice/2026 Data/LidarCanopyMoon_masterLog_20260911.csv")

# Basic sampling summary
names(data) 
nrow(data)

summary_sampling <- data %>%
  filter(night == TRUE) %>%
  summarise(
    unique_traps = n_distinct(stationID),
    unique_nights = n_distinct(date),
    total_observations = n(),
    total_PESO_detections = sum(PESO, na.rm = TRUE),
    detection_proportion = mean(PESO, na.rm = TRUE),
  )

summary_sampling
write.csv(
  summary_sampling,
  "Summary_sampling.csv",
  row.names = FALSE
)

trap_summary <- data %>%
  filter(night == TRUE) %>%
  group_by(stationID) %>%
  summarise(
    nights = n_distinct(date),
    observations = n(),
    PESO_detections = sum(PESO, na.rm = TRUE),
    detection_proportion = mean(PESO, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(
  trap_summary,
  "Trap_Summary_sampling.csv",
  row.names = FALSE
)
trap_summary

trap_summary %>%
  summarise(
    mean_detections = mean(PESO_detections),
    median_detections = median(PESO_detections),
    min_detections = min(PESO_detections),
    max_detections = max(PESO_detections)
  )


