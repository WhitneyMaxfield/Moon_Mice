library(dplyr)
library(ggplot2)
df <- read.csv("/Users/whitneymaxfield/Desktop/Moon_data_202606/Simulated_Data/SimulatedData_lidar_canopy_moon.csv")
moon_map <- df %>%
  filter(night == TRUE) %>%
  mutate(
    moon_bin = cut(
      moonlightModel,
      breaks = c(-Inf, 0.05, 0.10, 0.20, 0.35, Inf),
      labels = c(
        "Very low",
        "Low",
        "Moderate",
        "High",
        "Very high"
      )
    )
  ) %>%
  group_by(
    stationID,
    latitude,
    longitude,
    moon_bin
  ) %>%
  summarise(
    detections = sum(PESO == 1, na.rm = TRUE),
    observations = sum(!is.na(PESO)),
    detection_prop = detections / observations,
    .groups = "drop"
  )
moon_map

ggplot(
  moon_map %>% filter(stationID == "BOHO01")
) +
  geom_point(
    aes(
      x = longitude,
      y = latitude,
      size = detection_prop
    ),
    alpha = 0.8
  ) +
  facet_wrap(~ moon_bin) +
  coord_equal() +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    size = "PESO detection\nproportion"
  )



#circle size and color? 
ggplot(
  moon_map %>% filter(stationID == "BOHO01")
) +
  geom_point(
    aes(
      x = longitude,
      y = latitude,
      size = detection_prop,
      color = detection_prop
    ),
    alpha = .85
  ) +
  facet_wrap(~ moon_bin) +
  coord_equal() +
  scale_size_continuous(
    range = c(3, 12)
  ) +
  theme_minimal() +
  labs(
    title = "BOHO01",
    x = "Longitude",
    y = "Latitude",
    size = "Detection proportion",
    color = "Detection proportion"
  )



