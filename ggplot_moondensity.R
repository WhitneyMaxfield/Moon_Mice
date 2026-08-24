library(tidyverse)

#BOHO01
data %>%
  filter(
    stationID =="BOHO04",
    PESO > 0,
    night == TRUE
  ) %>%
  ggplot(
    aes(x = moonPhase)
  ) +
  geom_density(
    fill = "darkgreen",
    alpha = 0.5
  ) +
  labs(
    x = "Moon Phase",
    y = "Detection Density",
    title = "Distribution of positive detections across moon phases for BOHO04"
  ) +
  theme_classic()

ggsave("BOHO04_phase_dis.png")
