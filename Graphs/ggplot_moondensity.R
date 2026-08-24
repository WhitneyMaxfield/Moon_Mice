library(tidyverse)

#BOHO01 (just change stationID)
data %>%
  filter(
    stationID =="BOHO04",
    PESO > 0,
    night == TRUE
  ) %>%
  ggplot(
    aes(x = moonlightModel)
  ) +
  geom_density(
    fill = "darkgreen",
    alpha = .5
  ) +
  labs(
    x = "Moonlight model",
    y = "Detection Density",
    title = "Distribution of positive detections across moonlight conditions for BOHO04"
  ) +
  theme_classic()

ggsave("BOHO04_phase_dis.png")

######### barchart for detections by site #########

#take data, filter by night, group by station, summarize detections to get proportions 
# (detections over total)
install.packages("ggtext")
library(ggtext)
summary_night <- data %>%
  filter(night == TRUE) %>%
  group_by(stationID) %>%
  summarise(
    detections = sum(PESO),
    total = n(),
    proportion = detections / total
  ) 

ggplot(summary_night, aes(x = stationID, y = proportion)) + 
  geom_col(fill = "steelblue") + 
  labs( 
    x = "Site ID", 
    y = "Proportion of detections", 
    title = "Nighttime Detections of *Peromyscus sonoriensis* by Site" 
  ) + 
  theme(
    plot.title = element_markdown(hjust = 0.5), 
    panel.background = element_rect(fill = "white", colour = "black")
  )

ggsave("proportion_bysite.png")
