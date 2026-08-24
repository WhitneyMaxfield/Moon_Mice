library(tidyverse)

#okay this is just looking at positive PESO so from what I understand 
#basicaly answering "Where do my detections occur along the moonlight gradient?" 
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

###############################################################################
                    #barchart for detections by site 
###############################################################################

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

###############################################################################
#### similar to above but is detection probability/proportion across moonlight  
### moonlight is binned 
###############################################################################
data_plot <- data %>%
  filter(
    night == TRUE,
    !is.na(moonlightModel)
  ) %>%
  mutate(
    peso_detected = ifelse(PESO > 0, 1, 0)
  )

summary_moon <- data_plot %>%
  mutate(
    moon_bin = cut(
      moonlightModel,
      breaks = seq(0, 1, by = 0.05),
      include.lowest = TRUE
    )
  ) %>%
  group_by(stationID, moon_bin) %>%
  summarise(
    moonlight = mean(moonlightModel),
    proportion = mean(peso_detected),
    n = n(),
    .groups = "drop"
  )

ggplot(
  summary_moon %>% filter(stationID == "BOHO04"),
  aes(
    x = moonlight,
    y = proportion
  )
) +
  
  geom_point() +
  
  # Shaded area under LOESS curve
  stat_smooth(
    method = "loess",
    se = FALSE,
    geom = "ribbon",
    aes(
      ymin = 0,
      ymax = after_stat(y)
    ),
    fill = "darkgreen",
    alpha = 0.2
  ) +
  
  # LOESS line
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "darkgreen",
    linewidth = 1.2
  ) +
  
  # Bucket label
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "BOHO04",
    hjust = -6,
    vjust = 3,
    size = 5,
    fontface = "italic"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  
  labs(
    x = "Moonlight",
    y = "Proportion of detections"
    # title = "Nighttime Detections of *Peromyscus sonoriensis* by moonlight"
  ) +
  
  theme(
    plot.title = element_markdown(
      hjust = 0.5,
      margin = margin(b = 10),
      size = 15
    ),
    panel.background = element_rect(
      fill = "white",
      colour = "black"
    )
  )

ggsave(
  "detection_proportionBOHO04.png",
  width = 7,
  height = 5,
  dpi = 300
)


