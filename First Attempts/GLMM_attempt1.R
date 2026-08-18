#fitting GLMM
install.packages("glmmTMB")
library(glmmTMB)

model_data <- master_log_lidar_canopy |>
  filter(
    !is.na(PESO),
    !is.na(moonlightModel),
    !is.na(mean_height),
    !is.na(sd_height),
    !is.na(cover2m),
    !is.na(CC),
    !is.na(FC)
  )

nrow(model_data)

moon_model <- glmmTMB(
  PESO ~
    moonlightModel +
    mean_height +
    sd_height +
    cover2m +
    CC +
    FC +
    (1 | stationID),
  family = binomial,
  data = model_data
)

summary(moon_model)
