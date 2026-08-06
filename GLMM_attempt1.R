#fitting GLMM
install.packages("glmmTMB")
library(glmmTMB)

final_master_log <- read.csv("LidarCanopyMoon_masterLog.csv")
final_master_log$stationID <- as.factor(final_master_log$stationID)
class(final_master_log$stationID)
table(final_master_log$PESO)

model_data <- final_master_log |>
  filter(
    !is.na(PESO),
    !is.na(moonlightModel),
    !is.na(mean_height),
    !is.na(sd_height),
    !is.na(cover2m)
  )

nrow(model_data)

dark_moon_model <- glmmTMB(
  PESO ~
    moonlightModel +
    mean_height +
    sd_height +
    cover2m +
    (1 | stationID),
  family = binomial,
  data = model_data
)

summary(dark_moon_model)


