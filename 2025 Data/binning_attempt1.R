library(dplyr)
library(lubridate)
library(stringr)

canopy_2025 <- read.csv(
  "/Users/whitneymaxfield/Downloads/2025_Canopy_Data - Foliage cover.csv"
)
canopy_2025_clean <- canopy_2025 %>%
  mutate(
    Date = mdy(Date),
    
    # Keep May, June, July
    Image = case_when(
      type == "knee" ~ "Upper",
      type == "ground" ~ "Lower"
    )
  ) %>%
  filter(
    month(Date) %in% 5:7,
    Image %in% c("Upper", "Lower")
  ) %>%
  
  # One photo per bucket and image type
  group_by(BKT, Image) %>%
  slice(1) %>%
  ungroup() %>%
  
  # Create the filename-style Image column
  mutate(
    Image = paste0(
      Image,
      BKT,
      "_",
      format(Date, "%Y%m%d"),
      ".JPG"
    )
  ) %>%
  
  select(
    BKT,
    Date,
    Image,
    everything()
  )

write.csv(
  canopy_2025_clean,
  "2025_Canopy_Cover_Results_CLEANED.csv",
  row.names = FALSE
)
