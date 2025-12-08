library(caret)
library(tidyverse)

# Import data
data <- readRDS("data/combined-dataset.rds")

data.prep <- data %>%
  mutate(
    # Convert the column to a date-time object
    datetime = ymd_hms(Timestamp_UTC),
    
    # Get month and hour
    month_cos = ifelse(is.na(datetime), 6, cos((month(datetime)-6)*(pi/6))), # Assume peak brightness at June
    noon_cos = ifelse(is.na(datetime), 0, cos((hour(datetime)-12)*(pi/6)))    # Assume peak brightness at noon
  ) %>%
  select(-datetime) %>%
  mutate(
    # Mean imputing missing elec_summer_peak and elec_winter_peak
    across(
      .cols = c(elec_summer_peak,elec_winter_peak),
      .fns = ~ replace_na(., mean(., na.rm = TRUE))
    )
  )

