library(caret)
library(tidyverse)
library(earth)
library(doParallel)
library(ggbiplot)
library(ggplot2)

# Import data
data <- readRDS("data/combined-dataset.rds")

data.prep <- data %>%
  mutate(
    # Convert the column to a date-time object
    datetime = ymd_hms(Timestamp_UTC),
    
    # Get month and hour
    summer_cos = ifelse(is.na(datetime), cos((month(ymd(Timestamp_UTC))-6)*(pi/6)), 
                        cos((month(datetime)-6)*(pi/6))),                     # Assume peak brightness at June
    noon_cos = ifelse(is.na(datetime), -1, cos((hour(datetime)-12)*(pi/12)))  # Assume peak brightness at noon
  ) %>%
  select(-c(datetime,Timestamp_UTC,site,zip_code, lat, lon)) %>% # Drop timestamps and site specific data
  mutate(
    # Mean imputing missing elec_summer_peak and elec_winter_peak
    across(
      .cols = c(elec_summer_peak,elec_winter_peak),
      .fns = ~ replace_na(., mean(., na.rm = TRUE))
    )
  )

# Split the training and testing data (75%, 25%) respectively
set.seed(2025125103) 
test_index <- createDataPartition(
  y = data.prep$nsb, 
  p = 0.25, # 25% for the test set
  list = FALSE
)

# Separate the datasets
data.train <- data.prep[-test_index, ]
data.test <- data.prep[test_index, ]

saveRDS(data.prep, "data/data-prep.rds")
saveRDS(data.train, "data/data-train.rds")
saveRDS(data.test, "data/data-test.rds")

data.prep <- readRDS("data/data-prep.rds")
data.train <- readRDS("data/data-train.rds")
data.test <- readRDS("data/data-test.rds")

# Desperate to make things go faster
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5)

#fit.glm <- readRDS("glm-model.rds")
# glm.tune <- expand.grid(alpha = c(0.1, 0.55, 1.0),
#                         lambda = exp(seq(log(0.0001), log(0.005), length.out = 3)))
# fit.glm <- train(nsb ~ .,
#                  data = data.train,
#                  method = "glmnet",
#                  tuneGrid = glm.tune,
#                  trControl = ctrl,
#                  preProcess = c("center","scale"))
#fit.glm
#saveRDS(fit.glm, "glm-model.rds")

fit.mars <- readRDS("mars-model.rds")
mars.tune <- expand.grid(nprune = c(35,40,45),
                         degree = c(3,4,5))
fit.mars <- train(nsb ~ .,
                  data = data.train,
                  method = "earth",
                  tuneGrid = mars.tune,
                  trControl = ctrl,
                  preProcess = c("center", "scale"))
fit.mars
saveRDS(fit.mars, "mars-model.rds")

###############
# Stop parallel
stopCluster(cl)
registerDoSEQ() 
###############
