library(caret)
library(tidyverse)
library(earth)
library(doParallel)

# set up cluster to allow faster model training 
n.cores <- parallel::detectCores() - 2
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
print(my.cluster)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5)


data <- readRDS("data/combined-dataset.rds")

# remove site SR?
data <- data %>% dplyr::filter(site!='SR')

data.prep <- data %>%
  mutate(
    # Convert the column to a date-time object
    datetime = as_datetime(Timestamp_UTC),
    
    # Get month and hour
    summer_cos = ifelse(is.na(datetime), cos((month(ymd(Timestamp_UTC))-6)*(pi/6)), 
                        cos((month(datetime)-6)*(pi/6))),                     # Assume peak brightness at June
    noon_cos = ifelse(is.na(datetime), -1, cos((hour(datetime)-12)*(pi/12)))  # Assume peak brightness at noon
  ) %>%
  select(-c(datetime,Timestamp_UTC,zip_code,lat,lon)) %>% # Drop timestamps and site specific data
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

saveRDS(data.prep, "data/data-prep_no-SR.rds")
saveRDS(data.train, "data/data-train_no-SR.rds")
saveRDS(data.test, "data/data-test_no-SR.rds")

data.prep <- readRDS("data/data-prep_no-SR.rds")
data.train <- readRDS("data/data-train_no-SR.rds")
data.test <- readRDS("data/data-test_no-SR.rds")


mars.tune <- expand.grid(nprune = c(25,30,35,40,45),
                         degree = c(2,3,4,5))
fit.mars <- train(nsb ~ .,
                  data = data.train,
                  method = "earth",
                  tuneGrid = mars.tune,
                  trControl = ctrl,
                  preProcess = c("center", "scale"))
fit.mars
saveRDS(fit.mars, "mars-model_no-SR.rds")



















site_i <- "WODC"
  
data.prep <- readRDS(paste("data/data-prep-",site_i,".rds",sep=""))
data.train <- readRDS(paste("data/data-train-",site_i,".rds",sep=""))
data.test <- readRDS(paste("data/data-test-",site_i,".rds",sep=""))

mars.tune <- expand.grid(nprune = c(25,20,35,40,45),
                         degree = c(2,3,4,5))
fit.mars <- train(nsb ~ .,
                  data = data.train,
                  method = "earth",
                  tuneGrid = mars.tune,
                  trControl = ctrl,
                  preProcess = c("center", "scale"))
fit.mars
saveRDS(fit.mars, paste("mars-model-",site_i,".rds",sep=""))
  


# Stop parallel
parallel::stopCluster(cl = my.cluster)
registerDoSEQ() 





