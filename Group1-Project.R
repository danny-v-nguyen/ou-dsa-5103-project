################################################################################
# DSA 5103 - Intelligent Data Analytics - Fall 2025
# Course Project: Predicting Night Sky Brightness
# Group 1: Miles Leonard, Danny Nguyen
# 
# This is a combination of all individual R script scratchpads used
# during the development of the model used for the project report.
# There are a lot of references to exported RDS objects that were used to
# prevent re-training and all files expect relative pathing to the project
# data folder.
#
# Please reference our github repo for a full (chaotic) history of our progress
# and copy of all our R objects + data at 
# https://github.com/danny-v-nguyen/ou-dsa-5103-project
#
################################################################################

######################
# START: data-import.R
######################

library(tidyverse)
library(knitr)
library(kableExtra)
library(webshot2)

# Data import
#############
electric.service.raw <- read.csv("data/electric-retail-service-territories/electric-retail-service-territories.csv", 
                                 header = TRUE, na.strings = "NOT AVAILABLE",
                                 stringsAsFactors = TRUE)
electric.service.raw <- electric.service.raw %>%
  mutate(across(c(OBJECTID, NAICS_CODE), as.factor))

zip.centroids.raw <- read.csv("data/PopulationWeightedCentroids/ZIP_Code_Population_Weighted_Centroids_-8037460774014549482.csv",
                              header = TRUE, stringsAsFactors = TRUE)
zip.centroids.raw <- zip.centroids.raw %>%
  mutate(across(c(OBJECTID), as.factor))

zip.pop.raw <- read.csv("data/DECENNIALDHC2020.P1_2025-11-23T222307/DECENNIALDHC2020.P1-Data.csv",
                        header = TRUE, skip = 1)
zip.pop.raw <- zip.pop.raw %>%
  mutate(ZIP_Code = str_remove(Geographic.Area.Name, "ZCTA5\\s+")) %>%
  select(-c(X,Geographic.Area.Name)) %>%
  rename(Population = X...Total) %>%
  mutate(across(c(Geography), as.factor)) %>%
  mutate(across(c(ZIP_Code), as.numeric))

gan.raw <- read.csv("data/GaN_nsb-atmos_data.csv",
                    header = TRUE, stringsAsFactors = TRUE)
gan.prep <- gan.raw %>%
  mutate(Timestamp_dt = ymd_hms(Timestamp_UTC),
         Year = year(Timestamp_dt),
         Month = month(Timestamp_dt),
         Day = day(Timestamp_dt),
         Hour = hour(Timestamp_dt),
         Minute = minute(Timestamp_dt),
         Second = second(Timestamp_dt)
  )
# Data Quality Report
#####################

Q1 <- function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[2] }
Q3 <- function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[4] }

myNumericSummary <- function(x,na.rm=TRUE) {
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x,na.rm=na.rm), min(x,na.rm=na.rm),
    Q1(x,na.rm=na.rm), median(x,na.rm=na.rm), Q3(x,na.rm=na.rm), max(x,na.rm=na.rm),
    sd(x,na.rm=na.rm))
}

getmodes <- function(v,type=1){
  tbl <- table(v)
  m1 <- which.max(tbl)
  
  if(type == 1){
    return(names(m1))
  } else if (type == 2) {
    return(names(which.max(tbl[-m1])))
  } else if (type == -1) {
    return (names(which.min(tbl)))
  } else {
    stop("Invalid type selected")
  }
}

getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1 <- which.max(tbl)
  
  if(type == 1){
    return(max(tbl))
  } else if (type == 2) {
    return(max(tbl[-m1]))
  } else if (type == -1) {
    return (min(tbl))
  } else {
    stop("Invalid type selected")
  }
}

myFactorSummary <- function(x,na.rm=TRUE) {
  c(length(x), n_distinct(x), sum(is.na(x)))
}

# function to streamline data quality report creation for both training and testing data
DQreport <- function(data) {
  
  data.num <- data %>% dplyr::select(where(is.numeric))
  data.factor <- data %>% dplyr::select(where(is.factor))
  
  numericSummary <- data.num %>% summarize(across(colnames(data.num),myNumericSummary))
  numericSummary <- cbind(stat=c("n","unique","missing", "mean",
                                 "min", "Q1","median","Q3","max","sd"),
                          numericSummary)
  
  numericSummaryFinal <- numericSummary %>%
    pivot_longer(!c(stat),names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(missing_pct = 100*missing/n,
           unique_pct = 100*unique/n) %>%
    select(variable, n, missing, missing_pct, unique, unique_pct, everything())
  
  categoricSummary <- data.factor %>% summarize(across(colnames(data.factor),myFactorSummary))
  categoricSummary <- cbind(stat=c("n","unique","missing"), categoricSummary)
  
  categoricSummaryFinal <- categoricSummary %>%
    pivot_longer(!c(stat), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(missing_pct = 100*missing/n,
           unique_pct = 100*unique/n) %>%
    rowwise() %>%
    mutate("1st mode" = getmodes(data[[variable]]),
           "1st mode freq" = getmodesCnt(data[[variable]]),
           "2nd mode" = list(getmodes(data[[variable]],type=2)),
           "2nd mode freq" = getmodesCnt(data[[variable]],type=2),
           "least common" = getmodes(data[[variable]],type=-1),
           "least common freq" = getmodesCnt(data[[variable]],type=-1)) %>%
    ungroup() %>%
    select(variable, n, missing, missing_pct, unique, unique_pct, 
           "1st mode", "1st mode freq",
           "2nd mode", "2nd mode freq",
           "least common", "least common freq")
    
  return (list(categoricSummaryFinal, numericSummaryFinal))
  
}

# Data Quality
##############
gan.DQR <- DQreport(gan.prep)
kable(elec.service.DQR[[1]], "latex") %>%
  save_kable( "data/gan-DQR-factor.pdf")
kable(elec.service.DQR[[2]], "latex") %>%
  save_kable( "data/gan-DQR-numeric.pdf")

elec.service.DQR <- DQreport(electric.service.raw)
kable(elec.service.DQR[[1]], "latex") %>%
  save_kable( "data/elec-service-DQR-factor.pdf")
kable(elec.service.DQR[[2]], "latex") %>%
  save_kable( "data/elec-service-DQR-numeric.pdf")
#View(elec.service.DQR[[1]])
#View(elec.service.DQR[[2]])

zip.centroids.DQR <- DQreport(zip.centroids.raw)
kable(zip.centroids.DQR[[1]], "latex") %>%
  save_kable( "data/zip-centroids-DQR-factor.pdf")
kable(zip.centroids.DQR[[2]], "latex") %>%
  save_kable( "data/zip-centroids-DQR-numeric.pdf")
#View(zip.centroids.DQR[[1]])
#View(zip.centroids.DQR[[2]])

zip.pop.DQR <- DQreport(zip.pop.raw)
kable(zip.pop.DQR[[1]], "latex") %>%
  save_kable( "data/zip-pop-DQR-factor.pdf")
kable(zip.pop.DQR[[2]], "latex") %>%
  save_kable( "data/zip-pop-DQR-numeric.pdf")
#View(zip.pop.DQR[[1]])
#View(zip.pop.DQR[[2]])

# DATA EXPORT
#############

gan.data <- gan.prep %>%
  select(-c(Minute, Second)) %>%
  filter(!is.na(nsb)) %>%
  mutate(across(Timestamp_UTC, as.character.POSIXt))
saveRDS(gan.data, file = "data/gan-data.rds")

zip.data <- zip.centroids.raw %>%
  left_join(zip.pop.raw, by = c("STD_ZIP5" = "ZIP_Code")) %>%
  filter(!is.na(Population))
zip.data.DQR <- DQreport(zip.data)
#View(zip.data.DQR[[1]])
#View(zip.data.DQR[[2]])
saveRDS(zip.data, file = "data/zip-data.rds")
#test <- readRDS("data/zip-data.rds")

elec.service.data <- electric.service.raw %>%
  left_join(zip.centroids.raw, by = c("ZIP" = "STD_ZIP5")) %>%
  filter(!is.na(ZIP)) %>%
  filter(!is.na(LATITUDE))
elec.service.data.DQR <- DQreport(elec.service.data)
#View(elec.service.data.DQR[[1]])
#View(elec.service.data.DQR[[2]])
saveRDS(elec.service.data, file = "data/elec-service-data.rds")
#test <- readRDS("data/elec-service-data.rds")

########################
# START: data_analysis.R
########################

# Data visualizations
library(knitr)
options(digits=3)
options(scipen=99)
library(caret)
library(tidyverse)


################################# import data ##################################

atmos <- read.csv('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\GaN_nsb-atmos_data.csv',header=TRUE,stringsAsFactors=TRUE)


################################# data analysis report #########################


Q1 <- function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[2] }
Q3 <- function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[4] }

myNumericSummary <- function(x,na.rm=TRUE) {
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x,na.rm=na.rm), min(x,na.rm=na.rm),
    Q1(x,na.rm=na.rm), median(x,na.rm=na.rm), Q3(x,na.rm=na.rm), max(x,na.rm=na.rm),
    sd(x,na.rm=na.rm))
}

getmodes <- function(v,type=1){
  tbl <- table(v)
  m1 <- which.max(tbl)
  
  if(type == 1){
    return(names(m1))
  } else if (type == 2) {
    return(names(which.max(tbl[-m1])))
  } else if (type == -1) {
    return (names(which.min(tbl)))
  } else {
    stop("Invalid type selected")
  }
}

getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1 <- which.max(tbl)
  
  if(type == 1){
    return(max(tbl))
  } else if (type == 2) {
    return(max(tbl[-m1]))
  } else if (type == -1) {
    return (min(tbl))
  } else {
    stop("Invalid type selected")
  }
}

myFactorSummary <- function(x,na.rm=TRUE) {
  c(length(x), n_distinct(x), sum(is.na(x)))
}

# function to streamline data quality report creation for both training and testing data
DQreport <- function(data) {
  
  data.num <- data %>% dplyr::select(where(is.numeric))
  data.factor <- data %>% dplyr::select(where(is.factor))
  
  numericSummary <- data.num %>% summarize(across(colnames(data.num),myNumericSummary))
  numericSummary <- cbind(stat=c("n","unique","missing", "mean",
                                 "min", "Q1","median","Q3","max","sd"),
                          numericSummary)
  
  numericSummaryFinal <- numericSummary %>%
    pivot_longer((data %>% dplyr::select(where(is.numeric)) %>% names() %>% .[1]):(data %>% dplyr::select(where(is.numeric)) %>% names() %>% .[length(.)]),names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(missing_pct = 100*missing/n,
           unique_pct = 100*unique/n) %>%
    select(variable, n, missing, missing_pct, unique, unique_pct, everything())
  
  categoricSummary <- data.factor %>% summarize(across(colnames(data.factor),myFactorSummary))
  categoricSummary <- cbind(stat=c("n","unique","missing"), categoricSummary)
  
  categoricSummaryFinal <- categoricSummary %>%
    pivot_longer((data %>% dplyr::select(where(is.factor)) %>% names() %>% .[1]):(data %>% dplyr::select(where(is.factor)) %>% names() %>% .[length(.)]),names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(missing_pct = 100*missing/n,
           unique_pct = 100*unique/n) %>%
    rowwise() %>%
    mutate("1st mode" = getmodes(data[[variable]]),
           "1st mode freq" = getmodesCnt(data[[variable]]),
           "2nd mode" = list(getmodes(data[[variable]],type=2)),
           "2nd mode freq" = getmodesCnt(data[[variable]],type=2),
           "least common" = getmodes(data[[variable]],type=-1),
           "least common freq" = getmodesCnt(data[[variable]],type=-1)) %>%
    ungroup() %>%
    select(variable, n, missing, missing_pct, unique, unique_pct, 
           "1st mode", "1st mode freq",
           "2nd mode", "2nd mode freq",
           "least common", "least common freq")

  return (list(numericSummaryFinal, categoricSummaryFinal))
  
}


# Reports for training data
trainSummary <- DQreport(atmos) # return (list(numericSummaryFinal, categoricSummaryFinal))
trainSummary[1] %>% kable()
trainSummary[2] %>% kable()


################################# oither stuff #########################


atmos <- readRDS('data\\nsb-era5-data-with-missing.rds')

library(lubridate)

atmos$Timestamp_UTC <- as_datetime(atmos$Timestamp_UTC, tz = "UTC")

#library(VIM)  #package for "Visualization and Imputation of Missing Values"

# can use VIM's "aggr" function to also get overall information on missing
#a<-aggr(atmos)

# use VIM function "marginplot" to get a scatter plot that includes information on missing values
#marginplot(atmos[c('Timestamp_UTC','nsb')], col = c("blue", "red", "orange"))


library(tidyverse)

hourly_summary_floor <- atmos %>%
  mutate(hour_start = hour(Timestamp_UTC)) %>%
  group_by(hour_start) %>%
  summarise(
    n_observations = n(),
    n_missing = sum(is.na(nsb))
  ) %>%
  ungroup()

library(ggplot2)

ggplot(hourly_summary_floor, aes(x = hour_start, y = n_missing)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Measurement by Hour",
    x = "Hour of Day",
    y = "Average Measurement"
  ) +
  theme_minimal()

barplot(data=hourly_summary_floor, n_missing ~ hour_start,
        main = 'NSB Missing Values',
        xlab = "Hour of Day (UTC)",
        ylab = "Missing Value Count")




# combining datasets

data <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\nsb-era5-data.rds')
elec <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\elec-service-data.rds')
zip <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\zip-data.rds')

# Danny's linux paths
#data <- readRDS('data/nsb-era5-data.rds')
#elec <- readRDS('data/elec-service-data.rds')
#zip <- readRDS('data/zip-data.rds')

elec$ID <- as.numeric(elec$ID)

zip$OBJECTID <- as.numeric(zip$OBJECTID)

sites <- unique(data$site)

# Need to use great-circle distances for lat/lon
library(geosphere) #distHaversine

for (i in levels(sites)) {
  
  data_i <- data[data$site==i,]
  lat_i <- first(data_i$lat)
  lon_i <- first(data_i$lon)
  
  site_coord <- c(lon_i,lat_i)
  
  # find closest zip code to nsb monitoring site
  zip_coords <- cbind(zip$LON, zip$LAT)
  distance_matrix <- distm(x = site_coord, y = zip_coords, fun = distHaversine)
  min_dist_zip <- min(distance_matrix)
  zip_id <- which.min(distance_matrix)
  
  # find closest electrcity service provider to nsb monitoring site
  elec_coords <- cbind(elec$LON, elec$LAT)
  distance_matrix <- distm(x = site_coord, y = elec_coords, fun = distHaversine)
  min_dist_elec <- min(distance_matrix)
  elec_id <- which.min(distance_matrix)
  
  data[data$site==i,'zip_pop'] <- zip[zip_id,'Population']
  data[data$site==i,'zip_code'] <- zip[zip_id,'STD_ZIP5']
  data[data$site==i,'zip_dist_sq'] <- 1/min_dist_zip^2
  
  data[data$site==i,'elec_cust'] <- elec[elec_id,'CUSTOMERS']
  data[data$site==i,'elec_summer_peak'] <- elec[elec_id,'SUMMR_PEAK']
  data[data$site==i,'elec_winter_peak'] <- elec[elec_id,'WINTR_PEAK']
  data[data$site==i,'elec_total_mwh'] <- elec[elec_id,'TOTAL_MWH']
  data[data$site==i,'elec_dist_sq'] <- 1/min_dist_elec^2
  
}

saveRDS(data, file = 'C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')
#saveRDS(data, file = 'data/combined-dataset.rds')

vapor_pressure <- function(t) {
  # if t = d2m/dew point temperature ( K ) -> actual vapor pressure
  # if t = t2m/air temperature ( K ) -> saturation vapor pressure
  
  # convert to celcius
  t <- t - 273.15 
  
  e <- 0.6113 * 10**((7.5*t)/(237.3+t)) # kiloPascals
  
  return (e) 
}

mixing_ratio <- function(e,P) {
  # e = vapor pressure ( kPa )
  # P = atmospheric pressure ( Pa )
  
  # convert Pa to kPa
  P <- P/1000
  
  r <- (622*e)/(P-e) # grams per kilogram
  
  return (r) 
}

max_cc <- function(hcc,mcc,lcc) {
  
  return (max(hcc,mcc,lcc))
  
}

#data <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')
data <- data %>% dplyr::filter(nsb>0 & nsb <23) # accurate readings above 22 are highly unlikely 

# recode missing values from electricity dataset
data <- data %>% 
  dplyr::mutate(across(c('elec_cust','elec_summer_peak','elec_winter_peak','elec_total_mwh'), ~na_if(., -999999)))

data$ea <- mapply(vapor_pressure, t=data$d2m)
data$es <- mapply(vapor_pressure, t=data$t2m)
data$rh <- (data$ea/data$es)*100 # can maybe replace using both t2m and d2m since they are highly correlated
data$r  <- mapply(mixing_ratio, e=data$ea, P=data$sp) # can maybe replace sp since sp is VERY reliant on site location
data$cc <- mapply(max_cc, hcc=data$hcc, mcc=data$mcc, lcc=data$lcc)

#data$t2m.elec_mwh <- data$t2m * data$elec_total_mwh
#data$t2m.zip_pop <- data$t2m * log(data$zip_pop)

saveRDS(data, file = 'C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')
#saveRDS(data, file = 'data/combined-dataset.rds')

#data.frame(data$nsb, data$site, dataScaled$sp.elev)
#qplot(data=data.frame(data$nsb, data$site, dataScaled$sp.elev), x=data.nsb, y=dataScaled.sp.elev, color=factor(data.site))

# transform data to make more normal
i <- 'r'

hist(data[[i]])
b <- boxcox(lm(data[[i]]~1))
lambda <- b$x[which.max(b$y)]
d[,i] <- (data[[i]] ^ lambda - 1) / lambda
hist(d[[i]])

qplot(data=d, x=nsb, y=ea, color=factor(site))


install.packages("devtools")
library(devtools)

#install moonlit library from github repo
install_github("msmielak/moonlit",force=TRUE)

#load the moonlit library
library(moonlit)

data <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')


moon_phase <- function(lat,lon,datetime,elev) {
  
  elev.m <- elev/3.28084 # convert feet to meters
  
  # get extinction coefficient estimate 
  if (elev.m<500) {
    e = 0.28
  } else if (elev.m<1000) {
    e = 0.24
  } else if (elev.m<2000) {
    e = 0.21
  } else {
    e = 0.16
  }
  
  moon <- calculateMoonlightIntensity(lat, lon, datetime, e)
  
  return(moon$moonPhase)
  
}

data$moon_phase <- mapply(moon_phase, lat=data$lat, lon=data$lon, datetime=data$Timestamp_UTC, elev=data$elev)

saveRDS(data, file = 'C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')



# more visualization shit
data <- readRDS("data/combined-dataset_no-SR.rds")

#data <- data %>% dplyr::filter(site!='SR')
#saveRDS(data, "data/combined-dataset_no-SR.rds")

sites = levels(data$site)

counts <- data.frame(data %>% dplyr::group_by(data$site) %>% summarize(count=n()))
counts %>% as_tibble()

# adding state variable just for visualization purposes
for (i in sites) {
  
  if (i=='UBD' | i=='MBD' | i=='LBD' | i=='BMCO') {
    data[data$site==i,'state'] <- 'AR'
    counts[counts$data.site==i,'state'] <- 'AR'
  } else if (i=='GMARS' | i=='TSO') {
    data[data$site==i,'state'] <- 'CA'
    counts[counts$data.site==i,'state'] <- 'CA'
  } else if (i=='Cre') {
    data[data$site==i,'state'] <- 'CO'
    counts[counts$data.site==i,'state'] <- 'CO'
  } else {
    data[data$site==i,'state'] <- 'TX'
    counts[counts$data.site==i,'state'] <- 'TX'
  }
}


data %>%
  ggplot( aes(x=site, y=nsb, fill=state)) +
  geom_boxplot()


data %>%
  ggplot( aes(x=data$site, fill=state)) +
  geom_histogram(stat='count') +
  xlab('site') + 
  ylab('observation count')



# gonna train a mars model for each individual site instead of all 13 together
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

# split datasets into each site
for (site_i in levels(data.prep$site)) {
  
  data.site <- data.prep %>% dplyr::filter(site==site_i) %>% dplyr::select(-site)
  
  # Split the training and testing data (75%, 25%) respectively
  set.seed(2025125103)
  test_index <- createDataPartition(
    y = data.site[,'nsb'],
    p = 0.25, # 25% for the test set
    list = FALSE
  )
  
  # Separate the datasets
  data.train <- data.site[-test_index,]
  data.test <- data.site[test_index,]
  
  saveRDS(data.site, paste("data/data-prep-",site_i,".rds",sep=""))
  saveRDS(data.train, paste("data/data-train-",site_i,".rds",sep=""))
  saveRDS(data.test, paste("data/data-test-",site_i,".rds",sep=""))
  
}



########################
# START: train-models.R
########################

library(caret)
library(tidyverse)
library(earth)
library(doParallel)
library(ggbiplot)
library(ggplot2)

# Import data
data <- readRDS("data/combined-dataset_no-SR.rds")

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

data.prep <- readRDS("data/data-prep_no-SR.rds")
data.train <- readRDS("data/data-train_no-SR.rds")
data.test <- readRDS("data/data-test_no-SR.rds")

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

mars.tune <- expand.grid(nprune = c(35,40,45,50,55),
                         degree = c(3,4,5))

fit.mars <- train(nsb ~ . - site,
                  data = data.train,
                  method = "earth",
                  tuneGrid = mars.tune,
                  trControl = ctrl,
                  preProcess = c("center", "scale"))
fit.mars
summary(fit.mars)
saveRDS(fit.mars, "mars-model_no-SR.rds")

###############
# Stop parallel
stopCluster(cl)
registerDoSEQ() 
###############


################
# START: plots.R
################

library(caret)
library(tidyverse)
library(earth)
library(ggbiplot)
library(ggplot2)
library(knitr)
options(digits = 4,
        scipen = 10)

data <- readRDS("data/combined-dataset_no-SR.rds")
data.prep <- readRDS("data/data-prep_no-SR.rds")
data.train <- readRDS("data/data-train_no-SR.rds")
data.test <- readRDS("data/data-test_no-SR.rds")
fit.mars <- readRDS("FINAL_mars-model_no-SR.rds")

levels(data.test$site)

#data.test$site <- fct_other(data.test$site, keep=c('SR','TSU'))


#sites = levels(data$site)
#site_i <- sites[[13]]

#data.prep <- readRDS(paste("data/data-prep-",site_i,".rds",sep=""))
#data.train <- readRDS(paste("data/data-train-",site_i,".rds",sep=""))
#data.test <- readRDS(paste("data/data-test-",site_i,".rds",sep=""))
#fit.mars <- readRDS(paste("mars-model-",site_i,".rds",sep=""))

p <- predict(fit.mars, data.test)
p_post <- ifelse(p < 1, 1, p)
caret::postResample(p_post,data.test$nsb)

pca_index <- createDataPartition(
  y = data.prep$nsb, 
  p = 0.05,
  list = FALSE
)
data.pca <-data.prep[pca_index, ]
pca <- prcomp(data.pca %>% select(-c(nsb,site)), center = TRUE, scale = TRUE)
plot.pca <- ggbiplot(pca,
                     groups = data.pca$nsb,
                     obs.scale = 0.8,
                     var.scale = 1,
                     var.axes = FALSE,
                     point.size = 1) +
  scale_color_continuous(name = "NSB") +
  theme_minimal()

ggsave(
  filename = "../doc/draft/pca_no-SR.pdf",
  plot = plot.pca,
  width = 6,
  height = 4,
  units = "in"
)

df.pred <- data.frame(Actual = data.test$nsb, Predicted = p_post) %>%
  rename(Predicted = y)
plot.pred_vs_actual <- ggplot(df.pred,
                              aes(y = Actual, x = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Values", y = "Actual Value", x = "Predicted") +
  #labs(title = paste("Predicted vs. Actual Values - ",site_i,sep=""), y = "Actual Value", x = "Predicted") +
  theme_minimal()

ggsave(
  filename = "../doc/draft/predicted-v-actual_no-SR.pdf",
  #filename = paste("../doc/draft/predicted-v-actual-",site_i,".pdf",sep=""),
  plot = plot.pred_vs_actual,
  width = 6,
  height = 4,
  units = "in"
)

residuals <- data.test$nsb - p_post
df.res <- data.frame(Predicted = p_post, Residuals = residuals) %>%
  rename(Predicted = y, Residuals = y.1)
plot.residuals <- ggplot(df.res,
                         aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", y = "Residuals (Actual - Predicted)") +
  #labs(title = paste("Residuals Plot - ",site_i,sep=""), y = "Residuals (Actual - Predicted)") +
  theme_minimal()

ggsave(
  filename = "../doc/draft/residuals_no-SR.pdf",
  #fiame = paste("../doc/draft/residuals-",site_i,".pdf",sep=""),
  plot = plot.residuals,
  width = 6,
  height = 4,
  units = "in"
)

imp <- evimp(fit.mars$finalModel)
View(imp)
df.imp <- data.frame(nsubsets = imp[,"nsubsets"],
                     gcv = imp[,"gcv"],
                     rss = imp[,"rss"])
df.imp %>% kable()

sort(coef(fit.mars$finalModel))

summary(fit.mars)
fit.mars$finalModel$prune.terms


install.packages("vip")
library(vip)

vip(fit.mars, num_features = 26, geom = "point", value = "gcv") + ggtitle("GCV")

