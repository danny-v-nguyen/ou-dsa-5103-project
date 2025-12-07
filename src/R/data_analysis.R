# Data visualizations
library(knitr)
options(digits=3)
options(scipen=99)
library(caret)
library(tidyverse)
library(doParallel)


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

atmos$Timestamp_UTC <- as_datetime(atmos$Timestamp_UTC, tz = "UTC")

library(VIM)  #package for "Visualization and Imputation of Missing Values"

# can use VIM's "aggr" function to also get overall information on missing
a<-aggr(atmos)
summary(a)

# use VIM function "marginplot" to get a scatter plot that includes information on missing values
marginplot(atmos[c('Timestamp_UTC','nsb')], col = c("blue", "red", "orange"))

library(lubridate)

hourly_summary_floor <- atmos %>%
  mutate(hour_start = hour(Timestamp_UTC)) %>%
  group_by(hour_start) %>%
  summarise(
    n_observations = n(),
    n_missing = sum(is.na(nsb))
  ) %>%
  ungroup()


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
elec$ID <- as.numeric(elec$ID)
zip <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\zip-data.rds')
zip$OBJECTID <- as.numeric(zip$OBJECTID)

sites <- unique(data$site)

for (i in levels(sites)) {
  
  data_i <- data[data$site==i,]
  lat_i <- first(data_i$lat)
  lon_i <- first(data_i$lon)
  
  # find closest zip code to nsb monitoring site
  find_zip <- data.frame(id = zip$OBJECTID, diff_lat = abs(zip$LAT - lat_i), diff_lon = abs(zip$LON - lon_i))
  find_zip$dist <- sqrt(find_zip$diff_lat**2 + find_zip$diff_lon**2)
  zip_id <- first(find_zip[order(find_zip$dist),])
  
  # find closest electrcity service provider to nsb monitoring site
  find_elec <- data.frame(id = elec$ID, diff_lat = abs(elec$LAT - lat_i), diff_lon = abs(elec$LON - lon_i))
  find_elec$dist <- sqrt(find_elec$diff_lat**2 + find_elec$diff_lon**2)
  elec_id <- first(find_elec[order(find_elec$dist),])
  
  data[data$site==i,'zip_pop'] <- zip[zip$OBJECTID==zip_id$id,'Population']
  data[data$site==i,'zip_code'] <- zip[zip$OBJECTID==zip_id$id,'STD_ZIP5']
  data[data$site==i,'zip_dist'] <- zip_id$dist
  
  data[data$site==i,'elec_cust'] <- elec[elec$ID==elec_id$id,'CUSTOMERS']
  data[data$site==i,'elec_summer_peak'] <- elec[elec$ID==elec_id$id,'SUMMR_PEAK']
  data[data$site==i,'elec_winter_peak'] <- elec[elec$ID==elec_id$id,'WINTR_PEAK']
  data[data$site==i,'elec_total_mwh'] <- elec[elec$ID==elec_id$id,'TOTAL_MWH']
  data[data$site==i,'elec_dist'] <- elec_id$dist
  
}


saveRDS(data, file = 'C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')

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

data <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')
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
install_github("msmielak/moonlit")

#load the moonlit library
library(moonlit)

data <- readRDS('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\combined-dataset.rds')






















