library(tidyverse)
library(knitr)

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


elec.service.DQR <- DQreport(electric.service.raw)
View(elec.service.DQR[[1]])
View(elec.service.DQR[[2]])

zip.centroids.DQR <- DQreport(zip.centroids.raw)
View(zip.centroids.DQR[[1]])
View(zip.centroids.DQR[[2]])

zip.pop.DQR <- DQreport(zip.pop.raw)
View(zip.pop.DQR[[1]])
View(zip.pop.DQR[[2]])

zip.data <- zip.centroids.raw %>%
  left_join(zip.pop.raw, by = c("STD_ZIP5" = "ZIP_Code"))
saveRDS(zip.data, file = "data/zip-data.rds")
