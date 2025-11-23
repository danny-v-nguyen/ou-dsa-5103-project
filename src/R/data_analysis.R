# Data visualizations
library(knitr)
options(digits=3)
options(scipen=99)
library(caret)
library(tidyverse)
library(doParallel)


################################# import data ##################################

sites <- list.files('C:\\R\\Project\\GaN-MN_sites\\',full.names=TRUE)
sites

#atmos <- read.csv('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\GaN_nsb-atmos_data.csv',header=TRUE,stringsAsFactors=TRUE)
atmos <- read.csv(sites[5],header=TRUE,stringsAsFactors=TRUE)

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

#atmos <- read.csv('C:\\Users\\brook\\Documents\\GitHub\\ou-dsa-5103-project\\src\\data\\GaN_nsb-atmos_data.csv',header=TRUE,stringsAsFactors=TRUE)
atmos <- read.csv(sites[11],header=TRUE,stringsAsFactors=TRUE)

# Reports for training data
trainSummary <- DQreport(atmos) # return (list(numericSummaryFinal, categoricSummaryFinal))
trainSummary[1] %>% kable()
trainSummary[2] %>% kable()
