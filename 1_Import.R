## Script to create a Toxics data dashboard
## data from https://aqs.epa.gov/aqsweb/documents/data_api.html
## Created May 2021
## Created by Mike McCarthy, Radical Research LLC

rm(list = ls())

library(readxl) #used to import Excel Files
library(tidyverse) #used for data processing and display
library(janitor) #used for clean_names() function 
library(lubridate) #used to make date processing easier
library(data.table) #used for fread and column assignments
library(stringr) #used for string parsing
#library(scales) #used to make pretty date-axis labels
#library(htmltools) #used to make hovering popups on map
#library(DT) #used for interactive tables
#library(leaflet) #used to make map


wd <- getwd()
rawData_dir <- paste0(wd, '/raw_data')


#Processing steps to clean up data directory
#Some file.remote steps are commented out because they are only required one time.
filesCSV <- list.files(rawData_dir)
setwd(rawData_dir)

#create empty data frames for import
mergedFilesSample <- data.frame()
mergedFilesAnnual <- data.frame()

##Import loop to import all excel files and different worksheets for all sites and years
for (i in 1:length(filesCSV)){
  print(i)
  if(str_detect(filesCSV[i], 'annual')){
    #tempDataAnnual <- fread(filesCSV[i], colClasses = list(character = 1:4,53)) %>%
     # clean_names() 

    #mergedFilesAnnual <- rbind(mergedFilesAnnual, tempDataAnnual) %>% distinct()
  } else if(str_detect(filesCSV[i], 'sample')) {
    tempDataSample <- fread(filesCSV[i], colClasses = list(character = 1:4,16,18,25,28)) %>%
      clean_names() 
    if(nrow(tempDataSample) != 0) #deals with a single beryllium file that has no rows
    {
    mergedFilesSample <- rbind(mergedFilesSample, tempDataSample) %>% distinct()
    }
  }
  else {}
}
names(tempDataSample)
names(mergedFilesSample)
filesCSV[i]

#check <- fread(filesCSV[i], colClasses = list(character = 1:4,16,18,25))

#cleaned_annual_file <- mergedFilesAnnual %>%
#  mutate(aqs_sitecode = paste0(state_code,county_code, site_number)) %>%
#  select(aqs_sitecode, poc, parameter_code, parameter, sample_duration, method, 
#    year, units_of_measure, event_type, validity_indicator, valid_day_count,
#    required_day_count, ninety_fifth_percentile, fiftieth_percentile, arithmetic_mean,
#    standard_deviation) %>%
#  filter(event_type %in% c('Events Inclucded', 'No Events')) %>%
#  distinct()

#event_counts_by_year <- cleaned_annual_file %>%
#  select(parameter_code, event_type, year) %>%
#  group_by(parameter_code, event_type, year) %>%
#  summarize(count = n(), .groups = 'drop') %>%
#  spread(event_type, count)

#required_day_counts <- KeepEvents_only %>%
#  group_by(parameter, sample_duration, required_day_count) %>%
#  summarize(count = n(),.groups = 'drop') %>%
#  filter(sample_duration != '24 HOUR')

setwd(wd)

cleaned_sample_file <- mergedFilesSample %>%
  mutate(aqs_sitecode = paste0(state_code,county_code, site_number)) %>%
  select(aqs_sitecode, poc, parameter_code, parameter, date_local, time_local, 
    sample_duration, method_code, units_of_measure, detection_limit, 
    sample_frequency, method_type, sample_measurement) %>%
  distinct() %>%
  filter(!is.na(sample_measurement)) #%>%
  
rm(mergedFilesAnnual, mergedFilesSample)

unique(cleaned_annual_file$pollutant_standard)
unique(cleaned_annual_file$metric_used)
unique(cleaned_annual_file$event_type)
unique(cleaned_annual_file$null_observation_count)
unique(cleaned_sample_file$aqs_sitecode)
unique(cleaned_sample_file$sample_frequency)
unique(cleaned_sample_file$sample_duration)

#Chronic_risk_OAQPS
url <- 'https://www.epa.gov/sites/production/files/2018-06/chronicfinaloutput_6_18_2018_10-09-26_am_1.xlsx' 
tmp <- tempfile() 
curl::curl_download(url, tmp)
OAQPS_risk <- readxl::read_excel(tmp, skip = 4) %>%
  clean_names()

# AQS parameter list
aqs_parameter_list <- fread('https://aqs.epa.gov/aqsweb/documents/codetables/parameters.csv', 
  colClasses = list(character = 1)) %>% 
  clean_names()

# get sites
url2 <- 'https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip'
tmp <- tempfile()
curl::curl_download(url2, tmp)
aqs_sites <- fread(unzip(tmp), colClasses = list(character = 1:3)) %>%
  clean_names() %>%
  mutate(aqs_sitecode = paste0(state_code, county_code, site_number))

