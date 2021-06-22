## Script to create a Toxics data dashboard
## data from https://aqs.epa.gov/aqsweb/documents/data_api.html
## Created May 2021
## Created by Mike McCarthy, Radical Research LLC
## Run after 1_Import.R

# separate sample data into subdaily and daily duration samples by duration code
# Aggregate hourly and 3-hr data into 24-hr averages - check if any 4, 5, 8, 12 hour are worth dealing with

clean_24hr <- cleaned_sample_file %>%
  filter(sample_duration == '24 HOUR') %>%
  mutate(nBelowMDL = ifelse(sample_measurement < detection_limit, 1, 0),
    zeroSubbed = ifelse(sample_measurement <= 0, detection_limit/2, sample_measurement),
    count = 1) %>%
  rename(avg = sample_measurement, avgMDL = detection_limit)
  
clean_1hr <- cleaned_sample_file %>%
  filter(sample_duration == '1 HOUR')

clean_3hr <- cleaned_sample_file %>%
  filter(sample_duration == '3 HOURS')

#4, 5, and 6 hour sample counts are <300 samples each
#clean_4hr <- cleaned_sample_file %>%
#  filter(sample_duration == '4 HOUR')
#clean_5hr <- cleaned_sample_file %>%
#  filter(sample_duration == '5 HOUR')
#clean_6hr <- cleaned_sample_file %>%
#  filter(sample_duration == '6 HOUR')
clean_8hr <- cleaned_sample_file %>%
  filter(sample_duration == '8 HOUR')
clean_12hr <- cleaned_sample_file %>%
  filter(sample_duration == '12 HOUR')
clean_15min <- cleaned_sample_file %>%
  filter(sample_duration == '15 MINUTE')
compositeUNKNOWN <- cleaned_sample_file %>%
  filter(sample_duration == 'COMPOSITE DATA')

##Aggregate to daily values by parameter, poc, duration, sitecode, date, method, units

daily_1hr <- clean_1hr %>%
  mutate(belowMDL = ifelse(sample_measurement < detection_limit, 1, 0),) %>%
  mutate(zeroSubbed = ifelse(sample_measurement <= 0, detection_limit/2, sample_measurement)) %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, date_local, sample_duration, method_code,
           units_of_measure) %>%
  summarize(avg = mean(zeroSubbed), avgMDL = mean(detection_limit, na.rm = T),
            count = n(), nBelowMDL = sum(belowMDL), .groups = 'drop') %>%
  filter(count > 17)

daily_3hr <- clean_3hr %>%
  mutate(belowMDL = ifelse(sample_measurement < detection_limit, 1, 0),) %>%
  mutate(zeroSubbed = ifelse(sample_measurement <= 0, detection_limit/2, sample_measurement)) %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, date_local, sample_duration, method_code,
           units_of_measure) %>%
  summarize(avg = mean(zeroSubbed), avgMDL = mean(detection_limit, na.rm = T),
            count = n(), nBelowMDL = sum(belowMDL), .groups = 'drop') %>%
  filter(count > 5)

daily_12hr <- clean_12hr %>%
  mutate(belowMDL = ifelse(sample_measurement < detection_limit, 1, 0),) %>%
  mutate(zeroSubbed = ifelse(sample_measurement <= 0, detection_limit/2, sample_measurement)) %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, date_local, sample_duration, method_code,
           units_of_measure) %>%
  summarize(avg = mean(zeroSubbed), avgMDL = mean(detection_limit, na.rm = T),
            count = n(), nBelowMDL = sum(belowMDL), .groups = 'drop') %>%
  filter(count == 2)

daily_8hr <- clean_8hr %>%
  mutate(belowMDL = ifelse(sample_measurement < detection_limit, 1, 0),) %>%
  mutate(zeroSubbed = ifelse(sample_measurement <= 0, detection_limit/2, sample_measurement)) %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, date_local, sample_duration, method_code,
           units_of_measure) %>%
  summarize(avg = mean(zeroSubbed), avgMDL = mean(detection_limit, na.rm = T),
            count = n(), nBelowMDL = sum(belowMDL), .groups = 'drop') %>%
  filter(count == 3)

daily_15min <- clean_15min %>%
  mutate(belowMDL = ifelse(sample_measurement < detection_limit, 1, 0),) %>%
  mutate(zeroSubbed = ifelse(sample_measurement <= 0, detection_limit/2, sample_measurement)) %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, date_local, sample_duration, method_code,
           units_of_measure) %>%
  summarize(avg = mean(zeroSubbed), avgMDL = mean(detection_limit, na.rm = T),
            count = n(), nBelowMDL = sum(belowMDL), .groups = 'drop') %>%
  filter(count > 71)

names(daily_1hr)

##put all the daily averages in a single data frame - harmonize names

daily_subdaily <- rbind(daily_1hr, daily_3hr, daily_8hr, daily_12hr, daily_15min)
names(daily_subdaily)
names(clean_24hr)
daily_24hr <- clean_24hr %>%
  select(aqs_sitecode, parameter_code, parameter, poc, date_local, sample_duration, 
    method_code, units_of_measure, zeroSubbed, avgMDL, count, nBelowMDL) %>%
    rename(avg = zeroSubbed)

daily_all <- rbind(daily_subdaily, daily_24hr)
rm(clean_1hr, clean_24hr, clean_3hr, clean_8hr, cleaned_sample_file, daily_24hr,
   daily_1hr, daily_3hr, daily_subdaily)

## Convert units to ug/m3
unique(daily_all$units_of_measure)
#identify species with ppbC units
ppbCspecies <- daily_all %>%
  filter(units_of_measure == 'Parts per billion Carbon') %>%
  select(parameter, parameter_code) %>%
  distinct()

#write.csv(ppbCspecies, 'ppbCSpecies.csv')
getwd()
meta_wd <- paste0(wd,"/metadata")

ppbC_converter <- fread(paste0(meta_wd, '/ppbCSpecies_coreHAPs.csv'), colClasses = list(character = 1:3)) %>%
  select(parameter,parameter_code, MW, carbonCount)

daily_tidy <- daily_all %>% 
  mutate(belowMDLday = ifelse(avg < avgMDL, 1, 0)) %>%
  left_join(ppbC_converter) %>%
  mutate(concentration = case_when(
    units_of_measure == 'Micrograms/cubic meter (25 C)' ~ avg,
    units_of_measure == 'Micrograms/cubic meter (LC)' ~ avg,
    units_of_measure == 'Nanograms/cubic meter (25 C)' ~ avg/1000,
    units_of_measure == 'Parts per billion Carbon' ~ avg*MW/(carbonCount*24.46)
  )) %>%
  select(-MW, -carbonCount) %>%
  mutate(yr_qtr = lubridate::quarter(date_local, with_year = TRUE),
         yr = lubridate::year(date_local)) %>%
  rename(raw_units = units_of_measure)


ethylbenzene_check <- daily_all %>%
  #filter(parameter_code == '45203') %>%
  mutate(yr = year(date_local)) %>%
  group_by(parameter, yr) %>%
  summarize(count = n())

rm(daily_all)

Quarter_tidy <- daily_tidy %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, method_code, yr_qtr, yr) %>%
  summarize(qtr_avg = mean(concentration), qtr_MDL = mean(avgMDL), countQtr = n(),
    countBelowMDL = sum(belowMDLday), .groups = 'drop') %>%
  mutate(pctBelowMDL = 100*countBelowMDL/countQtr, complete = ifelse(countQtr >= 6, 1, 0)) 

ethylbenzene_check2 <- Quarter_tidy %>%
  #filter(parameter_code == '45203') %>%
  group_by(parameter, yr_qtr) %>%
  summarize(count = n(), .groups = 'drop')

##create a couple annual tables

Annual_poc_method <- Quarter_tidy %>%
  filter(complete == 1) %>%
  group_by(aqs_sitecode, parameter_code, parameter, poc, method_code, yr) %>%
  summarize(yr_avg = mean(qtr_avg), yr_MDL = mean(qtr_MDL), countYr = sum(countQtr),
            yrCountBelowMDL = sum(countBelowMDL), yrCount = sum(countQtr), 
            countQtr = n(), .groups = 'drop') %>%
  mutate(pctBelowMDL = 100*yrCountBelowMDL/yrCount, yrComplete = ifelse(countQtr > 3, 1, 0)) %>%
  filter(yrComplete == 1)

Annual_method <- Quarter_tidy %>%
  filter(complete == 1) %>%
  group_by(aqs_sitecode, parameter_code, parameter, method_code, yr, yr_qtr) %>%
  summarize(qtr_avg = mean(qtr_avg), qtr_MDL = mean(qtr_MDL), countQtr = sum(countQtr),
    countBelowMDL = sum(countBelowMDL), .groups='drop') %>%
  group_by(aqs_sitecode, parameter_code, parameter, method_code, yr) %>%
  summarize(yr_avg = mean(qtr_avg), yr_MDL = mean(qtr_MDL), countYr = sum(countQtr),
            yrCountBelowMDL = sum(countBelowMDL), yrCount = sum(countQtr), 
            countQtr = n(), .groups = 'drop') %>%
  mutate(pctBelowMDL = 100*yrCountBelowMDL/yrCount, yrComplete = ifelse(countQtr > 3, 1, 0)) %>%
  filter(yrComplete == 1)

## tidy cancer risk values and noncancer hazard values
# first select parameters of interest

parameter_list <- Annual_method %>%
  select(parameter, parameter_code) %>%
  distinct() %>%
  left_join(aqs_parameter_list) %>%
  filter(still_valid == 'YES') %>%
  select(parameter, parameter_code, cas_number, standard_units) %>%
  arrange(parameter)

parameterRiskList <- parameter_list %>%
  left_join(OAQPS_risk, by = c('cas_number' = 'cas_no')) %>%
  select(1:4, mg_m3, x1_mg_m3) %>%
  rename(hazard_mg = mg_m3, cancer_URE = x1_mg_m3) %>%
  mutate(hazard_ug = hazard_mg*1000) %>%
  select(-hazard_mg) %>%
  #Fix chromium - use a 1.5% Cr VI assumption
  mutate(cancer_URE = ifelse(parameter_code %in% c('88112', '82112', '12112'), 0.00018, cancer_URE),
    hazard_ug = ifelse(parameter_code %in% c('88112', '82112', '12112'), 0.0015, hazard_ug)) %>%
  #Fix 1,3-dichloropropene isomers
  mutate(cancer_URE = ifelse(parameter_code %in% c('43830', '43831'), 0.000004, cancer_URE),
    hazard_ug = ifelse(parameter_code %in% c('43830', '43831'), 20, hazard_ug)) %>%
  mutate(carcinogen = ifelse(cancer_URE == 0, 0, 1),
    hazard = ifelse(is.na(hazard_ug), 0, 1))

carcinogens <- parameterRiskList %>%
  filter(carcinogen == 1) %>%
  select(parameter, cancer_URE) 

## Next, risk-weight the annual method values

annual_risk <- Annual_method %>%
  left_join(parameterRiskList) %>%
  mutate(cancerRisk = as.numeric(ifelse(carcinogen == '0', NA , 1000000*yr_avg*cancer_URE)),
         hazard = as.numeric(ifelse(hazard_ug == 0, NA , yr_avg/hazard_ug)))

# Create site list

site_list <- annual_risk %>%
  select(aqs_sitecode) %>%
  distinct() %>%
  left_join(aqs_sites) %>%
  select(aqs_sitecode, latitude, longitude, elevation, land_use, location_setting, local_site_name)

trend_count <- annual_risk %>%
  select(aqs_sitecode, parameter, method_code, yr) %>%
  group_by(aqs_sitecode, parameter, method_code) %>%
  summarize(count = n(), minYr = min(yr), maxYr = max(yr), .groups = 'drop') %>%
  filter(count > 4) %>%
  filter((maxYr - minYr)>4) %>%
  mutate(trendLength = maxYr - minYr)

trend_data <- trend_count %>%
  left_join(annual_risk) %>%
  mutate(MDL_type = factor(case_when(
    pctBelowMDL < 50 ~ '<50% below MDL',
    pctBelowMDL < 75 ~ '50-75% below MDL',
    pctBelowMDL < 90 ~ '75-90% below MDL',
    pctBelowMDL >= 90 ~'>90% below MDL'
  ), levels = c('<50% below MDL','50-75% below MDL','75-90% below MDL', '>90% below MDL'))
  ) %>%
  filter(trendLength >= 5 & count >= 5)

trend_sites <- trend_count %>%
  select(aqs_sitecode) %>%
  group_by(aqs_sitecode) %>%
  summarize(trendCounts = n()) 

risk_sites <- annual_risk %>%
  select(aqs_sitecode, parameter, yr, cancerRisk) %>%
  filter(parameter %in% c('Formaldehyde', '1,3-Butadiene', 'Acetaldehyde', 'Benzene', 'Carbon tetrachloride', 
  'Arsenic PM2.5 LC', 'Arsenic PM10 STP', 'Ethylbenzene', 'Chromium VI (TSP) LC)', 'Chromium VI (TSP) STP',
  'Chromium PM2.5 LC', 'Chromium PM10 STP')) %>%
  #filter(yr >= 2018) %>%
  group_by(aqs_sitecode, parameter) %>%
  summarize(avgRisk = mean(cancerRisk), count = n(), .groups = 'drop') %>%
  group_by(aqs_sitecode) %>%
  summarize(countParams = n(), sumRisk = sum(avgRisk), .groups = 'drop') %>%
  filter(countParams >= 5)

site_list2 <- site_list %>%
  left_join(trend_sites) %>%
  left_join(risk_sites) %>%
  mutate(category = case_when(
    is.na(trendCounts) & !is.na(countParams) ~ 'Risk species site',
    is.na(countParams) & !is.na(trendCounts) ~ '5+ year trend site',
    is.na(countParams) & is.na(trendCounts) ~ 'Other',
    TRUE ~ 'Trend and Risk site')
    ) %>%
  mutate(category = factor(category, levels = c('Trend and Risk site',
    '5+ year trend site', 'Risk species site', 'Other')))

YrList <- annual_risk %>%
  select(yr) %>%
  distinct()

rm(daily_tidy, Annual_method, Annual_poc_method, clean_15min, Quarter_tidy,
   aqs_sites, compositeUNKNOWN)

annualRiskStats <- annual_risk %>%
  filter(carcinogen == 1) %>%
  mutate(above1 = ifelse(cancerRisk >= 1, 1, 0),
    above100 = ifelse(cancerRisk >= 100, 1, 0)) %>%
  group_by(parameter, yr) %>%
  summarize(countSites = n(), avgRisk = mean(cancerRisk, na.rm = T),
    countAbove1 = sum(above1), countAbove100 = sum(above100),
    maxRisk = max(cancerRisk), medRisk = median(cancerRisk, na.rm = T),
    avgPctBelowMDL = mean(pctBelowMDL), .groups = 'drop') %>%
  mutate(MDL.Category = as.factor(case_when(
    avgPctBelowMDL < 50 ~ '<50% below MDL on avg',
    avgPctBelowMDL < 75 ~ 'Between 50 and 75% below MDL on avg',
    avgPctBelowMDL < 90 ~ 'Between 75 and 90% below MDL on avg',
    avgPctBelowMDL <= 100 ~'More than 90% below MDL on avg'
  )))

app_dir <- paste0(wd, '/ToxicsDashboard')
setwd(app_dir)
save.image()

