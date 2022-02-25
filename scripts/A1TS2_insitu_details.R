# this script summarizes the insitu data collated in the 'validation - data download and collate.Rmd' script.

# authored by B. Steele

source('scripts/R_library.R')

datadir = 'data/'
figdir = 'figures/'

#load in all high-frequency insitu data for historical data analysis
insitu <- read.csv(paste0(datadir, 'insitu_temp_data_v2021-10-20.csv')) %>% 
  filter(!is.na(lat_dd)) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+5'))

## Summary Data Tables for insitu database

#number of observations for total DB
insitu %>% 
  summarize(nobs = length(datetime),
            ndays = length(unique(format(datetime, '%Y-%m-%d'))),
            year_start = min(as.numeric(format(datetime, '%Y'))),
            year_end = max(as.numeric(format(datetime, '%Y'))), 
            min_month = min(as.numeric(format(datetime, '%m'))),
            max_month = max(as.numeric(format(datetime, '%m'))),
            nsites = length(unique(location)))

#number of observations between time of 9 and 11
insitu %>% 
  mutate(hour = as.numeric(format(datetime, '%H'))) %>% 
  filter(hour >= 9 & hour < 11) %>% 
  summarize(nobs = length(datetime),
            ndays = length(unique(format(datetime, '%Y-%m-%d'))),
            year_start = min(as.numeric(format(datetime, '%Y'))),
            year_end = max(as.numeric(format(datetime, '%Y'))), 
            min_month = min(as.numeric(format(datetime, '%m'))),
            max_month = max(as.numeric(format(datetime, '%m'))),
            nsites = length(unique(location)))


#number of sites
length(unique(insitu$location))
unique(insitu$location)

#document max spread and IQR for C2_lst filtering
range_per_date <- insitu %>% 
  mutate(hour = as.numeric(format(datetime, '%H')),
         date = as.Date(datetime)) %>% 
  filter(hour >= 9 & hour < 11) %>% 
  group_by(date) %>% 
  summarize(temp_range = max(temp_degC) - min(temp_degC),
            IQR = IQR(temp_degC, na.rm = T),
            n_locs = length(unique(location)))
max_spread <- max(range_per_date$temp_range, na.rm = T)
max_IQR <- max(range_per_date$IQR, na.rm = T)

range_summary <- range_per_date %>% 
  mutate(month = as.numeric(format(date, '%m'))) %>% 
  group_by(month) %>% 
  summarise(min_range = min(temp_range, na.rm = T),
            max_range = max(temp_range, na.rm = T),
            min_IQR = min(IQR, na.rm = T),
            max_IQR = max(IQR, na.rm = T))


## Summary of location/depth/measurement
insitu %>% 
  mutate(year = format(datetime, '%Y'),
         month = as.numeric(format(datetime, '%m'))) %>% 
  group_by(year, location) %>% 
  summarise(min_depth_m = min(depth_m),
            max_depth_m = max(depth_m),
            min_month = min(month),
            max_month = max(month)) %>% 
  pivot_longer(!c(year, location), names_to = 'variable', values_to = 'value') %>% 
  pivot_wider(id_cols = c(year, variable, value),
              names_from = c(location, variable)) %>% 
  write.csv(., file.path(figdir, 'ST2_insitu_data_extent_summary.csv'), row.names = F)

