# script to download,filter, and collate insitu data for Herrick and Steele et al MS

#R version: 4.1.0 RStudio Version: 1.4.1707

# updated 26Mar2019 to include 2018 buoy thermistor data (no upper do data in 2018) 
# updated 05July2019 to include 17-18 winter data, 18-19 winter data, oct 18-may 19 data
# updated 26March2021 to pull from EDI and to filter for upper value at all locations 
# updated 20April2021 to pull LSPA LMP from GitHub
# updated 17May2021 to remove monthly sampling (LSPA LMP)
# updated 24March2022 with updated LSPA buoy data

#source libraries and functions
source('scripts/R_library.R')

dumpdir = '~/GitHub/ids-ne-lakes/data/'
figdir = '~/GitHub/ids-ne-lakes/figures/'

# Read in files from EDI and filter for desired data ####

## Winter Hobo files at the buoy site ----

# Package ID: edi.500.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: High-frequency under-ice water temperature and dissolved oxygen at Lake Sunapee, New Hampshire, USA, 2014-2019.
# Data set creator:   LSPA - Lake Sunapee Protective Association 
# Data set creator:  Kathleen Weathers - Cary Institute of Ecosystem Studies 
# Data set creator:  Bethel Steele - Cary Institute of Ecosystem Studies 
# Contact:   LSPA -  Lake Sunapee Protective Association  - lspa@lakesunapee.org
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

winterhobo <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/500/2/a64a1a7af863d8310daa9b8859060bc7"
                      ,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "datetime",     
                 "do_ppm",     
                 "TempC_1p5m",     
                 "TempC_2p5m",     
                 "TempC_3p5m",     
                 "TempC_4p5m",     
                 "TempC_5p5m",     
                 "TempC_6p5m",     
                 "TempC_7p5m",     
                 "TempC_8p5m",     
                 "TempC_9p5m",     
                 "TempC_10p5m",     
                 "location"    ), check.names=TRUE)


winterhobo_loc <-read.csv( "https://pasta.lternet.edu/package/data/eml/edi/500/2/9cd342fb70af70d79bc3a258aedeed26" ,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "location",     
                 "lat_dd",     
                 "long_dd"    ), check.names=TRUE)


## Sunapee Buoy data ----

# Package ID: edi.499.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Sunapee Instrumented Buoy: High Frequency Water Temperature and Dissolved Oxygen Data - 2007-2021
# Data set creator:   LSPA - Lake Sunapee Protective Association 
# Data set creator:  Bethel Steele - Cary Institute of Ecosystem Studies 
# Data set creator:  Kathleen Weathers - Cary Institute of Ecosystem Studies 
# Contact:   Bethel Steele steeleb@caryinstitute.org

buoy2007do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/d7678325ec3a430959b52c8a933c810a') 
buoy2007<- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/8aa419258c486d5533fc016c7385afdd')
buoy2008do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/212faee8c415ed9dd76941e1377b20e6')
buoy2008 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/107e661231ad954a2f9ce746ed934971')
buoy2009do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/0e6f5952d384e6a2543105cb76d125fe')
buoy2009 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/d9c1a224897c64fb505cc4ad9ff7d08d')
buoy2010do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/cfb900f44c5d36309b0b56e02711e293')
buoy2010 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/64780f1f23d94cbe2e05f9d1e2e255d1') 
buoy2011do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/60d5e40e996fc2108e24be4ad6d69c7e') 
buoy2011 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/26fed3d0522dab567be961851f21dcb9') 
buoy2012do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/b7d876e7473633966294314914fe388a') 
buoy2012 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/ea5fcc3b97f9f749f88209ca0303fb2d') 
buoy2013do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/7f95b231683aeeadc78833d8c4d66306')
buoy2013 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/a34bcb5569a2423ab612c87548f9a4a4')
buoy2014do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/4ac2414fd93a86def62805146ac2ed4e')
buoy2014 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/171b3fd9023d04a19da1ae15e315f5cc')
buoy2015do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/74e012a632de8e0db298bb2b4018297e') 
buoy2015hobo <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/1b29196704e1cc767317aeb5197eec15')
buoy2015 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/1d57e128cd609b79270521707cc7d54d') 
buoy2016do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/fcaad5eb5d0b16aeb1c93ebd7831a0a5')
buoy2016 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/4d6fd420e205197d5fca25cb53258bd7')
buoy2017do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/9e96ecffa80956309c38368a6f5fbf12')
buoy2017 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/e92780d73355a9aecb4c98ff02ee3d3f') 
buoy2018do <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/8fe7c6d1335a4010b05fe5c6fcb3066c') 
buoy2018hobo<- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/189b9375726f0f48b7ffc54f464b6b21') 
buoy2018  <-  read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/d6c8f8251e4f5873da2a3f69ad2d2d62') 
buoy2019do <-  read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/023fa990bf6fc6714268913a2c06ba39') 
buoy2019 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/66da82b133c13591015006149d0a4576') 
buoy2020do  <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/e8af5fb9f62c635a64470146cbe714c1') 
buoy2020 <-  read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/3ef7eaaea1fe6cb9ccbb9b7de173e790') 
buoy2021do<- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/59a07057a549ddcd3a2a4dc00f89f136') 
buoy2021exo <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/640fd64c7e6d67495ef4c72f771e6907') 
buoy2021 <- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/e69f75756b69ae1b7a6d20f7c4370671') 
buoylocation<- read.csv('https://pasta.lternet.edu/package/data/eml/edi/499/3/8ce0dbc0ba51cc2f3e8f9fd891486d57') 


## gloeo trap sensors ----

# Package ID: edi.498.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: High-frequency temperature data from four near-shore sites, Lake Sunapee, NH, USA, 2006-2018.
# Data set creator:  Kathryn Cottingham - Dartmouth College 
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Kathleen Weathers - Cary Institute of Ecosystem Studies 
# Contact:  Kathryn Cottingham -  Dartmouth College  - kathryn.l.cottingham@dartmouth.edu

 gloeo_depths <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/498/1/f707d161d96899983f0f82845849427e",header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "year",     
                    "site",     
                    "approx_depth_m"    ), check.names=TRUE)
               


gloeo_locs <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/498/1/074c7dcfa74cdafbc71ccee9c4b97293" ,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "site",     
                    "lat_dd",     
                    "long_dd"    ), check.names=TRUE)
               
gloeo_temp <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/498/1/b4f60789ceb87db613924ca43a2f71ed",header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "year",     
                    "dayofyr",     
                    "time",     
                    "datetime",     
                    "site",     
                    "temp_degC"    ), check.names=TRUE)
               


## 2018 mini buoys ----

# Package ID: edi.395.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Underwater temperature, light, and dissolved oxygen data from 3 mini-buoys in Lake Sunapee, NH, USA from June to October 2018.
# Data set creator:  Nicole Ward - Virginia Tech 
# Data set creator:  Jennifer Brentrup - Dartmouth College 
# Data set creator:  Ava Johnson - Virginia Tech 
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Kathleen Weathers - Cary Institute of Ecosystem Studies 
# Data set creator:  June Fichter - Lake Sunapee Protective Association 
# Contact:  Nicole Ward -  Virginia Tech  - nkward@vt.edu

 GM_temp <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/395/2/551bad10969f758453be2d3e1f540fac",header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "dateTime",     
                    "temp",     
                    "Depth",     
                    "SerialNum"    ), check.names=TRUE)
               

 HC_temp <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/395/2/fa1a9ddd31509aa2c5a559e20b75e015",header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "dateTime",     
                    "temp",     
                    "Depth",     
                    "SerialNum"    ), check.names=TRUE)
               

 SB_temp <-read.csv("https://pasta.lternet.edu/package/data/eml/edi/395/2/f07dc3a8ecb04f61747a82e7a1da5846" ,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "dateTime",     
                    "temp",     
                    "Depth",     
                    "SerialNum"    ), check.names=TRUE)
               
 

# Harmonize, Filter, Collate data ####

## winter hobo data ----

winterhobo_filter <- winterhobo %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  select(datetime, location, TempC_1p5m) %>% 
  rename(temp_degC = TempC_1p5m) %>% 
  mutate(depth_m = 1.5,
         source = 'edi.500.2') %>% 
  left_join(., winterhobo_loc) %>% 
  filter(!is.na(temp_degC))

## buoy data ----

buoytemp <- full_join(buoy2007, buoy2007do) %>% 
        full_join(., buoy2008)  %>% 
        full_join(., buoy2008do)  %>% 
        full_join(., buoy2009)  %>% 
        full_join(., buoy2009do)  %>% 
        full_join(., buoy2010)  %>% 
        full_join(., buoy2010do)  %>% 
        full_join(., buoy2011)  %>% 
        full_join(., buoy2011do)  %>% 
        full_join(., buoy2012)  %>% 
        full_join(., buoy2012do)  %>% 
        full_join(., buoy2013)  %>% 
        full_join(., buoy2013do)  %>% 
        full_join(., buoy2014)  %>% 
        full_join(., buoy2014do)  %>% 
        full_join(., buoy2015)  %>% 
        full_join(., buoy2015do)  %>% 
        full_join(., buoy2015hobo) %>% 
        full_join(., buoy2016)  %>% 
        full_join(., buoy2016do)  %>% 
        full_join(., buoy2017)  %>% 
        full_join(., buoy2017do)  %>% 
        full_join(., buoy2018)  %>% 
        full_join(., buoy2018do)  %>% 
        full_join(., buoy2018hobo) %>% 
        full_join(., buoy2019)  %>% 
        full_join(., buoy2019do)  %>% 
        full_join(., buoy2020)  %>% 
        full_join(., buoy2020do)  %>% 
        full_join(., buoy2021)  %>% 
        full_join(., buoy2021do) %>% 
        full_join(., buoy2021exo)

#select only temp <= 1.5m
buoytemp_filter <- buoytemp %>% 
        select(datetime, location, 
               waterTemperature_degC_0p1m, waterTemperature_degC_0p25m, 
               waterTemperature_degC_0p5m, waterTemperature_degC_0p75m,
                waterTemperature_degC_0p85m, waterTemperature_degC_1m, 
               waterTemperature_degC_1p5m, 
               flag_alltemp, flag_temp0p5m, flag_temp0p75m, flag_temp0p85m)


# look at flags
#flag definitions:
# n noise in data stream
# s	suspect readings
# i	intermittent readings
# d	depth likely incorrect/sensor hung up
# a	sensor reporting on 20min intervals *this is irrelevant to this purpose
# b	sensor may be in sediment *11m and 13m already removed, can ignore

unique(buoytemp_filter$flag_alltemp)
unique(buoytemp_filter$flag_temp0p5m) 
unique(buoytemp_filter$flag_temp0p75m)
unique(buoytemp_filter$flag_temp0p85m)
# remove suspect/x data/intermittent recordings and drop flags
buoytemp_filter <- buoytemp_filter %>% 
  filter(!grepl('i', flag_alltemp) &
           !grepl('i', flag_temp0p5m) &
           !grepl('i', flag_temp0p75m) &
           !grepl('i', flag_temp0p85m)) %>% 
  select(-c(flag_alltemp, flag_temp0p5m, flag_temp0p75m, flag_temp0p85m))


#filter buoy temp string
buoytemp_filter <- buoytemp_filter %>% 
  select(datetime, waterTemperature_degC_0p1m:waterTemperature_degC_1p5m, location) %>% #only select 1.5 or closer to surface
  pivot_longer(cols = c(waterTemperature_degC_0p1m:waterTemperature_degC_1p5m)) %>% 
  mutate(depth_m = case_when(grepl('0p1m', name) ~ 0.1,
                             grepl('0p25m', name) ~ 0.25,
                             grepl('0p5m', name) ~ 0.5,
                           grepl('0p75m', name) ~ 0.75,
                           grepl('0p85m', name) ~ 0.85,
                           grepl('1m', name) ~ 1.0,
                           grepl('1p5m', name) ~ 1.5,
                           TRUE ~ NA_real_)) %>% 
  filter(!is.na(value))

buoytemp_filter <- buoytemp_filter %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  rename(temp_degC = value) %>% 
  select(-name)

head(buoytemp_filter)
  
### filter do string ----

buoydo_filter <- buoytemp %>% 
  select(datetime, location,
         waterTemperature_EXO_degC_1m, waterTemperature_DO_degC_0p25m, waterTemperature_DO_degC_0p75m, waterTemperature_DO_degC_1p5m,
         flag_exodo, flag_do1p5m, flag_do0p25m)

#look at flags
#flag definitions
# d	depth likely incorrect/sensor hung up
# w	sensor cleaned, not calibrated
# c	sensor calibrated
# m	 miscalibrated - not to be used without correction
# p	cleaning/calibration not documented - presumed from data
# i	intermittent readings
# x	do sensor likely uncalibrated, no documentation of calibration
# o do subject to offset

unique(buoydo_filter$flag_exodo)
unique(buoydo_filter$flag_do1p5m)
unique(buoydo_filter$flag_do0p25m)

# flags that might impact temp from this sensor is 'i' and 's'
buoydo_filter <- buoydo_filter %>% 
  mutate(waterTemperature_DO_degC_1p5m = case_when(grepl('i', flag_do1p5m) ~ NA_real_,
                               grepl('s', flag_do1p5m) ~ NA_real_,
                               TRUE ~ waterTemperature_DO_degC_1p5m)) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  select(-c(flag_do1p5m, flag_exodo, flag_do0p25m))

buoydo_filter <- buoydo_filter %>% 
  pivot_longer(names_to = 'parameter',
               values_to = 'temp_degC',
               -c(datetime, location)) %>% 
  filter(!is.na(temp_degC))


buoydo_filter <- buoydo_filter %>% 
  mutate(depth_m = case_when(grepl('1p5', parameter) ~ 1.5,
                             grepl('0p25', parameter) ~ 0.25,
                             grepl('0p75', parameter) ~ 0.75,
                             grepl('1m', parameter) ~ 1.0,
                             TRUE ~ NA_real_)) %>% 
  select(-parameter)

#join buoy data together
buoy_filter <- full_join(buoytemp_filter, buoydo_filter) %>% 
  mutate(source = 'edi.499.3')

#add location information
buoy_filter <- buoy_filter %>% 
  left_join(., buoylocation) 

#remove data 'in-transit'
buoy_filter <- buoy_filter %>% 
  filter(location!='in transit')

#filter for topmost temp reading per timestamp
buoy_filter <- buoy_filter %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC')) %>% 
  arrange(datetime, depth_m) %>% 
  group_by(datetime, location, source, lat_dd, long_dd) %>% 
  summarize(temp_degC = first(temp_degC),
            depth_m = first(depth_m)) %>% 
  ungroup()


## gloeo trap sensors ----

# DST is observed in this dataset. See if any measurements taken over period of DST

#look for datetimes outside of dst
gloeo_temp %>% 
  group_by(year) %>% 
  arrange(datetime) %>% 
  summarise(firstdatetime = first(datetime),
            lastdatetime = last(datetime))

#There is no overlap with DST, just need to adjust time to no DST observed for integration with all other datasets

gloeo_temp_dstadj <- gloeo_temp %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  mutate(datetime_adj = datetime - lubridate::hours(1))

# Now filter with new datetime as datetime no dst observed

#remove unnecessary columns, rename adjusted time
gloeo_temp_dstadj <- gloeo_temp_dstadj %>% 
  select(-dayofyr, -time, -datetime) %>% 
  rename(datetime = datetime_adj)

#join with depth data
gloeo_temp_dstadj <- full_join(gloeo_temp_dstadj, gloeo_depths)

#where depth is na, fill in with 1.5 so data is included prior to 2011
gloeo_temp_dstadj <- gloeo_temp_dstadj %>% 
  mutate(depth_m = case_when(is.na(approx_depth_m) ~ 1.5,
                             TRUE ~ approx_depth_m)) %>% 
  select(-year, -approx_depth_m)

#join with loc data
gloeo_temp_dstadj <- full_join(gloeo_temp_dstadj, gloeo_locs) %>% 
        filter(depth_m <= 1.5)

#add source info, rename site to location
gloeo_temp_dstadj <- gloeo_temp_dstadj %>% 
  mutate(source = 'edi.498.1') %>% 
  rename(location = site)

gloeo_temp_dstadj


## mini buoy data ----

#add location info
GM_temp <- GM_temp %>% 
  mutate(location = 'GeorgesMills',
         lat_dd = 43.4288,
         long_dd = -72.0675)

HC_temp <- HC_temp %>% 
  mutate(location = 'HerrickCove',
         lat_dd = 43.4096,
         long_dd = -72.033)

SB_temp <- SB_temp %>% 
  mutate(location = 'StateBeach',
         lat_dd = 43.3436,
         long_dd = -72.0595)

#join and filter for topmost sensor
minibuoy_temp_filter <- full_join(GM_temp, HC_temp) %>% 
  full_join(SB_temp) %>% 
  rename(datetime = dateTime, 
         depth_m = Depth) %>% 
  filter(!is.na(temp)) %>% 
  arrange(datetime, depth_m) %>% 
  group_by(datetime, location, lat_dd, long_dd) %>% 
  summarize(temp_degC = first(temp),
            depth_m = first(depth_m)) %>% 
  ungroup()

#add source info
minibuoy_temp_filter <- minibuoy_temp_filter %>% 
  mutate(source = 'edi.395.2',
         datetime = as.POSIXct(datetime, 'UTC'))

# round to 2 digits after decimal
minibuoy_temp_filter <- minibuoy_temp_filter %>% 
  mutate(temp_degC = round(temp_degC, digits = 2))

minibuoy_temp_filter
  
# Join all data together ----

#join all and mark frequency
all_temp <- full_join(winterhobo_filter, buoy_filter) %>% 
  full_join(gloeo_temp_dstadj) %>% 
  full_join(minibuoy_temp_filter)

#filter for may through nov (ice out season)
all_temp_filtered <- all_temp %>% 
  mutate(month = as.numeric(format(datetime, '%m'))) %>% 
  filter(month >= 5 & month <= 11)

#reorg columns
all_temp_filtered <- all_temp_filtered %>% 
  select(datetime, lat_dd, long_dd, location, depth_m, temp_degC, source, month)

# export data ----
all_temp_filtered %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., paste0(dumpdir, 'insitu_temp_data_v', Sys.Date(), '.csv'))

all_temp_filtered  %>% 
  group_by(lat_dd, long_dd, location, source) %>% 
  summarize(lat_dd = first(lat_dd),
            long_dd = first(long_dd),
            location = first(location),
            source = first(source)) %>% 
  write_csv(., file.path(dumpdir, 'in-situ locs/HF_insitu_locs.csv'))

