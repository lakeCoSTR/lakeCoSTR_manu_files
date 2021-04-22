library(tidyverse)
library(readxl)

#read in regional assignments
regional <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/highfreq_mon_locs_inlake_201907b.csv') %>% 
  mutate(lat_dd = signif(lat_dd, digits = 6),
         long_dd = signif(long_dd, digits = 6))

#read in temp data
temp1 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20150507.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20150507')
temp2 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20150523.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20150523')
temp3 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20150624.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20150624')
temp4 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20150710.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20150710')
temp5 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20151030.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20151030')
temp6 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20160525.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20160525')
temp7 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20160829.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20160829')
temp8 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20160914.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20160914')
temp9 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20170901.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20170901')
temp10 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20170917.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20170917')
temp11 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20171003.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20171003')
temp12 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20180718.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20180718')
temp13 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20180819.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20180819')
temp14 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20180904.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20180904')
temp15 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20181022.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20181022')
temp16 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20190518.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20190518')
temp17 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/1_2_1_LE07_013030_20190603.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '1_2_1_LE07_013030_20190603')
temp18 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20150702.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20150702')
temp19 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20151022.csv',
                  col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20151022')
temp20 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20160618.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20160618')
temp21 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20160704.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20160704')
temp22 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20160720.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20160720')
temp23 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20160805.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20160805')
temp24 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20161024.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20161024')
temp25 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20170504.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20170504')
temp26 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20170621.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20170621')
temp27 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20170723.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20170723')
temp28 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20170824.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20170824')
temp29 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20171011.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20171011')
temp30 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20180523.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20180523')
temp31 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20180608.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20180608')
temp32 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20180710.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20180710')
temp33 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20181014.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20181014')
temp34 <- read_csv('C:/Users/steeleb/Dropbox/NASA working files/christina temp/measurements_used/2_1_LC08_013030_20190526.csv',
                   col_types = c('Tnnnncnnc')) %>% 
  mutate(`system:index` = '2_1_LC08_013030_20190526')

#collate files
all_temp_sun_LSflyover <- full_join(temp1, temp2) %>% 
  full_join(., temp3) %>% 
  full_join(., temp4) %>% 
  full_join(., temp5) %>% 
  full_join(., temp6) %>% 
  full_join(., temp7) %>% 
  full_join(., temp8) %>% 
  full_join(., temp9) %>% 
  full_join(., temp10) %>% 
  full_join(., temp11) %>% 
  full_join(., temp12) %>% 
  full_join(., temp13) %>% 
  full_join(., temp14) %>% 
  full_join(., temp15) %>% 
  full_join(., temp16) %>% 
  full_join(., temp17) %>% 
  full_join(., temp18) %>% 
  full_join(., temp19) %>% 
  full_join(., temp20) %>% 
  full_join(., temp21) %>% 
  full_join(., temp22) %>% 
  full_join(., temp23) %>% 
  full_join(., temp24) %>% 
  full_join(., temp25) %>% 
  full_join(., temp26) %>% 
  full_join(., temp27) %>% 
  full_join(., temp28) %>% 
  full_join(., temp29) %>% 
  full_join(., temp30) %>% 
  full_join(., temp31) %>% 
  full_join(., temp32) %>% 
  full_join(., temp33) %>% 
  full_join(., temp34) %>% 
  mutate(lat_dd = signif(lat_dd, digits = 6),
         long_dd = signif(long_dd, digits = 6))

#join with regional designation
all_temp_sun_LSflyover <- left_join(all_temp_sun_LSflyover, regional)


#filter with following rules:
#For a whole-lake value, here is my proposed workflow:
#   1) for locations with multiple depths: select the sensor closest to the lake surface
#   2) aggregate by the sensor at each location by choosing the median for that sensor over the time window
#     - as to not weight analysis by a single sensor that may have multiple observations during the time window
#   3) limit by region: i.e. limit data so that each 'region' of the lake has only one value Newbury New at Newbury or Onset over HOBOS // alternatively, aggregate to the median of each site
#     - this is to make sure we're not weighting one are of the lake because there is more than one sensor
#   4) aggregate by whole lake from the region-limited dataset (with as many summary stats as possible - including how many 'regions' are included and listing the sensor origin that contributed to the aggregated data)

#filter 1: group by image, lat, long, source; arrange by image, sensor, depth; summarize as first image, sensor, depth; left join with summary list
filter1 <- all_temp_sun_LSflyover %>% 
  group_by(`system:index`, lat_dd, long_dd, source) %>% 
  arrange(`system:index`, source, depth_m) %>% 
  summarise(depth_m = first(depth_m)) %>% 
  mutate(filter1 = 1)

temp_sun_filter1 <- full_join(all_temp_sun_LSflyover, filter1) %>% 
  filter(filter1 == 1)

#filter 2: aggregate to median temp by previous groups
temp_sun_filter2 <- temp_sun_filter1 %>% 
  group_by(`system:index`, lat_dd, long_dd, source, depth_m, flag, buff_125m, buff_500m, location, region) %>% 
  summarize(med_temp_degC = median(temp_degC),
            count_temp_degC = length(temp_degC))

#filter 3 aggregate by region, choose best source
filter3 <- temp_sun_filter2 %>% 
  group_by(region, source, location) %>% 
  summarize()
unique(filter3$region)
unique(filter3$source)
unique(filter3$location)
#create rankings for location, source
filter3 <- filter3 %>% 
  mutate(region_rank = case_when(region == 'loon' & source == 'buoy thermistors' ~ 1,
                                   region == 'loon' & source == 'upper do' ~ 2,
                                 region == 'loon' & grepl('hobo', source) ~ 3,
                                   region == 'harbor' ~ 1,
                                   region == 'coffin' & grepl('onset', source) ~ 1,
                                   region == 'coffin' & grepl('hobo', source) ~ 2,
                                   region == 'fichter' & grepl('onset', source) ~ 1,
                                   region == 'fichter' & grepl('hobo', source) ~ 2,
                                   region == 'georges mills' ~ 1,
                                   region == 'herrick cove' & grepl('buoy', source) ~ 1,
                                   region == 'herrick cove' & grepl('onset', source) ~ 2,
                                   region == 'herrick cove' & grepl('hobo', source) ~ 3,
                                   region == 'montgomery' ~ 1,
                                   region == 'newbury' & location == 'New Newbury' ~ 1,
                                   region == 'newbury' & grepl('onset', source) ~ 2,
                                   region == 'newbury' & grepl('hobo', source) & location == 'Old Newbury' ~ 3,
                                   region == 'state beach' ~ 1))
temp_sun_filter3 <- full_join(temp_sun_filter2, filter3) %>% 
  group_by(region, `system:index`) %>% 
  arrange(region_rank) %>% 
  summarize(temp_degC = first(med_temp_degC),
            depth_m = first(depth_m),
            source = first(source)) # choosing first, because there is only, at max data coming from 2 sources

#aggregate by system index
temp_sun_aggregated <- temp_sun_filter3 %>% 
  group_by(`system:index`) %>% 
  summarize(min_temp_degC = min(temp_degC),
            mean_temp_degC = mean(temp_degC),
            median_temp_degC = median(temp_degC),
            max_temp_degC = max(temp_degC),
            sd_temp_degC = sd(temp_degC),
            min_depth_m = min(depth_m),
            mean_depth_m = mean(depth_m),
            median_depth_m = median(depth_m),
            max_depth_m = max(depth_m),
            n_regions = length(temp_degC),
            regions = str_c(region, collapse = ', '))

write_csv(temp_sun_aggregated, 'C:/Users/steeleb/Dropbox/NASA working files/christina temp/temp_sun_aggregated_by_regionrank_14Dec2019.csv')                                   
                                   
