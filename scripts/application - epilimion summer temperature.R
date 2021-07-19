# application of the data, examining trends from lspa 200-230 to see if they are similar to Landsat-derived data

library(tidyverse)
library(ggthemes)

#Load lspa data

lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

lmp_temp_deep <- lmp %>% 
  filter(parameter == 'temp_C') %>% 
  filter(station == 200 | station ==210 | station == 220| station==230) %>% 
  filter(depth_m == 0.5)


#### by site ####
lmp_temp_summer_mean <- lmp_temp_deep %>% 
  mutate(month = as.numeric(format(as.Date(date), '%m')),
         year = as.numeric(format(as.Date(date), '%Y'))) %>% 
  filter(month >= 6 & month <=8) %>% 
  group_by(station, year) %>% 
  summarise(is_summer_mean_temp_degC = mean(value),
            is_sd_temp_degC = sd(value),
            is_n_obs = length(value)) %>% 
  mutate(station = as.factor(station)) %>% 
  filter(is_n_obs >= 3)

ggplot(lmp_temp_summer_mean, aes(x=year, y = is_summer_mean_temp_degC, color = station)) +
  geom_point()
ggplot(lmp_temp_summer_mean, aes(x=year, y = is_summer_mean_temp_degC)) +
  geom_point() +
  facet_grid(station ~ .) +
  geom_smooth(method = 'lm', se = F)


#Load Landsat data
landsat_station <- read.csv('data/temp_zonal_stats_sunapee_1985_2020_06_15.csv')
landsat_station_deep <- landsat_station %>% 
  filter(WAYPOINT == 200 | WAYPOINT ==210 | WAYPOINT == 220| WAYPOINT==230)

#filter data


# apply calibration




landsat_summer_mean <- landsat_station_deep %>% 
  mutate(date = as.Date(substr(uid, 15, 22), format = '%Y%m%d'),
           month = as.numeric(format(date, '%m')),
         year = as.numeric(format(date, '%Y'))) %>% 
  filter(month >= 6 & month <=8) %>% 
  filter(min > 0 & skew < 1 & skew >-1) %>% 
  group_by(WAYPOINT, year) %>% 
  summarise(ls_summer_mean_temp_degC = mean(median),
            ls_sd_temp_degC = sd(median),
            ls_n_obs = length(median)) %>% 
  mutate(WAYPOINT = as.factor(WAYPOINT)) %>% 
  filter(ls_n_obs >= 3)

ggplot(landsat_summer_mean, aes(x=year, y = ls_summer_mean_temp_degC, color = WAYPOINT)) +
  geom_point()
ggplot(landsat_summer_mean, aes(x=year, y = ls_summer_mean_temp_degC)) +
  geom_point() +
  facet_grid(WAYPOINT ~ .) +
  geom_smooth(method = 'lm', se = F)

ls_is_summer_mean <- landsat_summer_mean %>% 
  mutate(station = as.factor(WAYPOINT)) %>% 
  full_join(., lmp_temp_summer_mean)

ggplot(ls_is_summer_mean, aes(x = ls_summer_mean_temp_degC, y = is_summer_mean_temp_degC)) +
  geom_point() +
  facet_grid(station ~ .) +
  geom_smooth(method = 'lm', se=F)

#### as whole-lake mean ####
