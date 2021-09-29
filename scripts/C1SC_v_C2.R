# this script compares the C1 SC algorhim with the C2 surface temp data

#load libraries
library(tidyverse)
library(readr)

#functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#point to directories
datadir <- 'data/'
C2_datadir <- 'data/colab-output/C2/'

# read in C1 and C2 data
C1SC <- read_csv(file.path(datadir, 'LS_passQAQC_v24Sept2021.csv')) 
head(C1SC)

C1SC_vert <- C1SC%>% 
  select(-calib_range_med_temp_degC, -temp_spread, -surface_temp_count) %>% 
  mutate(pct_lake = pct_lake * 100) %>% 
  pivot_longer(cols = c(pct_lake:surface_temp_stdDev),
               names_to = 'variable',
               values_to = 'value_C1')
head(C1SC_vert)

C2 <- read_csv(file.path(C2_datadir, 'sunapee_1985_2020_C2_stats.csv')) 
head(C2)

C2_vert <- C2 %>% 
  mutate(date = as.Date(substrRight(`system:index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system:index`) ~ 'LS 5',
                                    grepl('LT04', `system:index`) ~ 'LS 4',
                                    grepl('LE07', `system:index`) ~ 'LS 7',
                                    grepl('LC08', `system:index`) ~ 'LS 8',
                                    TRUE ~ NA_character_)) %>% 
  mutate(pct_lake = (pixel_count/23598)*100) %>% 
  select(date, LSmission, pct_lake, temp_max:temp_stdDev) %>% 
  rename_at(vars('temp_max', 'temp_mean', 'temp_median', 'temp_min', 'temp_p25', 'temp_p75', 'temp_skew', 'temp_stdDev'),
            ~ paste('surface', ., sep = '_')) %>% 
  pivot_longer(cols = c(pct_lake:surface_temp_stdDev),
               names_to = 'variable',
               values_to = 'value_C2')

C1_C2 <- full_join(C1SC_vert, C2_vert) %>% 
  filter(!is.na(value_C1))

range(C1_C2$value_C1)
range(C1_C2$value_C2, na.rm = T)

C1_C2 %>% 
  filter(variable == 'surface_temp_mean' | variable == 'surface_temp_median' | variable == 'pct_lake') %>% 
  ggplot(., aes(x = value_C1, y = value_C2)) +
  geom_point() +
  labs(x = 'Collection 1 SC alg',
       y = 'Collection 2 value') +
  facet_grid(variable ~ .) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
