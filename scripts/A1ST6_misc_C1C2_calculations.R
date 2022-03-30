# miscellaneous calculations in the MS

source('scripts/R_library.R')

# point to directories
C1_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C1/'
C2_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C2/'
figdir <- '~/GitHub/ids-ne-lakes/figures/'

## load and summarize valid landsat data ####
C1 <- read.csv(file.path(C1_datadir, 'C1_SCA_v2_temp_stats.csv')) %>% 
  mutate(date = as.Date(substrRight(`system.index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system.index`) ~ 'LS 5',
                             grepl('LT04', `system.index`) ~ 'LS 4',
                             grepl('LE07', `system.index`) ~ 'LS 7',
                             grepl('LC08', `system.index`) ~ 'LS 8',
                             TRUE ~ NA_character_),
         collection = 'C1',
         filter = 'none')
  
C2 <- read.csv(file.path(C2_datadir, 'C2_v2_temp_stats.csv')) %>% 
  mutate(date = as.Date(substrRight(`system.index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system.index`) ~ 'LS 5',
                               grepl('LT04', `system.index`) ~ 'LS 4',
                               grepl('LE07', `system.index`) ~ 'LS 7',
                               grepl('LC08', `system.index`) ~ 'LS 8',
                               TRUE ~ NA_character_),
         collection = 'C2',
         filter = 'none')
  

# % increase in scenes from C1 to C2
(nrow(C2) - nrow(C1)) / nrow(C1)

# % increase in missions
alldata <- full_join(C1, C2)
mission_summary <- alldata %>% 
  group_by(LSmission, collection) %>% 
  summarise(count = length(LSmission)) %>% 
  pivot_wider(names_from = collection,
              values_from = count) %>% 
  mutate(perc_change_C1_C2 = round(((C2-C1)/C1)*100, digits = 1))

#calculate percentage change with kurtosis filter
C1_kurt = C1 %>% 
  filter(surface_temp_kurtosis > 2) %>% 
  mutate(filter = 'kurtosis')
C2_kurt = C2 %>% 
  filter(surface_temp_kurtosis > 2) %>% 
  mutate(filter = 'kurtosis')

C2_mission <- full_join(C2, C2_kurt) %>%
  mutate(month = format(date, '%m'),
         year = format(date, '%Y'),
         decade = case_when(year < 1990 ~ 'eighties',
                            year >= 1990 & year < 2000 ~ 'nineties',
                            year >= 2000 & year < 2010 ~ 'aughts',
                            year >= 2010 & year <= 2020 ~ 'teens',
                            TRUE ~ ''))

mission_summary_kurt <- C2_mission %>% 
  group_by(LSmission, filter) %>% 
  summarise(count = length(LSmission)) %>% 
  pivot_wider(names_from = filter,
              values_from = count) %>% 
  mutate(perc_change_kurt = round(((kurtosis-none)/abs(none))*100, digits = 1))

month_summary_kurt <- C2_mission %>% 
  group_by(month, filter) %>% 
  summarise(count = length(month)) %>% 
  pivot_wider(names_from = filter,
              values_from = count) %>% 
  mutate(perc_change_kurtmonth = round(((kurtosis-none)/abs(none))*100, digits = 1))

tableb <- full_join(mission_summary, mission_summary_kurt)

write.csv(tableb, file.path(figdir, 'ST6_C1C2filtercount.csv'), row.names = F)

