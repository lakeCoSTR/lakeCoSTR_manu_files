# application of the data, examining trends from lspa 200-230 to see if they are similar to Landsat-derived data

source('scripts/R_library.R')

figdir = 'figures/'
datadir = 'data/'

#Load lspa data from git hub

lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

lmp_temp_deep <- lmp %>% 
  filter(parameter == 'temp_C') %>% 
  filter(station == 200 | station ==210 | station == 220| station==230) %>% 
  filter(depth_m == 0.5) 

# load all LS data, filter for freeze and cloud cover
ls <- read.csv(file.path(datadir, 'colab-output/C2/sunapee_1980_2020_C2_stats.csv')) %>% 
  mutate(date = substrRight(`system.index`, 8),
         date = as.Date(date, format = '%Y%m%d'))
ls_cloud <- ls %>% 
  filter(temp_min > 0 & cloud_cover < 40)

#### whole lake median by month and year ####
lmp_temp_monthly_stats <- lmp_temp_deep %>% 
  mutate(month = as.numeric(format(as.Date(date), '%m')),
         day = as.numeric(format(as.Date(date), '%d')),
         year = as.numeric(format(as.Date(date), '%Y'))) %>% 
  group_by(year, month) %>% 
  summarise(is_summer_median_temp_degC = median(value),
            is_summer_max_temp_degC = max(value),
            is_summer_min_temp_degC = min(value),
            is_n_obs = length(value),
            day = min(day)) %>% 
  filter((month >=5 & month < 12)) 

ggplot(lmp_temp_monthly_stats, aes(x=year, y = is_summer_median_temp_degC)) +
  facet_grid(.~month) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)


ls_temp_summer_monthly_median_cloud <- ls_cloud %>% 
  mutate(month = as.numeric(format((date), '%m')),
         year = as.numeric(format((date), '%Y'))) %>% 
  group_by(year, month) %>% 
  summarise(ls_summer_median_temp_degC = median(temp_median),
            ls_n_obs = length(temp_median))  %>% 
  filter(!is.na(month))

ggplot(ls_temp_summer_monthly_median_cloud, aes(x=year, y = ls_summer_median_temp_degC)) +
  facet_grid(.~month) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)


df1_med <- lmp_temp_monthly_stats %>% 
  select(year, month, is_summer_median_temp_degC)

df2_med <- ls_temp_summer_monthly_median_cloud %>% 
  select(year, month, ls_summer_median_temp_degC)

temp_monthly_median <- full_join(df1_med, df2_med) %>% 
  pivot_longer(., cols = c(ls_summer_median_temp_degC, is_summer_median_temp_degC), names_to='source', values_to = 'value') %>% 
  mutate(source = case_when(grepl('is_', source) ~ 'in-situ',
                            grepl('ls_', source) ~ 'landsat',
                            TRUE ~ NA_character_)) %>% 
  filter(!is.na(value))

temp_monthly_median %>% 
  group_by(month) %>% 
  ggplot(., aes(x = year, y = value)) +
  geom_point(aes(color = source)) +
  # geom_line(aes(color = source)) +
  facet_grid(.~month) +
  labs(y = 'lake surface temperature\n(degrees C)') +
  # geom_smooth(method = 'lm', se = F, inherit.aes = T) +
  final_theme
ggsave(file.path(figdir, 'monthly_patterns_calib_temp_cloud.png'), height = 3, width = 9)


#### linear regression - significant slope? ####

linear_analysis = function(mm, ds){
  data <- temp_monthly_median %>% 
    filter(month == mm & source == ds)
  lm_result <- lm(data$value ~ data$year)
  print(summary(lm_result))
}

is_5_lm <- linear_analysis(5, 'in-situ')
is_6_lm <- linear_analysis(6, 'in-situ')
is_7_lm <- linear_analysis(7, 'in-situ')
is_8_lm <- linear_analysis(8, 'in-situ')
is_9_lm <- linear_analysis(9, 'in-situ')
is_10_lm <- linear_analysis(10, 'in-situ')
ls_5_lm <- linear_analysis(5, 'landsat')
ls_6_lm <- linear_analysis(6, 'landsat')
ls_7_lm <- linear_analysis(7, 'landsat')
ls_8_lm <- linear_analysis(8, 'landsat')
ls_9_lm <- linear_analysis(9, 'landsat')
ls_10_lm <- linear_analysis(10, 'landsat')

linear_analysis_all = function(mm){
  data <- temp_monthly_median %>% 
    filter(month == mm)
  lm_result <- lm(data$value ~ data$year)
  print(summary(lm_result))
}

all_5_lm <- linear_analysis_all(5)
all_6_lm <- linear_analysis_all(6)
all_7_lm <- linear_analysis_all(7)
all_8_lm <- linear_analysis_all(8)
all_9_lm <- linear_analysis_all(9)
all_10_lm <- linear_analysis_all(10)

# grom_lm = function(mm, ds){
#   data <- temp_monthly_median %>% 
#     filter(month == mm & source == ds)
#   lm_result <- lm(data$value ~ data$year)
#   if(is.na(summary(lm_result)$coefficients[2,4])) {
#     grom <- ggplot(data, aes(x = year, y = value)) +
#       geom_point() +
#       coord_cartesian(ylim = c(7, 27),
#                       xlim = c(1980, 2020)) +
#       labs(x = NULL, y = NULL) +
#       final_theme
#   } else {
#     if(summary(lm_result)$coefficients[2,4] < 0.05){ 
#       grom <- ggplot(data, aes(x = year, y = value)) +
#         geom_point() +
#         coord_cartesian(ylim = c(7, 27),
#                         xlim = c(1980, 2020)) +
#         geom_abline(slope = summary(lm_result)$coefficients[2, 1],
#                     intercept = summary(lm_result)$coefficients[1,1]) +
#         labs(x = NULL, y = NULL) +
#         final_theme
#     } else {
#       grom <- ggplot(data, aes(x = year, y = value)) +
#         geom_point() +
#         coord_cartesian(ylim = c(7, 27),
#                         xlim = c(1980, 2020)) +
#         labs(x = NULL, y = NULL) +
#         final_theme
#     }
#   }
#   print(grom)
# }
# 
# 
# is_5_gg <- grom_lm(5, 'in-situ')
# is_6_gg <- grom_lm(6, 'in-situ')
# is_7_gg <- grom_lm(7, 'in-situ')
# is_8_gg <- grom_lm(8, 'in-situ')
# is_9_gg <- grom_lm(9, 'in-situ')
# is_10_gg <- grom_lm(10, 'in-situ')
# ls_5_gg <- grom_lm(5, 'landsat')
# ls_6_gg <- grom_lm(6, 'landsat')
# ls_7_gg <- grom_lm(7, 'landsat')
# ls_8_gg <- grom_lm(8, 'landsat')
# ls_9_gg <- grom_lm(9, 'landsat')
# ls_10_gg <- grom_lm(10, 'landsat')


# #set up png device
# png(file.path(figdir, 'application_monthly_median_temp_IQR_bysource.png'),
#     width=16,height=6, units = 'in', res = 300)
# gridExtra::grid.arrange(is_5_gg, is_6_gg, is_7_gg, is_8_gg, is_9_gg, is_10_gg,
#                         ls_5_gg, ls_6_gg, ls_7_gg, ls_8_gg, ls_9_gg, ls_10_gg,
#                         nrow = 2,
#                         left = 'lake surface temperature\ndegrees C')
# 
# #Close pdf graphics device
# dev.off()

# indicator variable analysis ####
temp_monthly_median$source = as.factor(temp_monthly_median$source)

source_contrasts <- contrasts(temp_monthly_median$source)

may_data <- temp_monthly_median %>% 
  filter(month == 5)
jun_data <- temp_monthly_median %>% 
  filter(month == 6)
jul_data <- temp_monthly_median %>% 
  filter(month == 7)
aug_data <- temp_monthly_median %>% 
  filter(month == 8)
sep_data <- temp_monthly_median %>% 
  filter(month == 9)
oct_data <- temp_monthly_median %>% 
  filter(month == 10)
nov_data <- temp_monthly_median %>% 
  filter(month == 11)


# iva and multivariate linear models ####
lm_source_may_iva <- lm(value ~ year+source+year*source, data = may_data)
summary(lm_source_may_iva)
#no sig diff in slope
lm_source_may <- lm(value ~ year+source, data = may_data)
summary(lm_source_may)
#difference in intercept
is_5_lm
ls_5_lm
# no sig slope for either group; no slope

lm_source_jun_iva <- lm(value ~ year+source+year*source, data = jun_data)
summary(lm_source_jun_iva)
#no sig diff in slope
lm_source_jun <- lm(value ~ year+source, data = jun_data)
summary(lm_source_jun)
#no sig diff in intercept
all_6_lm
#no sig slope

lm_source_jul_iva <- lm(value ~ year+source+year*source, data = jul_data)
summary(lm_source_jul_iva)
# no sig diff in slope; intercept and slope are sig
lm_source_jul <- lm(value ~ year+source, data = jul_data)
summary(lm_source_jul)
# no sig diff in intercept; intercept and slope are sig
all_7_lm
#slope and intercept are significant; plot one line for the two groups of data
car::durbinWatsonTest(lm(value~year, data = jul_data))
#confirms autocorrelation

lm_source_aug_iva <- lm(value ~ year+source+year*source, data = aug_data)
summary(lm_source_aug_iva)
# no sig diff in slope
lm_source_aug <- lm(value ~ year+source, data = aug_data)
summary(lm_source_aug)
#no sig diff in intercept; intercept and slope sig
all_8_lm
#slope and intercept sig; plot one line for the two groups of data
car::durbinWatsonTest(lm(value~year, data = aug_data))
#confirms autocorrelation

# do the analysis without the outliers
aug_data_wo <- aug_data %>% 
  filter(value > 15)
lm_source_aug_iva_wo <- lm(value ~ year+source+year*source, data = aug_data_wo)
summary(lm_source_aug_iva_wo)
#no sig diff in slope
lm_source_aug_wo <- lm(value ~ year+source, data = aug_data_wo)
summary(lm_source_aug_wo)
# no sig diff in intercept
lm_aug_wo <- lm(value ~ year, data = aug_data_wo)
summary(lm_aug_wo)
# slope and intercept sig plot one line for the two groups
ls_aug_data_wo <- aug_data_wo %>% 
  filter(source == 'landsat')
is_aug_data_wo <- aug_data_wo %>% 
  filter(source == 'in-situ')
ls_8_lm_wo <- lm(value~year, data = ls_aug_data_wo)
is_8_lm_wo <- lm(value~year, data = is_aug_data_wo)

lm_source_sep_iva <- lm(value ~ year+source+year*source, data = sep_data)
summary(lm_source_sep_iva)
# there is a significant difference in slope
lm_source_sep <- lm(value ~ year+source, data = sep_data)
summary(lm_source_sep)
all_9_lm
is_9_lm
ls_9_lm
#plot one line for landsat
ls_9_data <- sep_data %>% 
  filter(source == 'landsat')
car::durbinWatsonTest(lm(value~year, data = ls_9_data))
#confirms autocorrelation

# do analysis without outliers
sep_data_wo <- sep_data %>% 
  filter(value >15)
lm_source_sep_iva_wo <- lm(value ~ year+source+year*source, data = sep_data_wo)
summary(lm_source_sep_iva_wo)
# sig diff in slope
lm_source_sep_wo <- lm(value ~ year+source, data = sep_data_wo)
summary(lm_source_sep_wo)
lm_sep_wo <- lm(value ~ year, data = sep_data_wo)
summary(lm_sep_wo)
ls_sep_data_Wo <- sep_data_wo %>% 
  filter(source == 'landsat')
is_sep_data_Wo <- sep_data_wo %>% 
  filter(source == 'in-situ')
ls_9_lm_wo = lm(value ~ year, data = ls_sep_data_Wo)
summary(ls_9_lm_wo)
is_9_lm_wo = lm(value ~ year, data = is_sep_data_Wo)
is_9_lm
#plot one line for landsat


lm_source_oct_iva <- lm(value ~ year+source+year*source, data = oct_data)
summary(lm_source_oct_iva)
#no diff in slope
lm_source_oct <- lm(value ~ year+source, data = oct_data)
summary(lm_source_oct)
#no diff in intercept
all_10_lm
#plot one line 
car::durbinWatsonTest(lm(value~year, data = oct_data))
#confirms autocorrelation

# IVA results to table ####
iva_table <- NULL

iva_table$month <- c('May', 'June', 'July', 'August', 'August, outliers removed', 'September', 'September, outliers removed', 'October')

iva_table$iva_pval <- c(summary(lm_source_may_iva)$coefficients[4,4],
                        summary(lm_source_jun_iva)$coefficients[4,4],
                        summary(lm_source_jul_iva)$coefficients[4,4],
                        summary(lm_source_aug_iva)$coefficients[4,4],
                        summary(lm_source_aug_iva_wo)$coefficients[4,4],
                        summary(lm_source_sep_iva)$coefficients[4,4],
                        summary(lm_source_sep_iva_wo)$coefficients[4,4],
                        summary(lm_source_oct_iva)$coefficients[4,4])

iva_table$multreg_pval <- c(summary(lm_source_may)$coefficients[3,4],
                           summary(lm_source_jun)$coefficients[3,4],
                           summary(lm_source_jul)$coefficients[3,4],
                           summary(lm_source_aug)$coefficients[3,4],
                           summary(lm_source_aug_wo)$coefficients[3,4],
                           summary(lm_source_sep)$coefficients[3,4],
                           summary(lm_source_sep_wo)$coefficients[3,4],
                           summary(lm_source_oct)$coefficients[3,4])

iva_table$alldata_slope_pval <- c(all_5_lm$coefficients[2,4],
                                  all_6_lm$coefficients[2,4],
                                  all_7_lm$coefficients[2,4],
                                  all_8_lm$coefficients[2,4],
                                  summary(lm_aug_wo)$coefficients[2,4],
                                  all_9_lm$coefficients[2,4],
                                  summary(lm_sep_wo)$coefficients[2,4],
                                  all_10_lm$coefficients[2,4])

iva_table$ls_slope_pval <- c(ls_5_lm$coefficients[2,4],
                             ls_6_lm$coefficients[2,4],
                             ls_7_lm$coefficients[2,4],
                             ls_8_lm$coefficients[2,4],
                             summary(ls_8_lm_wo)$coefficients[2,4],
                             ls_9_lm$coefficients[2,4],
                             summary(ls_9_lm_wo)$coefficients[2,4],
                             ls_10_lm$coefficients[2,4])

iva_table$is_slope_pval <- c(is_5_lm$coefficients[2,4],
                             is_6_lm$coefficients[2,4],
                             is_7_lm$coefficients[2,4],
                             is_8_lm$coefficients[2,4],
                             summary(is_8_lm_wo)$coefficients[2,4],
                             is_9_lm$coefficients[2,4],
                             summary(is_9_lm_wo)$coefficients[2,4],
                             is_10_lm$coefficients[2,4])
iva_table <- as.data.frame(iva_table) 
iva_table <- iva_table %>% 
  mutate_at(vars(iva_pval:is_slope_pval),
            ~ round(., digits = 3))

# recode table values where they are irrelevant
iva_table_recode <- iva_table %>% 
  mutate(multreg_pval = case_when(iva_pval <0.05 ~ NA_real_,
                                  TRUE ~ multreg_pval),
         alldata_slope_pval = case_when(multreg_pval <0.05 ~NA_real_,
                                        iva_pval <0.05~NA_real_,
                                        is.na(multreg_pval) ~ alldata_slope_pval,
                                        TRUE ~ alldata_slope_pval),
         is_slope_pval = case_when(iva_pval <0.05 ~ is_slope_pval,
                                   multreg_pval < 0.05 ~ is_slope_pval,
                                   TRUE ~ NA_real_),
         ls_slope_pval = case_when(iva_pval <0.05 ~ ls_slope_pval,
                                   multreg_pval < 0.05 ~ ls_slope_pval,
                                   TRUE ~ NA_real_))


#### plot on same axes with separate lines ####
may <- temp_monthly_median %>% 
  filter(month == 5) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'May') +
  theme(legend.position = 'none')

june <- temp_monthly_median %>% 
  filter(month == 6) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'June') +
  theme(legend.position = 'none')

jul <- temp_monthly_median %>% 
  filter(month == 7) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  # geom_abline(slope = is_7_lm$coefficients[2, 1],
  #             intercept = is_7_lm$coefficients[1,1],
  #             color = 'black') +
  # geom_abline(slope = ls_7_lm$coefficients[2, 1],
  #             intercept = ls_7_lm$coefficients[1,1],
  #             color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'July') +
  theme(legend.position = 'none')  

aug <- temp_monthly_median %>% 
  filter(month == 8) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  # geom_abline(slope = is_8_lm$coefficients[2, 1],
  #             intercept = is_8_lm$coefficients[1,1],
  #             color = 'black') +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'August') +
  theme(legend.position = 'none')  

sept <- temp_monthly_median %>% 
  filter(month == 9) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  # geom_abline(slope = ls_9_lm$coefficients[2, 1],
  #             intercept = ls_9_lm$coefficients[1,1],
  #             color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'September') +
  theme(legend.position = 'none')  

oct <- temp_monthly_median %>% 
  filter(month == 10) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  # geom_abline(slope = ls_10_lm$coefficients[2, 1],
  #             intercept = ls_10_lm$coefficients[1,1],
  #             color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'October') +
  theme(legend.position = 'none')  

#set up png device
png(file.path(figdir, 'application_monthly_median_temp_IQR_together.png'),
    width=9,height=6, units = 'in', res = 300)
gridExtra::grid.arrange(may, june, jul, aug, sept, oct,
                        nrow = 2,
                        left = 'lake surface temperature\ndegrees C',
                        bottom = 'black = in-situ   yellow = landsat')

#Close pdf graphics device
dev.off()



