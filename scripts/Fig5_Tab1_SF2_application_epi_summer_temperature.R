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
ls <- read.csv(file.path(datadir, 'colab-output/C2/C2_v2_temp_stats.csv')) %>% 
  mutate(date = substrRight(`system.index`, 8),
         date = as.Date(date, format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system.index`) ~ 'LS 5',
                               grepl('LT04', `system.index`) ~ 'LS 4',
                               grepl('LE07', `system.index`) ~ 'LS 7',
                               grepl('LC08', `system.index`) ~ 'LS 8',
                               TRUE ~ NA_character_)) 
ls_kurtosis <- ls %>% 
  filter(surface_temp_kurtosis >2 )

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
  filter((month >=5 & month <= 11)) 

ls_temp_summer_monthly_median_kurtosis <- ls_kurtosis %>% 
  mutate(month = as.numeric(format((date), '%m')),
         year = as.numeric(format((date), '%Y'))) %>% 
  group_by(year, month) %>% 
  summarise(ls_summer_median_temp_degC = median(surface_temp_median),
            ls_n_obs = length(surface_temp_median))  %>% 
  filter(!is.na(month))

df1_med <- lmp_temp_monthly_stats %>% 
  select(year, month, is_summer_median_temp_degC)

df2_med_kurtosis <- ls_temp_summer_monthly_median_kurtosis %>% 
  select(year, month, ls_summer_median_temp_degC)

temp_monthly_median <- full_join(df1_med, df2_med_kurtosis) %>% 
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


#### linear regression - significant slope? ####

# apply lm per dataset

linear_analysis = function(mm, ds){
  data <- temp_monthly_median %>% 
    filter(month == mm & source == ds)
  lm_result <- lm(data$value ~ data$year)
  print(lm_result)
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
ls_11_lm <- linear_analysis(11, 'landsat')

# do DW test for each month and each dataset
month = c(5:10, 5:10)
data = c('in-situ','in-situ','in-situ','in-situ','in-situ','in-situ',
         'landast','landast','landast','landast','landast','landast')
DW_stat = c(car::durbinWatsonTest(is_5_lm)$dw,
            car::durbinWatsonTest(is_6_lm)$dw,
            car::durbinWatsonTest(is_7_lm)$dw,
            car::durbinWatsonTest(is_8_lm)$dw,
            car::durbinWatsonTest(is_9_lm)$dw,
            car::durbinWatsonTest(is_10_lm)$dw,
            car::durbinWatsonTest(ls_5_lm)$dw,
            car::durbinWatsonTest(ls_6_lm)$dw,
            car::durbinWatsonTest(ls_7_lm)$dw,
            car::durbinWatsonTest(ls_8_lm)$dw,
            car::durbinWatsonTest(ls_9_lm)$dw,
            car::durbinWatsonTest(ls_10_lm)$dw)
DW_pval = c(car::durbinWatsonTest(is_5_lm)$p,
            car::durbinWatsonTest(is_6_lm)$p,
            car::durbinWatsonTest(is_7_lm)$p,
            car::durbinWatsonTest(is_8_lm)$p,
            car::durbinWatsonTest(is_9_lm)$p,
            car::durbinWatsonTest(is_10_lm)$p,
            car::durbinWatsonTest(ls_5_lm)$p,
            car::durbinWatsonTest(ls_6_lm)$p,
            car::durbinWatsonTest(ls_7_lm)$p,
            car::durbinWatsonTest(ls_8_lm)$p,
            car::durbinWatsonTest(ls_9_lm)$p,
            car::durbinWatsonTest(ls_10_lm)$p)
DW_table = data.frame(month, data, DW_stat, DW_pval) %>% 
  mutate(DW_stat = round(DW_stat, digits = 3),
         DW_pval = round(DW_pval, digits = 3))

DW_table

# linear model for all data together

linear_analysis_all = function(mm){
  data <- temp_monthly_median %>% 
    filter(month == mm)
  lm_result <- lm(data$value ~ data$year)
  print(lm_result)
}

all_5_lm <- linear_analysis_all(5)
all_6_lm <- linear_analysis_all(6)
all_7_lm <- linear_analysis_all(7)
all_8_lm <- linear_analysis_all(8)
all_9_lm <- linear_analysis_all(9)
all_10_lm <- linear_analysis_all(10)


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

# iva and multivariate linear models ####

## MAY ####
lm_source_may_iva <- lm(value ~ year+source+year*source, data = may_data)
summary(lm_source_may_iva)
#no sig diff in slope
lm_source_may <- lm(value ~ year+source, data = may_data)
summary(lm_source_may)
#difference in intercept
summary(is_5_lm)
summary(ls_5_lm)
# no sig slope for either group; no slope

## JUN ####
lm_source_jun_iva <- lm(value ~ year+source+year*source, data = jun_data)
summary(lm_source_jun_iva)
#no sig diff in slope
lm_source_jun <- lm(value ~ year+source, data = jun_data)
summary(lm_source_jun)
#no sig diff in intercept
summary(all_6_lm)
#no sig slope

## JULY ####
lm_source_jul_iva <- lm(value ~ year+source+year*source, data = jul_data)
summary(lm_source_jul_iva)
# no sig diff in slope; intercept and slope are sig
lm_source_jul <- lm(value ~ year+source, data = jul_data)
summary(lm_source_jul)
# no sig diff in intercept; intercept and slope are sig
summary(all_7_lm)
#slope and intercept are significant; plot one line for the two groups of data

#see if insitu and landsat have trends separately
summary(is_7_lm) #yes
summary(ls_7_lm) #no

## AUG ####
lm_source_aug_iva <- lm(value ~ year+source+year*source, data = aug_data)
summary(lm_source_aug_iva)
# no sig diff in slope
lm_source_aug <- lm(value ~ year+source, data = aug_data)
summary(lm_source_aug)
#no sig diff in intercept; intercept and slope sig
summary(all_8_lm)

#see if insitu and landsat have trends separately
summary(is_8_lm) #yes
summary(ls_8_lm) #yes


## SEPT ####
lm_source_sep_iva <- lm(value ~ year+source+year*source, data = sep_data)
summary(lm_source_sep_iva)
# there is a significant difference in slope
lm_source_sep <- lm(value ~ year+source, data = sep_data)
summary(lm_source_sep)

summary(is_9_lm)
summary(ls_9_lm)
#plot one line for landsat

## OCT ####
lm_source_oct_iva <- lm(value ~ year+source+year*source, data = oct_data)
summary(lm_source_oct_iva)
#no diff in slope
lm_source_oct <- lm(value ~ year+source, data = oct_data)
summary(lm_source_oct)
#no diff in intercept
summary(all_10_lm)

#see if insitu and landsat have trends separately
summary(is_10_lm) #no
summary(ls_10_lm) #yes


# IVA results to table ####
iva_table <- NULL

iva_table$month <- c('May', 'June', 'July', 'August', 'September', 'October')

iva_table$iva_pval <- c(summary(lm_source_may_iva)$coefficients[4,4],
                        summary(lm_source_jun_iva)$coefficients[4,4],
                        summary(lm_source_jul_iva)$coefficients[4,4],
                        summary(lm_source_aug_iva)$coefficients[4,4],
                        summary(lm_source_sep_iva)$coefficients[4,4],
                        summary(lm_source_oct_iva)$coefficients[4,4])

iva_table$multreg_pval <- c(summary(lm_source_may)$coefficients[3,4],
                           summary(lm_source_jun)$coefficients[3,4],
                           summary(lm_source_jul)$coefficients[3,4],
                           summary(lm_source_aug)$coefficients[3,4],
                           summary(lm_source_sep)$coefficients[3,4],
                           summary(lm_source_oct)$coefficients[3,4])

iva_table <- as.data.frame(iva_table) 
iva_table <- iva_table %>% 
  mutate_at(vars(iva_pval, multreg_pval),
            ~ round(., digits = 3))

# gather slope and CI data

slope_table <- NULL

slope_table$month <- c('May', 'June', 'July', 'August', 'September', 'October')

slope_table$alldata_slope <- c(summary(all_5_lm)$coefficients[2,1],
                               summary(all_6_lm)$coefficients[2,1],
                               summary(all_7_lm)$coefficients[2,1],
                               summary(all_8_lm)$coefficients[2,1],
                               summary(all_9_lm)$coefficients[2,1],
                               summary(all_10_lm)$coefficients[2,1])

slope_table$alldata_slope_lower <- c(confint(lm(value~year, data = may_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = jun_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = jul_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = aug_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = sep_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = oct_data),'year',level=0.95)[1])
slope_table$alldata_slope_upper <- c(confint(lm(value~year, data = may_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = jun_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = jul_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = aug_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = sep_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = oct_data),'year',level=0.95)[2])

slope_table$ls_slope <- c(summary(ls_5_lm)$coefficients[2,1],
                          summary(ls_6_lm)$coefficients[2,1],
                          summary(ls_7_lm)$coefficients[2,1],
                          summary(ls_8_lm)$coefficients[2,1],
                          summary(ls_9_lm)$coefficients[2,1],
                          summary(ls_10_lm)$coefficients[2,1])

slope_table$ls_slope_lower <- c(confint(lm(value~year, data = may_data[may_data$source == 'landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = jun_data[jun_data$source == 'landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = jul_data[jul_data$source == 'landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = oct_data[oct_data$source == 'landsat',]), 'year', level = 0.95)[1])

slope_table$ls_slope_upper <- c(confint(lm(value~year, data = may_data[may_data$source == 'landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = jun_data[jun_data$source == 'landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = jul_data[jul_data$source == 'landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = oct_data[oct_data$source == 'landsat',]), 'year', level = 0.95)[2])

slope_table$is_slope <- c(summary(is_5_lm)$coefficients[2,1],
                          summary(is_6_lm)$coefficients[2,1],
                          summary(is_7_lm)$coefficients[2,1],
                          summary(is_8_lm)$coefficients[2,1],
                          summary(is_9_lm)$coefficients[2,1],
                          summary(is_10_lm)$coefficients[2,1])

slope_table$is_slope_lower <- c(confint(lm(value~year, data = may_data[may_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = jun_data[jun_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = jul_data[jul_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = oct_data[oct_data$source == 'in-situ',]), 'year', level = 0.95)[1])

slope_table$is_slope_upper <- c(confint(lm(value~year, data = may_data[may_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = jun_data[jun_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = jul_data[jul_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = oct_data[oct_data$source == 'in-situ',]), 'year', level = 0.95)[2])

slope_table <- as.data.frame(slope_table)

slope_table <-slope_table %>% 
  pivot_longer(cols = alldata_slope:is_slope_upper, 
               names_to = 'variable',
               values_to = 'value') %>% 
  mutate(dataset = case_when(grepl('all', variable) ~ 'all data',
                             grepl('ls', variable) ~ 'landast',
                             grepl('is', variable) ~ 'in-situ',
                             TRUE ~ ''),
         variable = case_when(grepl('lower', variable) ~ 'lower',
                              grepl('upper', variable) ~ 'upper', 
                              grepl('slope', variable) ~ 'slope',
                              grepl('pval', variable) ~ 'p-value',
                              TRUE ~ ''))

slope_table <- slope_table %>% 
  mutate(value = round(value, digits = 3)) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value')

slope_table

write.csv(slope_table, file.path(figdir, 'Table1_slope_CI.csv'), row.names = F)

#### Figure 5: plot on same axes with separate lines ####
may_is <- may_data %>% 
  filter(source == 'in-situ') 
mean_may_is = mean(may_is$value)
may_ls <- may_data %>% 
  filter(source == 'landsat') 
mean_may_ls = mean(may_ls$value)
may <- temp_monthly_median %>% 
  filter(month == 5) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = 0, 
              intercept = mean_may_is, 
              lty = 2) +
  geom_abline(slope = 0,
              intercept = mean_may_ls,
              lty = 2,
              color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'May') +
  theme(legend.position = 'none')
may

mean_june = mean(jun_data$value)
june <- temp_monthly_median %>% 
  filter(month == 6) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = 0, 
              intercept = mean_june, 
              lty = 2,
              color = '#009E73') +
    final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'June') +
  theme(legend.position = 'none')
june

jul <- temp_monthly_median %>% 
  filter(month == 7) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = summary(all_7_lm)$coefficients[2, 1],
              intercept = summary(all_7_lm)$coefficients[1,1],
              color = '#009E73') +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'July') +
  theme(legend.position = 'none')  
jul

aug <- temp_monthly_median %>% 
  filter(month == 8) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = summary(all_8_lm)$coefficients[2, 1],
              intercept = summary(all_8_lm)$coefficients[1,1],
              color = '#009E73') +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'August') +
  theme(legend.position = 'none')  
aug

sept_is <- sep_data %>% 
  filter(source == 'in-situ') 
mean_sept_is = mean(sept_is$value)
sept <- temp_monthly_median %>% 
  filter(month ==9) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = summary(ls_9_lm)$coefficients[2, 1],
              intercept = summary(ls_9_lm)$coefficients[1,1],
              color = "#E69F00") +
  geom_abline(slope = 0,
              intercept = mean_sept_is,
              lty = 2) +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'September') +
  theme(legend.position = 'none')  
sept

oct <- temp_monthly_median %>% 
  filter(month == 10) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = summary(all_10_lm)$coefficients[2, 1],
              intercept = summary(all_10_lm)$coefficients[1,1],
              color = '#009E73') +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'October') +
  theme(legend.position = 'none')  
oct

forlegend <- temp_monthly_median %>% 
  filter(month == 10) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  final_theme+
  labs(color = 'data source', shape = 'data source') +
  scale_color_colorblind()+
  theme(legend.key.width = unit(1, 'cm'))
leg <- get_legend(forlegend)

for_legline = temp_monthly_median %>% 
  filter(month == 10) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point() +
  geom_smooth(method = 'lm', se = F, aes(color = '#009E73')) +
  geom_smooth(method = 'lm', se = F, aes(color = '#E69F00')) +
  geom_smooth(method = 'lm', se = F, aes(color = '#000000')) +
  theme_bw() +
  scale_color_identity(guide = 'legend',
                       name = 'trend model',
                       breaks = c('#000000', '#E69F00', '#009E73'),
                       labels = c('in-situ', 'landsat','insitu + landsat'))+
  theme(legend.key.width = unit(1, 'cm'))
leg_line = get_legend(for_legline)

for_legdash = temp_monthly_median %>% 
  filter(month == 10) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point() +
  geom_smooth(method = 'lm', se = F, linetype = 'dashed', aes(color = '#009E73')) +
  geom_smooth(method = 'lm', se = F, linetype = 'dashed', aes(color = '#E69F00')) +
  geom_smooth(method = 'lm', se = F, linetype = 'dashed', aes(color = '#000000')) +
  theme_bw() +
  scale_color_identity(guide = 'legend',
                       name = 'no slope model',
                       breaks = c('#000000', '#E69F00', '#009E73'),
                       labels = c('in-situ', 'landsat','insitu + landsat')) +
  theme(legend.key.width = unit(1, 'cm'))
leg_dash = get_legend(for_legdash)

#save title
y_axis_title = ggdraw() + draw_label('median lake surface temperature\n(degrees C)', size = 12, fontface = "bold", angle = 90) 
x_axis_title = ggdraw() + draw_label('year', size = 12, fontface = 'bold')

Fig5 = plot_grid(may, june, jul, aug, sept, oct,
                 labels = c('a', 'b', 'c', 'd', 'e', 'f'),
                 label_size = 10,
                 label_x = 0.05,
                 label_y = 0.97,
          ncol = 3)
Fig5

Fig5_labels = plot_grid(y_axis_title, Fig5,
                        NULL, x_axis_title,
                        ncol = 2, 
                        rel_widths = c(0.05, 0.95),
                        rel_heights = c(0.95, 0.05))

Fig5_labels

legend = plot_grid(NULL, leg, leg_line, leg_dash, NULL,
                   align = 'v', 
                   ncol = 1)

Fig5_labels_leg = plot_grid(Fig5_labels, legend,
                            ncol = 2, 
                            rel_widths = c(0.8, 0.2))
Fig5_labels_leg

ggsave(file.path(figdir, 'Fig5_application_monthly_median_temp_kurtosis_together.jpg'), 
          width=9,
          height=5, 
          units = 'in', 
          dpi = 300)

# Supplemental Figure 2: November data ####
#see if there is a trend
summary(ls_11_lm)
#no statistically significant trend, so no-slope model here
nov_data <- ls_temp_summer_monthly_median_kurtosis %>% 
  filter(month == 11)
nov_mean = mean(nov_data$ls_summer_median_temp_degC)
nov <- ls_temp_summer_monthly_median_kurtosis %>% 
  filter(month == 11) %>% 
  ggplot(., aes(x = year, y = ls_summer_median_temp_degC))+
  geom_point(color = '#E69F00',
             shape = 17) +
  geom_abline(slope = 0,
              intercept = nov_mean,
              linetype = 'dashed',
              color = '#E69F00') +
  labs(x = 'year', 
       y = 'median lake surface temperature\n(dgrees C)', 
       title = 'November') +
  final_theme +
  coord_cartesian(xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  theme(legend.position = 'none')  
nov

legend_nov = plot_grid(leg, leg_line, leg_dash,
                   align = 'v', 
                   ncol = 1)

nov_leg = plot_grid(nov, legend_nov,
                            ncol = 2, 
                            rel_widths = c(0.7, 0.3))
nov_leg

ggsave(file.path(figdir, 'SF2_Nov_mediantemp.jpg'), 
       width=5,
       height=3, 
       units = 'in', 
       dpi = 300)
