# application of the data, examining trends from lspa 200-230 to see if they are similar to Landsat-derived data

source('scripts/R_library.R')

figdir = 'figures_v1_12/'
datadir = 'data/'

#Load lspa data from git hub

lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

lmp_temp_deep <- lmp %>% 
  filter(parameter == 'temp_C') %>% 
  filter(station == 200 | station ==210 | station == 220| station==230) %>% 
  filter(depth_m == 0.5) 

# load all LS data, filter for freeze and cloud cover
ls <- read.csv(file.path(datadir, 'colab-output/C2_v1_12/sunapee_v1_12_temp_stats.csv')) %>% 
  mutate(date = substrRight(`system.index`, 8),
         date = as.Date(date, format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system.index`) ~ 'LS 5',
                               grepl('LT04', `system.index`) ~ 'LS 4',
                               grepl('LE07', `system.index`) ~ 'LS 7',
                               grepl('LC08', `system.index`) ~ 'LS 8',
                               TRUE ~ NA_character_)) 
ls_kurtosis <- ls %>% 
  filter(surface_temp_kurtosis >2 )

#### whole lake median by month and year Jul-Sept ####
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
  filter((month >=7 & month <= 9)) 

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
                            grepl('ls_', source) ~ 'Landsat',
                            TRUE ~ NA_character_)) %>% 
  filter(!is.na(value))

temp_monthly_median %>% 
  group_by(month) %>% 
  ggplot(., aes(x = year, y = value)) +
  geom_point(aes(color = source)) +
  # geom_line(aes(color = source)) +
  facet_grid(.~month) +
  labs(y = 'lake surface temperature\n(°C)') +
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

is_7_lm <- linear_analysis(7, 'in-situ')
is_8_lm <- linear_analysis(8, 'in-situ')
is_9_lm <- linear_analysis(9, 'in-situ')
ls_7_lm <- linear_analysis(7, 'Landsat')
ls_8_lm <- linear_analysis(8, 'Landsat')
ls_9_lm <- linear_analysis(9, 'Landsat')


# linear model for all data together

linear_analysis_all = function(mm){
  data <- temp_monthly_median %>% 
    filter(month == mm)
  lm_result <- lm(data$value ~ data$year)
  print(lm_result)
}

all_7_lm <- linear_analysis_all(7)
all_8_lm <- linear_analysis_all(8)
all_9_lm <- linear_analysis_all(9)


# indicator variable analysis ####
temp_monthly_median$source = as.factor(temp_monthly_median$source)

source_contrasts <- contrasts(temp_monthly_median$source)

jul_data <- temp_monthly_median %>% 
  filter(month == 7)
aug_data <- temp_monthly_median %>% 
  filter(month == 8)
sep_data <- temp_monthly_median %>% 
  filter(month == 9)

# iva and multivariate linear models ####

## JULY ####
#IVA
lm_source_jul_iva <- lm(value ~ year+source+year*source, data = jul_data)
summary(lm_source_jul_iva)
#test for autocorrelation within residuals
car::durbinWatsonTest(lm_source_jul_iva)
# p>0.05 no autocorrelation, continue!
summary(lm_source_jul_iva)
# no sig diff in slope
lm_source_jul <- lm(value ~ year+source, data = jul_data)
#test for autocorrelatoin within residuals
car::durbinWatsonTest(lm_source_jul)
#P>0.05, no autocorrelation, continue!
summary(lm_source_jul)
# no sig diff in intercept; intercept and slope are sig
summary(all_7_lm)
#test for autocorrelation within residuals
car::durbinWatsonTest(all_7_lm)
# no serial autocorrelation (p-value is >0.05). continue!

#slope and intercept are significant across both datasets; plot one line for the two groups of data


## AUG ####
#IVA
lm_source_aug_iva <- lm(value ~ year+source+year*source, data = aug_data)
#test for serial autocorrelation
car::durbinWatsonTest(lm_source_aug_iva)
#no autocorrelation, continue!
summary(lm_source_aug_iva)
# no sig diff in slope
lm_source_aug <- lm(value ~ year+source, data = aug_data)
car::durbinWatsonTest(lm_source_aug)
# no autocorrelation, continue!
summary(lm_source_aug)
#no sig diff in intercept; intercept and slope sig

#plot one line for both datasets
car::durbinWatsonTest(all_8_lm)
#no autocorrelation, coninue!
summary(all_8_lm)


## SEPT ####

#IVA
lm_source_sep_iva <- lm(value ~ year+source+year*source, data = sep_data)
#test for autocorrelation
car::durbinWatsonTest(lm_source_sep_iva)
#no autocorrlation, continute!
summary(lm_source_sep_iva)
# there is a significant difference in slope, plot separately

#perform Durbin Watson to test for serial autocorrelation in subset
car::durbinWatsonTest(is_9_lm)
# no serial autocorrelation (p-value is >0.05). continue with linear model
summary(is_9_lm)
#no slope model

#perform Durbin Watson to test for serial autocorrelation in subset
car::durbinWatsonTest(ls_9_lm)
# no serial autocorrelation (p-value is >0.05). continue with linear model
summary(ls_9_lm)
#plot one line for Landsat


# IVA results to table ####
iva_table <- NULL

iva_table$month <- c('July', 'August', 'September')

iva_table$iva_pval <- c(summary(lm_source_jul_iva)$coefficients[4,4],
                        summary(lm_source_aug_iva)$coefficients[4,4],
                        summary(lm_source_sep_iva)$coefficients[4,4])

iva_table$multreg_pval <- c(summary(lm_source_jul)$coefficients[3,4],
                           summary(lm_source_aug)$coefficients[3,4],
                           summary(lm_source_sep)$coefficients[3,4])

iva_table <- as.data.frame(iva_table) 
iva_table <- iva_table %>% 
  mutate_at(vars(iva_pval, multreg_pval),
            ~ round(., digits = 3))

# gather slope and CI data

slope_table <- NULL

slope_table$month <- c('July', 'August', 'September')

slope_table$alldata_slope <- c(summary(all_7_lm)$coefficients[2,1],
                               summary(all_8_lm)$coefficients[2,1],
                               summary(all_9_lm)$coefficients[2,1])

slope_table$alldata_slope_lower <- c(confint(lm(value~year, data = jul_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = aug_data),'year',level=0.95)[1],
                                   confint(lm(value~year, data = sep_data),'year',level=0.95)[1])
slope_table$alldata_slope_upper <- c(confint(lm(value~year, data = jul_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = aug_data),'year',level=0.95)[2],
                                   confint(lm(value~year, data = sep_data),'year',level=0.95)[2])

slope_table$ls_slope <- c(summary(ls_7_lm)$coefficients[2,1],
                          summary(ls_8_lm)$coefficients[2,1],
                          summary(ls_9_lm)$coefficients[2,1])

slope_table$ls_slope_lower <- c(confint(lm(value~year, data = jul_data[jul_data$source == 'Landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'Landsat',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'Landsat',]), 'year', level = 0.95)[1])

slope_table$ls_slope_upper <- c(confint(lm(value~year, data = jul_data[jul_data$source == 'Landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'Landsat',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'Landsat',]), 'year', level = 0.95)[2])

slope_table$is_slope <- c(summary(is_7_lm)$coefficients[2,1],
                          summary(is_8_lm)$coefficients[2,1],
                          summary(is_9_lm)$coefficients[2,1])

slope_table$is_slope_lower <- c(confint(lm(value~year, data = jul_data[jul_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'in-situ',]), 'year', level = 0.95)[1],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'in-situ',]), 'year', level = 0.95)[1])

slope_table$is_slope_upper <- c(confint(lm(value~year, data = jul_data[jul_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = aug_data[aug_data$source == 'in-situ',]), 'year', level = 0.95)[2],
                              confint(lm(value~year, data = sep_data[sep_data$source == 'in-situ',]), 'year', level = 0.95)[2])

slope_table <- as.data.frame(slope_table)

slope_table <-slope_table %>% 
  pivot_longer(cols = alldata_slope:is_slope_upper, 
               names_to = 'variable',
               values_to = 'value') %>% 
  mutate(dataset = case_when(grepl('all', variable) ~ 'all data',
                             grepl('ls', variable) ~ 'Landsat',
                             grepl('is', variable) ~ 'in situ',
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

jul <- temp_monthly_median %>% 
  filter(month == 7) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  geom_abline(slope = summary(all_7_lm)$coefficients[2, 1],
              intercept = summary(all_7_lm)$coefficients[1,1],
              color = '#009E73') +
  final_theme +
  coord_cartesian(ylim = c(14, 26),
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
  coord_cartesian(ylim = c(14, 26),
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
  coord_cartesian(ylim = c(14, 26),
                  xlim = c(1980, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'September') +
  theme(legend.position = 'none')  
sept


forlegend <- temp_monthly_median %>% 
  filter(month == 9) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source, shape = source)) +
  final_theme+
  labs(color = 'data source', shape = 'data source') +
  scale_color_colorblind(guide = 'legend',
                         name = 'data source', 
                         labels = c(expression(paste(italic('in situ'))), 'Landsat'))+
  scale_shape_manual(guide = 'legend',
                     name = 'data source',
                    values = c(16,17),
                     labels = c(expression(paste(italic('in situ'))), 'Landsat'))+
  theme(legend.key.width = unit(1, 'cm'))
leg <- get_legend(forlegend)

for_legline = temp_monthly_median %>% 
  filter(month == 9) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point() +
  geom_smooth(method = 'lm', se = F, aes(color = '#009E73')) +
  geom_smooth(method = 'lm', se = F, aes(color = '#E69F00')) +
  theme_bw() +
  scale_color_identity(guide = 'legend',
                       name = 'trend model',
                       breaks = c('#E69F00', '#009E73'),
                       labels = c('Landsat', expression(paste(italic('in situ'), ' + Landsat'))))+
  theme(legend.key.width = unit(1, 'cm'))
leg_line = get_legend(for_legline)

for_legdash = temp_monthly_median %>% 
  filter(month == 9) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point() +
  geom_smooth(method = 'lm', se = F, linetype = 'dashed', aes(color = '#000000')) +
  theme_bw() +
  scale_color_identity(guide = 'legend',
                       name = 'no slope model',
                       breaks = c('#000000'),
                       labels = c(expression(paste(italic('in situ')))))+
  theme(legend.key.width = unit(1, 'cm'))
leg_dash = get_legend(for_legdash)

#save title
y_axis_title = ggdraw() + draw_label('median lake surface temperature\n(°C)', size = 12, fontface = "bold", angle = 90) 
x_axis_title = ggdraw() + draw_label('year', size = 12, fontface = 'bold')

Fig5 = plot_grid(jul, aug, sept,
                 labels = c('a', 'b', 'c'),
                 label_size = 10,
                 label_x = 0.05,
                 label_y = 0.97,
          ncol = 3)
Fig5

Fig5_labels = plot_grid(y_axis_title, Fig5,
                        NULL, x_axis_title,
                        ncol = 2, 
                        rel_widths = c(0.1, 0.95),
                        rel_heights = c(0.95, 0.1))

Fig5_labels

legend = plot_grid(leg, leg_line, leg_dash,
                   align = 'v', 
                   ncol = 1)
legend

Fig5_labels_leg = plot_grid(Fig5_labels, legend,
                            ncol = 2, 
                            rel_widths = c(0.8, 0.2))
Fig5_labels_leg

ggsave(file.path(figdir, 'Fig5_application_monthly_median_temp_kurtosis_together.jpg'), 
          width=8,
          height=3, 
          units = 'in', 
          dpi = 600,
       bg = 'white')
