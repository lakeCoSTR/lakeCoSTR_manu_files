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

# load QAQC LS data
ls <- read.csv('data/LS_passQAQC_v19July2021.csv') %>% 
  mutate(date = as.Date(date))


#### whole lake median by month ####
lmp_temp_monthly_median <- lmp_temp_deep %>% 
  mutate(month = as.numeric(format(as.Date(date), '%m')),
         year = as.numeric(format(as.Date(date), '%Y'))) %>% 
  group_by(year, month) %>% 
  summarise(is_summer_median_temp_degC = median(value),
            is_n_obs = length(value)) %>% 
  filter(month !=3)

ggplot(lmp_temp_monthly_median, aes(x=year, y = is_summer_median_temp_degC)) +
  facet_grid(.~month) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

ls_temp_summer_monthly_median <- ls %>% 
  mutate(month = as.numeric(format((date), '%m')),
         year = as.numeric(format((date), '%Y'))) %>% 
  group_by(year, month) %>% 
  summarise(ls_summer_median_calib_temp_degC = median(calib_med_temp_degC),
            ls_n_obs = length(surface_temp_median))  %>% 
  filter(!is.na(month))

ggplot(ls_temp_summer_monthly_median, aes(x=year, y = ls_summer_median_calib_temp_degC)) +
  facet_grid(.~month) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)


df1 <- lmp_temp_monthly_median %>% 
  select(year, month, is_summer_median_temp_degC)

df2 <- ls_temp_summer_monthly_median %>% 
  select(year, month, ls_summer_median_calib_temp_degC)

temp_monthly_median <- full_join(df1, df2) %>% 
  pivot_longer(., cols = c(ls_summer_median_calib_temp_degC, is_summer_median_temp_degC), names_to='source', values_to = 'value') %>% 
  mutate(source = case_when(grepl('is_', source) ~ 'in-situ',
                            grepl('ls_', source) ~ 'landsat',
                            TRUE ~ NA_character_)) %>% 
  filter(!is.na(value))

temp_monthly_median %>% 
  group_by(month) %>% 
  ggplot(., aes(x = year, y = value)) +
  geom_point() +
  facet_grid(source~month) +
  geom_smooth(method = 'lm', se = F) +
  labs(y = 'lake surface temperature (degrees C)') +
  final_theme
ggsave(file.path(figdir, 'monthly_patterns_calib_temp_by_source.png'), height = 8, width = 12)

temp_monthly_median %>% 
  group_by(month) %>% 
  ggplot(., aes(x = year, y = value)) +
  geom_point(aes(color = source)) +
  facet_grid(.~month) +
  labs(y = 'lake surface temperature\n(degrees C)') +
  geom_smooth(method = 'lm', se = F, inherit.aes = T) +
  final_theme
ggsave(file.path(figdir, 'monthly_patterns_calib_temp.png'), height = 3, width = 9)


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

grom_lm = function(mm, ds){
  data <- temp_monthly_median %>% 
    filter(month == mm & source == ds)
  lm_result <- lm(data$value ~ data$year)
  if(is.na(summary(lm_result)$coefficients[2,4])) {
    grom <- ggplot(data, aes(x = year, y = value)) +
      geom_point() +
      coord_cartesian(ylim = c(7, 27),
                      xlim = c(1985, 2020)) +
      labs(x = NULL, y = NULL) +
      final_theme
  } else {
    if(summary(lm_result)$coefficients[2,4] < 0.05){ 
      grom <- ggplot(data, aes(x = year, y = value)) +
        geom_point() +
        coord_cartesian(ylim = c(7, 27),
                        xlim = c(1985, 2020)) +
        geom_abline(slope = summary(lm_result)$coefficients[2, 1],
                    intercept = summary(lm_result)$coefficients[1,1]) +
        labs(x = NULL, y = NULL) +
        final_theme
    } else {
      grom <- ggplot(data, aes(x = year, y = value)) +
        geom_point() +
        coord_cartesian(ylim = c(7, 27),
                        xlim = c(1985, 2020)) +
        labs(x = NULL, y = NULL) +
        final_theme
    }
  }
  print(grom)
}


is_5_gg <- grom_lm(5, 'in-situ')
is_6_gg <- grom_lm(6, 'in-situ')
is_7_gg <- grom_lm(7, 'in-situ')
is_8_gg <- grom_lm(8, 'in-situ')
is_9_gg <- grom_lm(9, 'in-situ')
is_10_gg <- grom_lm(10, 'in-situ')
ls_5_gg <- grom_lm(5, 'landsat')
ls_6_gg <- grom_lm(6, 'landsat')
ls_7_gg <- grom_lm(7, 'landsat')
ls_8_gg <- grom_lm(8, 'landsat')
ls_9_gg <- grom_lm(9, 'landsat')
ls_10_gg <- grom_lm(10, 'landsat')


#set up png device
png(file.path(figdir, 'application_monthly_median_temp_bysource.png'),
    width=16,height=6, units = 'in', res = 300)
gridExtra::grid.arrange(is_5_gg, is_6_gg, is_7_gg, is_8_gg, is_9_gg, is_10_gg,
                        ls_5_gg, ls_6_gg, ls_7_gg, ls_8_gg, ls_9_gg, ls_10_gg,
                        nrow = 2,
                        left = 'lake surface temperature\ndegrees C')

#Close pdf graphics device
dev.off()

#### plot on same axes with separate lines ####
may <- temp_monthly_median %>% 
  filter(month == 5) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1985, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'May') +
  theme(legend.position = 'none')

june <- temp_monthly_median %>% 
  filter(month == 6) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1985, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'June') +
  theme(legend.position = 'none')

jul <- temp_monthly_median %>% 
  filter(month == 7) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  geom_abline(slope = is_7_lm$coefficients[2, 1],
              intercept = is_7_lm$coefficients[1,1],
              color = 'black') +
  geom_abline(slope = ls_7_lm$coefficients[2, 1],
              intercept = ls_7_lm$coefficients[1,1],
              color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1985, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'Jul') +
  theme(legend.position = 'none')  

aug <- temp_monthly_median %>% 
  filter(month == 8) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  geom_abline(slope = is_8_lm$coefficients[2, 1],
              intercept = is_8_lm$coefficients[1,1],
              color = 'black') +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1985, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'August') +
  theme(legend.position = 'none')  

sept <- temp_monthly_median %>% 
  filter(month == 9) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  geom_abline(slope = ls_9_lm$coefficients[2, 1],
              intercept = ls_9_lm$coefficients[1,1],
              color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1985, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'September') +
  theme(legend.position = 'none')  

oct <- temp_monthly_median %>% 
  filter(month == 10) %>% 
  ggplot(., aes(x = year, y = value))+
  geom_point(aes(color = source)) +
  geom_abline(slope = ls_10_lm$coefficients[2, 1],
              intercept = ls_10_lm$coefficients[1,1],
              color = "#E69F00") +
  final_theme +
  coord_cartesian(ylim = c(7, 27),
                  xlim = c(1985, 2020)) +
  scale_color_colorblind() +
  labs(x = NULL, y = NULL, title = 'October') +
  theme(legend.position = 'none')  

#set up png device
png(file.path(figdir, 'application_monthly_median_temp_together.png'),
    width=8,height=6, units = 'in', res = 300)
gridExtra::grid.arrange(may, june, jul, aug, sept, oct,
                        nrow = 2,
                        left = 'lake surface temperature\ndegrees C',
                        bottom = 'black = in-situ   yellow = landsat')

#Close pdf graphics device
dev.off()
