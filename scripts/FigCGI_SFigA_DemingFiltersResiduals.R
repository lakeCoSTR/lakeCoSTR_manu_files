# this script creates Figure C, which compares deming regression and residuals by x using varying QAQC filters

# last modified 2021-11-15
# written by B. Steele 

# Set up Workspace ####

# read in libraries and functions
source('scripts/R_library.R')

# point to directories
datadir = '~/GitHub/ids-ne-lakes/data/'
C1_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C1/'
C2_datadir <- '~/GitHub/ids-ne-lakes/data/colab-output/C2/'
fig_dir <- '~/GitHub/ids-ne-lakes/figures/'

# Read in Data ####

# read in C1 single-channel and C2 surface temperature data
C1SC <- read_csv(file.path(C1_datadir, 'C1_temp_landsat_paired.csv')) %>% 
  mutate(date = as.Date(substrRight(`system:index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system:index`) ~ 'LS 5',
                               grepl('LT04', `system:index`) ~ 'LS 4',
                               grepl('LE07', `system:index`) ~ 'LS 7',
                               grepl('LC08', `system:index`) ~ 'LS 8',
                               TRUE ~ NA_character_)) %>% 
  mutate(collection = 1,
         month = format(date, '%m'),
         doy = format(date, '%j')) %>% 
  select(-X1)
head(C1SC)

# rename LS-related columns
C1SC <- C1SC %>% 
  rename(earthsun_d = esd,
         sunazi = azimuth,
         sunelev = elev,
         is_temp_avg = temp_avg,
         is_temp_stdev = t_stdev,
         is_depth_avg = depth_avg,
         is_depth_stdev = d_stdev,
         is_temp_med = temp_med)
head(C1SC)

# Collection 2 data
C2ST <- read_csv(file.path(C2_datadir, 'C2_v2_temp_landsat_paired.csv')) %>% 
  mutate(date = as.Date(substrRight(`system:index`, 8), format = '%Y%m%d')) %>% 
  mutate(LSmission = case_when(grepl('LT05', `system:index`) ~ 'LS 5',
                               grepl('LT04', `system:index`) ~ 'LS 4',
                               grepl('LE07', `system:index`) ~ 'LS 7',
                               grepl('LC08', `system:index`) ~ 'LS 8',
                               TRUE ~ NA_character_)) %>% 
  mutate(collection = 2,
         month = format(date, '%m'),
         doy = format(date, '%j'))
head(C2ST)


#load in all high-frequency insitu data for historical data analysis
insitu <- read.csv(paste0(datadir, 'insitu_temp_data_v2021-10-20.csv')) %>% 
  filter(!is.na(lat_dd)) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+5'))

# Summarize in-situ to define spread and IQR limits ####

# get range per day during hours of flyover
range_per_date <- insitu %>% 
  mutate(hour = as.numeric(format(datetime, '%H')),
         date = as.Date(datetime)) %>% 
  filter(hour >= 9 & hour < 11) %>% 
  group_by(date) %>% 
  summarize(temp_range = max(temp_degC) - min(temp_degC),
            IQR = IQR(temp_degC, na.rm = T),
            n_locs = length(unique(location)))

#save max observed spread and IQR for further filtering
max_spread <- max(range_per_date$temp_range, na.rm = T)
max_IQR <- max(range_per_date$IQR, na.rm = T)

# Flag C2 data for freezing temps, spread P/F, IQR P/F ####
C2ST <- C2ST %>% 
  mutate(freeze_QAQC = case_when(surface_temp_min < 0 ~ 'F', #pass/fail for freezing temps
                                 TRUE ~ 'P')) %>% 
  mutate(temp_spread = round(surface_temp_max, digits = 1) - round(surface_temp_min, digits = 1)) %>% #calc temp spread; pass/fail for temp spread
  mutate(spread_QAQC = case_when(temp_spread < max_spread*1.1 ~ 'P',
                                 TRUE ~ 'F')) %>% 
  mutate(IQR = round(surface_temp_p75, digits = 1) - round(surface_temp_p25, digits = 1))  %>%  #calc IQR; pass/fail for temp IQR
  mutate(IQR_QAQC = case_when(IQR < max_IQR*1.1 ~ 'P',
                              TRUE ~ 'F'))
write.csv(C2ST, file.path(C2_datadir,paste0('sunapee_paired_C2_QAQCflag_v', Sys.Date(), '.csv')), row.names = F)
                       
C1SC_kurtosis <- C1SC %>% 
  filter(surface_temp_kurtosis >2)

C1SC_kurtosis %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

C2ST_kurtosis <- C2ST %>% 
  filter(surface_temp_kurtosis > 2)

C2ST_kurtosis %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

# Deming Regressions ####

# deming regression for C1
C1_deming = deming::deming(C1SC$surface_temp_median ~ C1SC$is_temp_med)
C1_deming_forresid = mcreg(x = C1SC$is_temp_med, y = C1SC$surface_temp_median, method.reg = 'Deming')
C1SC$opt_resid = MCResult.getResiduals(C1_deming_forresid)$optimized
C1SC$filter = 'none'

C1_deming
cor(C1SC$surface_temp_median, C1SC$is_temp_med)

#deming regress for C1 with kurtosis >= 2
C1_kurtosis_deming = deming::deming(C1SC_kurtosis$surface_temp_median ~ 
                                      C1SC_kurtosis$is_temp_med)
C1_kurtosis_deming_forresid = mcreg(x = C1SC_kurtosis$is_temp_med, 
                                    y = C1SC_kurtosis$surface_temp_median, 
                                    method.reg = 'Deming')
C1SC_kurtosis$opt_resid = MCResult.getResiduals(C1_kurtosis_deming_forresid)$optimized
C1SC_kurtosis$filter = 'kurtosis'

C1_kurtosis_deming
cor(C1SC_kurtosis$surface_temp_median, C1SC_kurtosis$is_temp_med)


# deming regression for C2
C2_deming = deming::deming(C2ST$surface_temp_median ~ C2ST$is_temp_med)
C2_deming_forresid = mcreg(x = C2ST$is_temp_med, y = C2ST$surface_temp_median, method.reg = 'Deming')
C2ST$opt_resid = MCResult.getResiduals(C2_deming_forresid)$optimized
C2ST$filter = 'none'

C2_deming
cor(C2ST$surface_temp_median, C2ST$is_temp_med)

#deming regress for C2 with kurtosis >= 2
C2_kurtosis_deming = deming::deming(C2ST_kurtosis$surface_temp_median ~ 
                                   C2ST_kurtosis$is_temp_med)
C2_kurtosis_deming_forresid = mcreg(x = C2ST_kurtosis$is_temp_med, 
                                 y = C2ST_kurtosis$surface_temp_median, 
                                 method.reg = 'Deming')
C2ST_kurtosis$opt_resid = MCResult.getResiduals(C2_kurtosis_deming_forresid)$optimized
C2ST_kurtosis$filter = 'kurtosis'

C2_kurtosis_deming
cor(C2ST_kurtosis$surface_temp_median, C2ST_kurtosis$is_temp_med)

# Join all Deming data ####
all_surface_temp <- full_join(C1SC, C2ST) %>% 
  full_join(., C1SC_kurtosis) %>% 
  full_join(., C2ST_kurtosis) %>% 
  mutate(collection = as.factor(collection),
         filter = as.factor(filter))

# Figure C: Plot Deming regression for all filters ####
FigC_a <- ggplot(C1SC, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C1
  geom_abline(intercept = C1_deming$coefficients[1], slope = C1_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,1], slope = C1_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,2], slope = C1_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C1SC$surface_temp_median, C1SC$is_temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C1SC$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = '\n',
       title = 'Collection 1',
       subtitle = 'single-channel algorithm') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_a

FigC_b <- ggplot(C2ST, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2
  geom_abline(intercept = C2_deming$coefficients[1], slope = C2_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,1], slope = C2_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,2], slope = C2_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST$surface_temp_median, C2ST$is_temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = '\n',
       title = 'Collection 2',
       subtitle = 'surface temperature product') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_b

FigC_c <- ggplot(C1SC_kurtosis, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and kurtosis
  geom_abline(intercept = C1_kurtosis_deming$coefficients[1], slope = C1_kurtosis_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C1_kurtosis_deming$ci[1,1], slope = C1_kurtosis_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C1_kurtosis_deming$ci[1,2], slope = C1_kurtosis_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C1SC_kurtosis$surface_temp_median, C1SC_kurtosis$is_temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C1SC_kurtosis$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = '\n',
       title = 'Collection 1',
       subtitle = 'kurtosis filter') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_c

FigC_d <- ggplot(C2ST_kurtosis, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and kurtosis
  geom_abline(intercept = C2_kurtosis_deming$coefficients[1], slope = C2_kurtosis_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_kurtosis_deming$ci[1,1], slope = C2_kurtosis_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_kurtosis_deming$ci[1,2], slope = C2_kurtosis_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST_kurtosis$surface_temp_median, C2ST_kurtosis$is_temp_med), digits = 3)),
            x = 2,
            y = 25,
            size = 4,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST_kurtosis$date)),
            x = 2,
            y = 23,
            size = 4,
            hjust = 0)+
  labs(x = '',
       y = '\n',
       title = 'Collection 2',
       subtitle = 'kurtosis filter') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
FigC_d


FigC_plot <- plot_grid(FigC_a, FigC_b, FigC_c, FigC_d,
                   ncol = 2,
                   labels = c('a', 'b', 'c', 'd'),
                   label_y = 0.9,
                   label_x = 0.1)

FigC_plot

x_lab = ggdraw() + draw_label(label = expression(bold(paste(italic('in-situ'), ' median water temp (deg C)'))),
                      fontface = 'bold')

y_lab = ggdraw() + draw_label(label = 'Landsat median water temp (deg C)',
                              fontface = 'bold',
                              angle =90)

FigC_label = plot_grid(y_lab, FigC_plot,
                       NULL, x_lab,
                       ncol = 2,
                       rel_widths = c(0.05, 1.1),
                       rel_heights = c(1.1, 0.05))

FigC_label

ggsave(file.path(fig_dir, 'FigureC_deming_filters.jpg'), 
       dpi = 300,
       height = 10,
       width = 10,
       units = 'in')

# Supplemental Figure A: Look at C2 kurtosis data set residuals against other variables ####

# by insitu temp
istemp <- ggplot(C2ST_kurtosis, aes(x = is_temp_med, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'in-situ median water temp\n(deg C)',
       y = '\n') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  final_theme
istemp

# by doy
doy <- ggplot(C2ST_kurtosis, aes(x = as.numeric(doy), y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'day of year\n',
       y = '\n') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  final_theme
doy

# by pixel count
perclake <- ggplot(C2ST_kurtosis, aes(x = (pixel_count/17134)*100, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  labs(x = 'percent of lake\n',
       y = '\n') +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  final_theme
perclake

# by kurtosis cover
cloud <- ggplot(C2ST_kurtosis, aes(x = cloud_cover, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'cloud cover (percent)\n',
       y = '\n') +
  final_theme 
cloud

# by sun elevation
sunelev <- ggplot(C2ST_kurtosis, aes(x = elev, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'sun elevation (UNITS)\n',
       y = 'Deming-optimized residual\n(deg C)') +
  final_theme
sunelev

# by earth sun distance
esd <- ggplot(C2ST_kurtosis, aes(x = esd, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'earth sun distance (UNITS)\n',
       y = '\n') +
  final_theme 
esd

# by azimuth
azi <- ggplot(C2ST_kurtosis, aes(x = azimuth, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'sun azimuth (degrees)\n',
       y = '\n') +
  final_theme 
azi

# depth of sensor
depth <- ggplot(C2ST_kurtosis, aes(x = is_depth_avg, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'average depth of sensor (m)\n',
       y = '\n') +
  final_theme 
depth

# std dev in-situ temp
sd <- ggplot(C2ST_kurtosis, aes(x = is_temp_stdev, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'standard deviation of temps\ncontributing to median (deg C)',
       y = '\n') +
  final_theme 
sd

#in-situ count
count <- ggplot(C2ST_kurtosis, aes(x = insitu_count, y = opt_resid)) +
  geom_point() +
  # geom_line(aes(group = date)) +
  geom_abline(intercept =  0, slope = 0) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = 'number of insitu measurements\ncontributing to median',
       y = '\n') +
  final_theme 
count

plot_grid(istemp, doy, perclake, cloud, sunelev, esd, azi, depth, sd, count,
          labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
          ncol = 2,
          label_x = 0.07,
          label_y = 1)

ggsave(file.path(fig_dir, 'SFA_resid_summary_C2kurtosis_v12Nov2021.jpg'), 
       height = 12, width = 8, units = 'in', dpi = 300)



# Calculate error for various models ####
alldata_error <- all_surface_temp %>% 
  group_by(collection, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  mutate(month = 'All data',
         LSmission = 'All missions') %>% 
  pivot_longer(cols = c(mse:n_obs), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values) 

mission_error <- all_surface_temp %>% 
  group_by(collection, LSmission, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  mutate(month = 'All data') %>% 
  pivot_longer(cols = c(mse:n_obs), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values) 

alldata_error <- full_join(alldata_error, mission_error)
alldata_error

month_mission_error <- all_surface_temp %>% 
  group_by(collection, month, LSmission, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  select(month, collection, LSmission, filter,n_obs, mae, rmse, bias) %>% 
  pivot_longer(!c(month, LSmission, collection, filter), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values)

month_error <- all_surface_temp %>% 
  group_by(collection, month, filter) %>% 
  summarise(mse = round(sum((opt_resid^2))/n(), digits = 2),
            rmse = round(sqrt(mse), digits = 2),
            bias = round(sum(opt_resid)/n(), digits = 2),
            mae = round(sum(abs(opt_resid))/n(), digits = 2),
            n_obs = n())  %>% 
  mutate(LSmission = 'All missions') %>% 
  select(month, collection, LSmission, filter, n_obs, mae, rmse, bias) %>% 
  pivot_longer(!c(month, LSmission, filter, collection), names_to = 'variable', values_to = 'values') %>% 
  mutate(variable = factor(variable, levels = c('n_obs', 'mae', 'rmse', 'mse', 'bias'))) %>% 
  arrange(month, variable) %>% 
  pivot_wider(names_from = c(LSmission),
              values_from = values)

month_error <- full_join(month_mission_error, month_error)

alldata_error <- full_join(alldata_error, month_error)

write.csv(alldata_error, file.path(C2_datadir, 'LS_deming_predictionerror_C1C2_stats_v15Nov2021.csv'))

# FigGI create point (by mission for bias, mae, rmse) faceted by model ####

head(alldata_error)

missionmonth_biasmae <- alldata_error %>% 
  filter(month != 'All data' & (variable == 'mae' | variable == 'bias')) %>% 
  select(-`All missions`) %>% 
  filter(collection == 2) %>% 
  pivot_longer(cols = c(`LS 5`:`LS 8`),
               names_to = 'mission',
               values_to = 'value') %>% 
  mutate(c_filter = paste0('C', collection, ' ', filter)) %>% 
  mutate(c_filter = factor(c_filter, 
                           levels = c('C2 none', 'C2 kurtosis'),
                           labels = c('C2', 'C2 kurtosis')))



ggplot(missionmonth_biasmae, aes(x = month, y = value)) +
  geom_point(aes(color = mission)) +
  facet_grid(c_filter ~ variable)+
  final_theme+
  scale_color_colorblind()

month <- (c('05', '06', '07', '08', '09', '10', '11'))
month_list <- as.data.frame(month)

FigGI_a <- missionmonth_biasmae %>% 
  filter(variable == 'bias' & c_filter == 'C2') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  labs(x = '',
       y = '') +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  coord_cartesian(ylim = c(-2, 2)) +
  final_theme+
  theme(axis.title=element_text(size=10,face="bold")) +
  theme(legend.position = 'none') +
  scale_color_colorblind()
FigGI_a

FigGI_c <- missionmonth_biasmae %>% 
  filter(variable == 'bias' & c_filter == 'C2 kurtosis') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  labs(x = '',
       y = '') +
  final_theme+
  coord_cartesian(ylim = c(-2, 2)) +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  theme(legend.position = 'none') +
  theme(axis.title=element_text(size=10,face="bold")) +
  scale_color_colorblind()
FigGI_c

FigGI_b <- missionmonth_biasmae %>% 
  filter(variable == 'mae' & c_filter == 'C2') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  coord_cartesian(ylim = c(0, 2)) +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  labs(x = '',
       y = '') +
  final_theme+
  theme(axis.title=element_text(size=10,face="bold")) +
  theme(legend.position = 'none') +
  scale_color_colorblind()
FigGI_b

FigGI_d <- missionmonth_biasmae %>% 
  filter(variable == 'mae' & c_filter == 'C2 kurtosis') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  coord_cartesian(ylim = c(0, 2)) +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  labs(x = '',
       y = '') +
  final_theme+
  theme(axis.title=element_text(size=10,face="bold")) +
  theme(legend.position = 'none') +
  scale_color_colorblind()
FigGI_d

forlegend <- missionmonth_biasmae %>% 
  filter(variable == 'mae' & c_filter == 'C2 kurtosis') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  coord_cartesian(ylim = c(0, 2)) +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  theme(legend.position = 'bottom') +
  labs(x = 'month',
       y = '\n') +
  final_theme+
  scale_color_colorblind()
leg <- get_legend(forlegend)
leg


figure_title_bias <- ggdraw() + draw_label('   bias', fontface = 'bold')
figure_title_mae <- ggdraw() + draw_label('   mean absolute error', fontface = 'bold')
figure_title <- plot_grid(figure_title_bias, figure_title_mae, nrow = 1)
figure_title

nofilter <- plot_grid(FigGI_a, FigGI_b,
                      labels = c('a', 'b'),
                      ncol = 2)
nofilter_title <- ggdraw() + draw_label("Collection 2", fontface='bold')
nofilter <- plot_grid(nofilter_title, nofilter,
                      nrow = 2,
                      rel_heights = c(0.1, 1))
nofilter

kurtosis <- plot_grid(FigGI_c, FigGI_d,
                      labels = c('c', 'd'),
                      ncol = 2)
kurtosis_title <- ggdraw() + draw_label("Collection 2 - kurtosis", fontface='bold')
kurtosis <- plot_grid(kurtosis_title, kurtosis,
                 nrow = 2,
                 rel_heights = c(0.1, 1))
kurtosis

x_lab = ggdraw() + draw_label(label = 'month', fontface = 'bold')
y_lab = ggdraw() + draw_label(label = 'water temperature (deg C)',
                              angle = 90,
                              fontface = 'bold')

FigGI <- plot_grid(figure_title,
                   nofilter,
                   kurtosis,
                   rel_heights = c(0.05, 1, 1),
                   ncol =1)
FigGI

FigGI_label = plot_grid(y_lab, FigGI,
                        NULL, x_lab,
                        rel_heights = c(1.1, 0.05),
                        rel_widths = c(0.05, 1.1))

FigGI_label

plot_grid(FigGI_label, leg, rel_widths = c(0.9,0.1 ))

ggsave(file.path(fig_dir, 'FigGI_errorbymission.jpg'), 
       height = 6, width = 8, units = 'in', dpi = 300)

