# this script creates Figures in the Herrick, Steele, et al MS

# last modified 2022-01-14
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
         doy = format(date, '%j'))
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

#create filtered dataframes
C1SC_kurtosis <- C1SC %>% 
  filter(surface_temp_kurtosis >2)

C2ST_kurtosis <- C2ST %>% 
  filter(surface_temp_kurtosis > 2)

#summarize datasets
C1SC %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

C2ST %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

C1SC_kurtosis %>% 
  group_by(LSmission) %>% 
  summarize(count = length(LSmission))

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

# #deming regress for C1 with kurtosis >= 2
# C1_kurtosis_deming = deming::deming(C1SC_kurtosis$surface_temp_median ~ 
#                                       C1SC_kurtosis$is_temp_med)
# C1_kurtosis_deming_forresid = mcreg(x = C1SC_kurtosis$is_temp_med, 
#                                     y = C1SC_kurtosis$surface_temp_median, 
#                                     method.reg = 'Deming')
# C1SC_kurtosis$opt_resid = MCResult.getResiduals(C1_kurtosis_deming_forresid)$optimized
# C1SC_kurtosis$filter = 'kurtosis'
# 
# C1_kurtosis_deming
# cor(C1SC_kurtosis$surface_temp_median, C1SC_kurtosis$is_temp_med)


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
  # full_join(., C1SC_kurtosis) %>% 
  full_join(., C2ST_kurtosis) %>% 
  mutate(collection = as.factor(collection),
         filter = as.factor(filter))

# Figure 3: Plot Deming regression for all filters ####
Fig3_a <- ggplot(C1SC, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C1
  geom_abline(intercept = C1_deming$coefficients[1], slope = C1_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,1], slope = C1_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C1_deming$ci[1,2], slope = C1_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C1SC$surface_temp_median, C1SC$is_temp_med), digits = 3)),
            x = 0,
            y = 25,
            size = 3.5,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C1SC$date)),
            x = 0,
            y = 23,
            size = 3.5,
            hjust = 0)+
  geom_text(label = paste0('y = ', 
                           round(C1_deming$coefficients[1], digits = 2), 
                           ' + ', 
                           round(C1_deming$coefficients[2], digits = 2), 
                           ' * x'),
            x = 0,
            y = 27,
            size = 3.5,
            hjust = 0) +
  labs(x = NULL,
       y = NULL,
       title = 'Collection 1',
       subtitle = 'single-channel algorithm') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
Fig3_a

Fig3_b <- ggplot(C2ST, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2
  geom_abline(intercept = C2_deming$coefficients[1], slope = C2_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,1], slope = C2_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_deming$ci[1,2], slope = C2_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST$surface_temp_median, C2ST$is_temp_med), digits = 3)),
            x = 0,
            y = 25,
            size = 3.5,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST$date)),
            x = 0,
            y = 23,
            size = 3.5,
            hjust = 0)+
  geom_text(label = paste0('y = ', 
                           round(C2_deming$coefficients[1], digits = 2), 
                           ' + ', 
                           round(C2_deming$coefficients[2], digits = 2), 
                           ' * x'),
            x = 0,
            y = 27,
            size = 3.5,
            hjust = 0) +
  labs(x = NULL,
       y = NULL,
       title = 'Collection 2',
       subtitle = 'surface temperature product') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
Fig3_b

# Fig3_c <- ggplot(C1SC_kurtosis, aes(x = is_temp_med, y = surface_temp_median)) +
#   geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
#   geom_point() +
#   #add deming regression and prediction intervals for C2 filtered for sub zero and kurtosis
#   geom_abline(intercept = C1_kurtosis_deming$coefficients[1], slope = C1_kurtosis_deming$coefficients[2], size = 0.75) +
#   geom_abline(intercept = C1_kurtosis_deming$ci[1,1], slope = C1_kurtosis_deming$ci[2,1], linetype = 3, size = 0.75) +
#   geom_abline(intercept = C1_kurtosis_deming$ci[1,2], slope = C1_kurtosis_deming$ci[2,2], linetype = 3, size = 0.75) +
#   geom_text(label = paste0('r = ', round(cor(C1SC_kurtosis$surface_temp_median, C1SC_kurtosis$is_temp_med), digits = 3)),
#             x = 2,
#             y = 25,
#             size = 4,
#             hjust = 0)+
#   geom_text(label = paste0('n = ', length(C1SC_kurtosis$date)),
#             x = 2,
#             y = 23,
#             size = 4,
#             hjust = 0)+
#   labs(x = '',
#        y = '\n',
#        title = 'Collection 1',
#        subtitle = 'kurtosis filter') +
#   final_theme +
#   coord_cartesian(xlim = c(0, 27),
#                   ylim = c(0, 27))
# Fig3_c

Fig3_d <- ggplot(C2ST_kurtosis, aes(x = is_temp_med, y = surface_temp_median)) +
  geom_abline(slope = 1, intercept = 0, color = '#006cd1', size = 0.75) +
  geom_point() +
  #add deming regression and prediction intervals for C2 filtered for sub zero and kurtosis
  geom_abline(intercept = C2_kurtosis_deming$coefficients[1], slope = C2_kurtosis_deming$coefficients[2], size = 0.75) +
  geom_abline(intercept = C2_kurtosis_deming$ci[1,1], slope = C2_kurtosis_deming$ci[2,1], linetype = 3, size = 0.75) +
  geom_abline(intercept = C2_kurtosis_deming$ci[1,2], slope = C2_kurtosis_deming$ci[2,2], linetype = 3, size = 0.75) +
  geom_text(label = paste0('r = ', round(cor(C2ST_kurtosis$surface_temp_median, C2ST_kurtosis$is_temp_med), digits = 3)),
            x = 0,
            y = 25,
            size = 3.5,
            hjust = 0)+
  geom_text(label = paste0('n = ', length(C2ST_kurtosis$date)),
            x = 0,
            y = 23,
            size = 3.5,
            hjust = 0)+
  geom_text(label = paste0('y = ', 
                           round(C2_kurtosis_deming$coefficients[1], digits = 2), 
                           ' + ', 
                           round(C2_kurtosis_deming$coefficients[2], digits = 2), 
                           ' * x'),
            x = 0,
            y = 27,
            size = 3.5,
            hjust = 0) +
  labs(x = NULL,
       y = NULL,
       title = 'Collection 2',
       subtitle = 'kurtosis filter') +
  final_theme +
  coord_cartesian(xlim = c(0, 27),
                  ylim = c(0, 27))
Fig3_d


Fig3_plot <- plot_grid(Fig3_a, Fig3_b, 
                       #Fig3_c, 
                       Fig3_d,
                   # ncol = 2,
                   ncol = 3, 
                   labels = c('a', 'b','c'),
                   label_size = 12,
                   label_x = 0.07)

Fig3_plot

x_lab = ggdraw() + draw_label(label = expression(bold(paste(italic('in situ'), ' median water temp (deg C)'))),
                      fontface = 'bold', size = 12)

y_lab = ggdraw() + draw_label(label = 'Landsat median water temp (deg C)',
                              fontface = 'bold',
                              angle =90,
                              size = 12)

Fig3_label = plot_grid(y_lab, Fig3_plot,
                       NULL, x_lab,
                       ncol = 2,
                       rel_widths = c(0.07, 1),
                       rel_heights = c(1, 0.06))

Fig3_label

ggsave(file.path(fig_dir, 'Figure3_deming_filters.jpg'), 
       dpi = 300,
       height = 3,
       width = 7.5,
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



# Supplemental Table C and D Calculate error for various models ####
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

alldata_error <- full_join(alldata_error, mission_error) %>% 
  mutate(filter = factor(filter, levels = c('none', 'kurtosis'))) %>% 
  arrange(collection, filter, variable) 

alldata_error %>% 
  select(-month) %>% 
  write.csv(., file.path(figdir, 'STC_deming_performance_bymission.csv'), row.names = F)

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

month_error <- full_join(month_mission_error, month_error) %>% 
  ungroup() %>% 
  mutate(filter = factor(filter, levels = c('none', 'kurtosis'))) %>% 
  arrange(month, collection, filter, variable) %>% 
  filter(filter == 'kurtosis') %>% 
  select(month, variable, `All missions`, `LS 5`, `LS 7`, `LS 8`)

write.csv(month_error, file.path(figdir, 'STD_deming_kurtosis_performancebymonth.csv'), row.names = F)

# Figure 4 create point (by mission for bias, mae, rmse) faceted by model ####

head(month_error)

kurtosis_biasmae <- month_error %>% 
  filter((variable == 'mae' | variable == 'bias')) %>% 
  select(-`All missions`) %>% 
  pivot_longer(cols = c(`LS 5`:`LS 8`),
               names_to = 'mission',
               values_to = 'value')

ggplot(kurtosis_biasmae, aes(x = month, y = value)) +
  geom_point(aes(color = mission)) +
  facet_grid(. ~ variable)+
  final_theme+
  scale_color_colorblind()

month <- (c('05', '06', '07', '08', '09', '10', '11'))
month_list <- as.data.frame(month)

Fig4_a <- kurtosis_biasmae %>% 
  filter(variable == 'bias') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  labs(y = 'bias (deg C)',
       x = NULL) +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  coord_cartesian(ylim = c(-1.2, 1.1)) +
  final_theme+
  theme(axis.title=element_text(size=10,face="bold")) +
  theme(legend.position = 'none') +
  scale_color_colorblind()
Fig4_a

Fig4_b <- kurtosis_biasmae %>% 
  filter(variable == 'mae') %>% 
  ggplot(., aes(x = month, y = value)) +
  geom_point(aes(shape = mission), size =2) +
  coord_cartesian(ylim = c(0, 1.5)) +
  geom_abline(intercept = 0,
              slope = 0, 
              lty = 2) +
  labs(y = 'mean absolute error (deg C)',
       x = NULL) +
  final_theme+
  theme(axis.title=element_text(size=10,face="bold")) +
  theme(legend.position = 'none') +
  scale_color_colorblind()
Fig4_b

forlegend <- kurtosis_biasmae %>% 
  filter(variable == 'mae') %>% 
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

kurtosis <- plot_grid(Fig4_a, Fig4_b,
                      ncol = 1)
kurtosis

x_lab = ggdraw() + draw_label(label = 'month', fontface = 'bold', size = 10)

Fig4_label = plot_grid(kurtosis, x_lab,
                        rel_heights = c(1, 0.05),
                       ncol = 1)

Fig4_label

plot_grid(Fig4_label, leg, rel_widths = c(0.9,0.15))

ggsave(file.path(fig_dir, 'Fig4_errorbymission.jpg'), 
       height = 4, width = 6, units = 'in', dpi = 300)

